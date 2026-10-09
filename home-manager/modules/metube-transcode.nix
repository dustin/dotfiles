{ config, lib, pkgs, hostname, ... }:

# metube downloads the best YouTube has (VP9/AV1, up to 4K/HDR) into
# inputDir, but the Raspberry Pi 5 that plays it only hardware-decodes HEVC.
#
# A path unit watches metube's completed.json (which metube replaces
# atomically). On each change, metube-transcode-enqueue queues a pueue job
# in the `transcode` group for every finished download it hasn't queued
# before; a ledger in $XDG_STATE_HOME remembers what's been queued, keyed
# by URL and download timestamp, so `pueue clean` or metube clearing its
# list doesn't cause re-queues.
#
# Each job (metube-transcode-one) encodes one input file to 10-bit HEVC in
# a temp file under outputDir, checks the result, and only then moves it
# into place at the same relative path. The original is parked under
# inputDir/.done and a delayed task in pueue's `transcode-cleanup` group
# deletes it after keepInputDays. Inputs that are already HEVC are just
# linked into place. A missing input is a no-op, so duplicates are harmless.

let
  cfg = config.services.metubeTranscode;

  transcodeOne = pkgs.writeShellApplication {
    name = "metube-transcode-one";
    runtimeInputs = with pkgs; [ ffmpeg-headless coreutils util-linux gawk pueue jq mosquitto ];
    text = ''
      src=''${1:?usage: metube-transcode-one <file>}
      indir=${lib.escapeShellArg cfg.inputDir}
      outdir=${lib.escapeShellArg cfg.outputDir}
      keep=${toString (cfg.keepInputDays * 86400)}

      if [ ! -e "$src" ]; then
        echo "gone, nothing to do: $src"
        exit 0
      fi
      src=$(realpath "$src")

      # A file from metube's input dir lands at the same relative path under
      # the output dir; anything else is transcoded where it sits.
      case "$src" in
        "$indir"/*) rel=''${src#"$indir"/}; dest="$outdir/$(dirname "$rel")" ;;
        *)          dest=$(dirname "$src") ;;
      esac
      dest=''${dest%/.}
      base=$(basename "''${src%.*}")
      out="$dest/$base.mkv"

      # One run per file at a time, however it was queued or run.
      lockdir="''${XDG_RUNTIME_DIR:-/tmp}/metube-transcode"
      mkdir -p "$lockdir"
      exec 8>"$lockdir/$(printf '%s' "$src" | sha256sum | cut -c1-32).lock"
      if ! flock -n 8; then
        echo "already being transcoded elsewhere: $src" >&2
        exit 1
      fi

      # pueue runs commands through a shell; single-quote the path.
      quote() { printf "'%s'" "''${1//\'/\'\\\'\'}"; }

      # Park the original under $indir/.done and queue its deletion in
      # `keep` seconds, in pueue's transcode-cleanup group.
      retire() {
        local parked
        parked=$(mktemp -d "$indir/.done/XXXXXXXX")
        mv "$src" "$parked/"
        if ! pueue group --json | jq -e 'has("transcode-cleanup")' >/dev/null; then
          pueue group add transcode-cleanup
        fi
        pueue add -g transcode-cleanup -d "$keep" -l "delete $(basename "$src")" \
          -- rm -rf "$(quote "$parked")"
        echo "original kept in $parked for $((keep / 86400)) days"
      }

      mkdir -p "$dest" "$indir/.done"

      codec=$(ffprobe -v error -select_streams V:0 -show_entries stream=codec_name \
                -of default=nw=1:nk=1 "$src")
      case "$codec" in
        "") echo "no video stream: $src" >&2; exit 1 ;;
        hevc)
          if [ "$dest" = "$(dirname "$src")" ]; then
            echo "already HEVC: $src"
            exit 0
          fi
          if [ -e "$out" ]; then
            echo "already exists, leaving input alone: $out" >&2
            exit 1
          fi
          echo "already HEVC, moving into place: $out"
          ln "$src" "$out" 2>/dev/null || cp -p "$src" "$out"
          retire
          exit 0
          ;;
      esac

      if [ -e "$out" ] && [ "$out" != "$src" ]; then
        echo "already exists, leaving input alone: $out" >&2
        exit 1
      fi

      tmp=$(mktemp "$dest/.$base.XXXXXX.transcoding.mkv")
      trap 'rm -f "$tmp"' EXIT

      duration() {
        ffprobe -v error -show_entries format=duration -of default=nw=1:nk=1 "$1"
      }

${if cfg.encodeHost == null then ''
      echo "$codec -> hevc: $src"
      # -map 0:V skips cover-art "video" streams; audio, subtitles and
      # metadata are copied. Colour/HDR tags carry over from the input.
      nice -n 19 ffmpeg -nostdin -hide_banner -loglevel warning -stats -y \
        -i "$src" \
        -map 0:V:0 -map '0:a?' -map '0:s?' \
        -c:a copy -c:s copy \
        -c:v libx265 -preset ${lib.escapeShellArg cfg.preset} -crf ${toString cfg.crf} \
        -pix_fmt yuv420p10le -x265-params log-level=error \
        "$tmp"
'' else ''
      # Encode on ${cfg.encodeHost}: ask its worker over MQTT to read the
      # source from copyparty and upload the result to the staging volume.
      mqtt=${lib.escapeShellArg cfg.mqttHost}
      host=${lib.escapeShellArg cfg.encodeHost}
      cp_url=${lib.escapeShellArg cfg.copypartyUrl}
      staging="$indir/.staging"

      urlpath() { jq -rn --arg p "$1" '$p | split("/") | map(@uri) | join("/")'; }
      case "$src" in
        "$indir"/*)  src_url="$cp_url/yt-input/$(urlpath "''${src#"$indir"/}")" ;;
        "$outdir"/*) src_url="$cp_url/yt/$(urlpath "''${src#"$outdir"/}")" ;;
        *) echo "not reachable through copyparty: $src" >&2; exit 1 ;;
      esac
      id=$(uuidgen)
      put_url="$cp_url/yt-staging/$id.mkv"
      reply="transcode/reply/$id"

      work=$(mktemp -d)
      mkfifo "$work/sub"
      mosquitto_sub -h "$mqtt" -v -t "$reply" -t "transcode/ping/$id" > "$work/sub" &
      sub=$!
      exec 3< "$work/sub"
      trap 'kill "$sub" 2>/dev/null || true; rm -rf "$work"; rm -f "$tmp" "$staging/$id.mkv"' EXIT

      # Don't publish the request until our subscription is live: wait to
      # hear our own ping back.
      live=""
      for _ in $(seq 40); do
        mosquitto_pub -h "$mqtt" -t "transcode/ping/$id" -m ping
        if IFS= read -r -t 0.5 line <&3 && [ "''${line%% *}" = "transcode/ping/$id" ]; then
          live=1
          break
        fi
      done
      [ -n "$live" ] || { echo "couldn't subscribe on $mqtt" >&2; exit 1; }

      echo "$codec -> hevc on $host: $src"
      mosquitto_pub -h "$mqtt" -q 1 -t "transcode/$host/req" -m "$(
        jq -cn --arg id "$id" --arg src "$src_url" --arg put "$put_url" --arg reply "$reply" \
          --argjson sent "$(date +%s)" '{id: $id, src: $src, put: $put, reply: $reply, sent: $sent}')"

      # The worker acks at once, then reports progress at least every
      # few minutes until it's done or has failed.
      wait_for=30
      while :; do
        if ! IFS= read -r -t "$wait_for" line <&3; then
          if [ "$wait_for" = 30 ]; then
            echo "no answer from $host; is it awake?" >&2
          else
            echo "$host went quiet for ''${wait_for}s; giving up" >&2
          fi
          exit 1
        fi
        [ "''${line%% *}" = "$reply" ] || continue
        msg=''${line#* }
        case "$(jq -r .status <<<"$msg")" in
          accepted) echo "$host accepted"; wait_for=${toString cfg.stallSeconds} ;;
          progress) jq -r '"\(.time) at \(.speed)"' <<<"$msg" ;;
          done)     echo "$host done"; break ;;
          failed)   echo "$host failed: $(jq -r .error <<<"$msg")" >&2; exit 1 ;;
        esac
      done

      if [ ! -s "$staging/$id.mkv" ]; then
        echo "$host says done but nothing usable was uploaded" >&2
        exit 1
      fi
      # The upload belongs to copyparty; take a copy we own.
      cp --reflink=auto "$staging/$id.mkv" "$tmp"
      rm -f "$staging/$id.mkv"
''}
      # Only a good transcode goes into the tree: non-empty, readable, and
      # (when the source knows its length) within 2% of its duration.
      if [ ! -s "$tmp" ]; then
        echo "transcode produced an empty file; keeping original: $src" >&2
        exit 1
      fi
      want=$(duration "$src" || true)
      got=$(duration "$tmp" || true)
      if [ -z "$got" ] || [ "$got" = "N/A" ]; then
        echo "can't read duration of transcode; keeping original: $src" >&2
        exit 1
      fi
      if [ -n "$want" ] && [ "$want" != "N/A" ] &&
         ! awk -v w="$want" -v g="$got" 'BEGIN { exit !(g >= w * 0.98) }'; then
        echo "transcode is ''${got}s but source is ''${want}s; keeping original: $src" >&2
        exit 1
      fi

      # mktemp makes it 0600; match the original.
      chmod --reference="$src" "$tmp"
      touch -r "$src" "$tmp"
      if [ "$out" = "$src" ]; then
        # In-place .mkv: park the original before taking its name.
        retire
        mv "$tmp" "$out"
      else
        mv "$tmp" "$out"
        retire
      fi
      echo "done: $out"
    '';
  };

  enqueue = pkgs.writeShellApplication {
    name = "metube-transcode-enqueue";
    runtimeInputs = with pkgs; [ pueue jq coreutils gnugrep util-linux ];
    text = ''
      completed=${lib.escapeShellArg cfg.completedJson}
      dldir=${lib.escapeShellArg cfg.inputDir}
      state="''${XDG_STATE_HOME:-$HOME/.local/state}/metube-transcode"
      ledger="$state/queued"

      mkdir -p "$state"
      touch "$ledger"
      exec 9>"$state/lock"
      flock 9

      [ -e "$completed" ] || exit 0

      if ! pueue group --json | jq -e 'has("transcode")' >/dev/null; then
        pueue group add transcode
        pueue parallel 1 -g transcode
      fi

      # pueue runs the command through a shell; single-quote the path.
      quote() { printf "'%s'" "''${1//\'/\'\\\'\'}"; }

      jq -r '.items[]
             | select(.info.status == "finished" and .info.download_type == "video" and .info.filename != null)
             | [ "\(.key) \(.info.timestamp)", (.info.title // .key), (.info.folder // ""), .info.filename ]
             | join("\u001f")' "$completed" |
      while IFS=$'\x1f' read -r id title folder filename; do
        grep -qxF -- "$id" "$ledger" && continue
        path="$dldir/''${folder%/}/$filename"
        path=''${path//\/\//\/}
        pueue add -g transcode -l "$title" -- ${lib.getExe transcodeOne} "$(quote "$path")"
        echo "$id" >> "$ledger"
      done
    '';
  };
  # Runs on the encoding Mac: takes requests from MQTT, reads the source
  # from copyparty, encodes with VideoToolbox, uploads the result back.
  worker = pkgs.writeShellApplication {
    name = "metube-transcode-worker";
    runtimeInputs = with pkgs; [ ffmpeg-headless coreutils jq mosquitto curl ];
    text = ''
      mqtt=${lib.escapeShellArg cfg.mqttHost}
      name=${lib.escapeShellArg cfg.worker.name}
      pwfile=${lib.escapeShellArg cfg.worker.passwordFile}
      quality=${toString cfg.worker.quality}

      handle() {
        local req=$1 id src put reply sent pw work started last
        id=$(jq -r .id <<<"$req")
        src=$(jq -r .src <<<"$req")
        put=$(jq -r .put <<<"$req")
        reply=$(jq -r .reply <<<"$req")
        sent=$(jq -r '.sent // 0' <<<"$req")

        case "$reply" in
          transcode/reply/*) ;;
          *) echo "ignoring request with odd reply topic: $reply" >&2; return 0 ;;
        esac
        # Requests that sat in the pipe while we were busy have long since
        # given up on us.
        if [ $(( $(date +%s) - sent )) -gt 60 ]; then
          echo "ignoring stale request $id" >&2
          return 0
        fi

        pub() { mosquitto_pub -h "$mqtt" -t "$reply" -m "$1"; }
        fail() {
          echo "$id failed: $1" >&2
          pub "$(jq -cn --arg e "$1" '{status: "failed", error: $e}')"
        }

        pub '{"status":"accepted"}'
        echo "$id: $src"
        pw=$(cat "$pwfile")
        work=$(mktemp -d)

        started=$(date +%s)
        last=0
        # -map 0:V skips cover-art "video" streams; audio, subtitles and
        # metadata are copied. Colour/HDR tags carry over from the input.
        if ! ffmpeg -nostdin -hide_banner -loglevel error -nostats -y \
            -headers "PW: $pw"$'\r\n' -i "$src" \
            -map 0:V:0 -map '0:a?' -map '0:s?' \
            -c:a copy -c:s copy \
            -c:v hevc_videotoolbox -q:v "$quality" -profile:v main10 -pix_fmt p010le \
            -progress pipe:1 "$work/out.mkv" 2>"$work/err" |
          while IFS='=' read -r k v; do
            case "$k" in
              out_time) t=$v ;;
              speed) sp=$v ;;
              progress)
                now=$(date +%s)
                if [ $((now - last)) -ge 30 ]; then
                  last=$now
                  pub "$(jq -cn --arg t "''${t:-?}" --arg s "''${sp:-?}" '{status: "progress", time: $t, speed: $s}')"
                fi ;;
            esac
          done
        then
          fail "ffmpeg: $(tail -n 3 "$work/err" | tr '\n' ' ')"
          rm -rf "$work"
          return 0
        fi
        echo "$id encoded in $(( $(date +%s) - started ))s, uploading"

        if ! curl -fsS -o /dev/null -H "PW: $pw" -T "$work/out.mkv" "$put" 2>"$work/err"; then
          fail "upload: $(cat "$work/err")"
          rm -rf "$work"
          return 0
        fi
        rm -rf "$work"
        pub '{"status":"done"}'
        echo "$id done"
      }

      mosquitto_sub -h "$mqtt" -t "transcode/$name/req" |
        while IFS= read -r req; do
          [ -n "$req" ] || continue
          handle "$req" || true
        done
    '';
  };

in
{
  options.services.metubeTranscode = {
    enable = lib.mkEnableOption "queueing pueue jobs to transcode metube downloads to HEVC";

    inputDir = lib.mkOption {
      type = lib.types.str;
      default = "/media/entertainment/yt-input";
      description = ''
        metube's download directory, as seen from the host. Must be on the
        same filesystem as outputDir, so moving files between them is a rename.
      '';
    };

    outputDir = lib.mkOption {
      type = lib.types.str;
      default = "/media/entertainment/yt";
      description = "Where verified transcodes land, mirroring inputDir's layout.";
    };

    keepInputDays = lib.mkOption {
      type = lib.types.int;
      default = 7;
      description = "Days to keep an original after its transcode is in place.";
    };

    completedJson = lib.mkOption {
      type = lib.types.str;
      default = "${cfg.inputDir}/.metube/completed.json";
      description = "metube's completed-downloads state file.";
    };

    cleanInterval = lib.mkOption {
      type = lib.types.str;
      default = "*-*-* 03:00:00";
      description = "systemd OnCalendar for cleaning successful tasks out of pueue.";
    };

    encodeHost = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      example = "dsstudio";
      description = ''
        Worker name to send encodes to over MQTT (see `worker`). null encodes
        locally with x265.
      '';
    };

    mqttHost = lib.mkOption {
      type = lib.types.str;
      default = "mqtt";
      description = "MQTT broker the job and worker talk through.";
    };

    copypartyUrl = lib.mkOption {
      type = lib.types.str;
      default = "http://bee2:8808";
      description = "copyparty serving /yt-input and /yt (read) and /yt-staging (write).";
    };

    stallSeconds = lib.mkOption {
      type = lib.types.int;
      default = 900;
      description = "Give up on a remote encode that has been silent this long.";
    };

    worker = {
      enable = lib.mkEnableOption "the remote encode worker (VideoToolbox, macOS)";

      name = lib.mkOption {
        type = lib.types.str;
        default = hostname;
        description = "Name this worker takes requests for: transcode/<name>/req.";
      };

      passwordFile = lib.mkOption {
        type = lib.types.str;
        default = "${config.home.homeDirectory}/.config/sops-nix/secrets/copyparty-password";
        description = "File holding the copyparty password.";
      };

      quality = lib.mkOption {
        type = lib.types.int;
        default = 65;
        description = "hevc_videotoolbox -q:v (1-100, higher is better and bigger).";
      };
    };

    crf = lib.mkOption {
      type = lib.types.int;
      default = 22;
      description = "x265 CRF; lower is bigger and better.";
    };

    preset = lib.mkOption {
      type = lib.types.str;
      default = "medium";
      description = "x265 preset; slower is smaller at the same quality.";
    };
  };

  config = lib.mkMerge [ (lib.mkIf (cfg.enable && pkgs.stdenv.hostPlatform.isLinux) {
    home.packages = [ enqueue transcodeOne ];

    systemd.user.services.metube-transcode-enqueue = {
      Unit = {
        Description = "Queue pueue transcodes for finished metube downloads";
        After = [ "pueue.service" ];
        Wants = [ "pueue.service" ];
      };
      Service = {
        Type = "oneshot";
        ExecStart = lib.getExe enqueue;
      };
      # Also catch up at login/boot; the ledger makes reruns harmless.
      Install.WantedBy = [ "default.target" ];
    };

    # Successful transcodes and deletions are just noise in `pueue status`;
    # failures stay. (transcode-cleanup only exists after the first one.)
    # Also sweeps up after runs that were interrupted.
    systemd.user.services.metube-transcode-clean = {
      Unit = {
        Description = "Clean successful transcode tasks out of pueue";
        After = [ "pueue.service" ];
      };
      Service = {
        Type = "oneshot";
        ExecStart = [
          "${pkgs.pueue}/bin/pueue clean -s -g transcode"
          "-${pkgs.pueue}/bin/pueue clean -s -g transcode-cleanup"
          # Debris from interrupted runs: half-written transcodes, and
          # uploads that arrived after bee2 had given up on them.
          "-${pkgs.findutils}/bin/find ${lib.escapeShellArg cfg.outputDir} -name '.*.transcoding.mkv' -mtime +1 -delete"
          "-${pkgs.findutils}/bin/find ${lib.escapeShellArg "${cfg.inputDir}/.staging"} -type f -mtime +1 -delete"
        ];
      };
    };

    systemd.user.timers.metube-transcode-clean = {
      Unit.Description = "Periodically clean successful transcode tasks";
      Timer = {
        OnCalendar = cfg.cleanInterval;
        Persistent = true;
      };
      Install.WantedBy = [ "timers.target" ];
    };

    systemd.user.paths.metube-transcode-enqueue = {
      Unit.Description = "Watch metube's completed.json";
      Path.PathChanged = cfg.completedJson;
      Install.WantedBy = [ "default.target" ];
    };
  })

  (lib.mkIf cfg.worker.enable {
    home.packages = [ worker ];

    # Create with `sops secrets/copyparty-password.sops.yaml`, holding
    # `copyparty-password: <dustin's copyparty password>`.
    sops.secrets.copyparty-password = lib.mkIf (builtins.pathExists ../secrets/copyparty-password.sops.yaml) {
      sopsFile = ../secrets/copyparty-password.sops.yaml;
      path = cfg.worker.passwordFile;
    };

    launchd.agents.metube-transcode-worker = {
      enable = true;
      config = {
        Label = "net.spy.metube-transcode-worker";
        ProgramArguments = [ (lib.getExe worker) ];
        RunAtLoad = true;
        KeepAlive = true;
        ThrottleInterval = 30;
        StandardOutPath = "${config.xdg.stateHome}/metube-transcode-worker/stdout.log";
        StandardErrorPath = "${config.xdg.stateHome}/metube-transcode-worker/stderr.log";
      };
    };
  }) ];
}
