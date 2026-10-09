{ config, lib, pkgs, ... }:

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
    runtimeInputs = with pkgs; [ ffmpeg-headless coreutils util-linux gawk pueue jq ];
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

  config = lib.mkIf (cfg.enable && pkgs.stdenv.hostPlatform.isLinux) {
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
    };

    # Successful transcodes and deletions are just noise in `pueue status`;
    # failures stay. (transcode-cleanup only exists after the first one.)
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
  };
}
