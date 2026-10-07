{ config, lib, pkgs, ... }:

# metube downloads the best YouTube has (VP9/AV1, up to 4K/HDR), but the
# Raspberry Pi 5 that plays it only hardware-decodes HEVC.
#
# A path unit watches metube's completed.json (which metube replaces
# atomically). On each change, metube-transcode-enqueue queues a pueue job
# in the `transcode` group for every finished download it hasn't queued
# before; a ledger in $XDG_STATE_HOME remembers what's been queued, keyed
# by URL and download timestamp, so `pueue clean` or metube clearing its
# list doesn't cause re-queues. Each job (metube-transcode-one) re-encodes
# one file to 10-bit HEVC in place, and does nothing if the file is gone or
# already HEVC, so a stray duplicate is harmless.

let
  cfg = config.services.metubeTranscode;

  transcodeOne = pkgs.writeShellApplication {
    name = "metube-transcode-one";
    runtimeInputs = with pkgs; [ ffmpeg-headless coreutils ];
    text = ''
      src=''${1:?usage: metube-transcode-one <file>}

      if [ ! -e "$src" ]; then
        echo "gone, nothing to do: $src"
        exit 0
      fi

      codec=$(ffprobe -v error -select_streams V:0 -show_entries stream=codec_name \
                -of default=nw=1:nk=1 "$src")
      case "$codec" in
        hevc) echo "already HEVC: $src"; exit 0 ;;
        "")   echo "no video stream: $src" >&2; exit 1 ;;
      esac

      dir=$(dirname "$src")
      base=$(basename "''${src%.*}")
      out="$dir/$base.mkv"
      tmp="$dir/.$base.transcoding.mkv"
      trap 'rm -f "$tmp"' EXIT

      echo "$codec -> hevc: $src"
      # -map 0:V skips cover-art "video" streams; audio, subtitles and
      # metadata are copied. Colour/HDR tags carry over from the input.
      nice -n 19 ffmpeg -nostdin -hide_banner -loglevel warning -stats -y \
        -i "$src" \
        -map 0:V:0 -map '0:a?' -map '0:s?' \
        -c copy \
        -c:v libx265 -preset ${lib.escapeShellArg cfg.preset} -crf ${toString cfg.crf} \
        -pix_fmt yuv420p10le -x265-params log-level=error \
        "$tmp"

      touch -r "$src" "$tmp"
      mv -f "$tmp" "$out"
      if [ "$src" != "$out" ]; then
        rm -f "$src"
      fi
      echo "done: $out"
    '';
  };

  enqueue = pkgs.writeShellApplication {
    name = "metube-transcode-enqueue";
    runtimeInputs = with pkgs; [ pueue jq coreutils gnugrep util-linux ];
    text = ''
      completed=${lib.escapeShellArg cfg.completedJson}
      dldir=${lib.escapeShellArg cfg.downloadDir}
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

    downloadDir = lib.mkOption {
      type = lib.types.str;
      default = "/media/entertainment/yt";
      description = "metube's download directory, as seen from the host.";
    };

    completedJson = lib.mkOption {
      type = lib.types.str;
      default = "${cfg.downloadDir}/.metube/completed.json";
      description = "metube's completed-downloads state file.";
    };

    cleanInterval = lib.mkOption {
      type = lib.types.str;
      default = "*-*-* 03:00:00";
      description = "systemd OnCalendar for `pueue clean -s -g transcode`.";
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

  config = lib.mkIf (cfg.enable && pkgs.stdenv.isLinux) {
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

    # Successful transcodes are just noise in `pueue status`; failures stay.
    systemd.user.services.metube-transcode-clean = {
      Unit = {
        Description = "Clean successful transcode tasks out of pueue";
        After = [ "pueue.service" ];
      };
      Service = {
        Type = "oneshot";
        ExecStart = "${pkgs.pueue}/bin/pueue clean -s -g transcode";
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
