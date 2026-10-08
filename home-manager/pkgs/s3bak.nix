{ pkgs }:

# Mirrors every S3 bucket that has a directory under $base into that
# directory. Buckets to back up are chosen by creating the directory.
#
# Objects in GLACIER or DEEP_ARCHIVE can't be downloaded until they're
# restored. Any such object missing locally gets a Bulk restore
# requested and is left out of the sync until the restore finishes; a
# later run then fetches it, and the restored copy expires back into
# Glacier after $lifetime days.
pkgs.writeShellApplication {
  name = "s3bak";
  runtimeInputs = [ pkgs.rclone pkgs.jq pkgs.coreutils pkgs.gawk pkgs.gnused ];
  text = ''
    base=/overflow/backups/cloud/s3
    lifetime=7

    export LC_ALL=C

    tmp=$(mktemp -d)
    trap 'rm -rf "$tmp"' EXIT

    cd "$base"

    for i in *
    do
        echo "Doing $i"

        # Archived objects that we don't have a local copy of.
        rclone lsf -R --files-only --format Tp --separator $'\t' "s3:$i" \
            | awk -F'\t' '$1 == "GLACIER" || $1 == "DEEP_ARCHIVE"' \
            | cut -f2- | sort > "$tmp/archived"
        rclone lsf -R --files-only "$base/$i" | sort > "$tmp/local"
        comm -23 "$tmp/archived" "$tmp/local" > "$tmp/missing"

        : > "$tmp/exclude"
        if [ -s "$tmp/missing" ]
        then
            rclone backend restore-status "s3:$i" > "$tmp/status.json"
            jq -r '(. // [])[] | select(.RestoreStatus.IsRestoreInProgress == false) | .Remote' \
                "$tmp/status.json" | sort > "$tmp/ready"
            jq -r '(. // [])[] | select(.RestoreStatus.IsRestoreInProgress == true) | .Remote' \
                "$tmp/status.json" | sort > "$tmp/restoring"

            # Not fetchable yet: everything missing that isn't restored.
            comm -23 "$tmp/missing" "$tmp/ready" > "$tmp/pending"
            comm -23 "$tmp/pending" "$tmp/restoring" > "$tmp/request"

            if [ -s "$tmp/request" ]
            then
                echo "Requesting restore of $(wc -l < "$tmp/request") archived objects in $i"
                rclone backend restore "s3:$i" --files-from-raw "$tmp/request" \
                    -o priority=Bulk -o lifetime="$lifetime" \
                    | jq -r '(. // [])[] | select(.Status != "OK") | "restore \(.Remote): \(.Status)"'
            fi

            if [ -s "$tmp/pending" ]
            then
                echo "Skipping $(wc -l < "$tmp/pending") archived objects in $i until restored"
                # Anchor each path and escape filter glob characters.
                sed -e 's/[][*?{}\\]/\\&/g' -e 's|^|/|' "$tmp/pending" > "$tmp/exclude"
            fi
        fi

        rclone --checksum --s3-use-multipart-etag=true --stats-log-level=NOTICE --stats=30m \
            --exclude-from "$tmp/exclude" sync "s3:$i" "$base/$i"
    done
  '';
}
