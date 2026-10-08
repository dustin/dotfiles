{ pkgs }:

# Mirrors every S3 bucket that has a directory under $base into that
# directory. Buckets to back up are chosen by creating the directory.
#
# Files are compared by size alone. With --checksum, any object whose
# ETag isn't the MD5 of its contents (e.g. SSE-KMS encrypted) never
# matches the local copy, so rclone re-fetches it on every run, which
# fails outright for objects in Glacier.
pkgs.writeShellApplication {
  name = "s3bak";
  runtimeInputs = [ pkgs.rclone ];
  text = ''
    base=/overflow/backups/cloud/s3

    cd "$base"

    for i in *
    do
        echo "Doing $i"
        rclone --size-only --stats-log-level=NOTICE --stats=30m sync "s3:$i" "$base/$i"
    done
  '';
}
