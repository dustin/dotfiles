{ pkgs }:

# Mirrors every S3 bucket that has a directory under $base into that
# directory. Buckets to back up are chosen by creating the directory.
pkgs.writeShellApplication {
  name = "s3bak";
  runtimeInputs = [ pkgs.rclone ];
  text = ''
    base=/overflow/backups/cloud/s3

    cd "$base"

    for i in *
    do
        echo "Doing $i"
        rclone --checksum --s3-use-multipart-etag=true --stats-log-level=NOTICE --stats=30m sync "s3:$i" "$base/$i"
    done
  '';
}
