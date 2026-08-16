#!/usr/bin/env bash

set -euo pipefail

RCLONE_REMOTE="s3"
BUCKET="${BUCKET:-backup.west.spy.net}"
PREFIX="${PREFIX:?Set PREFIX to the backup object prefix for this machine}"
TIMESTAMP="$(date -u +%Y%m%dT%H%M%SZ)"
FORCE_FULL="${FORCE_FULL:-}"
AGE_RECIPIENT="${AGE_RECIPIENT:?Set AGE_RECIPIENT to your age public key (age1...)}"

if [[ $# -eq 0 ]]; then
  echo "Usage: $0 dataset [dataset...]" >&2
  exit 1
fi
DATASETS=("$@")

for ds in "${DATASETS[@]}"; do
  key_name="${ds//\//-}"
  snap="${ds}@s3bak-${TIMESTAMP}"

  echo "==> Creating snapshot: ${snap}"
  zfs snapshot "${snap}"

  prev_snap=""
  if [[ -z "${FORCE_FULL}" ]]; then
    # List what's already up there for this dataset and pull the newest
    # timestamp out of the filenames (they sort lexically, so plain
    # `sort` works). Object names look like:
    #   <key_name>-<timestamp>-full.zfs.zst
    #   <key_name>-<timestamp>-incr.zfs.zst
    last_ts="$(
      rclone lsf "${RCLONE_REMOTE}:${BUCKET}/${PREFIX}/${key_name}/" 2>/dev/null \
        | grep -E "^${key_name}-[0-9]{8}T[0-9]{6}Z-(full|incr)\.zfs\.age\$" \
        | sed -E "s/^${key_name}-([0-9]{8}T[0-9]{6}Z)-(full|incr)\.zfs\.age\$/\1/" \
        | sort \
        | tail -n1 || true
    )"

    if [[ -n "${last_ts}" ]]; then
      candidate="${ds}@s3bak-${last_ts}"
      if zfs list -H -o name -t snapshot "${candidate}" >/dev/null 2>&1; then
        prev_snap="${candidate}"
      else
        echo "==> Last remote snapshot ${candidate} no longer exists locally, falling back to full send"
      fi
    fi
  fi

  if [[ -n "${prev_snap}" ]]; then
    mode="incr"
    object_key="${PREFIX}/${key_name}/${key_name}-${TIMESTAMP}-incr.zfs.age"
  else
    mode="full"
    object_key="${PREFIX}/${key_name}/${key_name}-${TIMESTAMP}-full.zfs.age"
  fi
  dest="${RCLONE_REMOTE}:${BUCKET}/${object_key}"

  if [[ "${mode}" == "incr" ]]; then
    echo "==> Sending ${prev_snap} -> ${snap} (raw/encrypted incremental, age-wrapped) -> ${dest}"
    zfs send -w -i "${prev_snap}" "${snap}" \
      | age -r "${AGE_RECIPIENT}" \
      | rclone rcat -P --s3-chunk-size 256M "${dest}"
  else
    echo "==> Sending ${snap} (raw/encrypted full, age-wrapped) -> ${dest}"
    zfs send -w "${snap}" \
      | age -r "${AGE_RECIPIENT}" \
      | rclone rcat -P --s3-chunk-size 256M "${dest}"
  fi

  echo "==> Done: ${dest}"

  # Only now that the upload has succeeded: drop the old reference
  # snapshot. We keep the one we just created (it's the new chain head
  # and next run will discover it by listing the bucket).
  if [[ -n "${prev_snap}" && "${prev_snap}" != "${snap}" ]]; then
    echo "==> Destroying superseded local snapshot: ${prev_snap}"
    zfs destroy "${prev_snap}"
  fi
  echo
done

echo "All datasets sent."
