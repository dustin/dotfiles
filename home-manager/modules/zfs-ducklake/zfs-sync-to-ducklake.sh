#!/usr/bin/env bash
# zfs-sync-to-ducklake.sh
#
# Merges the latest rows from each per-host <hostname>.duckdb file
# (written by nixos-configs modules/zfs-manifest) into one schema inside
# your DuckLake. Incremental, not a full reload: for each (host, table),
# only rows with ts newer than whatever's already synced for that host
# get pulled in -- cheap on repeat runs. File listings are change-only
# since manifest schema v2 (listings + file_events); every listing's
# rows share its ts, and one host's listings are written one at a time,
# so ts works as the watermark there too.
#
# You provide your own ducklake connection details via --init-sql,
# since that's specific to how you've already got your ducklake set up
# (extension install, ATTACH, USE). This script doesn't guess at that
# syntax -- it just runs your init SQL first, then the schema file (if
# asked), then the generated merge statements, all in one duckdb session.
#
# Usage:
#   zfs-sync-to-ducklake --init-sql FILE [--manifest-dir DIR]
#                        [--catalog NAME] [--schema NAME]
#                        [--create-schema] [--rebuild] [--schema-sql FILE]
#
#   --init-sql FILE     Your own SQL to get the session to a working
#                        ducklake connection. Not modified or assumed
#                        beyond that. Default: ./init.sql
#   --catalog NAME       The name your init SQL's ATTACH gave the lake.
#                        Default: lake. Pass '' only if your init SQL
#                        already leaves the lake as the current catalog.
#   --schema NAME        Schema inside the lake to sync into. Default: z3
#   --manifest-dir DIR   Where to find *.duckdb files, one per host.
#                        Default: /Volumes/manifest
#   --create-schema      Run zfs-ducklake-schema.sql before syncing. Off
#                        by default -- on a DuckLake even a no-op DDL
#                        statement adds a catalog version. Pass it once
#                        for setup, or after the schema file changes.
#   --rebuild            Drop every table and view in the target schema
#                        that the schema file defines (plus the old
#                        `files` table), recreate them, and sync from
#                        scratch. Implies --create-schema. Needed once
#                        after the hosts move to manifest schema v2.
#                        Only touches --schema; another schema (e.g. an
#                        old `zfs` next to `z3`) is left alone -- drop it
#                        yourself once you don't need it:
#                          DROP SCHEMA lake.zfs CASCADE;
#   --schema-sql FILE    Path to zfs-ducklake-schema.sql.
#                        Default: alongside this script.
#
# A per-host db still at manifest schema v1 (has a `files` table, no
# `listings`) is skipped with a warning; run zfs-manifest-migrate on
# that host first.
#
# Requires: duckdb CLI (with whatever the ducklake extension needs
# available to it, per your init SQL).

set -euo pipefail

MANIFEST_DIR="/Volumes/manifest"
INIT_SQL="init.sql"
SCHEMA_SQL="$(dirname "$0")/zfs-ducklake-schema.sql"
CREATE_SCHEMA=false
REBUILD=false
CATALOG="lake"
SCHEMA="z3"

while [[ $# -gt 0 ]]; do
  case "$1" in
    --init-sql)      INIT_SQL="$2"; shift 2 ;;
    --manifest-dir)  MANIFEST_DIR="$2"; shift 2 ;;
    --schema-sql)    SCHEMA_SQL="$2"; shift 2 ;;
    --create-schema) CREATE_SCHEMA=true; shift ;;
    --rebuild)       REBUILD=true; CREATE_SCHEMA=true; shift ;;
    --catalog)       CATALOG="$2"; shift 2 ;;
    --schema)        SCHEMA="$2"; shift 2 ;;
    -h|--help)
      grep '^#' "$0" | sed 's/^# \{0,1\}//'; exit 0 ;;
    *) echo "Unknown argument: $1" >&2; exit 1 ;;
  esac
done

if [[ -z "$INIT_SQL" || ! -f "$INIT_SQL" ]]; then
  echo "ERROR: --init-sql FILE is required and must exist (your ducklake ATTACH/USE statements)." >&2
  exit 1
fi
if $CREATE_SCHEMA && [[ ! -f "$SCHEMA_SQL" ]]; then
  echo "ERROR: --create-schema was passed but no schema file found at $SCHEMA_SQL (pass --schema-sql to override)." >&2
  exit 1
fi

# Z is how every lake table reference gets written below.
if [[ -n "$CATALOG" ]]; then
  Z="${CATALOG}.${SCHEMA}"
else
  Z="${SCHEMA}"
fi

shopt -s nullglob
DB_FILES=("$MANIFEST_DIR"/*.duckdb)
shopt -u nullglob

if (( ${#DB_FILES[@]} == 0 )); then
  echo "No *.duckdb files found under $MANIFEST_DIR" >&2
  exit 1
fi

echo "Found ${#DB_FILES[@]} host database(s) under $MANIFEST_DIR, syncing into ${Z}:" >&2

SQL="$(cat "$INIT_SQL")
"

if $REBUILD; then
  echo "Rebuilding ${Z}: dropping its tables and views first." >&2
  SQL+="
DROP VIEW IF EXISTS ${Z}.latest_files_view;
DROP VIEW IF EXISTS ${Z}.latest_snapshots;
DROP VIEW IF EXISTS ${Z}.latest_datasets;
DROP TABLE IF EXISTS ${Z}.latest_files;
DROP TABLE IF EXISTS ${Z}.files;
DROP TABLE IF EXISTS ${Z}.file_events;
DROP TABLE IF EXISTS ${Z}.listings;
DROP TABLE IF EXISTS ${Z}.file_changes;
DROP TABLE IF EXISTS ${Z}.snapshots;
DROP TABLE IF EXISTS ${Z}.datasets;
"
fi

if $CREATE_SCHEMA; then
  # Targeted substitution: only "zfs." references and the one CREATE
  # SCHEMA line.
  SQL+="$(sed -e "s/zfs\./${Z}./g" \
              -e "s/CREATE SCHEMA IF NOT EXISTS zfs;/CREATE SCHEMA IF NOT EXISTS ${Z};/" \
              "$SCHEMA_SQL")
"
fi

# Watermark: rows newer than the newest already synced for this host.
newer() {
  local table="$1" host="$2"
  echo "s.ts > (SELECT coalesce(max(ts), TIMESTAMPTZ '-infinity') FROM ${Z}.${table} WHERE host = '${host}')"
}

for f in "${DB_FILES[@]}"; do
  host="$(basename "$f" .duckdb)"
  alias="src_${host//[^a-zA-Z0-9_]/_}"

  tables="$(duckdb -readonly -csv -noheader "$f" \
    "SELECT string_agg(table_name, ' ') FROM duckdb_tables() WHERE schema_name = 'main';" 2>/dev/null || true)"
  if [[ " $tables " != *" listings "* ]]; then
    echo "  ${host}: SKIPPED -- no listings table (still manifest schema v1? run zfs-manifest-migrate there first)." >&2
    continue
  fi
  echo "  ${host}" >&2

  SQL+="
ATTACH '${f}' AS ${alias} (READ_ONLY);
BEGIN TRANSACTION;

INSERT INTO ${Z}.datasets BY NAME
  SELECT * FROM ${alias}.datasets s WHERE $(newer datasets "$host");

INSERT INTO ${Z}.snapshots BY NAME
  SELECT * FROM ${alias}.snapshots s WHERE $(newer snapshots "$host");

INSERT INTO ${Z}.file_changes BY NAME
  SELECT * FROM ${alias}.file_changes s WHERE $(newer file_changes "$host");

INSERT INTO ${Z}.listings BY NAME
  SELECT * FROM ${alias}.listings s WHERE $(newer listings "$host");

INSERT INTO ${Z}.file_events BY NAME
  SELECT * FROM ${alias}.file_events s WHERE $(newer file_events "$host");

COMMIT;
DETACH ${alias};
"
done

SQL+="
-- Update the materialized latest files. Every row of a dataset's state
-- carries its latest listing's ts, so rows with an unchanged ts are
-- unchanged and left alone. (NOT MATCHED BY SOURCE instead of a separate
-- DELETE ... NOT EXISTS: that one hits a DuckDB binder bug against this
-- view.)
MERGE INTO ${Z}.latest_files AS t
USING (
    SELECT *, parse_filename(path) AS filename
    FROM ${Z}.latest_files_view
) AS s
ON t.host = s.host AND t.dataset = s.dataset AND t.path = s.path
WHEN MATCHED AND t.ts IS DISTINCT FROM s.ts THEN UPDATE SET *
WHEN NOT MATCHED THEN INSERT *
WHEN NOT MATCHED BY SOURCE THEN DELETE;
"

echo "$SQL" | duckdb

echo "Sync complete." >&2
