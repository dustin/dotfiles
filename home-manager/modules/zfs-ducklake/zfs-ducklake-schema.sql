-- zfs-ducklake-schema.sql
--
-- Lake-side schema for zfs-sync-to-ducklake.sh. Every "zfs." below is
-- rewritten to "<catalog>.<schema>." by the sync script (--catalog,
-- --schema), so views end up fully qualified. Only run with
-- --create-schema / --rebuild: on a DuckLake even a no-op DDL statement
-- adds a catalog version.
--
-- Mirrors the per-host tables (nixos-configs
-- modules/zfs-manifest/zfs-manifest-schema.sql, schema v2): file
-- listings are change-only, listings + file_events. No primary keys --
-- DuckLake doesn't support them.

CREATE SCHEMA IF NOT EXISTS zfs;

CREATE TABLE IF NOT EXISTS zfs.datasets (
  host VARCHAR, ts TIMESTAMPTZ, pool VARCHAR, name VARCHAR, type VARCHAR,
  used BIGINT, avail BIGINT, refer BIGINT, mountpoint VARCHAR, origin VARCHAR
);

CREATE TABLE IF NOT EXISTS zfs.snapshots (
  host VARCHAR, ts TIMESTAMPTZ, pool VARCHAR, dataset VARCHAR, name VARCHAR,
  creation TIMESTAMPTZ, used BIGINT, refer BIGINT
);

CREATE TABLE IF NOT EXISTS zfs.file_changes (
  host VARCHAR, ts TIMESTAMPTZ, pool VARCHAR, dataset VARCHAR,
  from_snap VARCHAR, to_snap VARCHAR, change_type VARCHAR, file_type VARCHAR,
  path VARCHAR, new_path VARCHAR
);

CREATE TABLE IF NOT EXISTS zfs.listings (
  host VARCHAR, dataset VARCHAR, seq INTEGER, snapshot VARCHAR,
  snapshot_creation TIMESTAMPTZ, ts TIMESTAMPTZ, n_files BIGINT
);

CREATE TABLE IF NOT EXISTS zfs.file_events (
  host VARCHAR, dataset VARCHAR, seq INTEGER, ts TIMESTAMPTZ, path VARCHAR,
  op VARCHAR, size BIGINT, mtime TIMESTAMPTZ, ctime TIMESTAMPTZ, inode BIGINT,
  nlink INTEGER, mode VARCHAR, uid INTEGER, gid INTEGER,
  checksum VARCHAR, checksum_algo VARCHAR
);

-- Materialized from latest_files_view at the end of every sync
-- (reconstructing state from file_events is too slow to query live).
CREATE TABLE IF NOT EXISTS zfs.latest_files (
  host VARCHAR, ts TIMESTAMPTZ, pool VARCHAR, dataset VARCHAR, snapshot VARCHAR,
  snapshot_creation TIMESTAMPTZ, path VARCHAR, size BIGINT, mtime TIMESTAMPTZ,
  device BIGINT, inode BIGINT, nlink INTEGER, mode VARCHAR, uid INTEGER,
  gid INTEGER, checksum VARCHAR, checksum_algo VARCHAR, filename VARCHAR
);

-- Latest views, chained on each host's latest collection run: a
-- destroyed dataset drops out once a newer run no longer has it.
CREATE OR REPLACE VIEW zfs.latest_datasets AS
  SELECT * FROM zfs.datasets d
  WHERE d.ts = (SELECT max(ts) FROM zfs.datasets WHERE host = d.host);

CREATE OR REPLACE VIEW zfs.latest_snapshots AS
  SELECT s.* FROM zfs.snapshots s
  JOIN zfs.latest_datasets ld ON ld.host = s.host AND ld.name = s.dataset
  QUALIFY row_number() OVER (PARTITION BY s.host, s.dataset ORDER BY s.creation DESC) = 1;

-- Files as of each existing dataset's latest listing: latest event per
-- path, minus removals. Same columns as the per-host latest_files
-- (device is always NULL).
CREATE OR REPLACE VIEW zfs.latest_files_view AS
  WITH ll AS (
    SELECT l.* FROM zfs.listings l
    JOIN zfs.latest_datasets ld ON ld.host = l.host AND ld.name = l.dataset
    QUALIFY l.seq = max(l.seq) OVER (PARTITION BY l.host, l.dataset)
  ),
  cur AS (
    SELECT e.* FROM zfs.file_events e
    JOIN ll ON ll.host = e.host AND ll.dataset = e.dataset
    QUALIFY row_number() OVER (PARTITION BY e.host, e.dataset, e.path ORDER BY e.seq DESC) = 1
  )
  SELECT ll.host, ll.ts, split_part(ll.dataset, '/', 1) AS pool, ll.dataset,
         ll.snapshot, ll.snapshot_creation,
         c.path, c.size, c.mtime, NULL::BIGINT AS device, c.inode, c.nlink, c.mode,
         c.uid, c.gid, c.checksum, c.checksum_algo
  FROM cur c JOIN ll ON ll.host = c.host AND ll.dataset = c.dataset
  WHERE c.op = 'P';
