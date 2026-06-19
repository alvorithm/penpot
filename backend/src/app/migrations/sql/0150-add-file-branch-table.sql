ALTER TABLE file
  ADD COLUMN is_branch boolean NOT NULL DEFAULT false;

CREATE TABLE file_branch (
  id               uuid PRIMARY KEY,

  created_at       timestamptz NOT NULL DEFAULT clock_timestamp(),
  updated_at       timestamptz NOT NULL DEFAULT clock_timestamp(),

  branch_file_id   uuid NOT NULL REFERENCES file(id) ON DELETE CASCADE DEFERRABLE,
  source_file_id   uuid NOT NULL REFERENCES file(id) ON DELETE CASCADE DEFERRABLE,

  base_snapshot_id uuid NULL,
  base_revn        bigint NOT NULL,

  created_by       uuid NULL REFERENCES profile(id) ON DELETE SET NULL,

  name             text NOT NULL,
  description      text NULL,

  status           text NOT NULL DEFAULT 'open'
                      CHECK (status IN ('open', 'merged', 'archived')),

  merged_at        timestamptz NULL,
  merged_by        uuid NULL REFERENCES profile(id) ON DELETE SET NULL,
  deleted_at       timestamptz NULL
);

CREATE INDEX file_branch__source_file_id__idx
    ON file_branch(source_file_id) WHERE deleted_at IS NULL;

CREATE UNIQUE INDEX file_branch__branch_file_id__idx
    ON file_branch(branch_file_id);
