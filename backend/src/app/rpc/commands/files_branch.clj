;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC Sucursal en España SL

(ns app.rpc.commands.files-branch
  "RPC commands for file branching: create an isolated copy of a file
  (a \"branch\") linked to its source file (\"main\"), and list the
  branches of a file. Merge/diff/update commands are added in later
  phases. See `app.rpc.commands.files-snapshot` for the patterns this
  namespace mirrors."
  (:require
   [app.binfile.common :as bfc]
   [app.common.exceptions :as ex]
   [app.common.schema :as sm]
   [app.common.time :as ct]
   [app.common.uuid :as uuid]
   [app.config :as cf]
   [app.db :as db]
   [app.features.file-snapshots :as fsnap]
   [app.loggers.webhooks :as-alias webhooks]
   [app.rpc :as-alias rpc]
   [app.rpc.climit :as-alias climit]
   [app.rpc.commands.files :as files]
   [app.rpc.commands.management :as mgmt]
   [app.rpc.doc :as-alias doc]
   [app.rpc.quotes :as quotes]
   [app.util.services :as sv]))

(defn- check-branching-enabled!
  "Guard the branching commands behind the `:branching` product flag."
  []
  (when-not (contains? cf/flags :branching)
    (ex/raise :type :restriction
              :code :branching-disabled
              :hint "the branching feature is not enabled on this instance")))

;; --- COMMAND: create-file-branch

(def ^:private schema:create-file-branch
  [:map {:title "create-file-branch"}
   [:file-id ::sm/uuid]
   [:name [:string {:max 250}]]
   [:description {:optional true} [:string {:max 4000}]]])

(sv/defmethod ::create-file-branch
  "Create a branch from a file. The branch is a full, isolated copy of
  the source file (created through the same pipeline as duplicate-file,
  which preserves the internal `:data` ids) plus a system snapshot of
  main captured as the merge base."
  {::doc/added "2.16"
   ::webhooks/event? true
   ::sm/params schema:create-file-branch
   ::climit/id [[:create-file-branch/by-profile ::rpc/profile-id]
                [:create-file-branch/global]]}
  [cfg {:keys [::rpc/profile-id file-id name description]}]
  (check-branching-enabled!)
  (files/check-edition-permissions! cfg profile-id file-id)

  (let [file    (bfc/get-file cfg file-id :realize? true)
        project (db/get-by-id cfg :project (:project-id file))]

    (-> cfg
        (assoc ::quotes/profile-id profile-id)
        (assoc ::quotes/project-id (:project-id file))
        (assoc ::quotes/team-id (:team-id project))
        (assoc ::quotes/file-id (:id file))
        (quotes/check! {::quotes/id ::quotes/branches-per-file}
                       {::quotes/id ::quotes/branches-per-team}))

    (db/tx-run! cfg
                (fn [{:keys [::db/conn] :as cfg}]
                  (db/exec-one! conn ["SET CONSTRAINTS ALL DEFERRED"])

                  ;; 1. Materialize the merge base: a system snapshot of
                  ;; main, kept long-lived so it is not pruned by the
                  ;; snapshot GC while the branch is open.
                  (let [base (fsnap/create! cfg file
                                            {:label (str "branch-base/" name)
                                             :profile-id profile-id
                                             :created-by "system"
                                             :deleted-at (ct/in-future {:days 3650})})

                        ;; 2. Duplicate main into a new branch file. The
                        ;; duplicate pipeline remaps the file/media/library
                        ;; ids but preserves the internal :data ids, which
                        ;; is the precondition for an id-based merge.
                        branch-id (uuid/next)
                        branch    (binding [bfc/*state* (volatile! {:index {file-id branch-id}})]
                                    (mgmt/duplicate-file
                                     (assoc cfg ::bfc/timestamp (ct/now))
                                     {:profile-id profile-id
                                      :file-id file-id
                                      :name name
                                      :reset-shared-flag true}))

                        meta-id   (uuid/next)]

                    ;; 3. Mark the new file row as a branch so it is hidden
                    ;; from the project/team file listings.
                    (db/update! conn :file
                                {:is-branch true}
                                {:id (:id branch)}
                                {::db/return-keys false})

                    ;; 4. Persist the branch metadata.
                    (db/insert! conn :file-branch
                                {:id meta-id
                                 :branch-file-id (:id branch)
                                 :source-file-id file-id
                                 :base-snapshot-id (:id base)
                                 :base-revn (:revn file)
                                 :created-by profile-id
                                 :name name
                                 :description description
                                 :status "open"}
                                {::db/return-keys false})

                    {:id meta-id
                     :branch-file-id (:id branch)
                     :source-file-id file-id
                     :base-revn (:revn file)
                     :name name
                     :description description
                     :status "open"})))))

;; --- COMMAND QUERY: get-file-branches

(def ^:private schema:get-file-branches
  [:map {:title "get-file-branches"}
   [:file-id ::sm/uuid]
   [:include-archived {:optional true} :boolean]])

(def ^:private sql:get-file-branches
  "SELECT fb.id,
          fb.branch_file_id,
          fb.source_file_id,
          fb.name,
          fb.description,
          fb.status,
          fb.created_by,
          fb.created_at,
          fb.updated_at,
          fb.merged_at,
          fb.merged_by,
          fb.base_revn,
          bf.revn AS branch_revn,
          sf.revn AS source_revn
     FROM file_branch AS fb
     JOIN file AS bf ON (bf.id = fb.branch_file_id)
     JOIN file AS sf ON (sf.id = fb.source_file_id)
    WHERE fb.source_file_id = ?
      AND fb.deleted_at IS NULL
      AND (?::boolean OR fb.status = 'open')
    ORDER BY fb.created_at DESC")

(sv/defmethod ::get-file-branches
  "List the branches of a file. `ahead`/`behind` are cheap revn-based
  approximations (commits made on the branch / commits main advanced
  since the merge base); the entity-level diff arrives with the merge
  engine in a later phase."
  {::doc/added "2.16"
   ::sm/params schema:get-file-branches}
  [cfg {:keys [::rpc/profile-id file-id include-archived]}]
  (check-branching-enabled!)
  (db/run! cfg
           (fn [{:keys [::db/conn]}]
             (files/check-read-permissions! conn profile-id file-id)
             (->> (db/exec! conn [sql:get-file-branches file-id (boolean include-archived)])
                  (mapv (fn [{:keys [branch-revn source-revn base-revn] :as row}]
                          (-> row
                              (assoc :ahead (max 0 (- branch-revn base-revn)))
                              (assoc :behind (max 0 (- source-revn base-revn)))
                              (dissoc :branch-revn :source-revn))))))))
