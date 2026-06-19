;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC Sucursal en España SL

(ns app.rpc.commands.files-branch
  "RPC commands for file branching: create an isolated copy of a file
  (a \"branch\") linked to its source file (\"main\"), list branches,
  diff a branch against main, merge a branch into main (with conflict
  resolutions), and update a branch from main. See
  `app.rpc.commands.files-snapshot` for the patterns this namespace
  mirrors."
  (:require
   [app.binfile.common :as bfc]
   [app.common.exceptions :as ex]
   [app.common.features :as cfeat]
   [app.common.files.branch-merge :as bm]
   [app.common.files.changes :as cpc]
   [app.common.files.repair :as cfr]
   [app.common.files.validate :as cfv]
   [app.common.schema :as sm]
   [app.common.time :as ct]
   [app.common.uuid :as uuid]
   [app.config :as cf]
   [app.db :as db]
   [app.features.fdata :as fdata]
   [app.features.file-snapshots :as fsnap]
   [app.features.logical-deletion :as ldel]
   [app.loggers.webhooks :as-alias webhooks]
   [app.msgbus :as mbus]
   [app.rpc :as-alias rpc]
   [app.rpc.climit :as-alias climit]
   [app.rpc.commands.files :as files]
   [app.rpc.commands.files-update :as fupd]
   [app.rpc.commands.management :as mgmt]
   [app.rpc.commands.teams :as teams]
   [app.rpc.doc :as-alias doc]
   [app.rpc.quotes :as quotes]
   [app.util.blob :as blob]
   [app.util.pointer-map :as pmap]
   [app.util.services :as sv]
   [app.worker :as wrk]))

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

;; --- COMMAND QUERY: get-branch-diff

(def ^:private schema:get-branch-diff
  [:map {:title "get-branch-diff"}
   [:branch-id ::sm/uuid]])

(sv/defmethod ::get-branch-diff
  "Read-only three-way diff between a branch and its source (main),
  using the merge base captured at branch creation. Returns the summary
  produced by `branch-merge/compute-merge` (stats, changes, conflicts).

  NOTE (Phase 2): base/main/branch are assumed to share the same file
  data version; explicit migration normalization before diffing is a
  later refinement."
  {::doc/added "2.16"
   ::sm/params schema:get-branch-diff
   ::db/transaction true}
  [{:keys [::db/conn] :as cfg} {:keys [::rpc/profile-id branch-id]}]
  (check-branching-enabled!)
  (let [branch (db/get* conn :file-branch {:id branch-id})]
    (when (or (nil? branch) (some? (:deleted-at branch)))
      (ex/raise :type :not-found
                :code :branch-not-found
                :hint "unable to find branch with the provided id"
                :branch-id branch-id))

    (files/check-read-permissions! conn profile-id (:source-file-id branch))

    (let [main-data   (:data (bfc/get-file cfg (:source-file-id branch) :realize? true))
          branch-data (:data (bfc/get-file cfg (:branch-file-id branch) :realize? true))
          base-data   (or (when-let [snap-id (:base-snapshot-id branch)]
                            (:data (fsnap/get-snapshot cfg (:source-file-id branch) snap-id)))
                          main-data)]
      (bm/compute-merge base-data main-data branch-data :branch->main))))

;; --- COMMAND: merge-file-branch

(def ^:private schema:merge-file-branch
  [:map {:title "merge-file-branch"}
   [:branch-id ::sm/uuid]
   [:resolutions {:optional true} [:map-of ::sm/uuid :keyword]]
   [:expected-main-revn {:optional true} ::sm/int]])

(sv/defmethod ::merge-file-branch
  "Merge a branch into its source file (main).

  Phase 3 scope: clean merges only. If the three-way diff has conflicts
  the command returns `{:status :conflicts}` (resolution UI lands in a
  later phase); if it contains change kinds not yet translatable
  (components, pages, tokens) it returns `{:status :unsupported}` so no
  change is silently dropped. Otherwise it applies the merge to main
  through the production change pipeline, takes a safety snapshot, marks
  the branch merged and notifies open clients via msgbus."
  {::doc/added "2.16"
   ::webhooks/event? true
   ::sm/params schema:merge-file-branch
   ::climit/id [[:merge-file-branch/global]]}
  [{:keys [::mbus/msgbus] :as cfg}
   {:keys [::rpc/profile-id ::rpc/session-id branch-id resolutions expected-main-revn]}]
  (check-branching-enabled!)
  (let [branch (db/get* cfg :file-branch {:id branch-id})]
    (when (or (nil? branch) (some? (:deleted-at branch)))
      (ex/raise :type :not-found
                :code :branch-not-found
                :branch-id branch-id))
    (when (not= "open" (:status branch))
      (ex/raise :type :validation
                :code :branch-not-open
                :branch-id branch-id))

    (let [main-id (:source-file-id branch)]
      ;; Only editors of main can integrate (same rule as Figma).
      (files/check-edition-permissions! cfg profile-id main-id)

      (db/tx-run!
       cfg
       (fn [{:keys [::db/conn] :as cfg}]
         ;; Serialize against concurrent edits/merges on main using the
         ;; same advisory lock the normal update-file path takes.
         (db/xact-lock! conn main-id)

         (let [main-file   (bfc/get-file cfg main-id :realize? true)
               branch-file (bfc/get-file cfg (:branch-file-id branch) :realize? true)
               base-data   (or (when-let [snap-id (:base-snapshot-id branch)]
                                 (:data (fsnap/get-snapshot cfg main-id snap-id)))
                               (:data main-file))]

           (when (and (some? expected-main-revn)
                      (not= expected-main-revn (:revn main-file)))
             (ex/raise :type :conflict
                       :code :file-modified
                       :hint "main was modified, recompute the diff and retry"))

           (let [{:keys [conflicts]} (bm/compute-merge base-data (:data main-file)
                                                       (:data branch-file) :branch->main)
                 resolved?  (fn [c] (contains? #{:main :branch} (get resolutions (:id c))))
                 unresolved (remove resolved? conflicts)]
             (cond
               (seq unresolved)
               {:status :conflicts :conflicts conflicts}

               :else
               (let [{:keys [changes unsupported]}
                     (bm/compute-changes base-data (:data main-file) (:data branch-file)
                                         (or resolutions {}))]
                 (cond
                   (seq unsupported)
                   {:status :unsupported :kinds (vec unsupported)}

                   (empty? changes)
                   ;; Nothing to integrate (branch matches main): mark the
                   ;; branch merged without touching main.
                   (let [ts (ct/now)]
                     (db/update! conn :file-branch
                                 {:status "merged"
                                  :merged-at ts
                                  :merged-by profile-id
                                  :updated-at ts}
                                 {:id branch-id}
                                 {::db/return-keys false})
                     {:status :merged :revn (:revn main-file)})

                   :else
                   (let [team  (teams/get-team conn :profile-id profile-id :file-id main-id)
                         delay (ldel/get-deletion-delay team)
                         ts    (ct/now)]
                     (binding [pmap/*tracked* (pmap/create-tracked)
                               pmap/*load-fn*  (partial fdata/load-pointer cfg main-id)
                               cfeat/*current*  (:features main-file)
                               cfeat/*previous* (:features main-file)]

                       ;; Safety snapshot of pre-merge main (rollback via versions).
                       (fsnap/create! cfg main-file
                                      {:label (str "pre-merge/" (:name branch))
                                       :created-by "system"
                                       :deleted-at (ct/in-future delay)
                                       :profile-id profile-id})

                       (let [merged (-> main-file
                                        (update :revn inc)
                                        (update :data #(cpc/process-changes % changes)))
                             libs   (bfc/get-resolved-file-libraries cfg merged)
                             errors (not-empty (cfv/validate-file merged libs))
                             merged (if errors
                                      (update merged :data cpc/process-changes
                                              (cfr/repair-file merged libs errors))
                                      merged)]

                         ;; Change log (xlog), GC-eligible after the delay.
                         (db/insert! conn :file-change
                                     {:id (uuid/next)
                                      :session-id session-id
                                      :profile-id profile-id
                                      :created-at ts
                                      :updated-at ts
                                      :deleted-at (ct/in-future {:hours 1})
                                      :file-id main-id
                                      :revn (:revn merged)
                                      :version (:version merged)
                                      :features (into-array (:features merged))
                                      :changes (blob/encode (vec changes))}
                                     {::db/return-keys false})

                         (fupd/persist-file! (assoc cfg ::fupd/timestamp ts) merged)

                         (db/update! conn :file-branch
                                     {:status "merged"
                                      :merged-at ts
                                      :merged-by profile-id
                                      :updated-at ts}
                                     {:id branch-id}
                                     {::db/return-keys false})

                         (mbus/pub! msgbus
                                    :topic main-id
                                    :message {:type :file-merged
                                              :file-id main-id
                                              :session-id session-id
                                              :revn (:revn merged)})

                         {:status :merged :revn (:revn merged)})))))))))))))

;; --- COMMAND: update-branch-from-main

(def ^:private schema:update-branch-from-main
  [:map {:title "update-branch-from-main"}
   [:branch-id ::sm/uuid]])

(sv/defmethod ::update-branch-from-main
  "Bring the changes main received since the merge base into the branch
  (the reverse direction of a merge). Phase 5 scope: clean updates only.
  Returns `{:status :conflicts}` when main and the branch diverged on the
  same entity (resolve at merge time for now), or `{:status :unsupported}`
  for change kinds not yet translatable. On success it applies main's
  changes to the branch, takes a safety snapshot, and repositions the
  merge base to the current state of main."
  {::doc/added "2.16"
   ::webhooks/event? true
   ::sm/params schema:update-branch-from-main
   ::climit/id [[:update-branch-from-main/global]]}
  [{:keys [::mbus/msgbus] :as cfg}
   {:keys [::rpc/profile-id ::rpc/session-id branch-id]}]
  (check-branching-enabled!)
  (let [branch (db/get* cfg :file-branch {:id branch-id})]
    (when (or (nil? branch) (some? (:deleted-at branch)))
      (ex/raise :type :not-found :code :branch-not-found :branch-id branch-id))
    (when (not= "open" (:status branch))
      (ex/raise :type :validation :code :branch-not-open :branch-id branch-id))

    (let [branch-file-id (:branch-file-id branch)
          main-id        (:source-file-id branch)]
      ;; Editing the branch -> need edition permissions on the branch file.
      (files/check-edition-permissions! cfg profile-id branch-file-id)

      (db/tx-run!
       cfg
       (fn [{:keys [::db/conn] :as cfg}]
         (db/xact-lock! conn branch-file-id)

         (let [main-file   (bfc/get-file cfg main-id :realize? true)
               branch-file (bfc/get-file cfg branch-file-id :realize? true)
               base-data   (or (when-let [snap-id (:base-snapshot-id branch)]
                                 (:data (fsnap/get-snapshot cfg main-id snap-id)))
                               (:data branch-file))

               reposition-base!
               (fn [ts]
                 (let [new-base (fsnap/create! cfg main-file
                                               {:label (str "branch-base/" (:name branch))
                                                :created-by "system"
                                                :deleted-at (ct/in-future {:days 3650})
                                                :profile-id profile-id})]
                   (db/update! conn :file-branch
                               {:base-snapshot-id (:id new-base)
                                :base-revn (:revn main-file)
                                :updated-at ts}
                               {:id branch-id}
                               {::db/return-keys false})))

               conflicts
               (:conflicts (bm/compute-merge base-data (:data main-file)
                                             (:data branch-file) :main->branch))]

           (cond
             (seq conflicts)
             {:status :conflicts :count (count conflicts)}

             :else
             ;; target = branch, source = main -> changes that bring main's
             ;; net changes into the branch
             (let [{:keys [changes unsupported]}
                   (bm/compute-changes base-data (:data branch-file) (:data main-file))]
               (cond
                 (seq unsupported)
                 {:status :unsupported :kinds (vec unsupported)}

                 (empty? changes)
                 (let [ts (ct/now)]
                   (reposition-base! ts)
                   {:status :updated :revn (:revn branch-file)})

                 :else
                 (let [team  (teams/get-team conn :profile-id profile-id :file-id branch-file-id)
                       delay (ldel/get-deletion-delay team)
                       ts    (ct/now)]
                   (binding [pmap/*tracked* (pmap/create-tracked)
                             pmap/*load-fn*  (partial fdata/load-pointer cfg branch-file-id)
                             cfeat/*current*  (:features branch-file)
                             cfeat/*previous* (:features branch-file)]

                     (fsnap/create! cfg branch-file
                                    {:label (str "pre-update/" (:name branch))
                                     :created-by "system"
                                     :deleted-at (ct/in-future delay)
                                     :profile-id profile-id})

                     (let [updated (-> branch-file
                                       (update :revn inc)
                                       (update :data #(cpc/process-changes % changes)))
                           libs    (bfc/get-resolved-file-libraries cfg updated)
                           errors  (not-empty (cfv/validate-file updated libs))
                           updated (if errors
                                     (update updated :data cpc/process-changes
                                             (cfr/repair-file updated libs errors))
                                     updated)]

                       (db/insert! conn :file-change
                                   {:id (uuid/next)
                                    :session-id session-id
                                    :profile-id profile-id
                                    :created-at ts
                                    :updated-at ts
                                    :deleted-at (ct/in-future {:hours 1})
                                    :file-id branch-file-id
                                    :revn (:revn updated)
                                    :version (:version updated)
                                    :features (into-array (:features updated))
                                    :changes (blob/encode (vec changes))}
                                   {::db/return-keys false})

                       (fupd/persist-file! (assoc cfg ::fupd/timestamp ts) updated)
                       (reposition-base! ts)

                       (mbus/pub! msgbus
                                  :topic branch-file-id
                                  :message {:type :file-merged
                                            :file-id branch-file-id
                                            :session-id session-id
                                            :revn (:revn updated)})

                       {:status :updated :revn (:revn updated)}))))))))))))

;; --- COMMAND QUERY: get-file-branch-info

(def ^:private schema:get-file-branch-info
  [:map {:title "get-file-branch-info"}
   [:file-id ::sm/uuid]])

(def ^:private sql:get-file-branch-info
  "SELECT fb.id,
          fb.branch_file_id,
          fb.source_file_id,
          fb.name,
          fb.description,
          fb.status,
          fb.created_by,
          fb.created_at,
          fb.base_revn,
          bf.revn AS branch_revn,
          sf.revn AS source_revn
     FROM file_branch AS fb
     JOIN file AS bf ON (bf.id = fb.branch_file_id)
     JOIN file AS sf ON (sf.id = fb.source_file_id)
    WHERE fb.branch_file_id = ?
      AND fb.deleted_at IS NULL")

(sv/defmethod ::get-file-branch-info
  "If the given file is a branch, return its branch metadata (with cheap
  revn-based ahead/behind); otherwise nil. Returns nil when branching is
  disabled, so it is safe to call on every file open."
  {::doc/added "2.16"
   ::sm/params schema:get-file-branch-info}
  [cfg {:keys [::rpc/profile-id file-id]}]
  (when (contains? cf/flags :branching)
    (db/run! cfg
             (fn [{:keys [::db/conn]}]
               (files/check-read-permissions! conn profile-id file-id)
               (when-let [{:keys [branch-revn source-revn base-revn] :as row}
                          (db/exec-one! conn [sql:get-file-branch-info file-id])]
                 (-> row
                     (assoc :ahead (max 0 (- branch-revn base-revn)))
                     (assoc :behind (max 0 (- source-revn base-revn)))
                     (dissoc :branch-revn :source-revn)))))))

;; --- COMMAND: update-file-branch (rename / description)

(def ^:private schema:update-file-branch
  [:map {:title "update-file-branch"}
   [:id ::sm/uuid]
   [:name {:optional true} [:string {:max 250}]]
   [:description {:optional true} [:string {:max 4000}]]])

(sv/defmethod ::update-file-branch
  "Rename a branch or update its description."
  {::doc/added "2.16"
   ::sm/params schema:update-file-branch
   ::db/transaction true}
  [{:keys [::db/conn] :as cfg} {:keys [::rpc/profile-id id name description]}]
  (check-branching-enabled!)
  (let [branch (db/get* conn :file-branch {:id id})]
    (when (or (nil? branch) (some? (:deleted-at branch)))
      (ex/raise :type :not-found :code :branch-not-found :branch-id id))
    (files/check-edition-permissions! cfg profile-id (:branch-file-id branch))
    (db/update! conn :file-branch
                (cond-> {:updated-at (ct/now)}
                  (some? name)        (assoc :name name)
                  (some? description) (assoc :description description))
                {:id id}
                {::db/return-keys false})
    {:id id :name name :description description}))

;; --- COMMAND: archive-file-branch

(def ^:private schema:archive-file-branch
  [:map {:title "archive-file-branch"}
   [:id ::sm/uuid]
   [:archived {:optional true} :boolean]])

(sv/defmethod ::archive-file-branch
  "Archive (hide from the default list) or restore a branch."
  {::doc/added "2.16"
   ::sm/params schema:archive-file-branch
   ::db/transaction true}
  [{:keys [::db/conn] :as cfg} {:keys [::rpc/profile-id id archived]}]
  (check-branching-enabled!)
  (let [branch (db/get* conn :file-branch {:id id})]
    (when (or (nil? branch) (some? (:deleted-at branch)))
      (ex/raise :type :not-found :code :branch-not-found :branch-id id))
    (when (= "merged" (:status branch))
      (ex/raise :type :validation :code :branch-merged :branch-id id))
    (files/check-edition-permissions! cfg profile-id (:branch-file-id branch))
    (let [status (if (false? archived) "open" "archived")]
      (db/update! conn :file-branch
                  {:status status :updated-at (ct/now)}
                  {:id id}
                  {::db/return-keys false})
      {:id id :status status})))

;; --- COMMAND: delete-file-branch

(def ^:private schema:delete-file-branch
  [:map {:title "delete-file-branch"}
   [:id ::sm/uuid]])

(sv/defmethod ::delete-file-branch
  "Logically delete a branch: marks the branch metadata and the branch
  file as deleted (with the team's deletion delay) and schedules the
  file object for GC, mirroring the normal file deletion path."
  {::doc/added "2.16"
   ::webhooks/event? true
   ::sm/params schema:delete-file-branch}
  [{:keys [::mbus/msgbus] :as cfg} {:keys [::rpc/profile-id id]}]
  (check-branching-enabled!)
  (let [branch (db/get* cfg :file-branch {:id id})]
    (when (or (nil? branch) (some? (:deleted-at branch)))
      (ex/raise :type :not-found :code :branch-not-found :branch-id id))
    (let [branch-file-id (:branch-file-id branch)]
      (files/check-edition-permissions! cfg profile-id branch-file-id)
      (db/tx-run!
       cfg
       (fn [{:keys [::db/conn] :as cfg}]
         (let [team (teams/get-team conn :profile-id profile-id :file-id branch-file-id)
               dt   (ct/in-future (ldel/get-deletion-delay team))]
           (db/update! conn :file {:deleted-at dt} {:id branch-file-id} {::db/return-keys false})
           (db/delete! conn :file-library-rel {:library-file-id branch-file-id})
           (db/update! conn :file-branch
                       {:deleted-at dt :status "archived" :updated-at (ct/now)}
                       {:id id}
                       {::db/return-keys false})
           (wrk/submit! {::db/conn conn
                         ::wrk/task :delete-object
                         ::wrk/params {:object :file :deleted-at dt :id branch-file-id}})
           (mbus/pub! msgbus
                      :topic branch-file-id
                      :message {:type :file-deleted
                                :file-id branch-file-id
                                :profile-id profile-id})
           {:status :deleted}))))))
