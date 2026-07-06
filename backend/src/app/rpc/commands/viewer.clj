;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC Sucursal en España SL

(ns app.rpc.commands.viewer
  (:require
   [app.binfile.common :as bfc]
   [app.common.exceptions :as ex]
   [app.common.features :as cfeat]
   [app.common.schema :as sm]
   [app.config :as cf]
   [app.db :as db]
   [app.features.file-snapshots :as fsnap]
   [app.rpc :as-alias rpc]
   [app.rpc.commands.files-pull-request :as fpr]
   [app.rpc.commands.teams :as teams]
   [app.rpc.cond :as-alias cond]
   [app.rpc.doc :as-alias doc]
   [app.rpc.permissions :as perms]
   [app.util.services :as sv]
   [cuerdas.core :as str]))

;; --- QUERY: View Only Bundle

(defn- remove-not-allowed-pages
  [data allowed]
  (-> data
      (update :pages (fn [pages] (filterv #(contains? allowed %) pages)))
      (update :pages-index select-keys allowed)))

(defn obfuscate-email
  "Obfuscate the `email` for share-link members so the viewer only sees a
   partially redacted address. Accepts any string shape (including nil,
   missing `@`, or a domain with no `.`) and falls back to a fully-masked
   result rather than throwing — the function is called while building the
   view-only bundle for anonymous viewers, so an NPE here would abort the
   entire share-link response."
  [email]
  (let [[name domain]
        (str/split (or email "") "@" 2)

        [_ rest]
        (str/split (or domain "") "." 2)

        name
        (if (> (count name) 3)
          (str (subs name 0 1) (apply str (take (dec (count name)) (repeat "*"))))
          "****")]

    (str name "@****" (when rest (str "." rest)))))

(defn anonymize-member
  [member]
  (-> (select-keys member [:id :email :name :fullname :photo-id])
      (update :email obfuscate-email)
      (assoc :can-read true)))

(defn- get-view-only-bundle
  [{:keys [::db/conn] :as cfg} {:keys [profile-id file-id ::perms] :as params}]
  (let [file    (bfc/get-file cfg file-id)

        ;; pull request review sandbox: the viewer shows the pinned
        ;; review snapshot of the branch, not its live state
        file    (if-let [snapshot (::snapshot params)]
                  (-> file
                      (assoc :data (:data snapshot))
                      (assoc :version (:version snapshot))
                      (assoc :features (:features snapshot))
                      (assoc :revn (:revn snapshot)))
                  file)

        project (db/get conn :project
                        {:id (:project-id file)}
                        {:columns [:id :name :team-id]})

        team    (-> (db/get conn :team {:id (:team-id project)})
                    (teams/decode-row))

        members    (cond->> (teams/get-team-members conn (:team-id project))
                     (= :share-link (:type perms))
                     (mapv anonymize-member))

        member-ids (into #{} (map :id) members)

        perms   (assoc perms :in-team (contains? member-ids profile-id))

        _       (-> (cfeat/get-team-enabled-features cf/flags team)
                    (cfeat/check-client-features! (:features params))
                    (cfeat/check-file-features! (:features file)))

        file    (cond-> file
                  (= :share-link (:type perms))
                  (update :data remove-not-allowed-pages (:pages perms))

                  :always
                  (update :data select-keys [:id :options :pages :pages-index :components]))

        libs    (->> (bfc/get-file-libraries conn file-id)
                     (mapv (fn [{:keys [id] :as lib}]
                             (merge lib (bfc/get-file cfg id)))))

        links   (->> (db/query conn :share-link {:file-id file-id})
                     (mapv (fn [row]
                             (-> row
                                 (update :pages db/decode-pgarray #{})
                                 ;; NOTE: the flags are deprecated but are still present
                                 ;; on the table on old rows. The flags are pgarray and
                                 ;; for avoid decoding it (because they are no longer used
                                 ;; on frontend) we just dissoc the column attribute from
                                 ;; row.
                                 (dissoc :flags)))))

        fonts   (db/query conn :team-font-variant
                          {:team-id (:id team)
                           :deleted-at nil})]

    {:users members
     :profiles members
     :fonts fonts
     :project project
     :share-links links
     :libraries libs
     :file file
     :team (assoc team :permissions perms)
     :permissions perms}))

(defn- get-pull-request-view-context
  "Resolve the review-sandbox context of `pr-id`: permissions taken from
  the pull request's TARGET file (share-links do not apply here) with
  edition stripped, plus the pinned review snapshot whose data replaces
  the branch file's live state. Refuses when the pull request is not
  open or does not belong to `file-id` — the sandbox must never silently
  degrade to the live branch."
  [system profile-id file-id pr-id]
  (fpr/check-pull-requests-enabled!)
  (let [pr (db/get* system :file-pull-request {:id pr-id})]
    (when (or (nil? pr)
              (some? (:deleted-at pr))
              (not= file-id (:source-file-id pr)))
      (ex/raise :type :not-found
                :code :pull-request-not-found
                :hint "unable to find pull request with the provided id"
                :pull-request-id pr-id))
    (when (not= "open" (:status pr))
      (ex/raise :type :validation
                :code :pull-request-not-open
                :hint "the review sandbox only exists while the pull request is open"
                :pull-request-id pr-id))
    (let [perms    (perms/get-file-read-permissions system profile-id (:target-file-id pr))
          snapshot (when (:review-snapshot-id pr)
                     (fsnap/get-snapshot system (:source-file-id pr) (:review-snapshot-id pr)))]
      (when (and perms (nil? snapshot))
        (ex/raise :type :not-found
                  :code :review-snapshot-missing
                  :hint "the pull request review snapshot cannot be resolved"
                  :pull-request-id pr-id))
      {:perms (some-> perms (assoc :can-edit false :is-admin false :is-owner false))
       :snapshot snapshot})))

(def schema:get-view-only-bundle
  [:map {:title "get-view-only-bundle"}
   [:file-id ::sm/uuid]
   [:share-id {:optional true} ::sm/uuid]
   [:pr-id {:optional true} ::sm/uuid]
   [:features {:optional true} ::cfeat/features]])

(sv/defmethod ::get-view-only-bundle
  {::rpc/auth false
   ::doc/added "1.17"
   ::sm/params schema:get-view-only-bundle}
  [system {:keys [::rpc/profile-id file-id share-id pr-id] :as params}]
  (db/run! system
           (fn [system]
             (let [{:keys [perms snapshot]}
                   (if (some? pr-id)
                     (get-pull-request-view-context system profile-id file-id pr-id)
                     {:perms (perms/get-file-read-permissions system profile-id file-id share-id)})

                   params (-> params
                              (assoc ::perms perms)
                              (assoc :profile-id profile-id)
                              (cond-> (some? snapshot) (assoc ::snapshot snapshot)))]

               ;; When we have neither profile nor share, we just return a not
               ;; found response to the user.
               (when-not perms
                 (ex/raise :type :not-found
                           :code :object-not-found
                           :hint "object not found"))

               (get-view-only-bundle system params)))))
