;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC Sucursal en España SL

(ns app.common.files.branch-merge
  "Three-way, entity-level merge/diff engine for file branching.

  Phase 2 scope: read-only diff. `compute-merge` takes the file `:data`
  of the merge base, main and branch and returns a serializable summary
  of what the branch changes relative to main, plus the set of
  conflicting entities. It never mutates the blob; translating the
  result into `changes` (for an actual merge) and token diffing arrive
  in later phases.

  Direction `:branch->main` (merge/compare) treats main as `theirs` and
  branch as `ours`; `:main->branch` (update from main) swaps them."
  (:require
   [app.common.types.tokens-lib :as ctob]
   [clojure.set :as set]))

(defn- entity-label
  [kind v]
  (or (:name v)
      (some-> (:id v) str)
      (name kind)))

(defn- shallow-attr-diff
  "Map of attr -> {:main v :branch v} for the keys whose values differ
  between the `theirs` and `ours` entity maps. Powers the
  \"property changes\" detail in the compare view. Returns `{}` for
  non-map entities (e.g. order/name tuples)."
  [theirs ours]
  (if (and (map? theirs) (map? ours))
    (let [ks (set/union (set (keys theirs)) (set (keys ours)))]
      (reduce (fn [acc k]
                (let [tv (get theirs k)
                      ov (get ours k)]
                  (if (= tv ov)
                    acc
                    (assoc acc k {:main tv :branch ov}))))
              {}
              ks))
    {}))

(defn three-way-entities
  "Diff one indexed entity collection (id->value) across base/theirs/ours.

  Returns `{:changes [..] :conflicts [..]}` where changes are the
  branch's net additions/modifications/deletions that apply cleanly to
  main, and conflicts are entities both sides diverged on.

  `ctx`: `{:kind <keyword> :page-id <optional uuid>}`."
  [base theirs ours {:keys [kind] :as ctx}]
  (let [extras (dissoc ctx :kind)
        ids (set/union (set (keys base)) (set (keys theirs)) (set (keys ours)))
        mk  (fn [status extra]
              (merge extra extras {:kind kind :status status}))]
    (reduce
     (fn [acc id]
       (let [b (get base id)
             t (get theirs id)
             o (get ours id)
             in-b? (contains? base id)
             in-t? (contains? theirs id)
             in-o? (contains? ours id)]
         (cond
           ;; --- deletions ---
           (and in-b? (not in-o?) (not in-t?))            ; deleted in both
           acc

           (and in-b? (not in-o?) (= b t))                ; deleted in branch, main intact
           (update acc :changes conj (mk :deleted {:id id :label (entity-label kind t)}))

           (and in-b? (not in-t?) (= b o))                ; deleted in main, branch intact
           acc

           (and in-b? (not in-o?) (not= b t))             ; delete (branch) / modify (main)
           (update acc :conflicts conj (mk :conflict {:id id :reason :delete-modify
                                                      :label (entity-label kind t)
                                                      :base b :main t :branch nil}))

           (and in-b? (not in-t?) (not= b o))             ; modify (branch) / delete (main)
           (update acc :conflicts conj (mk :conflict {:id id :reason :modify-delete
                                                      :label (entity-label kind o)
                                                      :base b :main nil :branch o}))

           ;; --- additions ---
           (and (not in-b?) in-o? (not in-t?))            ; new in branch
           (update acc :changes conj (mk :added {:id id :label (entity-label kind o)}))

           (and (not in-b?) in-t? (not in-o?))            ; new in main only
           acc

           (and (not in-b?) in-o? in-t? (= o t))          ; both added the same
           acc

           (and (not in-b?) in-o? in-t? (not= o t))       ; both added, different
           (update acc :conflicts conj (mk :conflict {:id id :reason :add-add
                                                      :label (entity-label kind o)
                                                      :changed-attrs (shallow-attr-diff t o)
                                                      :base nil :main t :branch o}))

           ;; --- present in all three ---
           (= o b)                                        ; branch didn't touch -> main wins
           acc

           (= t b)                                        ; main didn't touch, branch did
           (update acc :changes conj (mk :modified {:id id :label (entity-label kind o)
                                                    :changed-attrs (shallow-attr-diff t o)}))

           (= o t)                                        ; both reached the same value
           acc

           :else                                          ; both diverged differently
           (update acc :conflicts conj (mk :conflict {:id id :reason :modify-modify
                                                      :label (entity-label kind o)
                                                      :changed-attrs (shallow-attr-diff t o)
                                                      :base b :main t :branch o})))))
     {:changes [] :conflicts []}
     ids)))

(defn- merge-results
  [results]
  {:changes   (into [] (mapcat :changes) results)
   :conflicts (into [] (mapcat :conflicts) results)})

(defn- present-map
  [ids]
  (zipmap ids (repeat true)))

(defn- page-meta
  "Page attrs mergeable via `:mod-page` (name/background/pixel-grid)."
  [page]
  (select-keys page [:name :background :pixel-grid-color :pixel-grid-opacity]))

(defn- page-extra
  "Other page attrs (options/guides/flows/...) — not yet mergeable."
  [page]
  (dissoc page :objects :id :name :background :pixel-grid-color :pixel-grid-opacity))

(defn- diff-pages
  [base theirs ours]
  (let [bpi (or (:pages-index base) {}) tpi (or (:pages-index theirs) {}) opi (or (:pages-index ours) {})
        bids (set (keys bpi)) tids (set (keys tpi)) oids (set (keys opi))
        common (set/intersection bids tids oids)

        ;; page add/delete (mergeable: :page)
        presence (three-way-entities (present-map bids) (present-map tids) (present-map oids)
                                     {:kind :page})
        ;; page name/background/grid on common pages (mergeable: :page via mod-page)
        meta-map (fn [pi] (into {} (map (fn [id] [id (page-meta (get pi id))])) common))
        meta-diff (three-way-entities (meta-map bpi) (meta-map tpi) (meta-map opi) {:kind :page})
        ;; other page attrs (options/guides/flows) on common pages (NOT yet mergeable)
        extra-map (fn [pi] (into {} (map (fn [id] [id (page-extra (get pi id))])) common))
        extra-diff (three-way-entities (extra-map bpi) (extra-map tpi) (extra-map opi) {:kind :page-attrs})
        ;; page order on common pages (NOT yet mergeable)
        order-of (fn [data] (filterv common (or (:pages data) [])))
        order (three-way-entities {:order (order-of base)} {:order (order-of theirs)} {:order (order-of ours)}
                                  {:kind :page-order})
        ;; objects (shapes) on common pages (mergeable: :shape)
        obj-diffs (map (fn [pid]
                         (three-way-entities (get-in base [:pages-index pid :objects] {})
                                             (get-in theirs [:pages-index pid :objects] {})
                                             (get-in ours [:pages-index pid :objects] {})
                                             {:kind :shape :page-id pid}))
                       common)]
    (merge-results (concat [presence meta-diff extra-diff order] obj-diffs))))

;; --- Tokens ---
;;
;; Token *values* (tokens within an existing set) are mergeable (kind
;; :token -> :set-token). Structural token changes (adding/renaming sets,
;; themes, active-theme/active-set toggles) are surfaced with
;; non-mergeable kinds (:token-set, :token-theme, :token-active-themes)
;; so the merge refuses them rather than dropping them silently — a full
;; structural token merge is a later step.

(defn- lib-set-ids
  [lib]
  (if lib (into #{} (map ctob/get-id) (ctob/get-sets lib)) #{}))

(defn- lib-set-order
  [lib]
  (if lib (mapv ctob/get-id (ctob/get-sets lib)) []))

(defn- lib-set-meta
  "set-id -> [name description] for each set."
  [lib]
  (if lib
    (into {} (map (fn [s] [(ctob/get-id s) [(ctob/get-name s) (ctob/get-description s)]]))
          (ctob/get-sets lib))
    {}))

(defn- set-add-attrs
  "Metadata-only attrs to (re)create a set; its tokens are added by the
  per-token pass so unchanged tokens are preserved."
  [lib set-id]
  (let [s (ctob/get-set lib set-id)]
    {:id set-id :name (ctob/get-name s) :description (ctob/get-description s)}))

(defn- lib-tokens-by-id
  "token-id -> token (plain map) for a single set."
  [lib set-id]
  (if (and lib (ctob/get-set lib set-id))
    (into {} (map (fn [t] [(:id t) (into {} t)])) (vals (ctob/get-tokens lib set-id)))
    {}))

(defn- lib-themes
  "theme-id -> theme (plain map), excluding the internal hidden theme."
  [lib]
  (if lib
    (into {} (comp (remove #(= (:id %) ctob/hidden-theme-id))
                   (map (fn [t] [(:id t) (into {} t)])))
          (ctob/get-themes lib))
    {}))

(defn- lib-active-state
  "Active-theme paths + the hidden theme's active sets, as one comparable
  value. Changes here mean active-theme / active-set toggles, which are
  not merged yet (surfaced as :token-active-themes)."
  [lib]
  {:paths (if lib (set (ctob/get-active-theme-paths lib)) #{})
   :hidden-sets (some-> lib (ctob/get-theme ctob/hidden-theme-id) :sets set)})

(defn- diff-tokens
  [base theirs ours]
  (let [bl (:tokens-lib base) tl (:tokens-lib theirs) ol (:tokens-lib ours)
        bids (lib-set-ids bl) tids (lib-set-ids tl) oids (lib-set-ids ol)
        common (set/intersection bids tids oids)

        ;; set add/delete (mergeable: :token-set)
        presence (three-way-entities (present-map bids) (present-map tids) (present-map oids)
                                     {:kind :token-set})
        ;; set rename/description on common sets (NOT yet mergeable)
        rename   (three-way-entities (select-keys (lib-set-meta bl) common)
                                     (select-keys (lib-set-meta tl) common)
                                     (select-keys (lib-set-meta ol) common)
                                     {:kind :token-set-rename})
        ;; set order on common sets (NOT yet mergeable)
        order-of (fn [lib] (filterv common (lib-set-order lib)))
        order    (three-way-entities {:order (order-of bl)} {:order (order-of tl)} {:order (order-of ol)}
                                     {:kind :token-set-order})
        ;; themes, excluding hidden (mergeable: :token-theme)
        themes   (three-way-entities (lib-themes bl) (lib-themes tl) (lib-themes ol)
                                     {:kind :token-theme})
        ;; active themes / active sets (NOT yet mergeable)
        active   (three-way-entities {:active (lib-active-state bl)}
                                     {:active (lib-active-state tl)}
                                     {:active (lib-active-state ol)}
                                     {:kind :token-active-themes})
        ;; per-token values (mergeable: :token)
        set-ids  (set/union bids tids oids)
        tokens   (map (fn [sid]
                        (three-way-entities (lib-tokens-by-id bl sid)
                                            (lib-tokens-by-id tl sid)
                                            (lib-tokens-by-id ol sid)
                                            {:kind :token :set-id sid}))
                      set-ids)]
    (merge-results (concat [presence rename order themes active] tokens))))

(defn compute-merge
  "Compute the three-way diff between the merge `base`, `main` and
  `branch` file `:data`. Returns:

    {:changes   [<change descriptor> ...]   ; clean, branch -> main
     :conflicts [<conflict descriptor> ...] ; need resolution
     :stats     {:added n :modified n :deleted n :conflicts n}}

  NOTE: token-lib diffing is not yet implemented (handled in a later
  phase); `:tokens-lib` changes are not reported here."
  [base main branch dir]
  (let [[theirs ours] (if (= dir :main->branch) [branch main] [main branch])
        results  [(three-way-entities (:colors base) (:colors theirs) (:colors ours)
                                      {:kind :color})
                  (three-way-entities (:typographies base) (:typographies theirs) (:typographies ours)
                                      {:kind :typography})
                  (three-way-entities (:components base) (:components theirs) (:components ours)
                                      {:kind :component})
                  (three-way-entities (:media base) (:media theirs) (:media ours)
                                      {:kind :media})
                  (diff-pages base theirs ours)
                  (diff-tokens base theirs ours)]
        {:keys [changes conflicts]} (merge-results results)]
    {:changes   changes
     :conflicts conflicts
     :stats     {:added     (count (filterv #(= :added (:status %)) changes))
                 :modified  (count (filterv #(= :modified (:status %)) changes))
                 :deleted   (count (filterv #(= :deleted (:status %)) changes))
                 :conflicts (count conflicts)}}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MERGE -> CHANGES (Phase 3, no-conflict path)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Kinds `compute-changes` can translate into change ops. Components,
;; pages (add/remove/rename) and tokens are not yet supported and cause
;; the merge to refuse rather than silently drop changes.
(def ^:private mergeable-kinds
  #{:color :typography :media :shape :token :token-set :token-theme :page :component})

(defn unsupported-kinds
  "Set of change kinds present in `changes` that `compute-changes`
  cannot translate yet."
  [changes]
  (into #{} (comp (map :kind) (remove mergeable-kinds)) changes))

(defn- flat-changes
  "Emit add/mod/del change maps for one flat id->value collection.

  Clean (non-conflicting) changes are always emitted. Conflicting
  entities are emitted only when `resolutions` selects `:branch` for that
  id (taking the branch side); `:main` (or absent) leaves main untouched.
  The caller is responsible for refusing the merge while conflicts remain
  unresolved."
  [base theirs ours resolutions add-fn mod-fn del-fn]
  (reduce
   (fn [acc id]
     (let [b (get base id) t (get theirs id) o (get ours id)
           in-b? (contains? base id)
           in-t? (contains? theirs id)
           in-o? (contains? ours id)
           res   (get resolutions id)]
       (cond
         ;; present in all three
         (and in-b? in-o? in-t?)
         (cond (= o b) acc                          ; branch didn't touch
               (= t b) (conj acc (mod-fn id o))     ; main didn't touch, branch did
               (= o t) acc                          ; both reached same value
               (= res :branch) (conj acc (mod-fn id o)) ; modify/modify -> branch
               :else acc)

         ;; deleted in branch, still in main
         (and in-b? (not in-o?) in-t?)
         (cond (= b t) (conj acc (del-fn id))       ; clean delete
               (= res :branch) (conj acc (del-fn id)) ; delete/modify -> branch (delete)
               :else acc)

         ;; deleted in main, still in branch
         (and in-b? in-o? (not in-t?))
         (cond (= b o) acc                          ; clean (already gone in main)
               (= res :branch) (conj acc (add-fn id o)) ; modify/delete -> keep branch
               :else acc)

         ;; new in branch only
         (and (not in-b?) in-o? (not in-t?))
         (conj acc (add-fn id o))

         ;; added on both sides with different values (add/add)
         (and (not in-b?) in-o? in-t? (not= o t))
         (cond (= res :branch) (conj acc (mod-fn id o)) :else acc)

         :else acc)))
   []
   (set/union (set (keys base)) (set (keys theirs)) (set (keys ours)))))

(defn- index-of
  [coll x]
  (first (keep-indexed (fn [i v] (when (= v x) i)) coll)))

(defn- shape-set-ops
  "`:set` operations for the attrs that differ between the main and branch
  shape, excluding `:shapes` (children membership/order is driven by the
  add/del object changes instead)."
  [t o]
  (->> (shallow-attr-diff t o)
       (into [] (comp (remove (fn [[k _]] (= k :shapes)))
                      (map (fn [[k {:keys [branch]}]] {:type :set :attr k :val branch}))))))

(defn- page-shape-changes
  [base theirs ours resolutions page-id]
  (let [bo (get-in base [:pages-index page-id :objects] {})
        to (get-in theirs [:pages-index page-id :objects] {})
        oo (get-in ours [:pages-index page-id :objects] {})

        ;; modifications + deletions (additions handled below, ordered)
        mod-del
        (->> (flat-changes bo to oo resolutions
                           (fn [_ _] nil)
                           (fn [id o]
                             (let [ops (shape-set-ops (get to id) o)]
                               (when (seq ops)
                                 {:type :mod-obj :page-id page-id :id id :operations ops})))
                           (fn [id]
                             {:type :del-obj :page-id page-id :id id :ignore-touched true}))
             (filterv some?))

        ;; additions: present in branch, absent from base and main
        added-set (into #{} (filter (fn [id]
                                      (and (contains? oo id)
                                           (not (contains? bo id))
                                           (not (contains? to id)))))
                        (keys oo))

        ;; topological order so a newly-added parent is created before its
        ;; newly-added children
        ordered
        (loop [pending (vec added-set) done #{} out []]
          (if (empty? pending)
            out
            (let [ready (filterv (fn [id]
                                   (let [p (:parent-id (get oo id))]
                                     (or (not (contains? added-set p))
                                         (contains? done p))))
                                 pending)
                  ready (if (seq ready) ready (subvec pending 0 1))]
              (recur (filterv (complement (set ready)) pending)
                     (into done ready)
                     (into out ready)))))

        add-changes
        (mapv (fn [id]
                (let [o      (get oo id)
                      parent (:parent-id o)
                      index  (index-of (get-in oo [parent :shapes]) id)]
                  {:type :add-obj
                   :page-id page-id
                   :id id
                   :obj (dissoc o :shapes)
                   :parent-id parent
                   :frame-id (:frame-id o)
                   :index index
                   :ignore-touched true}))
              ordered)]
    (into mod-del add-changes)))

(defn compute-changes
  "Translate the branch→main merge into a vector of raw change maps
  applicable to main via `app.common.files.changes/process-changes`.

  Clean changes are always included. Conflicting entities are included
  only for those `resolutions` selects `:branch` (the caller must ensure
  no conflict remains unresolved before applying).

  Returns `{:changes [..] :unsupported #{kinds..}}`. When `:unsupported`
  is non-empty the caller must refuse the merge (translation for those
  kinds — components, pages, tokens — is not implemented yet)."
  ([base main branch]
   (compute-changes base main branch {}))
  ([base main branch resolutions]
   (let [merge       (compute-merge base main branch :branch->main)
         unsupported (unsupported-kinds (concat (:changes merge) (:conflicts merge)))

         colors (flat-changes (:colors base) (:colors main) (:colors branch) resolutions
                              (fn [_ o] {:type :add-color :color o})
                              (fn [_ o] {:type :mod-color :color o})
                              (fn [id] {:type :del-color :id id}))

         typos  (flat-changes (:typographies base) (:typographies main) (:typographies branch) resolutions
                              (fn [_ o] {:type :add-typography :typography o})
                              (fn [_ o] {:type :mod-typography :typography o})
                              (fn [id] {:type :del-typography :id id}))

         media  (flat-changes (:media base) (:media main) (:media branch) resolutions
                              (fn [_ o] {:type :add-media :object o})
                              (fn [_ o] {:type :mod-media :object o})
                              (fn [id] {:type :del-media :id id}))

         ;; pages: add (full page incl. objects) / delete + rename (mod-page)
         bpi (:pages-index base) mpi (:pages-index main) opi (:pages-index branch)
         bpids (set (keys bpi)) mpids (set (keys mpi)) opids (set (keys opi))
         common-pages (set/intersection mpids opids)
         tri-common-pages (set/intersection bpids mpids opids)

         page-presence (->> (flat-changes (present-map bpids) (present-map mpids) (present-map opids) resolutions
                                          (fn [pid _] {:type :add-page :page (get opi pid)})
                                          (fn [_ _] nil)
                                          (fn [pid] {:type :del-page :id pid}))
                            (filterv some?))
         pmeta (fn [pi] (into {} (map (fn [id] [id (page-meta (get pi id))])) tri-common-pages))
         page-meta-changes (->> (flat-changes (pmeta bpi) (pmeta mpi) (pmeta opi) resolutions
                                              (fn [_ _] nil)
                                              (fn [pid m] (assoc m :type :mod-page :id pid))
                                              (fn [_] nil))
                                (filterv some?))

         shapes (into [] (mapcat #(page-shape-changes base main branch resolutions %)) common-pages)

         ;; components: row metadata (shapes handled by the shape/page passes)
         components (flat-changes (:components base) (:components main) (:components branch) resolutions
                                  (fn [_ c] (assoc c :type :add-component))
                                  (fn [_ c] (assoc c :type :mod-component))
                                  (fn [id] {:type :del-component :id id}))

         bl (:tokens-lib base) ml (:tokens-lib main) ol (:tokens-lib branch)
         set-ids (set/union (lib-set-ids bl) (lib-set-ids ml) (lib-set-ids ol))

         ;; set add (create empty set, tokens added by the per-token pass) / delete
         set-presence (->> (flat-changes (present-map (lib-set-ids bl))
                                         (present-map (lib-set-ids ml))
                                         (present-map (lib-set-ids ol))
                                         resolutions
                                         (fn [sid _] {:type :set-token-set :id sid :attrs (set-add-attrs ol sid)})
                                         (fn [_ _] nil)
                                         (fn [sid] {:type :set-token-set :id sid :attrs nil}))
                           (filterv some?))
         ;; sets deleted from the branch are dropped wholesale; skip their per-token diff
         deleted-set-ids (into #{} (filter (fn [sid]
                                             (and (contains? (lib-set-ids bl) sid)
                                                  (contains? (lib-set-ids ml) sid)
                                                  (not (contains? (lib-set-ids ol) sid)))))
                               set-ids)

         token-vals (into []
                          (comp (remove deleted-set-ids)
                                (mapcat (fn [sid]
                                          (flat-changes (lib-tokens-by-id bl sid)
                                                        (lib-tokens-by-id ml sid)
                                                        (lib-tokens-by-id ol sid)
                                                        resolutions
                                                        (fn [tid t] {:type :set-token :set-id sid :token-id tid :attrs t})
                                                        (fn [tid t] {:type :set-token :set-id sid :token-id tid :attrs t})
                                                        (fn [tid] {:type :set-token :set-id sid :token-id tid :attrs nil})))))
                          set-ids)

         themes (flat-changes (lib-themes bl) (lib-themes ml) (lib-themes ol) resolutions
                              (fn [tid t] {:type :set-token-theme :id tid :attrs t})
                              (fn [tid t] {:type :set-token-theme :id tid :attrs t})
                              (fn [tid] {:type :set-token-theme :id tid :attrs nil}))]

     {:unsupported unsupported
      :changes     (vec (concat page-presence shapes page-meta-changes components
                                colors typos media
                                set-presence token-vals themes))})))
