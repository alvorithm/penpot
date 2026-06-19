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
  \"property changes\" detail in the compare view."
  [theirs ours]
  (let [ks (set/union (set (keys theirs)) (set (keys ours)))]
    (reduce (fn [acc k]
              (let [tv (get theirs k)
                    ov (get ours k)]
                (if (= tv ov)
                  acc
                  (assoc acc k {:main tv :branch ov}))))
            {}
            ks)))

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

(defn- diff-pages
  "Diff page set (add/remove/rename, objects excluded) plus, for every
  page present on both sides, the page's objects (shapes)."
  [base theirs ours]
  (let [strip      (fn [index]
                     (persistent!
                      (reduce-kv (fn [acc k v] (assoc! acc k (dissoc v :objects)))
                                 (transient {})
                                 (or index {}))))
        page-diff  (three-way-entities (strip (:pages-index base))
                                       (strip (:pages-index theirs))
                                       (strip (:pages-index ours))
                                       {:kind :page})
        common-ids (set/intersection (set (keys (:pages-index theirs)))
                                     (set (keys (:pages-index ours))))
        obj-diffs  (map (fn [page-id]
                          (three-way-entities (get-in base [:pages-index page-id :objects] {})
                                              (get-in theirs [:pages-index page-id :objects] {})
                                              (get-in ours [:pages-index page-id :objects] {})
                                              {:kind :shape :page-id page-id}))
                        common-ids)]
    (merge-results (cons page-diff obj-diffs))))

;; --- Tokens ---
;;
;; Token *values* (tokens within an existing set) are mergeable (kind
;; :token -> :set-token). Structural token changes (adding/renaming sets,
;; themes, active-theme/active-set toggles) are surfaced with
;; non-mergeable kinds (:token-set, :token-theme, :token-active-themes)
;; so the merge refuses them rather than dropping them silently — a full
;; structural token merge is a later step.

(defn- lib-set-meta
  [lib]
  (if lib
    (into {} (map (fn [s] [(ctob/get-id s) {:name (ctob/get-name s)
                                            :description (ctob/get-description s)}]))
          (ctob/get-sets lib))
    {}))

(defn- lib-set-ids
  [lib]
  (if lib (into #{} (map ctob/get-id) (ctob/get-sets lib)) #{}))

(defn- lib-tokens-by-id
  "token-id -> token (plain map) for a single set."
  [lib set-id]
  (if (and lib (ctob/get-set lib set-id))
    (into {} (map (fn [t] [(:id t) (into {} t)])) (vals (ctob/get-tokens lib set-id)))
    {}))

(defn- lib-themes
  [lib]
  (if lib (into {} (map (fn [t] [(:id t) (into {} t)])) (ctob/get-themes lib)) {}))

(defn- lib-active
  [lib]
  (if lib (set (ctob/get-active-theme-paths lib)) #{}))

(defn- diff-tokens
  [base theirs ours]
  (let [bl (:tokens-lib base) tl (:tokens-lib theirs) ol (:tokens-lib ours)
        set-diff    (three-way-entities (lib-set-meta bl) (lib-set-meta tl) (lib-set-meta ol)
                                        {:kind :token-set})
        theme-diff  (three-way-entities (lib-themes bl) (lib-themes tl) (lib-themes ol)
                                        {:kind :token-theme})
        active-diff (three-way-entities {:active (lib-active bl)}
                                        {:active (lib-active tl)}
                                        {:active (lib-active ol)}
                                        {:kind :token-active-themes})
        set-ids     (set/union (lib-set-ids bl) (lib-set-ids tl) (lib-set-ids ol))
        token-diffs (map (fn [sid]
                           (three-way-entities (lib-tokens-by-id bl sid)
                                               (lib-tokens-by-id tl sid)
                                               (lib-tokens-by-id ol sid)
                                               {:kind :token :set-id sid}))
                         set-ids)]
    (merge-results (concat [set-diff theme-diff active-diff] token-diffs))))

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
(def ^:private mergeable-kinds #{:color :typography :media :shape :token})

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

         common-pages (set/intersection (set (keys (:pages-index main)))
                                        (set (keys (:pages-index branch))))
         shapes (into [] (mapcat #(page-shape-changes base main branch resolutions %)) common-pages)

         set-ids (set/union (lib-set-ids (:tokens-lib base))
                            (lib-set-ids (:tokens-lib main))
                            (lib-set-ids (:tokens-lib branch)))
         tokens  (into []
                       (mapcat (fn [sid]
                                 (flat-changes (lib-tokens-by-id (:tokens-lib base) sid)
                                               (lib-tokens-by-id (:tokens-lib main) sid)
                                               (lib-tokens-by-id (:tokens-lib branch) sid)
                                               resolutions
                                               (fn [tid t] {:type :set-token :set-id sid :token-id tid :attrs t})
                                               (fn [tid t] {:type :set-token :set-id sid :token-id tid :attrs t})
                                               (fn [tid] {:type :set-token :set-id sid :token-id tid :attrs nil}))))
                       set-ids)]

     {:unsupported unsupported
      :changes     (vec (concat colors typos media shapes tokens))})))
