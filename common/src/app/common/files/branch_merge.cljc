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
   [app.common.types.component :as ctk]
   [app.common.types.tokens-lib :as ctob]
   [app.common.uuid :as uuid]
   [clojure.set :as set]
   [clojure.string :as str]))

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

(def ^:private shape-ignored-attrs
  "Purely derived/structural shape attrs that are noise in the compare
  summary: children membership/order (`:shapes`, redundant with the
  child's own add/move), the sync flag (`:touched`) and geometry caches
  (`:selrect`, `:points`, recomputed from position/size). Containment
  (`:parent-id`/`:frame-id`) is deliberately NOT here: reparenting a layer
  into a board is a real, user-meaningful change worth surfacing. Hiding
  these from the summary never affects merge correctness (the merge drives
  them through add/del/move ops regardless)."
  #{:shapes :touched :selrect :points})

(defn- shape-display-meta
  "Display-only metadata for a shape diff entry — its type and component
  nature — so the compare view can pick a type-accurate icon and label
  instead of a generic one."
  [shape]
  (when (map? shape)
    (cond-> {:shape-type (:type shape)}
      (ctk/main-instance? shape)            (assoc :component? true)
      (and (ctk/instance-head? shape)
           (not (ctk/main-instance? shape))) (assoc :component-copy? true)
      (ctk/is-variant? shape)               (assoc :variant? true)
      (= :bool (:type shape))               (assoc :bool-type (:bool-type shape))
      (:masked-group shape)                 (assoc :masked? true))))

(defn three-way-entities
  "Diff one indexed entity collection (id->value) across base/theirs/ours.

  Returns `{:changes [..] :conflicts [..]}` where changes are the
  branch's net additions/modifications/deletions that apply cleanly to
  main, and conflicts are entities both sides diverged on.

  `ctx`: `{:kind <keyword> :page-id <optional uuid>}`. Optional display
  knobs (do NOT affect the actual merge, only this summary):
    `:ignore-ids`   ids skipped entirely (e.g. the page root frame);
    `:ignore-attrs` attr keys stripped from `:changed-attrs` (structural
                    noise like `:shapes` whose merge is driven by other ops);
    `:drop-empty-modified?` when a `:modified` entry's `:changed-attrs`
                    becomes empty after stripping, omit it altogether."
  [base theirs ours {:keys [kind ignore-ids ignore-attrs drop-empty-modified?] :as ctx}]
  (let [extras (dissoc ctx :kind :ignore-ids :ignore-attrs :drop-empty-modified?)
        attr-diff (fn [t o]
                    (let [d (shallow-attr-diff t o)]
                      (if (seq ignore-attrs) (apply dissoc d ignore-attrs) d)))
        ids (cond-> (set/union (set (keys base)) (set (keys theirs)) (set (keys ours)))
              (seq ignore-ids) (set/difference (set ignore-ids)))
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
                                                      :changed-attrs (attr-diff t o)
                                                      :base nil :main t :branch o}))

           ;; --- present in all three ---
           (= o b)                                        ; branch didn't touch -> main wins
           acc

           (= t b)                                        ; main didn't touch, branch did
           (let [ca (attr-diff t o)]
             (if (and drop-empty-modified? (map? o) (empty? ca))
               acc
               (update acc :changes conj (mk :modified {:id id :label (entity-label kind o)
                                                        :changed-attrs ca}))))

           (= o t)                                        ; both reached the same value
           acc

           :else                                          ; both diverged differently
           (update acc :conflicts conj (mk :conflict {:id id :reason :modify-modify
                                                      :label (entity-label kind o)
                                                      :changed-attrs (attr-diff t o)
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
  "Residual page attrs that no pass handles — kept as `:page-attrs` to be
  refused (never silently dropped). Objects, name/background/grid, guides,
  flows, default-grids and plugin-data have their own passes; `:index` is
  derived from page order; comment-thread-positions deliberately stay on
  the branch (comments are not migrated, like Figma)."
  [page]
  (dissoc page :objects :id :name :background :pixel-grid-color :pixel-grid-opacity
          :guides :flows :default-grids :plugin-data
          :index :comment-thread-positions))

(defn- flatten-plugin-data
  "{namespace {key value}} -> {[namespace key] value} for diffing."
  [pd]
  (persistent!
   (reduce-kv (fn [acc ns kvs]
                (reduce-kv (fn [acc k v] (assoc! acc [ns k] v)) acc kvs))
              (transient {})
              (or pd {}))))

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
        ;; residual page attrs on common pages (NOT mergeable -> refused, never dropped)
        extra-map (fn [pi] (into {} (map (fn [id] [id (page-extra (get pi id))])) common))
        extra-diff (three-way-entities (extra-map bpi) (extra-map tpi) (extra-map opi) {:kind :page-attrs})
        ;; page order on common pages (NOT yet mergeable)
        order-of (fn [data] (filterv common (or (:pages data) [])))
        order (three-way-entities {:order (order-of base)} {:order (order-of theirs)} {:order (order-of ours)}
                                  {:kind :page-order})
        ;; guides / flows per common page (mergeable: :page-guide / :page-flow)
        sub-of (fn [data pid k] (get-in data [:pages-index pid k] {}))
        guides-diffs (map (fn [pid]
                            (three-way-entities (sub-of base pid :guides)
                                                (sub-of theirs pid :guides)
                                                (sub-of ours pid :guides)
                                                {:kind :page-guide :page-id pid}))
                          common)
        flows-diffs (map (fn [pid]
                           (three-way-entities (sub-of base pid :flows)
                                               (sub-of theirs pid :flows)
                                               (sub-of ours pid :flows)
                                               {:kind :page-flow :page-id pid}))
                         common)
        ;; default-grids per common page (mergeable: :page-grid -> :set-default-grid)
        grids-diffs (map (fn [pid]
                           (three-way-entities (sub-of base pid :default-grids)
                                               (sub-of theirs pid :default-grids)
                                               (sub-of ours pid :default-grids)
                                               {:kind :page-grid :page-id pid}))
                         common)
        ;; page-level plugin-data per common page (mergeable: :page-plugin -> :set-plugin-data)
        plugin-of (fn [data pid] (flatten-plugin-data (get-in data [:pages-index pid :plugin-data] {})))
        plugins-diffs (map (fn [pid]
                             (three-way-entities (plugin-of base pid)
                                                 (plugin-of theirs pid)
                                                 (plugin-of ours pid)
                                                 {:kind :page-plugin :page-id pid}))
                           common)
        ;; objects (shapes) on common pages (mergeable: :shape). The page
        ;; root frame (uuid/zero) is skipped and structural attrs are
        ;; stripped: they are pure noise in the summary (their merge is
        ;; driven by add/del/move ops, not by these values).
        obj-diffs (map (fn [pid]
                         (let [bo  (get-in base [:pages-index pid :objects] {})
                               to  (get-in theirs [:pages-index pid :objects] {})
                               oo  (get-in ours [:pages-index pid :objects] {})
                               res (three-way-entities bo to oo
                                                       {:kind :shape :page-id pid
                                                        :ignore-ids #{uuid/zero}
                                                        :ignore-attrs shape-ignored-attrs
                                                        :drop-empty-modified? true})
                               ;; enrich each entry with the shape's type/component
                               ;; nature, read from whichever side still has it
                               enrich (fn [e]
                                        (merge e (shape-display-meta
                                                  (or (get oo (:id e))
                                                      (get to (:id e))
                                                      (get bo (:id e))))))]
                           {:changes   (mapv enrich (:changes res))
                            :conflicts (mapv enrich (:conflicts res))}))
                       common)]
    (merge-results (concat [presence meta-diff extra-diff order]
                           guides-diffs flows-diffs grids-diffs plugins-diffs obj-diffs))))

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

(defn- set-rename-attrs
  "Attrs to rename a set: take branch's name/description but keep MAIN's
  tokens — emitted before the per-token pass, which then layers branch's
  token edits on top. `:set-token-set`/`update-set` relocates the set and
  updates theme references for the new name."
  [main-lib branch-lib set-id]
  (let [bs (ctob/get-set branch-lib set-id)]
    {:id set-id
     :name (ctob/get-name bs)
     :description (ctob/get-description bs)
     :tokens (ctob/get-tokens main-lib set-id)}))

(defn- set-meta-of
  [lib set-id]
  (let [s (ctob/get-set lib set-id)]
    [(ctob/get-name s) (ctob/get-description s)]))

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

(defn- lib-active-paths
  "Active theme paths (mergeable via :set-active-token-themes)."
  [lib]
  (if lib (set (ctob/get-active-theme-paths lib)) #{}))

(defn- lib-hidden-sets
  "The hidden theme's active sets (active-set toggles)."
  [lib]
  (some-> lib (ctob/get-theme ctob/hidden-theme-id) :sets set))

(defn- hidden-theme-map
  "The hidden theme as a plain map (for :set-token-theme)."
  [lib]
  (some->> (when lib (ctob/get-theme lib ctob/hidden-theme-id)) (into {})))

(defn- set-order-by-id
  "Set ids in their stored order, filtered to `ids`."
  [lib ids]
  (filterv ids (lib-set-order lib)))

(defn- set-name->path
  "Split a token-set name into its path vector (sets are referenced by
  path, separator \"/\")."
  [name]
  (str/split name #"/"))

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
        ;; active theme paths (mergeable: :token-active-themes)
        active-paths (three-way-entities {:active-themes (lib-active-paths bl)}
                                         {:active-themes (lib-active-paths tl)}
                                         {:active-themes (lib-active-paths ol)}
                                         {:kind :token-active-themes})
        ;; active-set toggles (hidden theme) — NOT yet mergeable
        active-sets (three-way-entities {:active-sets (lib-hidden-sets bl)}
                                        {:active-sets (lib-hidden-sets tl)}
                                        {:active-sets (lib-hidden-sets ol)}
                                        {:kind :token-active-sets})
        ;; per-token values (mergeable: :token)
        set-ids  (set/union bids tids oids)
        tokens   (map (fn [sid]
                        (three-way-entities (lib-tokens-by-id bl sid)
                                            (lib-tokens-by-id tl sid)
                                            (lib-tokens-by-id ol sid)
                                            {:kind :token :set-id sid}))
                      set-ids)]
    (merge-results (concat [presence rename order themes active-paths active-sets] tokens))))

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
  #{:color :typography :media :shape :token :token-set :token-set-rename :token-set-order
    :token-theme :token-active-themes :token-active-sets
    :page :page-order :page-guide :page-flow :page-grid :page-plugin :component})

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
                   ;; container shapes (frame/group/…) require `:shapes`; reset
                   ;; it to empty so the schema is valid and children get
                   ;; appended by their own add-obj (add-shape inserts at index)
                   :obj (cond-> o (contains? o :shapes) (assoc :shapes []))
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

         ;; page guides / flows per common page
         page-guides (into []
                           (mapcat (fn [pid]
                                     (flat-changes (get-in base [:pages-index pid :guides] {})
                                                   (get-in main [:pages-index pid :guides] {})
                                                   (get-in branch [:pages-index pid :guides] {})
                                                   resolutions
                                                   (fn [gid g] {:type :set-guide :page-id pid :id gid :params g})
                                                   (fn [gid g] {:type :set-guide :page-id pid :id gid :params g})
                                                   (fn [gid] {:type :set-guide :page-id pid :id gid :params nil}))))
                           common-pages)
         page-flows (into []
                          (mapcat (fn [pid]
                                    (flat-changes (get-in base [:pages-index pid :flows] {})
                                                  (get-in main [:pages-index pid :flows] {})
                                                  (get-in branch [:pages-index pid :flows] {})
                                                  resolutions
                                                  (fn [fid f] {:type :set-flow :page-id pid :id fid :params f})
                                                  (fn [fid f] {:type :set-flow :page-id pid :id fid :params f})
                                                  (fn [fid] {:type :set-flow :page-id pid :id fid :params nil}))))
                          common-pages)

         ;; page default-grids per common page
         page-grids (into []
                          (mapcat (fn [pid]
                                    (flat-changes (get-in base [:pages-index pid :default-grids] {})
                                                  (get-in main [:pages-index pid :default-grids] {})
                                                  (get-in branch [:pages-index pid :default-grids] {})
                                                  resolutions
                                                  (fn [gt p] {:type :set-default-grid :page-id pid :grid-type gt :params p})
                                                  (fn [gt p] {:type :set-default-grid :page-id pid :grid-type gt :params p})
                                                  (fn [gt] {:type :set-default-grid :page-id pid :grid-type gt :params nil}))))
                          common-pages)

         ;; page-level plugin-data per common page (flattened to [ns key] -> value)
         page-plugins (into []
                            (mapcat (fn [pid]
                                      (flat-changes (flatten-plugin-data (get-in base [:pages-index pid :plugin-data] {}))
                                                    (flatten-plugin-data (get-in main [:pages-index pid :plugin-data] {}))
                                                    (flatten-plugin-data (get-in branch [:pages-index pid :plugin-data] {}))
                                                    resolutions
                                                    (fn [[ns k] v] {:type :set-plugin-data :object-type :page :object-id pid :namespace ns :key k :value v})
                                                    (fn [[ns k] v] {:type :set-plugin-data :object-type :page :object-id pid :namespace ns :key k :value v})
                                                    (fn [[ns k]] {:type :set-plugin-data :object-type :page :object-id pid :namespace ns :key k :value nil}))))
                            common-pages)

         ;; page order: reorder common pages to branch's order via mov-page
         page-order-changes
         (let [order-of (fn [data] (filterv tri-common-pages (or (:pages data) [])))
               bo (order-of base) mo (order-of main) oo (order-of branch)]
           (if (and (not= oo bo)
                    (or (= mo bo) (= (get resolutions :page-order) :branch)))
             (vec (map-indexed (fn [i pid] {:type :mov-page :id pid :index i}) oo))
             []))

         shapes (into [] (mapcat #(page-shape-changes base main branch resolutions %)) common-pages)

         ;; components: row metadata (shapes handled by the shape/page passes).
         ;; A branch soft-delete keeps the row with `:deleted true`; surface it
         ;; as a proper del-component so the deletion propagates.
         components (flat-changes (:components base) (:components main) (:components branch) resolutions
                                  (fn [_ c] (assoc c :type :add-component))
                                  (fn [id c] (if (:deleted c)
                                               {:type :del-component :id id}
                                               (assoc c :type :mod-component)))
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

         ;; set rename: take branch name/description, keep main's tokens
         ;; (the per-token pass then layers branch's token edits). Emitted
         ;; before token-vals.
         common-sets (set/intersection (lib-set-ids bl) (lib-set-ids ml) (lib-set-ids ol))
         set-rename-changes
         (into []
               (comp (filter (fn [sid]
                               (let [b (set-meta-of bl sid)
                                     m (set-meta-of ml sid)
                                     o (set-meta-of ol sid)]
                                 (and (not= o b)
                                      (or (= m b) (= (get resolutions sid) :branch))))))
                     (map (fn [sid] {:type :set-token-set :id sid :attrs (set-rename-attrs ml ol sid)})))
               common-sets)

         ;; active theme paths
         active-changes
         (let [bp (lib-active-paths bl) mp (lib-active-paths ml) op (lib-active-paths ol)]
           (cond
             (= op bp) []
             (= mp bp) [{:type :set-active-token-themes :theme-paths op}]
             (= op mp) []
             (= (get resolutions :active-themes) :branch) [{:type :set-active-token-themes :theme-paths op}]
             :else []))

         ;; active sets (hidden theme): bring branch's hidden theme
         active-sets-changes
         (let [bs (lib-hidden-sets bl) ms (lib-hidden-sets ml) os (lib-hidden-sets ol)
               take! (fn [] (if-let [h (hidden-theme-map ol)]
                              [{:type :set-token-theme :id ctob/hidden-theme-id :attrs h}]
                              []))]
           (cond
             (= os bs) []
             (= ms bs) (take!)
             (= os ms) []
             (= (get resolutions :active-sets) :branch) (take!)
             :else []))

         ;; set order: reorder main's common sets to branch's order using
         ;; "move nᵢ before nᵢ₊₁" right-to-left. Emitted after renames so set
         ;; names are settled.
         set-order-changes
         (let [bo (set-order-by-id bl common-sets)
               mo (set-order-by-id ml common-sets)
               oo (set-order-by-id ol common-sets)]
           (if (and (not= oo bo)
                    (or (= mo bo) (= (get resolutions :order) :branch)))
             (let [names (mapv #(ctob/get-name (ctob/get-set ol %)) oo)]
               (vec (for [i (range (- (count names) 2) -1 -1)]
                      {:type :move-token-set
                       :from-path (set-name->path (nth names i))
                       :to-path (set-name->path (nth names i))
                       :before-path (set-name->path (nth names (inc i)))
                       :before-group false})))
             []))
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
      ;; components before shapes so del-component can store the main-instance
      ;; objects (still on the page) before del-obj removes them
      :changes     (vec (concat page-presence components shapes page-meta-changes
                                page-guides page-flows page-grids page-plugins page-order-changes
                                colors typos media
                                set-presence set-rename-changes set-order-changes
                                token-vals themes
                                active-changes active-sets-changes))})))
