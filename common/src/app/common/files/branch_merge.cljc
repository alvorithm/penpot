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
  [base theirs ours {:keys [kind page-id]}]
  (let [ids (set/union (set (keys base)) (set (keys theirs)) (set (keys ours)))
        mk  (fn [status extra]
              (cond-> (assoc extra :kind kind :status status)
                (some? page-id) (assoc :page-id page-id)))]
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
                  (diff-pages base theirs ours)]
        {:keys [changes conflicts]} (merge-results results)]
    {:changes   changes
     :conflicts conflicts
     :stats     {:added     (count (filterv #(= :added (:status %)) changes))
                 :modified  (count (filterv #(= :modified (:status %)) changes))
                 :deleted   (count (filterv #(= :deleted (:status %)) changes))
                 :conflicts (count conflicts)}}))
