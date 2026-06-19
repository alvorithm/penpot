;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC Sucursal en España SL

(ns common-tests.files-branch-merge-test
  (:require
   [app.common.files.branch-merge :as bm]
   [app.common.files.changes :as cfc]
   [app.common.types.tokens-lib :as ctob]
   [app.common.uuid :as uuid]
   [clojure.test :as t]))

(defn- mkdata
  [objects & {:keys [colors]}]
  {:pages-index  {:p1 {:id :p1 :name "Page 1" :objects objects}}
   :colors       (or colors {})
   :typographies {}
   :components   {}
   :media        {}})

(t/deftest identity-no-changes
  (let [base (mkdata {:s1 {:id :s1 :name "A" :fill "red"}})
        r    (bm/compute-merge base base base :branch->main)]
    (t/is (= [] (:changes r)))
    (t/is (= [] (:conflicts r)))
    (t/is (= {:added 0 :modified 0 :deleted 0 :conflicts 0} (:stats r)))))

(t/deftest added-in-branch
  (let [base   (mkdata {:s1 {:id :s1 :name "A"}})
        branch (mkdata {:s1 {:id :s1 :name "A"}
                        :s2 {:id :s2 :name "B"}})
        r      (bm/compute-merge base base branch :branch->main)
        c      (first (filter #(= :added (:status %)) (:changes r)))]
    (t/is (= 1 (-> r :stats :added)))
    (t/is (= 0 (-> r :stats :conflicts)))
    (t/is (= :shape (:kind c)))
    (t/is (= :s2 (:id c)))
    (t/is (= :p1 (:page-id c)))
    (t/is (= "B" (:label c)))))

(t/deftest modified-in-branch
  (let [base   (mkdata {:s1 {:id :s1 :name "A" :fill "red"}})
        branch (mkdata {:s1 {:id :s1 :name "A" :fill "blue"}})
        r      (bm/compute-merge base base branch :branch->main)
        c      (first (:changes r))]
    (t/is (= 1 (-> r :stats :modified)))
    (t/is (= :modified (:status c)))
    (t/is (= {:main "red" :branch "blue"} (get-in c [:changed-attrs :fill])))))

(t/deftest deleted-in-branch
  (let [base   (mkdata {:s1 {:id :s1 :name "A"} :s2 {:id :s2 :name "B"}})
        branch (mkdata {:s1 {:id :s1 :name "A"}})
        r      (bm/compute-merge base base branch :branch->main)
        c      (first (:changes r))]
    (t/is (= 1 (-> r :stats :deleted)))
    (t/is (= :deleted (:status c)))
    (t/is (= :s2 (:id c)))))

(t/deftest fast-forward-main-untouched
  ;; main == base; branch changed -> all branch changes apply cleanly
  (let [base   (mkdata {:s1 {:id :s1 :name "A" :fill "red"}})
        branch (mkdata {:s1 {:id :s1 :name "A" :fill "blue"}})
        r      (bm/compute-merge base base branch :branch->main)]
    (t/is (= 0 (-> r :stats :conflicts)))
    (t/is (= 1 (-> r :stats :modified)))))

(t/deftest main-only-change-not-reported
  ;; branch == base; main changed -> branch contributes nothing
  (let [base (mkdata {:s1 {:id :s1 :name "A" :fill "red"}})
        main (mkdata {:s1 {:id :s1 :name "A" :fill "green"}})
        r    (bm/compute-merge base main base :branch->main)]
    (t/is (= [] (:changes r)))
    (t/is (= 0 (-> r :stats :conflicts)))))

(t/deftest modify-modify-conflict
  (let [base   (mkdata {:s1 {:id :s1 :name "A" :fill "red"}})
        main   (mkdata {:s1 {:id :s1 :name "A" :fill "green"}})
        branch (mkdata {:s1 {:id :s1 :name "A" :fill "blue"}})
        r      (bm/compute-merge base main branch :branch->main)
        cf     (first (:conflicts r))]
    (t/is (= 1 (-> r :stats :conflicts)))
    (t/is (= :modify-modify (:reason cf)))
    (t/is (= "green" (get-in cf [:changed-attrs :fill :main])))
    (t/is (= "blue" (get-in cf [:changed-attrs :fill :branch])))))

(t/deftest delete-modify-conflict
  ;; deleted in branch, modified in main
  (let [base   (mkdata {:s1 {:id :s1 :name "A" :fill "red"}})
        main   (mkdata {:s1 {:id :s1 :name "A" :fill "green"}})
        branch (mkdata {})
        r      (bm/compute-merge base main branch :branch->main)
        cf     (first (:conflicts r))]
    (t/is (= 1 (-> r :stats :conflicts)))
    (t/is (= :delete-modify (:reason cf)))))

(t/deftest color-added-in-branch
  (let [base   (mkdata {} :colors {})
        branch (mkdata {} :colors {:c1 {:id :c1 :name "Primary"}})
        r      (bm/compute-merge base base branch :branch->main)
        c      (first (filter #(= :color (:kind %)) (:changes r)))]
    (t/is (= :added (:status c)))
    (t/is (= "Primary" (:label c)))))

;; --- compute-changes (merge -> change maps)

(t/deftest compute-changes-colors
  (let [base   (mkdata {} :colors {:c1 {:id :c1 :name "A"}})
        branch (mkdata {} :colors {:c1 {:id :c1 :name "A2"}
                                   :c2 {:id :c2 :name "B"}})
        {:keys [changes unsupported]} (bm/compute-changes base base branch)
        by-type (group-by :type changes)]
    (t/is (empty? unsupported))
    (t/is (= :c2 (-> by-type :add-color first :color :id)))
    (t/is (= "A2" (-> by-type :mod-color first :color :name)))))

(t/deftest compute-changes-shape-mod-del
  (let [base   (mkdata {:s1 {:id :s1 :name "A" :fill "red"}
                        :s2 {:id :s2 :name "B"}})
        branch (mkdata {:s1 {:id :s1 :name "A" :fill "blue"}})
        {:keys [changes]} (bm/compute-changes base base branch)
        by-type (group-by :type changes)]
    (t/is (= {:type :set :attr :fill :val "blue"}
             (-> by-type :mod-obj first :operations first)))
    (t/is (= :p1 (-> by-type :mod-obj first :page-id)))
    (t/is (= :s2 (-> by-type :del-obj first :id)))))

(t/deftest compute-changes-shape-add-topological
  (let [objs   {:root {:id :root :name "Root" :shapes []}}
        base   (mkdata objs)
        branch (mkdata {:root {:id :root :name "Root" :shapes [:f1]}
                        :f1   {:id :f1 :name "Frame" :parent-id :root :frame-id :root :shapes [:c1]}
                        :c1   {:id :c1 :name "Child" :parent-id :f1 :frame-id :f1}})
        {:keys [changes unsupported]} (bm/compute-changes base base branch)
        adds   (filterv #(= :add-obj (:type %)) changes)]
    (t/is (empty? unsupported))
    ;; parent added before child
    (t/is (= [:f1 :c1] (mapv :id adds)))
    (t/is (= :root (:parent-id (first adds))))
    (t/is (= 0 (:index (first adds))))
    ;; children membership is not re-set via :shapes ops
    (t/is (empty? (filter #(and (= :mod-obj (:type %)) (= :root (:id %))) changes)))))

(t/deftest compute-changes-refuses-unsupported-kinds
  (let [base   {:pages-index {} :colors {} :typographies {} :media {} :components {}}
        branch (assoc base :components {:cmp1 {:id :cmp1 :name "Button"}})
        {:keys [unsupported]} (bm/compute-changes base base branch)]
    (t/is (contains? unsupported :component))))

(t/deftest compute-changes-resolve-shape-conflict
  (let [base   (mkdata {:s1 {:id :s1 :name "A" :fill "red"}})
        main   (mkdata {:s1 {:id :s1 :name "A" :fill "green"}})
        branch (mkdata {:s1 {:id :s1 :name "A" :fill "blue"}})]
    ;; resolved to branch -> sets the branch value
    (let [{:keys [changes]} (bm/compute-changes base main branch {:s1 :branch})
          op (-> (group-by :type changes) :mod-obj first :operations first)]
      (t/is (= {:type :set :attr :fill :val "blue"} op)))
    ;; resolved to main -> nothing emitted
    (let [{:keys [changes]} (bm/compute-changes base main branch {:s1 :main})]
      (t/is (empty? (filterv #(= :mod-obj (:type %)) changes))))
    ;; unresolved -> nothing emitted (caller refuses the merge)
    (let [{:keys [changes]} (bm/compute-changes base main branch {})]
      (t/is (empty? (filterv #(= :mod-obj (:type %)) changes))))))

(t/deftest compute-changes-resolve-color-conflict
  (let [base   (mkdata {} :colors {:c1 {:id :c1 :name "A"}})
        main   (mkdata {} :colors {:c1 {:id :c1 :name "Main"}})
        branch (mkdata {} :colors {:c1 {:id :c1 :name "Branch"}})
        {:keys [changes]} (bm/compute-changes base main branch {:c1 :branch})]
    (t/is (= "Branch" (-> (group-by :type changes) :mod-color first :color :name)))))

;; --- tokens

(defn- token-lib
  [set-id token-id value]
  (-> (ctob/make-tokens-lib)
      (ctob/add-set (ctob/make-token-set {:id set-id :name "core"}))
      (ctob/add-token set-id (ctob/make-token {:id token-id
                                               :name "color.primary"
                                               :type :color
                                               :value value}))))

(defn- with-tokens
  [lib]
  (assoc (mkdata {}) :tokens-lib lib))

(t/deftest compute-changes-token-value-roundtrip
  (let [sid (uuid/next)
        tid (uuid/next)
        base-lib   (token-lib sid tid "#ff0000")
        branch-lib (ctob/update-token base-lib sid tid
                                      (fn [t] (ctob/make-token (assoc (into {} t) :value "#0000ff"))))
        base   (with-tokens base-lib)
        main   (with-tokens base-lib)
        branch (with-tokens branch-lib)

        {:keys [changes unsupported]} (bm/compute-changes base main branch)
        set-token-change (first (filter #(= :set-token (:type %)) changes))]

    (t/is (empty? unsupported))
    (t/is (= sid (:set-id set-token-change)))
    (t/is (= tid (:token-id set-token-change)))
    (t/is (= "#0000ff" (-> set-token-change :attrs :value)))

    ;; round-trip: apply the change to main's tokens-lib and read the value back
    (let [data' (cfc/process-changes {:tokens-lib base-lib} changes)
          tok   (ctob/get-token (:tokens-lib data') sid tid)]
      (t/is (= "#0000ff" (:value tok))))))

(t/deftest compute-changes-token-set-add-is-unsupported
  (let [sid  (uuid/next)
        tid  (uuid/next)
        sid2 (uuid/next)
        base-lib   (token-lib sid tid "#ff0000")
        branch-lib (ctob/add-set base-lib (ctob/make-token-set {:id sid2 :name "extra"}))
        {:keys [unsupported]} (bm/compute-changes (with-tokens base-lib)
                                                  (with-tokens base-lib)
                                                  (with-tokens branch-lib))]
    (t/is (contains? unsupported :token-set))))
