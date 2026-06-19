;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC Sucursal en España SL

(ns common-tests.files-branch-merge-test
  (:require
   [app.common.files.branch-merge :as bm]
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
