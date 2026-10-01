;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns common-tests.files-migrations-test
  (:require
   [app.common.data :as d]
   [app.common.files.migrations :as cfm]
   [app.common.types.file :as ctf]
   [app.common.uuid :as uuid]
   [clojure.test :as t]))

(defmethod cfm/migrate-data "test/1" [data _] (update data :sum inc))
(defmethod cfm/migrate-data "test/2" [data _] (update data :sum inc))
(defmethod cfm/migrate-data "test/3" [data _] (update data :sum inc))

(t/deftest generic-migration-subsystem-1
  (let [migrations (into (d/ordered-set) ["test/1" "test/2" "test/3"])]
    (with-redefs [cfm/available-migrations migrations
                  ctf/check-file-data identity]
      (let [file  {:data {:sum 1}
                   :id 1
                   :migrations (d/ordered-set "test/1")}
            file' (cfm/migrate file nil)]
        (t/is (= cfm/available-migrations (:migrations file')))
        (t/is (= 3 (:sum (:data file'))))))))

(t/deftest data-version-migrations-cut-the-list-at-a-version
  (let [migrations (into (d/ordered-set)
                         ["legacy-2" "legacy-10" "0001-a" "0002-b" "0002-c" "0003-d" "0005b-f"])]
    (with-redefs [cfm/available-migrations migrations]
      (t/is (= "0005b-f" (cfm/data-version)))

      (t/testing "an available name ends the prefix at itself"
        (t/is (= ["legacy-2" "legacy-10" "0001-a" "0002-b"]
                 (vec (cfm/data-version-migrations "0002-b")))))

      (t/testing "a removed name ends the prefix at the nearest earlier number"
        (t/is (= ["legacy-2" "legacy-10" "0001-a" "0002-b" "0002-c" "0003-d"]
                 (vec (cfm/data-version-migrations "0004-e"))))
        (t/is (= ["legacy-2"]
                 (vec (cfm/data-version-migrations "legacy-5")))))

      (t/testing "a name with the same number replaces a removed one, so it is not earlier"
        (t/is (= ["legacy-2" "legacy-10" "0001-a" "0002-b" "0002-c" "0003-d"]
                 (vec (cfm/data-version-migrations "0005-f")))))

      (t/testing "a removed name with no number cuts nothing"
        (t/is (empty? (cfm/data-version-migrations "unknown")))))))

(t/deftest migrate-file-to-stops-at-a-version
  (let [migrations (into (d/ordered-set) ["test/1" "test/2" "test/3"])
        file       {:data {:sum 1}
                    :id 1
                    :migrations (d/ordered-set "test/1")}]
    ;; the file data schema describes the current version only, so a
    ;; document stopped at an older one is not checked against it
    (with-redefs [cfm/available-migrations migrations
                  ctf/check-file-data (fn [_] (throw (ex-info "checked" {})))]
      (t/is (cfm/need-migration-to? file "test/2"))
      (t/is (not (cfm/need-migration-to? file "test/1")))
      (t/is (not (cfm/need-migration-to? file nil)))

      (let [file' (cfm/migrate-file-to file nil "test/2")]
        (t/is (= 2 (-> file' :data :sum)))
        (t/is (= #{"test/1" "test/2"} (set (:migrations file'))))
        (t/is (not (cfm/need-migration-to? file' "test/2")))
        (t/is (cfm/need-migration-to? file' "test/3"))

        (t/testing "the step that reaches the current version is checked"
          (t/is (thrown? #?(:clj Exception :cljs :default)
                         (cfm/migrate-file-to file' nil "test/3"))))))))

(t/deftest migration-0024b-fix-stroke-cap-placement
  (let [shape-id (uuid/next)
        page-id  (uuid/next)
        data     {:pages-index
                  {page-id
                   {:objects
                    {shape-id {:id         shape-id
                               :type       :path
                               :stroke-cap-start "round"
                               :stroke-cap-end   "round"
                               :strokes    [{:stroke-color "#000000"
                                             :stroke-opacity 1
                                             :stroke-style :svg
                                             :stroke-width 2}
                                            {:stroke-color "#000000"
                                             :stroke-cap-start "round"
                                             :stroke-cap-end   "round"
                                             :stroke-opacity 1
                                             :stroke-style :svg
                                             :stroke-width 2}]}}}}}
        data'    (cfm/migrate-data data "0024b-fix-stroke-cap-placement")]

    (let [shape (get-in data' [:pages-index page-id :objects shape-id])]
      (t/is (nil? (:stroke-cap-start shape)) "top-level cap removed")
      (t/is (nil? (:stroke-cap-end shape)) "top-level cap removed")
      (t/is (= :round (get-in shape [:strokes 0 :stroke-cap-start])) "cap moved into stroke")
      (t/is (= :round (get-in shape [:strokes 0 :stroke-cap-end])) "cap moved into stroke")
      (t/is (= :round (get-in shape [:strokes 1 :stroke-cap-start])) "correct cap type")
      (t/is (= :round (get-in shape [:strokes 1 :stroke-cap-end])) "correct cap type"))))

(t/deftest migration-0024-fix-stroke-cap-no-strokes
  (let [shape-id (uuid/next)
        page-id  (uuid/next)
        data     {:pages-index
                  {page-id
                   {:objects
                    {shape-id {:id               shape-id
                               :type             :path
                               :stroke-cap-start :round
                               :stroke-cap-end   :round
                               :strokes          []}}}}}
        data'    (cfm/migrate-data data "0024b-fix-stroke-cap-placement")]

    (let [shape (get-in data' [:pages-index page-id :objects shape-id])]
      (t/is (nil? (:stroke-cap-start shape)) "top-level cap removed even with no strokes")
      (t/is (nil? (:stroke-cap-end shape)) "top-level cap removed even with no strokes"))))
