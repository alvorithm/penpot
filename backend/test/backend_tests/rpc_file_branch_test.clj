;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC Sucursal en España SL

(ns backend-tests.rpc-file-branch-test
  (:require
   [app.common.features :as cfeat]
   [app.common.uuid :as uuid]
   [app.config :as cf]
   [app.rpc :as-alias rpc]
   [backend-tests.helpers :as th]
   [clojure.test :as t]))

(t/use-fixtures :once th/state-init)
(t/use-fixtures :each th/database-reset)

(t/deftest create-and-list-branches
  (with-redefs [cf/flags (conj cf/flags :branching)]
    (let [profile (th/create-profile* 1 {:is-active true})
          proj-id (:default-project-id profile)
          file    (th/create-file* 1 {:profile-id (:id profile)
                                      :project-id proj-id
                                      :is-shared false})
          branch-file-id (volatile! nil)
          branch-meta-id (volatile! nil)]

      (t/testing "create branch"
        (let [params {::th/type :create-file-branch
                      ::rpc/profile-id (:id profile)
                      :file-id (:id file)
                      :name "redesign-checkout"
                      :description "A/B test de checkout"}
              out    (th/command! params)]
          ;; (th/print-result! out)
          (t/is (nil? (:error out)))
          (let [result (:result out)]
            (t/is (uuid? (:id result)))
            (t/is (uuid? (:branch-file-id result)))
            (t/is (= (:id file) (:source-file-id result)))
            (t/is (= "redesign-checkout" (:name result)))
            (t/is (= "open" (:status result)))
            (vreset! branch-file-id (:branch-file-id result))
            (vreset! branch-meta-id (:id result)))))

      (t/testing "branch file is flagged and hidden from project listing"
        (let [[row] (th/db-query :file {:id @branch-file-id})]
          (t/is (true? (:is-branch row))))

        (let [out (th/command! {::th/type :get-project-files
                                ::rpc/profile-id (:id profile)
                                :project-id proj-id})]
          (t/is (nil? (:error out)))
          (let [rows (:result out)
                ids  (set (map :id rows))
                src  (first (filter #(= (:id %) (:id file)) rows))]
            (t/is (contains? ids (:id file)))
            (t/is (not (contains? ids @branch-file-id)))
            ;; the source file card reports its open branch count
            (t/is (= 1 (:branches-count src))))))

      (t/testing "list branches"
        (let [out (th/command! {::th/type :get-file-branches
                                ::rpc/profile-id (:id profile)
                                :file-id (:id file)})]
          (t/is (nil? (:error out)))
          (let [[row :as result] (:result out)]
            (t/is (= 1 (count result)))
            (t/is (= "redesign-checkout" (:name row)))
            (t/is (= "open" (:status row)))
            (t/is (= 0 (:ahead row)))
            (t/is (= 0 (:behind row)))
            (t/is (= @branch-file-id (:branch-file-id row))))))

      (t/testing "branch context info"
        ;; the branch file reports its branch metadata
        (let [out (th/command! {::th/type :get-file-branch-info
                                ::rpc/profile-id (:id profile)
                                :file-id @branch-file-id})
              info (:result out)]
          (t/is (nil? (:error out)))
          (t/is (= "redesign-checkout" (:name info)))
          (t/is (= "open" (:status info)))
          (t/is (= (:id file) (:source-file-id info)))
          (t/is (= 0 (:ahead info)))
          (t/is (= 0 (:behind info))))
        ;; the main file is not a branch -> nil
        (let [out (th/command! {::th/type :get-file-branch-info
                                ::rpc/profile-id (:id profile)
                                :file-id (:id file)})]
          (t/is (nil? (:error out)))
          (t/is (nil? (:result out)))))

      (t/testing "merge base snapshot created on main"
        (let [rows (th/db-query :file-change {:file-id (:id file)})]
          (t/is (pos? (count rows)))
          (t/is (some #(= "system" (:created-by %)) rows))))

      (t/testing "diff right after creation has no conflicts"
        (let [out   (th/command! {::th/type :get-branch-diff
                                  ::rpc/profile-id (:id profile)
                                  :branch-id @branch-meta-id})
              stats (-> out :result :stats)]
          (t/is (nil? (:error out)))
          (t/is (map? stats))
          ;; base == main == branch at fork -> nothing to merge, no conflicts
          (t/is (= 0 (:conflicts stats)))))

      (t/testing "merge branch into main (clean, no-op)"
        (let [out (th/command! {::th/type :merge-file-branch
                                ::rpc/profile-id (:id profile)
                                :branch-id @branch-meta-id})]
          (t/is (nil? (:error out)))
          (t/is (= :merged (-> out :result :status))))
        (let [[row] (th/db-query :file-branch {:id @branch-meta-id})]
          (t/is (= "merged" (:status row))))))))

(t/deftest merge-applies-branch-changes
  (with-redefs [cf/flags (conj cf/flags :branching)]
    (let [profile (th/create-profile* 1 {:is-active true})
          proj-id (:default-project-id profile)
          file    (th/create-file* 1 {:profile-id (:id profile)
                                      :project-id proj-id
                                      :is-shared false})

          create  (:result (th/command! {::th/type :create-file-branch
                                         ::rpc/profile-id (:id profile)
                                         :file-id (:id file)
                                         :name "feature-colors"}))
          branch-file-id (:branch-file-id create)
          branch-id      (:id create)

          color-id (uuid/random)
          color    {:id color-id :name "Brand" :color "#ff0000" :opacity 1}]

      (t/testing "add a color on the branch"
        (let [bf  (th/db-get :file {:id branch-file-id})
              out (th/command! {::th/type :update-file
                                ::rpc/profile-id (:id profile)
                                :id branch-file-id
                                :session-id (uuid/random)
                                :revn (:revn bf)
                                :vern (:vern bf)
                                :features cfeat/supported-features
                                :changes [{:type :add-color :color color}]})]
          (t/is (nil? (:error out)))))

      (t/testing "merge brings the new color into main"
        (let [out (th/command! {::th/type :merge-file-branch
                                ::rpc/profile-id (:id profile)
                                :branch-id branch-id})]
          (t/is (nil? (:error out)))
          (t/is (= :merged (-> out :result :status))))

        (let [out    (th/command! {::th/type :get-file
                                   ::rpc/profile-id (:id profile)
                                   :id (:id file)})
              colors (-> out :result :data :colors)]
          (t/is (nil? (:error out)))
          (t/is (contains? colors color-id))
          (t/is (= "Brand" (get-in colors [color-id :name]))))))))

(t/deftest update-branch-pulls-main-changes
  (with-redefs [cf/flags (conj cf/flags :branching)]
    (let [profile (th/create-profile* 1 {:is-active true})
          proj-id (:default-project-id profile)
          file    (th/create-file* 1 {:profile-id (:id profile)
                                      :project-id proj-id
                                      :is-shared false})

          create  (:result (th/command! {::th/type :create-file-branch
                                         ::rpc/profile-id (:id profile)
                                         :file-id (:id file)
                                         :name "long-lived"}))
          branch-file-id (:branch-file-id create)
          branch-id      (:id create)

          color-id (uuid/random)
          color    {:id color-id :name "MainColor" :color "#00ff00" :opacity 1}]

      (t/testing "add a color on main (after the branch was created)"
        (let [mf  (th/db-get :file {:id (:id file)})
              out (th/command! {::th/type :update-file
                                ::rpc/profile-id (:id profile)
                                :id (:id file)
                                :session-id (uuid/random)
                                :revn (:revn mf)
                                :vern (:vern mf)
                                :features cfeat/supported-features
                                :changes [{:type :add-color :color color}]})]
          (t/is (nil? (:error out)))))

      (t/testing "update the branch from main"
        (let [out (th/command! {::th/type :update-branch-from-main
                                ::rpc/profile-id (:id profile)
                                :branch-id branch-id})]
          (t/is (nil? (:error out)))
          (t/is (= :updated (-> out :result :status))))

        (let [out    (th/command! {::th/type :get-file
                                   ::rpc/profile-id (:id profile)
                                   :id branch-file-id})
              colors (-> out :result :data :colors)]
          (t/is (contains? colors color-id)))

        ;; merge base repositioned to the current state of main
        (let [[row] (th/db-query :file-branch {:id branch-id})
              mf    (th/db-get :file {:id (:id file)})]
          (t/is (= (:revn mf) (:base-revn row))))))))

(t/deftest merge-applies-page-add
  (with-redefs [cf/flags (conj cf/flags :branching)]
    (let [profile (th/create-profile* 1 {:is-active true})
          proj-id (:default-project-id profile)
          file    (th/create-file* 1 {:profile-id (:id profile)
                                      :project-id proj-id
                                      :is-shared false})
          create  (:result (th/command! {::th/type :create-file-branch
                                         ::rpc/profile-id (:id profile)
                                         :file-id (:id file)
                                         :name "feature-page"}))
          branch-id      (:id create)
          branch-file-id (:branch-file-id create)
          new-page-id    (uuid/random)]

      (t/testing "add a page on the branch"
        (let [bf  (th/db-get :file {:id branch-file-id})
              out (th/command! {::th/type :update-file
                                ::rpc/profile-id (:id profile)
                                :id branch-file-id
                                :session-id (uuid/random)
                                :revn (:revn bf)
                                :vern (:vern bf)
                                :features cfeat/supported-features
                                :changes [{:type :add-page :id new-page-id :name "Branch Page"}]})]
          (t/is (nil? (:error out)))))

      (t/testing "merge brings the new page into main"
        (let [out (th/command! {::th/type :merge-file-branch
                                ::rpc/profile-id (:id profile)
                                :branch-id branch-id})]
          (t/is (nil? (:error out)))
          (t/is (= :merged (-> out :result :status))))

        (let [out   (th/command! {::th/type :get-file
                                  ::rpc/profile-id (:id profile)
                                  :id (:id file)})
              pages (-> out :result :data :pages-index)]
          (t/is (contains? pages new-page-id)))))))

(t/deftest branch-lifecycle
  (with-redefs [cf/flags (conj cf/flags :branching)]
    (let [profile (th/create-profile* 1 {:is-active true})
          proj-id (:default-project-id profile)
          file    (th/create-file* 1 {:profile-id (:id profile)
                                      :project-id proj-id
                                      :is-shared false})
          create  (:result (th/command! {::th/type :create-file-branch
                                         ::rpc/profile-id (:id profile)
                                         :file-id (:id file)
                                         :name "tmp"}))
          branch-id      (:id create)
          branch-file-id (:branch-file-id create)]

      (t/testing "rename"
        (let [out (th/command! {::th/type :update-file-branch
                                ::rpc/profile-id (:id profile)
                                :id branch-id
                                :name "renamed"})]
          (t/is (nil? (:error out))))
        (let [[row] (th/db-query :file-branch {:id branch-id})]
          (t/is (= "renamed" (:name row)))))

      (t/testing "archive hides it from the default list"
        (let [out (th/command! {::th/type :archive-file-branch
                                ::rpc/profile-id (:id profile)
                                :id branch-id})]
          (t/is (nil? (:error out)))
          (t/is (= "archived" (-> out :result :status))))
        (let [out (th/command! {::th/type :get-file-branches
                                ::rpc/profile-id (:id profile)
                                :file-id (:id file)})]
          (t/is (empty? (:result out))))
        (let [out (th/command! {::th/type :get-file-branches
                                ::rpc/profile-id (:id profile)
                                :file-id (:id file)
                                :include-archived true})]
          (t/is (= 1 (count (:result out))))))

      (t/testing "delete marks branch and branch file deleted"
        (let [out (th/command! {::th/type :delete-file-branch
                                ::rpc/profile-id (:id profile)
                                :id branch-id})]
          (t/is (nil? (:error out)))
          (t/is (= :deleted (-> out :result :status))))
        (let [[row]  (th/db-query :file-branch {:id branch-id})
              [frow] (th/db-query :file {:id branch-file-id})]
          (t/is (some? (:deleted-at row)))
          (t/is (some? (:deleted-at frow))))))))

(t/deftest branching-disabled-raises
  (let [profile (th/create-profile* 1 {:is-active true})
        proj-id (:default-project-id profile)
        file    (th/create-file* 1 {:profile-id (:id profile)
                                    :project-id proj-id
                                    :is-shared false})
        out     (th/command! {::th/type :create-file-branch
                              ::rpc/profile-id (:id profile)
                              :file-id (:id file)
                              :name "nope"})
        error   (:error out)
        data    (ex-data error)]
    (t/is (th/ex-info? error))
    (t/is (= :restriction (:type data)))
    (t/is (= :branching-disabled (:code data)))))
