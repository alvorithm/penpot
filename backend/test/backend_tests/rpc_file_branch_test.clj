;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC Sucursal en España SL

(ns backend-tests.rpc-file-branch-test
  (:require
   [app.common.features :as cfeat]
   [app.common.types.shape :as cts]
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
          (t/is (= (:name file) (:source-name info)))
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

      (t/testing "branch info reports diff-based ahead/behind"
        ;; one logical change on the branch (the added color), main untouched
        (let [info (:result (th/command! {::th/type :get-file-branch-info
                                          ::rpc/profile-id (:id profile)
                                          :file-id branch-file-id}))]
          (t/is (= 1 (:ahead info)))
          (t/is (= 0 (:behind info)))
          (t/is (= 0 (:conflicts info)))))

      (t/testing "branch list reports the same diff-based counts"
        (let [branches (:result (th/command! {::th/type :get-file-branches
                                              ::rpc/profile-id (:id profile)
                                              :file-id (:id file)}))
              row      (first (filter #(= branch-id (:id %)) branches))]
          (t/is (= 1 (:ahead row)))
          (t/is (= 0 (:behind row)))
          (t/is (= 0 (:conflicts row)))))

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

(t/deftest merge-reparents-existing-shape
  ;; main: a rect at the page root. branch: that rect moved into a new board.
  ;; after merge main must have the rect INSIDE the board and NOT loose at
  ;; root (regression: reparenting was applied as :set parent-id, which left
  ;; the shape duplicated/loose).
  (with-redefs [cf/flags (conj cf/flags :branching)]
    (let [profile (th/create-profile* 1 {:is-active true})
          proj-id (:default-project-id profile)
          file    (th/create-file* 1 {:profile-id (:id profile)
                                      :project-id proj-id
                                      :is-shared false})
          page-id (-> (th/command! {::th/type :get-file
                                    ::rpc/profile-id (:id profile)
                                    :id (:id file)})
                      :result :data :pages first)
          rect-id (uuid/random)
          board-id (uuid/random)]

      (t/testing "add a rect at the root of main"
        (let [mf  (th/db-get :file {:id (:id file)})
              out (th/command! {::th/type :update-file
                                ::rpc/profile-id (:id profile)
                                :id (:id file)
                                :session-id (uuid/random)
                                :revn (:revn mf)
                                :vern (:vern mf)
                                :features cfeat/supported-features
                                :changes [{:type :add-obj
                                           :page-id page-id
                                           :id rect-id
                                           :parent-id uuid/zero
                                           :frame-id uuid/zero
                                           :obj (cts/setup-shape
                                                 {:id rect-id :name "Rect" :type :rect
                                                  :parent-id uuid/zero :frame-id uuid/zero})}]})]
          (t/is (nil? (:error out)))))

      (let [create  (:result (th/command! {::th/type :create-file-branch
                                           ::rpc/profile-id (:id profile)
                                           :file-id (:id file)
                                           :name "into-board"}))
            branch-id      (:id create)
            branch-file-id (:branch-file-id create)]

        (t/testing "on the branch, move the rect into a new board"
          (let [bf  (th/db-get :file {:id branch-file-id})
                out (th/command! {::th/type :update-file
                                  ::rpc/profile-id (:id profile)
                                  :id branch-file-id
                                  :session-id (uuid/random)
                                  :revn (:revn bf)
                                  :vern (:vern bf)
                                  :features cfeat/supported-features
                                  :changes [{:type :add-obj
                                             :page-id page-id
                                             :id board-id
                                             :parent-id uuid/zero
                                             :frame-id uuid/zero
                                             :obj (cts/setup-shape
                                                   {:id board-id :name "Board" :type :frame
                                                    :parent-id uuid/zero :frame-id uuid/zero})}
                                            {:type :mov-objects
                                             :page-id page-id
                                             :parent-id board-id
                                             :shapes [rect-id]
                                             :index 0}]})]
            (t/is (nil? (:error out)))))

        (t/testing "merge places the rect inside the board, not loose"
          (let [out (th/command! {::th/type :merge-file-branch
                                  ::rpc/profile-id (:id profile)
                                  :branch-id branch-id})]
            (t/is (nil? (:error out)))
            (t/is (= :merged (-> out :result :status))))

          (let [objects (-> (th/command! {::th/type :get-file
                                          ::rpc/profile-id (:id profile)
                                          :id (:id file)})
                            :result :data :pages-index (get page-id) :objects)]
            ;; the board exists and contains the rect
            (t/is (contains? objects board-id))
            (t/is (= [rect-id] (get-in objects [board-id :shapes])))
            ;; the rect points to the board as its parent
            (t/is (= board-id (get-in objects [rect-id :parent-id])))
            ;; and is NOT left loose at the root
            (t/is (not (contains? (set (get-in objects [uuid/zero :shapes])) rect-id)))))))))

(t/deftest merge-component-stays-a-component
  ;; create a component on a branch, merge it into main, and verify the
  ;; merged main instance is still a valid component head (component-id set,
  ;; component-file re-pointed to main) — regression: it was detached.
  (with-redefs [cf/flags (conj cf/flags :branching)]
    (let [profile (th/create-profile* 1 {:is-active true})
          proj-id (:default-project-id profile)
          file    (th/create-file* 1 {:profile-id (:id profile)
                                      :project-id proj-id
                                      :is-shared false})
          page-id (-> (th/command! {::th/type :get-file
                                    ::rpc/profile-id (:id profile)
                                    :id (:id file)})
                      :result :data :pages first)
          create  (:result (th/command! {::th/type :create-file-branch
                                         ::rpc/profile-id (:id profile)
                                         :file-id (:id file)
                                         :name "with-component"}))
          branch-id      (:id create)
          branch-file-id (:branch-file-id create)
          comp-id (uuid/random)
          mi-id   (uuid/random)]

      (t/testing "create a component on the branch"
        (let [bf  (th/db-get :file {:id branch-file-id})
              out (th/command! {::th/type :update-file
                                ::rpc/profile-id (:id profile)
                                :id branch-file-id
                                :session-id (uuid/random)
                                :revn (:revn bf)
                                :vern (:vern bf)
                                :features cfeat/supported-features
                                :changes
                                [{:type :add-obj
                                  :page-id page-id
                                  :id mi-id
                                  :parent-id uuid/zero
                                  :frame-id uuid/zero
                                  :obj (-> (cts/setup-shape
                                            {:id mi-id :name "Component 01" :type :frame
                                             :parent-id uuid/zero :frame-id uuid/zero})
                                           (assoc :component-root true
                                                  :main-instance true
                                                  :component-id comp-id
                                                  :component-file branch-file-id))}
                                 {:type :add-component
                                  :id comp-id
                                  :name "Component 01"
                                  :path ""
                                  :main-instance-id mi-id
                                  :main-instance-page page-id}]})]
          (t/is (nil? (:error out)))))

      (t/testing "merge integrates the component into main as a real head"
        (let [out (th/command! {::th/type :merge-file-branch
                                ::rpc/profile-id (:id profile)
                                :branch-id branch-id})]
          (t/is (nil? (:error out)))
          (t/is (= :merged (-> out :result :status))))

        (let [data    (-> (th/command! {::th/type :get-file
                                        ::rpc/profile-id (:id profile)
                                        :id (:id file)})
                          :result :data)
              shape   (get-in data [:pages-index page-id :objects mi-id])]
          ;; the component registry row landed in main
          (t/is (contains? (:components data) comp-id))
          ;; the main instance is NOT detached: still a head pointing at main
          (t/is (= comp-id (:component-id shape)))
          (t/is (= (:id file) (:component-file shape)))
          (t/is (true? (:main-instance shape)))
          (t/is (true? (:component-root shape))))))))

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

(t/deftest update-resolves-conflict-to-main
  (with-redefs [cf/flags (conj cf/flags :branching)]
    (let [profile (th/create-profile* 1 {:is-active true})
          proj-id (:default-project-id profile)
          file    (th/create-file* 1 {:profile-id (:id profile)
                                      :project-id proj-id
                                      :is-shared false})
          cid     (uuid/random)
          apply-change
          (fn [file-id change]
            (let [f (th/db-get :file {:id file-id})]
              (th/command! {::th/type :update-file
                            ::rpc/profile-id (:id profile)
                            :id file-id
                            :session-id (uuid/random)
                            :revn (:revn f)
                            :vern (:vern f)
                            :features cfeat/supported-features
                            :changes [change]})))]

      ;; color exists on main before branching (base = red)
      (apply-change (:id file) {:type :add-color :color {:id cid :name "Brand" :color "#ff0000" :opacity 1}})

      (let [create (:result (th/command! {::th/type :create-file-branch
                                          ::rpc/profile-id (:id profile)
                                          :file-id (:id file)
                                          :name "b"}))
            branch-id      (:id create)
            branch-file-id (:branch-file-id create)]

        ;; main edits the color -> green; branch edits it -> blue (conflict)
        (apply-change (:id file) {:type :mod-color :color {:id cid :name "Brand" :color "#00ff00" :opacity 1}})
        (apply-change branch-file-id {:type :mod-color :color {:id cid :name "Brand" :color "#0000ff" :opacity 1}})

        (t/testing "update without resolutions reports conflicts"
          (let [out (th/command! {::th/type :update-branch-from-main
                                  ::rpc/profile-id (:id profile)
                                  :branch-id branch-id})]
            (t/is (= :conflicts (-> out :result :status)))))

        (t/testing "resolving to main brings main's value into the branch"
          (let [out (th/command! {::th/type :update-branch-from-main
                                  ::rpc/profile-id (:id profile)
                                  :branch-id branch-id
                                  :resolutions {cid :main}})]
            (t/is (nil? (:error out)))
            (t/is (= :updated (-> out :result :status))))
          (let [out   (th/command! {::th/type :get-file
                                    ::rpc/profile-id (:id profile)
                                    :id branch-file-id})
                color (get-in out [:result :data :colors cid])]
            (t/is (= "#00ff00" (:color color)))))))))

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
