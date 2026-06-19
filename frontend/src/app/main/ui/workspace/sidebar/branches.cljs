;; This Source Code Form is subject to the terms of the Mozilla Public
;; License v. 2.0. If a copy of the MPL was not distributed with this
;; file You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC Sucursal en España SL

(ns app.main.ui.workspace.sidebar.branches
  (:require-macros [app.main.style :as stl])
  (:require
   [app.common.data.macros :as dm]
   [app.config :as cf]
   [app.main.data.modal :as modal]
   [app.main.data.workspace.branches :as dwb]
   [app.main.refs :as refs]
   [app.main.store :as st]
   [app.main.ui.components.dropdown :refer [dropdown]]
   [app.main.ui.ds.buttons.button :refer [button*]]
   [app.main.ui.ds.buttons.icon-button :refer [icon-button*]]
   [app.main.ui.ds.controls.input :refer [input*]]
   [app.main.ui.ds.foundations.assets.icon :as i]
   [app.main.ui.ds.notifications.context-notification :refer [context-notification*]]
   [app.main.ui.ds.product.empty-state :refer [empty-state*]]
   [app.util.dom :as dom]
   [app.util.i18n :refer [tr]]
   [app.util.keyboard :as kbd]
   [cuerdas.core :as str]
   [okulary.core :as l]
   [rumext.v2 :as mf]))

(def ^:private branches
  (l/derived :workspace-branches st/state))

(def ^:private branch-diff
  (l/derived :workspace-branch-diff st/state))

(def ^:private branch-context
  (l/derived :workspace-branch-context st/state))

(def ^:private kind->icon
  {:shape               i/board
   :component           i/component
   :color               i/picker
   :typography          i/text
   :media               i/img
   :page                i/document
   :page-attrs          i/document
   :page-order          i/document
   :token               i/tokens
   :token-set           i/tokens
   :token-set-rename    i/tokens
   :token-set-order     i/tokens
   :token-theme         i/tokens
   :token-active-themes i/tokens
   :token-active-sets   i/tokens})

(def ^:private status->label
  {:added    "workspace.branches.status.added"
   :modified "workspace.branches.status.modified"
   :deleted  "workspace.branches.status.deleted"
   :conflict "workspace.branches.status.conflict"})

(defn- short-str
  [v]
  (let [s (pr-str v)]
    (if (> (count s) 40)
      (str (subs s 0 40) "…")
      s)))

;; --- Create branch dialog (modal)

(mf/defc create-branch-dialog*
  {::mf/register modal/components
   ::mf/register-as :create-branch}
  [{:keys [file-name file-id]}]
  (let [branch-name* (mf/use-state "")
        description*  (mf/use-state "")
        branch-name   (deref branch-name*)
        description   (deref description*)
        valid?        (not (str/blank? branch-name))

        on-name-change
        (mf/use-fn #(reset! branch-name* (dom/get-target-val %)))

        on-description-change
        (mf/use-fn #(reset! description* (dom/get-target-val %)))

        on-close
        (mf/use-fn #(st/emit! (modal/hide)))

        on-submit
        (mf/use-fn
         (mf/deps branch-name description valid? file-id)
         (fn [_]
           (when valid?
             (st/emit! (dwb/create-branch file-id (str/trim branch-name) (str/trim description))
                       (modal/hide)))))]

    [:div {:class (stl/css :modal-overlay)}
     [:div {:class (stl/css :modal-container)}
      [:div {:class (stl/css :modal-header)}
       [:div {:class (stl/css :modal-header-icon)}
        [:> i/icon* {:icon-id i/git-branch}]]
       [:div {:class (stl/css :modal-header-text)}
        [:h2 {:class (stl/css :modal-title)} (tr "workspace.branches.create.title")]
        [:span {:class (stl/css :modal-subtitle)}
         (tr "workspace.branches.create.subtitle" (or file-name ""))]]
       [:> button* {:variant "ghost"
                    :icon i/close
                    :aria-label (tr "labels.close")
                    :on-click on-close}]]

      [:div {:class (stl/css :modal-content)}
       [:> input* {:label (tr "workspace.branches.create.name-label")
                   :icon i/git-branch
                   :placeholder (tr "workspace.branches.create.name-placeholder")
                   :value branch-name
                   :on-change on-name-change}]

       [:div {:class (stl/css :field)}
        [:label {:class (stl/css :field-label)}
         (tr "workspace.branches.create.description-label")]
        [:textarea {:class (stl/css :textarea)
                    :value description
                    :on-change on-description-change
                    :rows 2}]]

       [:div {:class (stl/css :field)}
        [:label {:class (stl/css :field-label)}
         (tr "workspace.branches.create.derive-from")]
        [:div {:class (stl/css :derive-from)}
         [:span {:class (stl/css :derive-from-dot)}]
         [:span {:class (stl/css :derive-from-name)}
          (tr "workspace.branches.create.derive-from-main")]
         [:span {:class (stl/css :derive-from-hint)}
          (tr "workspace.branches.create.derive-from-latest")]]]

       [:> context-notification* {:level :info :type :context}
        (tr "workspace.branches.create.info")]]

      [:div {:class (stl/css :modal-footer)}
       [:> button* {:variant "ghost" :on-click on-close}
        (tr "labels.cancel")]
       [:> button* {:variant "primary"
                    :icon i/git-branch
                    :disabled (not valid?)
                    :on-click on-submit}
        (tr "workspace.branches.create.submit")]]]]))

;; --- Branch list entry

(mf/defc branch-entry*
  {::mf/private true}
  [{:keys [entry profiles]}]
  (let [author    (get profiles (:created-by entry))
        ahead     (:ahead entry)
        behind    (:behind entry)
        archived? (contains? #{"archived" "merged"} (:status entry))

        show-menu? (mf/use-state false)
        editing?   (mf/use-state false)

        on-open
        (mf/use-fn
         (mf/deps entry editing?)
         (fn [_]
           (when-not (deref editing?)
             (st/emit! (dwb/open-branch (:branch-file-id entry))))))

        on-compare
        (mf/use-fn
         (mf/deps entry)
         (fn [event]
           (dom/stop-propagation event)
           (modal/show! :branch-compare {:branch entry})))

        on-update
        (mf/use-fn
         (mf/deps entry)
         (fn [event]
           (dom/stop-propagation event)
           (st/emit! (dwb/update-branch-from-main entry))))

        on-open-menu
        (mf/use-fn (fn [event]
                     (dom/stop-propagation event)
                     (reset! show-menu? true)))
        on-close-menu (mf/use-fn #(reset! show-menu? false))

        on-start-rename
        (mf/use-fn (fn [] (reset! show-menu? false) (reset! editing? true)))

        on-rename-commit
        (mf/use-fn
         (mf/deps entry)
         (fn [event]
           (let [value (str/trim (dom/get-target-val event))]
             (when (and (seq value) (not= value (:name entry)))
               (st/emit! (dwb/rename-branch (:id entry) value)))
             (reset! editing? false))))

        on-rename-key-down
        (mf/use-fn
         (mf/deps on-rename-commit)
         (fn [event]
           (cond
             (kbd/enter? event) (on-rename-commit event)
             (kbd/esc? event)   (reset! editing? false))))

        on-archive
        (mf/use-fn (mf/deps entry)
                   (fn [] (reset! show-menu? false)
                     (st/emit! (dwb/archive-branch (:id entry) (not archived?)))))

        on-delete
        (mf/use-fn
         (mf/deps entry)
         (fn []
           (reset! show-menu? false)
           (st/emit! (modal/show {:type :confirm
                                  :title (tr "workspace.branches.delete.title")
                                  :message (tr "workspace.branches.delete.message" (:name entry))
                                  :accept-label (tr "labels.delete")
                                  :accept-style :danger
                                  :on-accept (fn [_] (st/emit! (dwb/delete-branch (:id entry))))}))))]

    [:li {:class (stl/css-case :branch-entry true :is-archived archived?)
          :role "button"
          :on-click on-open}
     [:div {:class (stl/css :branch-entry-icon)}
      [:> i/icon* {:icon-id i/git-branch}]]

     [:div {:class (stl/css :branch-entry-body)}
      (if (deref editing?)
        [:input {:class (stl/css :branch-rename-input)
                 :default-value (:name entry)
                 :auto-focus true
                 :on-click (fn [e] (dom/stop-propagation e))
                 :on-blur on-rename-commit
                 :on-key-down on-rename-key-down}]
        [:span {:class (stl/css :branch-entry-name)} (:name entry)])
      [:div {:class (stl/css :branch-entry-meta)}
       [:span {:class (stl/css :branch-entry-author)}
        (:fullname author)]]]

     (when-not archived?
       [:div {:class (stl/css :branch-entry-counts)}
        [:span {:class (stl/css :count-ahead)}
         [:> i/icon* {:icon-id i/arrow-up :size "s"}]
         (dm/str ahead)]
        [:span {:class (stl/css :count-behind)}
         [:> i/icon* {:icon-id i/arrow-down :size "s"}]
         (dm/str behind)]])

     (when (and (not archived?) (pos? behind))
       [:> icon-button* {:variant "ghost"
                         :icon i/status-update
                         :aria-label (tr "workspace.branches.update")
                         :on-click on-update}])

     (when-not archived?
       [:> icon-button* {:variant "ghost"
                         :icon i/switch
                         :aria-label (tr "workspace.branches.compare")
                         :on-click on-compare}])

     [:> icon-button* {:variant "ghost"
                       :icon i/menu
                       :aria-label (tr "labels.options")
                       :on-click on-open-menu}]

     [:& dropdown {:show (deref show-menu?) :on-close on-close-menu}
      [:ul {:class (stl/css :branch-options-dropdown)}
       (when-not archived?
         [:li {:class (stl/css :menu-option) :role "button" :on-click on-start-rename}
          (tr "labels.rename")])
       [:li {:class (stl/css :menu-option) :role "button" :on-click on-archive}
        (tr (if archived?
              "workspace.branches.menu.restore"
              "workspace.branches.menu.archive"))]
       [:li {:class (stl/css :menu-option) :role "button" :on-click on-delete}
        (tr "labels.delete")]]]]))

;; --- Branches panel

(mf/defc branches-toolbox*
  []
  (let [profiles (mf/deref refs/profiles)
        file     (mf/deref refs/file)

        {:keys [status data] :as _state}
        (mf/deref branches)

        filter*  (mf/use-state "")
        filter-v (deref filter*)

        show-archived? (mf/use-state false)

        entries
        (mf/with-memo [data filter-v]
          (->> data
               (filter #(or (str/blank? filter-v)
                            (str/includes? (str/lower (or (:name %) ""))
                                           (str/lower filter-v))))))

        open-entries     (filter #(= "open" (:status %)) entries)
        archived-entries (remove #(= "open" (:status %)) entries)

        on-filter-change
        (mf/use-fn #(reset! filter* (dom/get-target-val %)))

        on-toggle-archived
        (mf/use-fn #(swap! show-archived? not))

        on-create
        (mf/use-fn
         (mf/deps file)
         (fn [_]
           (modal/show! :create-branch {:file-name (:name file)})))]

    (mf/with-effect []
      (st/emit! (dwb/init-branches-state)))

    [:div {:class (stl/css :branches-toolbox)}
     [:div {:class (stl/css :branches-header)}
      [:> input* {:variant "dense"
                  :icon i/search
                  :placeholder (tr "workspace.branches.search.placeholder")
                  :value filter-v
                  :on-change on-filter-change}]
      [:> button* {:variant "primary"
                   :icon i/add
                   :on-click on-create}
       (tr "workspace.branches.new")]]

     (cond
       (= status :loading)
       [:div {:class (stl/css :branches-empty)}
        [:> empty-state* {:icon i/git-branch
                          :text (tr "workspace.branches.loading")}]]

       (empty? entries)
       [:div {:class (stl/css :branches-empty)}
        [:> empty-state* {:icon i/git-branch
                          :text (tr "workspace.branches.empty")}]]

       :else
       [:div {:class (stl/css :branches-list)}
        [:ul {:class (stl/css :branches-entries)}
         (for [entry open-entries]
           [:> branch-entry* {:key (dm/str (:id entry))
                              :entry entry
                              :profiles profiles}])]

        (when (seq archived-entries)
          [:div {:class (stl/css :branches-archived)}
           [:button {:class (stl/css :branches-archived-toggle)
                     :on-click on-toggle-archived}
            [:> i/icon* {:icon-id (if (deref show-archived?) i/arrow-down i/arrow-right) :size "s"}]
            [:span (tr "workspace.branches.archived")]
            [:span {:class (stl/css :branches-archived-count)} (dm/str (count archived-entries))]]
           (when (deref show-archived?)
             [:ul {:class (stl/css :branches-entries)}
              (for [entry archived-entries]
                [:> branch-entry* {:key (dm/str (:id entry))
                                   :entry entry
                                   :profiles profiles}])])])])]))

;; --- Compare changes dialog (read-only 3-way diff)

(mf/defc branch-compare-item*
  {::mf/private true}
  [{:keys [item index selected on-select]}]
  (let [on-click (mf/use-fn (mf/deps index on-select) #(on-select index))
        status   (:status item)]
    [:li {:class (stl/css-case :compare-item true
                               :is-selected (= index selected))
          :role "button"
          :on-click on-click}
     [:> i/icon* {:icon-id (get kind->icon (:kind item) i/git-branch)}]
     [:span {:class (stl/css :compare-item-label)} (:label item)]
     [:span {:class (stl/css-case :item-badge true
                                  :badge-added    (= :added status)
                                  :badge-modified (= :modified status)
                                  :badge-deleted  (= :deleted status)
                                  :badge-conflict (= :conflict status))}
      (tr (get status->label status "workspace.branches.status.modified"))]]))

(mf/defc branch-compare-detail*
  {::mf/private true}
  [{:keys [item]}]
  (let [attrs (:changed-attrs item)]
    (cond
      (nil? item)
      [:p {:class (stl/css :compare-detail-hint)}
       (tr "workspace.branches.compare.select-hint")]

      (seq attrs)
      [:div {:class (stl/css :attr-list)}
       [:div {:class (stl/css :attr-row :attr-head)}
        [:span (tr "workspace.branches.compare.attr")]
        [:span (tr "workspace.branches.compare.main")]
        [:span (tr "workspace.branches.compare.branch")]]
       (for [[attr {:keys [main branch]}] attrs]
         [:div {:class (stl/css :attr-row) :key (str attr)}
          [:span {:class (stl/css :attr-name)} (name attr)]
          [:span {:class (stl/css :attr-main)} (short-str main)]
          [:span {:class (stl/css :attr-branch)} (short-str branch)]])]

      :else
      [:p {:class (stl/css :compare-detail-hint)} (:label item)])))

(mf/defc branch-compare-dialog*
  {::mf/register modal/components
   ::mf/register-as :branch-compare}
  [{:keys [branch]}]
  (let [{:keys [status diff selected]} (mf/deref branch-diff)

        items
        (mf/with-memo [diff]
          (into (vec (:changes diff)) (:conflicts diff)))

        stats    (:stats diff)
        sel-item (when (and (some? selected) (< selected (count items)))
                   (nth items selected))

        on-close  (mf/use-fn #(st/emit! (modal/hide)))
        on-select (mf/use-fn #(st/emit! (dwb/select-diff-change %)))
        on-merge   (mf/use-fn (mf/deps branch)
                              #(st/emit! (dwb/merge-branch (:id branch))))
        on-resolve (mf/use-fn (mf/deps branch)
                              #(modal/show! :branch-conflicts {:branch branch}))]

    (mf/with-effect [(:id branch)]
      (st/emit! (dwb/fetch-branch-diff (:id branch))))

    [:div {:class (stl/css :compare-overlay)}
     [:div {:class (stl/css :compare-container)}
      [:div {:class (stl/css :compare-header)}
       [:div {:class (stl/css :compare-title-group)}
        [:> i/icon* {:icon-id i/switch}]
        [:div
         [:h2 {:class (stl/css :modal-title)} (tr "workspace.branches.compare.title")]
         [:span {:class (stl/css :compare-subtitle)}
          (dm/str (:name branch) " → main")]]]

       (when stats
         [:div {:class (stl/css :compare-stats)}
          [:span {:class (stl/css :stat-badge :badge-added)}
           (dm/str (:added stats) " " (tr "workspace.branches.compare.stats.added"))]
          [:span {:class (stl/css :stat-badge :badge-modified)}
           (dm/str (:modified stats) " " (tr "workspace.branches.compare.stats.modified"))]
          [:span {:class (stl/css :stat-badge :badge-deleted)}
           (dm/str (:deleted stats) " " (tr "workspace.branches.compare.stats.deleted"))]
          (when (pos? (:conflicts stats))
            [:span {:class (stl/css :stat-badge :badge-conflict)}
             (dm/str (:conflicts stats) " " (tr "workspace.branches.compare.stats.conflicts"))])])

       [:> icon-button* {:variant "ghost"
                         :icon i/close
                         :aria-label (tr "labels.close")
                         :on-click on-close}]]

      (cond
        (= status :loading)
        [:div {:class (stl/css :compare-empty)}
         [:> empty-state* {:icon i/switch
                           :text (tr "workspace.branches.compare.loading")}]]

        (= status :error)
        [:div {:class (stl/css :compare-empty)}
         [:> empty-state* {:icon i/triangle-alert
                           :text (tr "workspace.branches.create.error")}]]

        (empty? items)
        [:div {:class (stl/css :compare-empty)}
         [:> empty-state* {:icon i/git-branch
                           :text (tr "workspace.branches.compare.empty")}]]

        :else
        [:div {:class (stl/css :compare-body)}
         [:ul {:class (stl/css :compare-list)}
          (for [[idx item] (map-indexed vector items)]
            [:> branch-compare-item* {:key idx
                                      :item item
                                      :index idx
                                      :selected selected
                                      :on-select on-select}])]
         [:div {:class (stl/css :compare-detail)}
          [:> branch-compare-detail* {:item sel-item}]]])

      (when (= status :loaded)
        (let [conflicts? (pos? (long (or (:conflicts stats) 0)))
              total      (long (+ (or (:added stats) 0)
                                  (or (:modified stats) 0)
                                  (or (:deleted stats) 0)))]
          [:div {:class (stl/css :compare-footer)}
           (if conflicts?
             [:> button* {:variant "primary"
                          :icon i/triangle-alert
                          :on-click on-resolve}
              (tr "workspace.branches.conflicts.resolve")]
             [:> button* {:variant "primary"
                          :icon i/git-merge
                          :disabled (zero? total)
                          :on-click on-merge}
              (tr "workspace.branches.merge.action")])]))]]))

;; --- Resolve conflicts dialog

(mf/defc branch-conflict-item*
  {::mf/private true}
  [{:keys [conflict index selected resolution on-select]}]
  (let [on-click (mf/use-fn (mf/deps index on-select) #(on-select index))]
    [:li {:class (stl/css-case :compare-item true
                               :is-selected (= index selected))
          :role "button"
          :on-click on-click}
     [:> i/icon* {:icon-id (get kind->icon (:kind conflict) i/git-branch)}]
     [:span {:class (stl/css :compare-item-label)} (:label conflict)]
     (cond
       (= resolution :main)
       [:span {:class (stl/css :resolution-badge :resolved-main)} (tr "workspace.branches.conflicts.chosen-main")]
       (= resolution :branch)
       [:span {:class (stl/css :resolution-badge :resolved-branch)} (tr "workspace.branches.conflicts.chosen-branch")]
       :else
       [:span {:class (stl/css :resolution-badge :resolved-pending)}
        [:> i/icon* {:icon-id i/triangle-alert :size "s"}]])]))

(mf/defc branch-conflicts-dialog*
  {::mf/register modal/components
   ::mf/register-as :branch-conflicts}
  [{:keys [branch mode]}]
  (let [{:keys [diff selected resolutions]} (mf/deref branch-diff)

        conflicts   (:conflicts diff)
        resolutions (or resolutions {})
        total       (count conflicts)
        resolved    (count (filterv #(contains? #{:main :branch} (get resolutions (:id %))) conflicts))
        all-done?   (and (pos? total) (= resolved total))

        sel-idx     (min (or selected 0) (max 0 (dec total)))
        sel         (when (seq conflicts) (nth conflicts sel-idx))
        sel-res     (when sel (get resolutions (:id sel)))

        on-close      (mf/use-fn #(st/emit! (modal/hide)))
        on-select     (mf/use-fn #(st/emit! (dwb/select-diff-change %)))
        on-all-main   (mf/use-fn #(st/emit! (dwb/set-all-resolutions :main)))
        on-all-branch (mf/use-fn #(st/emit! (dwb/set-all-resolutions :branch)))
        on-use-main   (mf/use-fn (mf/deps sel)
                                 #(when sel (st/emit! (dwb/set-conflict-resolution (:id sel) :main))))
        on-use-branch (mf/use-fn (mf/deps sel)
                                 #(when sel (st/emit! (dwb/set-conflict-resolution (:id sel) :branch))))
        on-apply      (mf/use-fn (mf/deps branch resolutions mode)
                                 #(st/emit! (if (= mode :update)
                                              (dwb/update-branch-from-main branch resolutions)
                                              (dwb/merge-branch (:id branch) resolutions))))]

    (mf/with-effect [(:id branch)]
      (st/emit! (dwb/fetch-branch-diff (:id branch))))

    [:div {:class (stl/css :compare-overlay)}
     [:div {:class (stl/css :compare-container)}
      [:div {:class (stl/css :compare-header)}
       [:div {:class (stl/css :compare-title-group)}
        [:> i/icon* {:icon-id i/triangle-alert}]
        [:div
         [:h2 {:class (stl/css :modal-title)} (tr "workspace.branches.conflicts.title")]
         [:span {:class (stl/css :compare-subtitle)}
          (tr "workspace.branches.conflicts.subtitle" (:name branch))]]]
       [:span {:class (stl/css :conflicts-progress)}
        (tr "workspace.branches.conflicts.progress" (str resolved) (str total))]
       [:> icon-button* {:variant "ghost"
                         :icon i/close
                         :aria-label (tr "labels.close")
                         :on-click on-close}]]

      [:div {:class (stl/css :bulk-actions)}
       [:> button* {:variant "ghost" :icon i/git-branch :on-click on-all-main}
        (tr "workspace.branches.conflicts.all-main")]
       [:> button* {:variant "ghost" :icon i/git-branch :on-click on-all-branch}
        (tr "workspace.branches.conflicts.all-branch")]]

      (if (empty? conflicts)
        [:div {:class (stl/css :compare-empty)}
         [:> empty-state* {:icon i/git-merge
                           :text (tr "workspace.branches.conflicts.empty")}]]

        [:div {:class (stl/css :compare-body)}
         [:ul {:class (stl/css :compare-list)}
          (for [[idx c] (map-indexed vector conflicts)]
            [:> branch-conflict-item* {:key idx
                                       :conflict c
                                       :index idx
                                       :selected sel-idx
                                       :resolution (get resolutions (:id c))
                                       :on-select on-select}])]

         [:div {:class (stl/css :compare-detail)}
          [:> branch-compare-detail* {:item sel}]
          [:div {:class (stl/css :choice-buttons)}
           [:> button* {:variant (if (= sel-res :main) "primary" "secondary")
                        :on-click on-use-main}
            (tr "workspace.branches.conflicts.use-main")]
           [:> button* {:variant (if (= sel-res :branch) "primary" "secondary")
                        :on-click on-use-branch}
            (tr "workspace.branches.conflicts.use-branch")]]]])

      [:div {:class (stl/css :compare-footer)}
       [:> button* {:variant "ghost" :on-click on-close} (tr "labels.cancel")]
       [:> button* {:variant "primary"
                    :icon i/git-merge
                    :disabled (not all-done?)
                    :on-click on-apply}
        (tr "workspace.branches.conflicts.apply")]]]]))

;; --- Branch context banner (shown while editing a branch)

(mf/defc branch-context-banner*
  [{:keys [file-id]}]
  (let [ctx     (mf/deref branch-context)
        merged? (contains? #{"merged" "archived"} (:status ctx))

        on-compare   (mf/use-fn (mf/deps ctx) #(modal/show! :branch-compare {:branch ctx}))
        on-update    (mf/use-fn (mf/deps ctx) #(st/emit! (dwb/update-branch-from-main ctx)))
        on-merge     (mf/use-fn (mf/deps ctx) #(st/emit! (dwb/merge-branch (:id ctx))))
        on-open-main (mf/use-fn (mf/deps ctx) #(st/emit! (dwb/open-branch (:source-file-id ctx))))]

    (mf/with-effect [file-id]
      (when (contains? cf/flags :branching)
        (st/emit! (dwb/fetch-branch-context))))

    (when ctx
      [:div {:class (stl/css-case :branch-banner true
                                  :branch-banner-merged merged?)}
       [:div {:class (stl/css :branch-banner-icon)}
        [:> i/icon* {:icon-id i/git-branch}]]
       [:div {:class (stl/css :branch-banner-text)}
        [:span {:class (stl/css :branch-banner-title)}
         (tr (if merged?
               "workspace.branches.banner.merged"
               "workspace.branches.banner.on-branch"))]
        [:span {:class (stl/css :branch-banner-name)} (:name ctx)]
        [:span {:class (stl/css :branch-banner-from)} (tr "workspace.branches.banner.from-main")]]

       (if merged?
         [:> button* {:variant "secondary"
                      :icon i/arrow-up-right
                      :on-click on-open-main}
          (tr "workspace.branches.banner.view-main")]

         [:div {:class (stl/css :branch-banner-actions)}
          [:span {:class (stl/css :branch-banner-counts)}
           [:span {:class (stl/css :count-ahead)}
            [:> i/icon* {:icon-id i/arrow-up :size "s"}] (dm/str (:ahead ctx))]
           [:span {:class (stl/css :count-behind)}
            [:> i/icon* {:icon-id i/arrow-down :size "s"}] (dm/str (:behind ctx))]]

          [:> button* {:variant "secondary"
                       :icon i/switch
                       :on-click on-compare}
           (tr "workspace.branches.compare")]

          (when (pos? (:behind ctx))
            [:> button* {:variant "primary"
                         :icon i/status-update
                         :on-click on-update}
             (tr "workspace.branches.update")])

          (when (and (zero? (:behind ctx)) (pos? (:ahead ctx)))
            [:> button* {:variant "primary"
                         :icon i/git-merge
                         :on-click on-merge}
             (tr "workspace.branches.merge.action")])])])))
