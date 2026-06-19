;; This Source Code Form is subject to the terms of the Mozilla Public
;; License v. 2.0. If a copy of the MPL was not distributed with this
;; file You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC Sucursal en España SL

(ns app.main.ui.workspace.sidebar.branches
  (:require-macros [app.main.style :as stl])
  (:require
   [app.common.data.macros :as dm]
   [app.main.data.modal :as modal]
   [app.main.data.workspace.branches :as dwb]
   [app.main.refs :as refs]
   [app.main.store :as st]
   [app.main.ui.ds.buttons.button :refer [button*]]
   [app.main.ui.ds.buttons.icon-button :refer [icon-button*]]
   [app.main.ui.ds.controls.input :refer [input*]]
   [app.main.ui.ds.foundations.assets.icon :as i]
   [app.main.ui.ds.notifications.context-notification :refer [context-notification*]]
   [app.main.ui.ds.product.empty-state :refer [empty-state*]]
   [app.util.dom :as dom]
   [app.util.i18n :refer [tr]]
   [cuerdas.core :as str]
   [okulary.core :as l]
   [rumext.v2 :as mf]))

(def ^:private branches
  (l/derived :workspace-branches st/state))

(def ^:private branch-diff
  (l/derived :workspace-branch-diff st/state))

(def ^:private kind->icon
  {:shape      i/board
   :component  i/component
   :color      i/picker
   :typography i/text
   :media      i/img
   :page       i/document})

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
  [{:keys [file-name]}]
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
         (mf/deps branch-name description valid?)
         (fn [_]
           (when valid?
             (st/emit! (dwb/create-branch (str/trim branch-name) (str/trim description))
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

        on-open
        (mf/use-fn
         (mf/deps entry)
         (fn [_]
           (st/emit! (dwb/open-branch (:branch-file-id entry)))))

        on-compare
        (mf/use-fn
         (mf/deps entry)
         (fn [event]
           (dom/stop-propagation event)
           (modal/show! :branch-compare {:branch entry})))]

    [:li {:class (stl/css :branch-entry)
          :role "button"
          :on-click on-open}
     [:div {:class (stl/css :branch-entry-icon)}
      [:> i/icon* {:icon-id i/git-branch}]]

     [:div {:class (stl/css :branch-entry-body)}
      [:span {:class (stl/css :branch-entry-name)} (:name entry)]
      [:div {:class (stl/css :branch-entry-meta)}
       [:span {:class (stl/css :branch-entry-author)}
        (:fullname author)]]]

     [:div {:class (stl/css :branch-entry-counts)}
      [:span {:class (stl/css :count-ahead)}
       [:> i/icon* {:icon-id i/arrow-up :size "s"}]
       (dm/str ahead)]
      [:span {:class (stl/css :count-behind)}
       [:> i/icon* {:icon-id i/arrow-down :size "s"}]
       (dm/str behind)]]

     [:> icon-button* {:variant "ghost"
                       :icon i/switch
                       :aria-label (tr "workspace.branches.compare")
                       :on-click on-compare}]]))

;; --- Branches panel

(mf/defc branches-toolbox*
  []
  (let [profiles (mf/deref refs/profiles)
        file     (mf/deref refs/file)

        {:keys [status data] :as _state}
        (mf/deref branches)

        filter*  (mf/use-state "")
        filter-v (deref filter*)

        entries
        (mf/with-memo [data filter-v]
          (->> data
               (filter #(or (str/blank? filter-v)
                            (str/includes? (str/lower (or (:name %) ""))
                                           (str/lower filter-v))))))

        on-filter-change
        (mf/use-fn #(reset! filter* (dom/get-target-val %)))

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
       [:ul {:class (stl/css :branches-entries)}
        (for [entry entries]
          [:> branch-entry* {:key (dm/str (:id entry))
                             :entry entry
                             :profiles profiles}])])]))

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
        on-select (mf/use-fn #(st/emit! (dwb/select-diff-change %)))]

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
          [:> branch-compare-detail* {:item sel-item}]]])]]))
