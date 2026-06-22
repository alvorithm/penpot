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
   [app.main.ui.components.dropdown-menu :refer [dropdown-menu*
                                                 dropdown-menu-item*]]
   [app.main.ui.ds.buttons.button :refer [button*]]
   [app.main.ui.ds.buttons.icon-button :refer [icon-button*]]
   [app.main.ui.ds.controls.input :refer [input*]]
   [app.main.ui.ds.foundations.assets.icon :as i]
   [app.main.ui.ds.notifications.context-notification :refer [context-notification*]]
   [app.main.ui.ds.product.avatar :refer [avatar*]]
   [app.main.ui.ds.product.empty-state :refer [empty-state*]]
   [app.util.dom :as dom]
   [app.util.globals :as globals]
   [app.util.i18n :refer [tr]]
   [app.util.keyboard :as kbd]
   [cuerdas.core :as str]
   [goog.events :as events]
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
   :page-guide          i/document
   :page-flow           i/document
   :page-grid           i/document
   :page-plugin         i/document
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

(def ^:private attr->label
  "Friendlier names for attrs whose raw key is unclear; every other attr
  is humanized generically (kebab-case -> Capitalized words)."
  {:rx "Corner radius" :ry "Corner radius"
   :grow-type "Text auto-grow" :blend-mode "Blend mode"
   :shape-ref "Component reference" :component-id "Component"
   :component-file "Component library" :hidden "Visibility"
   :blocked "Locked" :proportion-lock "Lock proportions"
   :parent-id "Parent" :frame-id "Container"})

(defn- attr-label
  "Human-readable label for a changed property key."
  [attr]
  (or (get attr->label attr)
      (-> (name attr) (str/replace #"-" " ") str/capital)))

;; --- Compare: grouping by category + per-item type labels

(def ^:private kind->category
  {:shape :pages :page :pages :page-attrs :pages :page-order :pages
   :page-guide :pages :page-flow :pages :page-grid :pages :page-plugin :pages
   :component :components
   :color :colors
   :typography :typographies
   :media :media
   :token :tokens :token-set :tokens :token-set-rename :tokens :token-set-order :tokens
   :token-theme :tokens :token-active-themes :tokens :token-active-sets :tokens})

(def ^:private category-order [:pages :components :colors :typographies :media :tokens])

(def ^:private category->icon
  {:pages i/document :components i/component :colors i/picker
   :typographies i/text :media i/img :tokens i/tokens})

(def ^:private category->label
  {:pages "workspace.branches.compare.group.pages"
   :components "workspace.branches.compare.group.components"
   :colors "workspace.branches.compare.group.colors"
   :typographies "workspace.branches.compare.group.typographies"
   :media "workspace.branches.compare.group.media"
   :tokens "workspace.branches.compare.group.tokens"})

(def ^:private kind->type-label
  {:shape "workspace.branches.compare.type.shape"
   :component "workspace.branches.compare.type.component"
   :color "workspace.branches.compare.type.color"
   :typography "workspace.branches.compare.type.typography"
   :media "workspace.branches.compare.type.media"
   :token "workspace.branches.compare.type.token"})

(def ^:private shape-type->icon
  {:frame "board" :rect "rectangle" :line "rectangle" :circle "ellipse"
   :path "path" :text "text" :image "img" :svg-raw "img" :group "group"
   :bool "boolean-union"})

(defn- shape-icon-id
  "Type-accurate icon for a shape diff entry (component nature wins over
  raw type), falling back to the board icon for unknown types."
  [{:keys [shape-type component? component-copy? variant? masked?]}]
  (cond
    variant?        "variant"
    component?      "component"
    component-copy? "component-copy"
    (and (= :group shape-type) masked?) "mask"
    :else           (get shape-type->icon shape-type "board")))

(def ^:private shape-type->label
  {:frame "workspace.branches.compare.shape.board"
   :rect "workspace.branches.compare.shape.rect"
   :line "workspace.branches.compare.shape.rect"
   :circle "workspace.branches.compare.shape.ellipse"
   :path "workspace.branches.compare.shape.path"
   :text "workspace.branches.compare.shape.text"
   :image "workspace.branches.compare.shape.image"
   :svg-raw "workspace.branches.compare.shape.image"
   :group "workspace.branches.compare.shape.group"
   :bool "workspace.branches.compare.shape.bool"})

(defn- type-label-key
  "i18n key for an item's specific type — type-accurate for shapes
  (board/text/rectangle/component/…), generic for other kinds."
  [item]
  (if (= :shape (:kind item))
    (let [{:keys [component? component-copy? variant? shape-type]} item]
      (cond
        variant?        "workspace.branches.compare.shape.variant"
        component?      "workspace.branches.compare.shape.component"
        component-copy? "workspace.branches.compare.shape.component-copy"
        :else           (get shape-type->label shape-type
                             "workspace.branches.compare.type.shape")))
    (get kind->type-label (:kind item))))

(defn- item-status
  "Conflicts carry `:conflict`; otherwise the regular add/mod/del status."
  [item]
  (if (= :conflict (:status item)) :conflict (:status item)))

(defn- hex-color
  "Return a usable hex string when `v` looks like a color (a hex string, a
  color map, or a fills/strokes vector), else nil."
  [v]
  (let [s (cond
            (string? v) v
            (and (map? v) (string? (:color v))) (:color v)
            (and (map? v) (string? (:fill-color v))) (:fill-color v)
            (and (map? v) (string? (:stroke-color v))) (:stroke-color v)
            (and (sequential? v) (map? (first v)))
            (or (:fill-color (first v)) (:stroke-color (first v)) (:color (first v)))
            :else nil)]
    (when (and (string? s) (re-matches #"#?[0-9a-fA-F]{3,8}" s))
      (if (str/starts-with? s "#") s (str "#" s)))))

(defn- fmt-number
  "At most two decimals, trailing zeros stripped (89.99999 -> \"90\",
  439.4999 -> \"439.5\", 386 -> \"386\")."
  [v]
  (-> (.toFixed v 2)
      (str/replace #"\.?0+$" "")))

(defn- display-val
  "Compact, human-readable rendering of a property value. Raw uuids and
  collections are summarized rather than dumped (they carry no meaning to
  the user as raw data)."
  [v]
  (cond
    (nil? v)         "—"
    (string? v)      (if (> (count v) 32) (str (subs v 0 32) "…") v)
    (number? v)      (fmt-number v)
    (boolean? v)     (if v "true" "false")
    (keyword? v)     (name v)
    (uuid? v)        (str (subs (str v) 0 8) "…")
    (sequential? v)  (tr "workspace.branches.compare.n-items" (dm/str (count v)))
    (map? v)         (tr "workspace.branches.compare.n-props" (dm/str (count v)))
    :else            (short-str v)))

(defn- status-matches?
  [filter status]
  (case filter
    :all      true
    :added    (= status :added)
    :modified (contains? #{:modified :conflict} status)
    :deleted  (= status :deleted)
    true))

(def ^:private compare-filters
  [[:all "labels.all"]
   [:added "workspace.branches.status.added"]
   [:modified "workspace.branches.status.modified"]
   [:deleted "workspace.branches.status.deleted"]])

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

;; --- Confirmation for relevant branch actions (merge / update)

(defn- confirm-merge!
  "Ask for confirmation before merging `branch` into main (irreversible)."
  [branch]
  (st/emit! (modal/show {:type :confirm
                         :title (tr "workspace.branches.merge.confirm-title")
                         :message (tr "workspace.branches.merge.confirm-message" (:name branch))
                         :accept-label (tr "workspace.branches.merge.confirm-accept")
                         :accept-style :primary
                         :on-accept (fn [_] (st/emit! (dwb/merge-branch (:id branch))))})))

(defn- confirm-update!
  "Ask for confirmation before updating `branch` from main (overwrites
  conflicting branch changes)."
  [branch]
  (st/emit! (modal/show {:type :confirm
                         :title (tr "workspace.branches.update.confirm-title")
                         :message (tr "workspace.branches.update.confirm-message" (:name branch))
                         :accept-label (tr "workspace.branches.update.confirm-accept")
                         :accept-style :primary
                         :on-accept (fn [_] (st/emit! (dwb/update-branch-from-main branch)))})))

;; --- Branch list entry

(mf/defc branch-entry*
  {::mf/private true}
  [{:keys [entry profiles current]}]
  (let [author    (get profiles (:created-by entry))
        ahead     (:ahead entry)
        behind    (:behind entry)
        main?     (:is-main entry)
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
           (reset! show-menu? false)
           (modal/show! :branch-compare {:branch entry})))

        on-update
        (mf/use-fn
         (mf/deps entry)
         (fn [event]
           (dom/stop-propagation event)
           (reset! show-menu? false)
           (confirm-update! entry)))

        on-open-menu
        (mf/use-fn (fn [event]
                     (dom/stop-propagation event)
                     (reset! show-menu? true)))
        on-close-menu (mf/use-fn #(reset! show-menu? false))

        on-start-rename
        (mf/use-fn (fn [event]
                     (dom/stop-propagation event)
                     (reset! show-menu? false)
                     (reset! editing? true)))

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
        (mf/use-fn (mf/deps entry archived?)
                   (fn [event]
                     (dom/stop-propagation event)
                     (reset! show-menu? false)
                     (st/emit! (dwb/archive-branch (:id entry) (not archived?)))))

        on-delete
        (mf/use-fn
         (mf/deps entry)
         (fn [event]
           (dom/stop-propagation event)
           (reset! show-menu? false)
           (st/emit! (modal/show {:type :confirm
                                  :title (tr "workspace.branches.delete.title")
                                  :message (tr "workspace.branches.delete.message" (:name entry))
                                  :accept-label (tr "labels.delete")
                                  :accept-style :danger
                                  :on-accept (fn [_] (st/emit! (dwb/delete-branch (:id entry))))}))))]

    [:li {:class (stl/css-case :branch-entry true
                               :is-archived archived?
                               :is-current current
                               :is-menu-open (deref show-menu?))
          :role "button"
          :on-click (when-not current on-open)}
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
       (when author
         [:> avatar* {:profile author :variant "S"}])
       [:span {:class (stl/css :branch-entry-author)}
        (cond
          (and main? current) (tr "workspace.branches.you-editing" (:fullname author))
          main?               (tr "workspace.branches.main-subtitle")
          :else               (:fullname author))]]]

     [:div {:class (stl/css :branch-entry-aside)}
      (when (and (not archived?) (not main?))
        [:div {:class (stl/css :branch-entry-counts)}
         [:span {:class (stl/css :count-ahead)}
          [:> i/icon* {:icon-id i/arrow-up :size "s"}]
          (dm/str ahead)]
         [:span {:class (stl/css :count-behind)}
          [:> i/icon* {:icon-id i/arrow-down :size "s"}]
          (dm/str behind)]])

      (when-not main?
        [:> icon-button* {:variant "ghost"
                          :icon i/menu
                          :aria-label (tr "labels.options")
                          :on-click on-open-menu}])

      [:> dropdown-menu* {:show (and (not main?) (deref show-menu?))
                          :on-close on-close-menu
                          :class (stl/css :branch-options-dropdown)}
       (when-not archived?
         [:> dropdown-menu-item* {:class (stl/css :menu-option) :on-click on-compare}
          (tr "workspace.branches.compare")])
       (when (and (not archived?) (pos? behind))
         [:> dropdown-menu-item* {:class (stl/css :menu-option) :on-click on-update}
          (tr "workspace.branches.update")])
       (when-not archived?
         [:> dropdown-menu-item* {:class (stl/css :menu-option) :on-click on-start-rename}
          (tr "labels.rename")])
       [:> dropdown-menu-item* {:class (stl/css :menu-option) :on-click on-archive}
        (tr (if archived?
              "workspace.branches.menu.restore"
              "workspace.branches.menu.archive"))]
       [:> dropdown-menu-item* {:class (stl/css :menu-option) :on-click on-delete}
        (tr "labels.delete")]]]]))

;; --- Branches panel

(mf/defc branches-toolbox*
  []
  (let [profiles   (mf/deref refs/profiles)
        profile    (mf/deref refs/profile)
        file       (mf/deref refs/file)
        branch-ctx (mf/deref branch-context)

        {:keys [status data] :as _state}
        (mf/deref branches)

        ;; make sure the current user resolves as an author (e.g. for the
        ;; synthetic Main entry and branches the user created)
        profiles  (cond-> profiles
                    (:id profile) (assoc (:id profile) profile))

        ;; the branch (or main) the open file currently is
        current-entry
        (if branch-ctx
          branch-ctx
          {:name (tr "workspace.branches.main")
           :created-by (:id profile)
           :is-main true
           :status "open"
           :branch-file-id (:id file)})

        filter*  (mf/use-state "")
        filter-v (deref filter*)

        show-archived? (mf/use-state false)

        entries
        (mf/with-memo [data filter-v]
          (->> data
               (filter #(or (str/blank? filter-v)
                            (str/includes? (str/lower (or (:name %) ""))
                                           (str/lower filter-v))))))

        ;; the current branch (if any) is shown in its own section; drop it
        ;; from the rest of the list
        open-entries     (->> entries
                              (filter #(= "open" (:status %)))
                              (remove #(= (:id %) (:id branch-ctx))))
        archived-entries (->> entries
                              (remove #(= "open" (:status %)))
                              (remove #(= (:id %) (:id branch-ctx))))

        ;; when the open file is a branch, offer Main as a switch target
        main-entry       (when branch-ctx
                           {:name (tr "workspace.branches.main")
                            :is-main true
                            :status "open"
                            :branch-file-id (:source-file-id branch-ctx)})

        other-entries    (cond->> open-entries
                           main-entry (cons main-entry))

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

       :else
       [:div {:class (stl/css :branches-list)}
        ;; current branch (or main) the open file currently is
        [:div {:class (stl/css :branches-section-header)}
         [:span (tr "workspace.branches.section.current")]]
        [:ul {:class (stl/css :branches-entries)}
         [:> branch-entry* {:entry current-entry
                            :profiles profiles
                            :current true}]]

        (when (seq other-entries)
          [:div {:class (stl/css :branches-section-header)}
           [:span (tr "workspace.branches.section.others")]
           [:span {:class (stl/css :branches-section-count)} (dm/str (count other-entries))]])
        (when (seq other-entries)
          [:ul {:class (stl/css :branches-entries)}
           (for [entry other-entries]
             [:> branch-entry* {:key (dm/str (or (:id entry) (:branch-file-id entry)))
                                :entry entry
                                :profiles profiles}])])

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

(mf/defc compare-value-chip*
  {::mf/private true}
  [{:keys [value tone]}]
  (let [hex (hex-color value)]
    [:span {:class (stl/css-case :value-chip true
                                 :value-main (= tone :main)
                                 :value-branch (= tone :branch))}
     (when hex
       [:span {:class (stl/css :value-swatch)
               :style {:background-color hex}}])
     (if (and hex (not (sequential? value)))
       hex
       (display-val value))]))

(mf/defc branch-compare-item*
  {::mf/private true}
  [{:keys [item index selected on-select]}]
  (let [on-click (mf/use-fn (mf/deps index on-select) #(on-select index))
        status   (item-status item)
        attrs    (:changed-attrs item)
        type-lbl (type-label-key item)
        icon-id  (if (= :shape (:kind item))
                   (shape-icon-id item)
                   (get kind->icon (:kind item) i/git-branch))
        subtitle (cond
                   (= :added status)   (tr "workspace.branches.compare.subtitle-new"
                                           (if type-lbl (tr type-lbl) ""))
                   (and (= :modified status) (seq attrs))
                   (tr "workspace.branches.compare.subtitle-mods" (count attrs))
                   type-lbl (tr type-lbl)
                   :else nil)]
    [:li {:class (stl/css-case :compare-item true
                               :is-selected (= index selected)
                               :status-added    (= :added status)
                               :status-modified (= :modified status)
                               :status-deleted  (= :deleted status)
                               :status-conflict (= :conflict status))
          :role "button"
          :on-click on-click}
     [:div {:class (stl/css :compare-item-icon)}
      [:> i/icon* {:icon-id icon-id}]]
     [:div {:class (stl/css :compare-item-body)}
      [:span {:class (stl/css :compare-item-label)} (:label item)]
      (when subtitle
        [:span {:class (stl/css :compare-item-subtitle)} subtitle])]
     [:span {:class (stl/css-case :item-badge true
                                  :badge-added    (= :added status)
                                  :badge-modified (= :modified status)
                                  :badge-deleted  (= :deleted status)
                                  :badge-conflict (= :conflict status))}
      (tr (get status->label status "workspace.branches.status.modified"))]]))

(mf/defc branch-compare-detail*
  {::mf/private true}
  [{:keys [item]}]
  (let [attrs    (:changed-attrs item)
        type-lbl (type-label-key item)]
    (cond
      (nil? item)
      [:div {:class (stl/css :compare-detail-empty)}
       [:> empty-state* {:icon i/switch
                         :text (tr "workspace.branches.compare.select-hint")}]]

      :else
      [:div {:class (stl/css :compare-detail-content)}
       [:div {:class (stl/css :compare-detail-head)}
        [:h3 {:class (stl/css :compare-detail-title)} (:label item)]
        [:span {:class (stl/css :compare-detail-subtitle)}
         (cond-> ""
           type-lbl     (str (tr type-lbl))
           (seq attrs)  (str " · " (tr "workspace.branches.compare.subtitle-mods" (count attrs))))]]

       (if (seq attrs)
         [:div {:class (stl/css :prop-changes)}
          [:div {:class (stl/css :prop-changes-head)}
           [:span (tr "workspace.branches.compare.prop-changes")]
           [:span {:class (stl/css :prop-changes-count)} (dm/str (count attrs))]]
          (for [[attr {:keys [main branch]}] attrs]
            [:div {:class (stl/css :prop-row) :key (str attr)}
             [:span {:class (stl/css :prop-name)} (attr-label attr)]
             [:div {:class (stl/css :prop-values)}
              [:> compare-value-chip* {:value main :tone :main}]
              [:> i/icon* {:icon-id i/arrow-up-right :size "s"}]
              [:> compare-value-chip* {:value branch :tone :branch}]]])]
         [:p {:class (stl/css :compare-detail-hint)}
          (tr "workspace.branches.compare.no-props")])])))

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

        active-filter* (mf/use-state :all)
        active-filter  (deref active-filter*)

        ;; keep the original index for selection while filtering/grouping
        indexed   (map-indexed vector items)
        filtered  (filterv (fn [[_ it]] (status-matches? active-filter (item-status it))) indexed)
        by-cat    (group-by (fn [[_ it]] (get kind->category (:kind it) :pages)) filtered)

        on-close   (mf/use-fn #(st/emit! (modal/hide)))
        on-select  (mf/use-fn #(st/emit! (dwb/select-diff-change %)))
        on-filter  (mf/use-fn (fn [f] (reset! active-filter* f)))
        on-merge   (mf/use-fn (mf/deps branch)
                              #(confirm-merge! branch))
        on-resolve (mf/use-fn (mf/deps branch)
                              #(modal/show! :branch-conflicts {:branch branch}))
        on-export  (mf/use-fn
                    (mf/deps diff branch)
                    (fn []
                      (let [payload (clj->js {:stats stats
                                              :changes (:changes diff)
                                              :conflicts (:conflicts diff)})
                            text    (js/JSON.stringify payload nil 2)
                            blob    (js/Blob. #js [text] #js {:type "application/json"})
                            url     (js/URL.createObjectURL blob)
                            a       (.createElement js/document "a")]
                        (set! (.-href a) url)
                        (set! (.-download a) (dm/str (:name branch) "-diff.json"))
                        (.click a)
                        (js/URL.revokeObjectURL url))))]

    (mf/with-effect [(:id branch)]
      (st/emit! (dwb/fetch-branch-diff (:id branch))))

    ;; auto-select the first change once loaded
    (mf/with-effect [status (count items)]
      (when (and (= status :loaded) (nil? selected) (seq items))
        (st/emit! (dwb/select-diff-change 0))))

    [:div {:class (stl/css :compare-overlay)}
     [:div {:class (stl/css :compare-container)}
      [:div {:class (stl/css :compare-header)}
       [:div {:class (stl/css :compare-title-group)}
        [:div {:class (stl/css :compare-title-icon)}
         [:> i/icon* {:icon-id i/switch}]]
        [:div {:class (stl/css :compare-title-text)}
         [:h2 {:class (stl/css :modal-title)} (tr "workspace.branches.compare.title")]
         [:div {:class (stl/css :compare-breadcrumb)}
          [:span {:class (stl/css :breadcrumb-branch)}
           [:> i/icon* {:icon-id i/git-branch :size "s"}]
           (:name branch)]
          [:> i/icon* {:icon-id i/arrow-up-right :size "s"}]
          [:span {:class (stl/css :breadcrumb-main)}
           [:span {:class (stl/css :breadcrumb-dot)}]
           "main"]]]]

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
         [:div {:class (stl/css :compare-side)}
          [:div {:class (stl/css :compare-filters)}
           (for [[f label] compare-filters]
             [:button {:key (name f)
                       :class (stl/css-case :filter-chip true
                                            :is-active (= active-filter f)
                                            :dot-added    (= f :added)
                                            :dot-modified (= f :modified)
                                            :dot-deleted  (= f :deleted))
                       :on-click #(on-filter f)}
              (when-not (= f :all) [:span {:class (stl/css :filter-dot)}])
              (tr label)])]

          [:div {:class (stl/css :compare-list)}
           (for [cat category-order
                 :let [group (get by-cat cat)]
                 :when (seq group)]
             [:div {:class (stl/css :compare-group) :key (name cat)}
              [:div {:class (stl/css :compare-group-head)}
               [:> i/icon* {:icon-id (get category->icon cat i/document) :size "s"}]
               [:span {:class (stl/css :compare-group-label)} (tr (get category->label cat))]
               (let [freqs (frequencies (map (fn [[_ it]] (item-status it)) group))]
                 [:span {:class (stl/css :compare-group-counts)}
                  (when (pos? (get freqs :added 0))
                    [:span {:class (stl/css :count-added)} (dm/str "+" (get freqs :added))])
                  (when (pos? (+ (get freqs :modified 0) (get freqs :conflict 0)))
                    [:span {:class (stl/css :count-modified)}
                     (dm/str "~" (+ (get freqs :modified 0) (get freqs :conflict 0)))])
                  (when (pos? (get freqs :deleted 0))
                    [:span {:class (stl/css :count-deleted)} (dm/str "−" (get freqs :deleted))])])]
              [:ul {:class (stl/css :compare-group-items)}
               (for [[idx item] group]
                 [:> branch-compare-item* {:key idx
                                           :item item
                                           :index idx
                                           :selected selected
                                           :on-select on-select}])]])]]

         [:div {:class (stl/css :compare-detail)}
          [:> branch-compare-detail* {:item sel-item}]]])

      (when (= status :loaded)
        (let [conflicts  (long (or (:conflicts stats) 0))
              conflicts? (pos? conflicts)
              total      (long (+ (or (:added stats) 0)
                                  (or (:modified stats) 0)
                                  (or (:deleted stats) 0)))]
          [:div {:class (stl/css :compare-footer)}
           [:div {:class (stl/css :compare-footer-info)}
            [:> i/icon* {:icon-id i/info :size "s"}]
            [:span (tr "workspace.branches.compare.footer-changes" total)]
            (when conflicts?
              [:span {:class (stl/css :compare-footer-conflicts)}
               (dm/str " · " (tr "workspace.branches.compare.footer-conflicts" conflicts))])]
           [:div {:class (stl/css :compare-footer-actions)}
            [:> button* {:variant "ghost"
                         :icon i/download
                         :on-click on-export}
             (tr "workspace.branches.compare.export")]
            (if conflicts?
              [:> button* {:variant "primary"
                           :icon i/git-merge
                           :on-click on-resolve}
               (tr "workspace.branches.conflicts.resolve")]
              [:> button* {:variant "primary"
                           :icon i/git-merge
                           :disabled (zero? total)
                           :on-click on-merge}
               (tr "workspace.branches.merge.action")])]]))]]))

;; --- Resolve conflicts dialog

(def ^:private conflict-reason->label
  {:modify-modify "workspace.branches.conflicts.reason.modify-modify"
   :add-add       "workspace.branches.conflicts.reason.add-add"
   :delete-modify "workspace.branches.conflicts.reason.delete-modify"
   :modify-delete "workspace.branches.conflicts.reason.modify-delete"})

(mf/defc branch-conflict-item*
  {::mf/private true}
  [{:keys [conflict index selected resolution on-select]}]
  (let [on-click  (mf/use-fn (mf/deps index on-select) #(on-select index))
        resolved? (contains? #{:main :branch} resolution)
        icon-id   (if (= :shape (:kind conflict))
                    (shape-icon-id conflict)
                    (get kind->icon (:kind conflict) i/git-branch))
        type-lbl  (type-label-key conflict)]
    [:li {:class (stl/css-case :conflict-item true
                               :is-selected (= index selected)
                               :is-pending (not resolved?))
          :role "button"
          :on-click on-click}
     [:div {:class (stl/css :conflict-item-icon)}
      [:> i/icon* {:icon-id icon-id}]]
     [:div {:class (stl/css :conflict-item-body)}
      [:span {:class (stl/css :conflict-item-label)} (:label conflict)]
      [:span {:class (stl/css :conflict-item-subtitle)}
       (cond-> ""
         type-lbl (str (tr type-lbl) " · ")
         :always  (str (tr (if resolved?
                             "workspace.branches.conflicts.state-resolved"
                             "workspace.branches.conflicts.state-pending"))))]]
     (cond
       (= resolution :main)
       [:span {:class (stl/css :conflict-pill :pill-main)}
        [:span {:class (stl/css :pill-dot)}]
        (tr "workspace.branches.conflicts.side-main")]
       (= resolution :branch)
       [:span {:class (stl/css :conflict-pill :pill-branch)}
        [:span {:class (stl/css :pill-dot)}]
        (tr "workspace.branches.conflicts.side-branch")]
       :else
       [:span {:class (stl/css :conflict-pill :pill-pending)}
        [:> i/icon* {:icon-id i/triangle-alert :size "s"}]])]))

(mf/defc branch-conflict-card*
  {::mf/private true}
  [{:keys [conflict side label meta selected on-select]}]
  (let [value       (get conflict side)
        hex         (hex-color value)
        attrs       (:changed-attrs conflict)
        selectable? (some? on-select)]
    [:div {:class (stl/css-case :conflict-card true
                                :is-base (not selectable?)
                                :is-selected (true? selected))}
     [:div {:class (stl/css :conflict-card-head)}
      [:div {:class (stl/css :conflict-card-titles)}
       [:span {:class (stl/css :conflict-card-label)} (tr label)]
       (when meta
         [:span {:class (stl/css :conflict-card-meta)} (tr meta)])]
      (when selectable?
        [:span {:class (stl/css-case :conflict-radio true :is-on (true? selected))}])]

     [:div {:class (stl/css :conflict-card-body)}
      (cond
        (some? hex)
        [:*
         [:span {:class (stl/css :conflict-card-swatch)
                 :style {:background-color hex}}]
         [:span {:class (stl/css :conflict-card-value)} hex]]

        (seq attrs)
        [:div {:class (stl/css :conflict-card-attrs)}
         (for [[attr {:keys [main branch]}] attrs]
           (let [v (case side :base (get-in conflict [:base attr]) :main main :branch branch)]
             [:div {:class (stl/css :conflict-attr-row) :key (str attr)}
              [:span {:class (stl/css :conflict-attr-name)} (attr-label attr)]
              [:span {:class (stl/css :conflict-attr-val)} (display-val v)]]))]

        :else
        [:span {:class (stl/css :conflict-card-value)} (display-val value)])]

     (when selectable?
       [:div {:class (stl/css :conflict-card-action)}
        (if selected
          [:span {:class (stl/css :conflict-card-chosen)}
           [:> i/icon* {:icon-id i/tick :size "s"}]
           (tr "workspace.branches.conflicts.chosen")]
          [:> button* {:variant "secondary" :on-click on-select}
           (tr "workspace.branches.conflicts.use-this")])])]))

(mf/defc branch-conflicts-dialog*
  {::mf/register modal/components
   ::mf/register-as :branch-conflicts}
  [{:keys [branch mode]}]
  (let [{:keys [diff selected resolutions]} (mf/deref branch-diff)

        conflicts   (:conflicts diff)
        resolutions (or resolutions {})
        total       (count conflicts)
        resolved    (count (filterv #(contains? #{:main :branch} (get resolutions (:id %))) conflicts))
        pending     (- total resolved)
        all-done?   (and (pos? total) (zero? pending))

        sel-idx     (min (or selected 0) (max 0 (dec total)))
        sel         (when (seq conflicts) (nth conflicts sel-idx))
        sel-res     (when sel (get resolutions (:id sel)))
        sel-type    (when sel (type-label-key sel))
        sel-reason  (when sel (get conflict-reason->label (:reason sel)))
        sel-icon    (when sel (if (= :shape (:kind sel))
                                (shape-icon-id sel)
                                (get kind->icon (:kind sel) i/git-branch)))

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
        [:div {:class (stl/css :compare-title-icon :title-icon-warning)}
         [:> i/icon* {:icon-id i/triangle-alert}]]
        [:div {:class (stl/css :compare-title-text)}
         [:h2 {:class (stl/css :modal-title)} (tr "workspace.branches.conflicts.title")]
         [:span {:class (stl/css :compare-subtitle)}
          (tr "workspace.branches.conflicts.subtitle" (:name branch))]]]
       [:span {:class (stl/css :conflicts-progress)}
        (tr "workspace.branches.conflicts.progress" (str resolved) (str total))]
       [:> icon-button* {:variant "ghost"
                         :icon i/close
                         :aria-label (tr "labels.close")
                         :on-click on-close}]]

      [:div {:class (stl/css :conflicts-toolbar)}
       [:span {:class (stl/css :bulk-label)} (tr "workspace.branches.conflicts.bulk-label")]
       [:> button* {:variant "ghost" :icon i/git-branch :on-click on-all-main}
        (tr "workspace.branches.conflicts.all-main")]
       [:> button* {:variant "ghost" :icon i/git-branch :on-click on-all-branch}
        (tr "workspace.branches.conflicts.all-branch")]
       (when (pos? pending)
         [:span {:class (stl/css :conflicts-pending)}
          (tr "workspace.branches.conflicts.pending" (str pending))])]

      (if (empty? conflicts)
        [:div {:class (stl/css :compare-empty)}
         [:> empty-state* {:icon i/git-merge
                           :text (tr "workspace.branches.conflicts.empty")}]]

        [:div {:class (stl/css :compare-body)}
         [:div {:class (stl/css :conflicts-list-side)}
          [:span {:class (stl/css :conflicts-list-head)}
           (tr "workspace.branches.conflicts.list-head" (str total))]
          [:ul {:class (stl/css :compare-list)}
           (for [[idx c] (map-indexed vector conflicts)]
             [:> branch-conflict-item* {:key idx
                                        :conflict c
                                        :index idx
                                        :selected sel-idx
                                        :resolution (get resolutions (:id c))
                                        :on-select on-select}])]]

         [:div {:class (stl/css :compare-detail)}
          (when sel
            [:*
             [:div {:class (stl/css :conflict-detail-head)}
              [:div {:class (stl/css :conflict-detail-titlerow)}
               [:div {:class (stl/css :conflict-detail-icon)}
                [:> i/icon* {:icon-id sel-icon}]]
               [:h3 {:class (stl/css :conflict-detail-title)} (:label sel)]]
              [:span {:class (stl/css :conflict-detail-subtitle)}
               (cond-> ""
                 sel-type   (str (tr sel-type))
                 sel-reason (str " · " (tr sel-reason)))]]

             [:div {:class (stl/css :conflict-cards)}
              [:> branch-conflict-card* {:conflict sel
                                         :side :base
                                         :label "workspace.branches.conflicts.card.base"
                                         :meta "workspace.branches.conflicts.card.base-meta"}]
              [:> branch-conflict-card* {:conflict sel
                                         :side :main
                                         :label "workspace.branches.conflicts.card.main"
                                         :selected (= sel-res :main)
                                         :on-select on-use-main}]
              [:> branch-conflict-card* {:conflict sel
                                         :side :branch
                                         :label "workspace.branches.conflicts.card.branch"
                                         :selected (= sel-res :branch)
                                         :on-select on-use-branch}]]])]])

      [:div {:class (stl/css :compare-footer)}
       [:div {:class (stl/css :compare-footer-info)}
        (if all-done?
          [:*
           [:> i/icon* {:icon-id i/tick :size "s"}]
           (tr "workspace.branches.conflicts.footer-ready")]
          [:*
           [:span {:class (stl/css :compare-footer-conflicts)}
            [:> i/icon* {:icon-id i/triangle-alert :size "s"}]]
           [:span {:class (stl/css :compare-footer-conflicts)}
            (tr "workspace.branches.conflicts.footer-pending" (str pending))]])]
       [:div {:class (stl/css :compare-footer-actions)}
        [:> button* {:variant "ghost" :on-click on-close} (tr "labels.cancel")]
        [:> button* {:variant "primary"
                     :icon i/git-merge
                     :disabled (not all-done?)
                     :on-click on-apply}
         (tr "workspace.branches.conflicts.apply")]]]]]))

;; --- Branch context banner (shown while editing a branch)

(mf/defc branch-context-banner*
  [{:keys [file-id]}]
  (let [ctx     (mf/deref branch-context)
        merged? (contains? #{"merged" "archived"} (:status ctx))

        on-compare   (mf/use-fn (mf/deps ctx) #(modal/show! :branch-compare {:branch ctx}))
        on-update    (mf/use-fn (mf/deps ctx) #(confirm-update! ctx))
        on-merge     (mf/use-fn (mf/deps ctx) #(confirm-merge! ctx))
        on-open-main (mf/use-fn (mf/deps ctx) #(st/emit! (dwb/open-branch (:source-file-id ctx))))
        on-resolve   (mf/use-fn (mf/deps ctx) #(modal/show! :branch-conflicts {:branch ctx :mode :merge}))]

    (mf/with-effect [file-id]
      (when (contains? cf/flags :branching)
        (st/emit! (dwb/fetch-branch-context))))

    ;; refresh when returning to the tab, so "main advanced" is surfaced
    ;; while working on the branch
    (mf/with-effect []
      (let [key (events/listen globals/window "focus"
                               (fn [_]
                                 (when (contains? cf/flags :branching)
                                   (st/emit! (dwb/fetch-branch-context)))))]
        (fn [] (events/unlistenByKey key))))

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

         (let [conflicts (or (:conflicts ctx) 0)]
           [:div {:class (stl/css :branch-banner-actions)}
            [:span {:class (stl/css :branch-banner-counts)}
             [:span {:class (stl/css :count-ahead)}
              [:> i/icon* {:icon-id i/arrow-up :size "s"}] (dm/str (:ahead ctx))]
             [:span {:class (stl/css :count-behind)}
              [:> i/icon* {:icon-id i/arrow-down :size "s"}] (dm/str (:behind ctx))]]

            (when (pos? conflicts)
              [:span {:class (stl/css :item-badge :badge-conflict)}
               (tr "workspace.branches.banner.conflicts" (dm/str conflicts))])

            [:> button* {:variant "secondary"
                         :icon i/switch
                         :on-click on-compare}
             (tr "workspace.branches.compare")]

            (cond
              (pos? conflicts)
              [:> button* {:variant "primary"
                           :icon i/triangle-alert
                           :on-click on-resolve}
               (tr "workspace.branches.conflicts.resolve")]

              (pos? (:behind ctx))
              [:> button* {:variant "primary"
                           :icon i/status-update
                           :on-click on-update}
               (tr "workspace.branches.update")]

              (pos? (:ahead ctx))
              [:> button* {:variant "primary"
                           :icon i/git-merge
                           :on-click on-merge}
               (tr "workspace.branches.merge.action")])]))])))
