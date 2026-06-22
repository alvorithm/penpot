;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC Sucursal en España SL

(ns app.main.data.workspace.branches
  "Data layer for file branching (Phase 1: create + list). Mirrors the
  patterns in `app.main.data.workspace.versions`."
  (:require
   [app.config :as cf]
   [app.main.broadcast :as mbc]
   [app.main.data.common :as dcm]
   [app.main.data.event :as ev]
   [app.main.data.modal :as modal]
   [app.main.data.notifications :as ntf]
   [app.main.data.persistence :as dwp]
   [app.main.data.workspace.layout :as layout]
   [app.main.repo :as rp]
   [app.util.dom :as dom]
   [app.util.i18n :refer [tr]]
   [app.util.storage :as storage]
   [beicon.v2.core :as rx]
   [potok.v2.core :as ptk]))

(defn- reload-file-window
  "Hard-reload the browser window so the open file is re-fetched from
  scratch. Used after merge / update-from-main, which rewrite the file
  server-side (including repair changes); a full reload is the most
  robust way to show the new state without local/server divergence."
  []
  (ptk/reify ::reload-file-window
    ptk/EffectEvent
    (effect [_ _ _]
      (dom/reload-current-window))))

(declare open-branch)

(defn- show-merge-result
  "After merging a branch into main, surface the merged result: if main is
  the file currently open, hard-reload it; otherwise navigate to main."
  [source-file-id]
  (ptk/reify ::show-merge-result
    ptk/WatchEvent
    (watch [_ state _]
      (cond
        (nil? source-file-id)                          (rx/of (reload-file-window))
        (= source-file-id (:current-file-id state))    (rx/of (reload-file-window))
        :else                                          (rx/of (open-branch source-file-id))))))

(defonce default-state
  {:status :loading
   :data nil
   :filter ""})

(declare fetch-branches)
(declare fetch-branch-context)

(defn init-branches-state
  []
  (ptk/reify ::init-branches-state
    ptk/UpdateEvent
    (update [_ state]
      (assoc state :workspace-branches default-state))

    ptk/WatchEvent
    (watch [_ _ _]
      (rx/of (fetch-branches)))))

(defn update-branches-state
  [branch-state]
  (ptk/reify ::update-branches-state
    ptk/UpdateEvent
    (update [_ state]
      (update state :workspace-branches merge branch-state))))

(defn fetch-branches
  []
  (ptk/reify ::fetch-branches
    ptk/WatchEvent
    (watch [_ state _]
      (when-let [file-id (:current-file-id state)]
        ;; the open file may itself be a branch; branches are listed by their
        ;; source (main), so resolve the source first (nil when on main) and
        ;; list siblings against it, so the panel is complete from a branch too
        (->> (rp/cmd! :get-file-branch-info {:file-id file-id})
             (rx/mapcat (fn [info]
                          (let [root (or (:source-file-id info) file-id)]
                            (rp/cmd! :get-file-branches {:file-id root :include-archived true}))))
             (rx/map #(update-branches-state {:status :loaded :data %}))
             (rx/catch (fn [_]
                         (rx/of (update-branches-state {:status :loaded :data []})))))))))

;; --- Dashboard popover (list a file's branches from the dashboard)

(defn- set-dashboard-branches
  [bs]
  (ptk/reify ::set-dashboard-branches
    ptk/UpdateEvent
    (update [_ state]
      (assoc state :dashboard-branches bs))))

(defn load-file-branches
  "Load the open branches of `file-id` into `:dashboard-branches` (used by
  the dashboard branches popover)."
  [file-id]
  (ptk/reify ::load-file-branches
    ptk/WatchEvent
    (watch [_ _ _]
      (->> (rp/cmd! :get-file-branches {:file-id file-id})
           (rx/map set-dashboard-branches)
           (rx/catch (fn [_] (rx/of (set-dashboard-branches []))))))))

(defn rename-branch
  [id name]
  (assert (uuid? id) "expected valid uuid for `id`")
  (ptk/reify ::rename-branch
    ptk/WatchEvent
    (watch [_ _ _]
      (->> (rp/cmd! :update-file-branch {:id id :name name})
           (rx/mapcat (fn [_] (rx/of (fetch-branches) (fetch-branch-context))))
           (rx/catch (fn [_] (rx/of (ntf/error (tr "workspace.branches.lifecycle.error")))))))))

(defn archive-branch
  ([id] (archive-branch id true))
  ([id archived?]
   (assert (uuid? id) "expected valid uuid for `id`")
   (ptk/reify ::archive-branch
     ptk/WatchEvent
     (watch [_ _ _]
       (->> (rp/cmd! :archive-file-branch {:id id :archived archived?})
            (rx/mapcat (fn [_] (rx/of (fetch-branches))))
            (rx/catch (fn [_] (rx/of (ntf/error (tr "workspace.branches.lifecycle.error"))))))))))

(defn delete-branch
  [id]
  (assert (uuid? id) "expected valid uuid for `id`")
  (ptk/reify ::delete-branch
    ptk/WatchEvent
    (watch [_ _ _]
      (->> (rp/cmd! :delete-file-branch {:id id})
           (rx/mapcat (fn [_] (rx/of (ntf/success (tr "workspace.branches.lifecycle.deleted"))
                                     (fetch-branches))))
           (rx/catch (fn [_] (rx/of (ntf/error (tr "workspace.branches.lifecycle.error")))))))))

(defn create-branch
  "Create a branch from a file. With no `file-id`, branches the currently
  open file (force-persisting first so the merge base captures the latest
  edits) and refreshes the panel. With an explicit `file-id` (e.g. from
  the dashboard, where the file is not open), creates it directly and
  opens the new branch. Errors surface as a toast; the caller closes the
  dialog."
  ([name description] (create-branch nil name description))
  ([file-id name description]
   (assert (string? name) "expected string for `name`")
   (ptk/reify ::create-branch
     ptk/WatchEvent
     (watch [_ state _]
       (let [current-id (:current-file-id state)
             from-ws?   (and (nil? file-id) (some? current-id))
             target-id  (or file-id current-id)
             ;; Wait for persistence only when branching the open file;
             ;; from the dashboard the file is not open, so there is
             ;; nothing to flush. Emits exactly one value, so the branch
             ;; is created exactly once.
             wait       (if from-ws?
                          (dwp/wait-persisted)
                          (rx/of :ready))]
         (rx/concat
          ;; Force-persist the open file first so the merge base captures
          ;; the latest edits. The ::force-persist event must be dispatched
          ;; at the top level (not piped into the mapcat below); off the
          ;; workspace there is nothing to persist.
          (if from-ws?
            (rx/of ::dwp/force-persist (ev/event {::ev/name "create-branch"}))
            (rx/of (ev/event {::ev/name "create-branch"})))
          (->> wait
               (rx/mapcat #(rp/cmd! :create-file-branch
                                    {:file-id target-id
                                     :name name
                                     :description description}))
               (rx/mapcat
                (fn [{:keys [branch-file-id]}]
                  (rx/concat
                   (rx/of (ntf/success (tr "workspace.branches.create.success" name)))
                   (if from-ws?
                     (rx/of (fetch-branches))
                     ;; from dashboard: jump into the new branch
                     (rx/of (dcm/go-to-workspace :file-id branch-file-id))))))
               (rx/catch
                (fn [_]
                  (rx/of (ntf/error (tr "workspace.branches.create.error"))))))))))))

(defn open-branch
  "Navigate to the branch file as a normal workspace file."
  [branch-file-id]
  (assert (uuid? branch-file-id) "expected valid uuid for `branch-file-id`")
  (ptk/reify ::open-branch
    ptk/WatchEvent
    (watch [_ _ _]
      (rx/of (dcm/go-to-workspace :file-id branch-file-id)))))

(def history-sidebar-tab-key
  "Storage + broadcast key for the selected tab of the version-history
  sidebar. Shared with `app.main.ui.workspace.sidebar`, which reads it
  through `hooks/use-shared-state` so this event can switch the active
  tab even when the panel is already open."
  ::history-sidebar-tab)

(defn show-branches-panel
  "Open the version-history sidebar and switch it to the Branches tab.
  No-op when the branching feature flag is disabled (the tab does not
  exist in that case). Writing storage covers a closed→open panel; the
  broadcast emit covers an already-open one."
  []
  (ptk/reify ::show-branches-panel
    ptk/WatchEvent
    (watch [_ _ _]
      (when (contains? cf/flags :branching)
        (rx/of (layout/toggle-layout-flag :document-history :force? true))))

    ptk/EffectEvent
    (effect [_ _ _]
      (when (contains? cf/flags :branching)
        (swap! storage/user assoc history-sidebar-tab-key "branches")
        (mbc/emit! history-sidebar-tab-key "branches")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMPARE (read-only 3-way diff)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn update-branch-diff
  [diff-state]
  (ptk/reify ::update-branch-diff
    ptk/UpdateEvent
    (update [_ state]
      (update state :workspace-branch-diff merge diff-state))))

(defn fetch-branch-diff
  [branch-id]
  (assert (uuid? branch-id) "expected valid uuid for `branch-id`")
  (ptk/reify ::fetch-branch-diff
    ptk/UpdateEvent
    (update [_ state]
      (assoc state :workspace-branch-diff {:status :loading
                                           :branch-id branch-id
                                           :selected nil}))
    ptk/WatchEvent
    (watch [_ _ _]
      (->> (rp/cmd! :get-branch-diff {:branch-id branch-id})
           (rx/map #(update-branch-diff {:status :loaded :diff %}))
           (rx/catch (fn [_]
                       (rx/of (update-branch-diff {:status :error}))))))))

(defn select-diff-change
  "Select a change/conflict (by its index in the diff) to show its detail."
  [selected]
  (ptk/reify ::select-diff-change
    ptk/UpdateEvent
    (update [_ state]
      (assoc-in state [:workspace-branch-diff :selected] selected))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BRANCH CONTEXT (banner when the open file is a branch)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- set-branch-context
  [info]
  (ptk/reify ::set-branch-context
    ptk/UpdateEvent
    (update [_ state]
      (assoc state :workspace-branch-context info))))

(defn fetch-branch-context
  "Load branch metadata for the current file (nil when it is not a branch).
  If `behind` grew since the previous value (main advanced while working on
  the branch), surface a notification."
  []
  (ptk/reify ::fetch-branch-context
    ptk/WatchEvent
    (watch [_ state _]
      (when-let [file-id (:current-file-id state)]
        (let [prev-behind (-> state :workspace-branch-context :behind (or 0))]
          (->> (rp/cmd! :get-file-branch-info {:file-id file-id})
               (rx/mapcat
                (fn [info]
                  (rx/concat
                   (rx/of (set-branch-context info))
                   (if (and info (> (or (:behind info) 0) prev-behind))
                     (rx/of (ntf/info (tr "workspace.branches.main-advanced")))
                     (rx/empty)))))
               (rx/catch (fn [_] (rx/of (set-branch-context nil))))))))))

(defn update-branch-from-main
  "Bring main's changes into the branch (reverse of merge). `branch` is the
  branch row. With conflicts and no `resolutions`, opens the conflict
  resolution modal in update mode; with resolutions, applies them."
  ([branch] (update-branch-from-main branch nil))
  ([branch resolutions]
   (ptk/reify ::update-branch-from-main
     ptk/WatchEvent
     (watch [_ _ _]
       (rx/concat
        (rx/of (ev/event {::ev/name "update-branch-from-main"}))
        (->> (rp/cmd! :update-branch-from-main (cond-> {:branch-id (:id branch)}
                                                 (seq resolutions) (assoc :resolutions resolutions)))
             (rx/mapcat
              (fn [{:keys [status]}]
                (case status
                  ;; the open branch file just changed server-side: hard-reload
                  ;; it so the pulled changes are shown
                  :updated     (rx/of (ntf/success (tr "workspace.branches.update.success"))
                                      (reload-file-window))
                  :conflicts   (rx/of (modal/show :branch-conflicts {:branch branch :mode :update}))
                  :unsupported (rx/of (ntf/warn (tr "workspace.branches.update.unsupported")))
                  (rx/of (ntf/error (tr "workspace.branches.update.error"))))))
             (rx/catch (fn [_]
                         (rx/of (ntf/error (tr "workspace.branches.update.error")))))))))))

(defn set-conflict-resolution
  "Choose `:main` or `:branch` for a single conflicting entity (by id)."
  [id choice]
  (ptk/reify ::set-conflict-resolution
    ptk/UpdateEvent
    (update [_ state]
      (assoc-in state [:workspace-branch-diff :resolutions id] choice))))

(defn set-all-resolutions
  "Bulk-resolve every current conflict to `:main` or `:branch`."
  [choice]
  (ptk/reify ::set-all-resolutions
    ptk/UpdateEvent
    (update [_ state]
      (let [conflicts (get-in state [:workspace-branch-diff :diff :conflicts])
            res       (into {} (map (fn [c] [(:id c) choice])) conflicts)]
        (assoc-in state [:workspace-branch-diff :resolutions] res)))))

(defn merge-branch
  "Merge a branch into main. On success the branch is marked merged and
  open clients reload main via the `:file-merged` msgbus event. Conflicts
  and not-yet-supported change kinds surface as notifications.

  `resolutions` is an optional `{entity-id (:main|:branch)}` map used to
  resolve conflicts before integrating."
  ([branch-id] (merge-branch branch-id nil))
  ([branch-id resolutions]
   (assert (uuid? branch-id) "expected valid uuid for `branch-id`")
   (ptk/reify ::merge-branch
     ptk/WatchEvent
     (watch [_ _ _]
       (rx/concat
        (rx/of (ev/event {::ev/name "merge-branch"}))
        (->> (rp/cmd! :merge-file-branch (cond-> {:branch-id branch-id}
                                           (seq resolutions) (assoc :resolutions resolutions)))
             (rx/mapcat
              (fn [{:keys [status source-file-id]}]
                (case status
                  ;; the merged result lives in main: take the user there to
                  ;; see it (navigate if elsewhere, hard-reload if already on
                  ;; main). Other clients reload via the `:file-merged` event.
                  :merged      (rx/of (ntf/success (tr "workspace.branches.merge.success"))
                                      (show-merge-result source-file-id))
                  :conflicts   (rx/of (ntf/warn (tr "workspace.branches.merge.conflicts")))
                  :unsupported (rx/of (ntf/warn (tr "workspace.branches.merge.unsupported")))
                  (rx/of (ntf/error (tr "workspace.branches.merge.error"))))))
             (rx/catch (fn [_]
                         (rx/of (ntf/error (tr "workspace.branches.merge.error")))))))))))
