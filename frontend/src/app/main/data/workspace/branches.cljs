;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC Sucursal en España SL

(ns app.main.data.workspace.branches
  "Data layer for file branching (Phase 1: create + list). Mirrors the
  patterns in `app.main.data.workspace.versions`."
  (:require
   [app.main.data.common :as dcm]
   [app.main.data.event :as ev]
   [app.main.data.modal :as modal]
   [app.main.data.notifications :as ntf]
   [app.main.data.persistence :as dwp]
   [app.main.repo :as rp]
   [app.util.i18n :refer [tr]]
   [beicon.v2.core :as rx]
   [potok.v2.core :as ptk]))

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
        (->> (rp/cmd! :get-file-branches {:file-id file-id :include-archived true})
             (rx/map #(update-branches-state {:status :loaded :data %}))
             (rx/catch (fn [_]
                         (rx/of (update-branches-state {:status :loaded :data []})))))))))

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
             ;; Force-persist only makes sense for the open file.
             persist    (if from-ws?
                          (rx/concat (rx/of ::dwp/force-persist) (dwp/wait-persisted))
                          (rx/of :ready))]
         (rx/concat
          (rx/of (ev/event {::ev/name "create-branch"}))
          (->> persist
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
  "Load branch metadata for the current file (nil when it is not a branch)."
  []
  (ptk/reify ::fetch-branch-context
    ptk/WatchEvent
    (watch [_ state _]
      (when-let [file-id (:current-file-id state)]
        (->> (rp/cmd! :get-file-branch-info {:file-id file-id})
             (rx/map set-branch-context)
             (rx/catch (fn [_] (rx/of (set-branch-context nil)))))))))

(defn update-branch-from-main
  "Bring main's changes into the branch (reverse of merge). Surfaces
  conflicts / unsupported kinds as notifications."
  [branch-id]
  (assert (uuid? branch-id) "expected valid uuid for `branch-id`")
  (ptk/reify ::update-branch-from-main
    ptk/WatchEvent
    (watch [_ _ _]
      (rx/concat
       (rx/of (ev/event {::ev/name "update-branch-from-main"}))
       (->> (rp/cmd! :update-branch-from-main {:branch-id branch-id})
            (rx/mapcat
             (fn [{:keys [status]}]
               (case status
                 :updated     (rx/of (ntf/success (tr "workspace.branches.update.success"))
                                     (fetch-branches)
                                     (fetch-branch-context))
                 :conflicts   (rx/of (ntf/warn (tr "workspace.branches.update.conflicts")))
                 :unsupported (rx/of (ntf/warn (tr "workspace.branches.update.unsupported")))
                 (rx/of (ntf/error (tr "workspace.branches.update.error"))))))
            (rx/catch (fn [_]
                        (rx/of (ntf/error (tr "workspace.branches.update.error"))))))))))

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
              (fn [{:keys [status]}]
                (case status
                  :merged      (rx/of (modal/hide)
                                      (ntf/success (tr "workspace.branches.merge.success"))
                                      (fetch-branches)
                                      (fetch-branch-context))
                  :conflicts   (rx/of (ntf/warn (tr "workspace.branches.merge.conflicts")))
                  :unsupported (rx/of (ntf/warn (tr "workspace.branches.merge.unsupported")))
                  (rx/of (ntf/error (tr "workspace.branches.merge.error"))))))
             (rx/catch (fn [_]
                         (rx/of (ntf/error (tr "workspace.branches.merge.error")))))))))))
