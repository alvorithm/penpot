;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC Sucursal en España SL

(ns app.main.data.workspace.branches
  "Data layer for file branching (Phase 1: create + list). Mirrors the
  patterns in `app.main.data.workspace.versions`."
  (:require
   [app.common.uuid :as uuid]
   [app.main.data.common :as dcm]
   [app.main.data.event :as ev]
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
        (->> (rp/cmd! :get-file-branches {:file-id file-id})
             (rx/map #(update-branches-state {:status :loaded :data %}))
             (rx/catch (fn [_]
                         (rx/of (update-branches-state {:status :loaded :data []})))))))))

(defn create-branch
  "Force-persist the current file, then create a branch from it. Closing
  the dialog is responsibility of the caller; errors surface as a toast."
  [name description]
  (assert (string? name) "expected string for `name`")
  (ptk/reify ::create-branch
    ptk/WatchEvent
    (watch [_ state _]
      (let [file-id (:current-file-id state)]
        ;; Force persist before branching, otherwise the merge base
        ;; snapshot could miss the latest local changes.
        (rx/concat
         (rx/of ::dwp/force-persist
                (ev/event {::ev/name "create-branch"}))

         (->> (dwp/wait-persisted)
              (rx/mapcat #(rp/cmd! :create-file-branch
                                   {:file-id file-id
                                    :name name
                                    :description description}))
              (rx/mapcat
               (fn [_]
                 (rx/of (ntf/success (tr "workspace.branches.create.success" name))
                        (fetch-branches))))
              (rx/catch
               (fn [_]
                 (rx/of (ntf/error (tr "workspace.branches.create.error")))))))))))

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
