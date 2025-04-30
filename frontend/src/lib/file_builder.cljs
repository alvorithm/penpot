;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns lib.file-builder
  (:require
   [app.common.data.macros :as dm]
   [app.common.data :as d]
   [app.common.schema :as sm]
   [app.common.features :as cfeat]
   [app.common.files.builder :as fb]
   [app.common.media :as cm]
   [app.common.types.file :as types.file]
   [app.common.types.page :as types.page]
   [app.common.types.shape :as types.shape]
   [app.common.types.color :as types.color]
   [app.common.types.components-list :as ctkl]
   [app.common.uuid :as uuid]
   [app.common.json :as json]
   [app.util.object :as obj]
   [app.util.webapi :as wapi]
   [app.util.zip :as uz]
   [app.worker.export :as e]
   [beicon.v2.core :as rx]
   [cuerdas.core :as str]
   [promesa.core :as p]))

(defn parse-data [data]
  (as-> data $
    (js->clj $ :keywordize-keys true)
    ;; Transforms camelCase to kebab-case
    (d/deep-mapm
     (fn [[key value]]
       (let [value (if (= (type value) js/Symbol)
                     (keyword (js/Symbol.keyFor value))
                     value)
             key (-> key d/name str/kebab keyword)]
         [key value])) $)))

(defn data-uri->blob
  [data-uri]
  (let [[mtype b64-data] (str/split data-uri ";base64,")
        mtype   (subs mtype (inc (str/index-of mtype ":")))
        decoded (.atob js/window b64-data)
        size    (.-length ^js decoded)
        content (js/Uint8Array. size)]

    (doseq [i (range 0 size)]
      (aset content i (.charCodeAt decoded i)))

    (wapi/create-blob content mtype)))

(defn parse-library-media
  [[file-id media]]
  (rx/merge
   (let [markup
         (->> (vals media)
              (reduce e/collect-media {})
              #_(json/encode)
              )]
     (rx/of (vector (str file-id "/media.json") markup)))

   (->> (rx/from (vals media))
        (rx/map #(assoc % :file-id file-id))
        (rx/merge-map
         (fn [media]
           (let [file-path (str/concat file-id "/media/" (:id media) (cm/mtype->extension (:mtype media)))
                 blob (data-uri->blob (:uri media))]
             (rx/of (vector file-path blob))))))))

(defn export-file
  [file]
  (let [file (assoc file
                    :name (:name file)
                    :file-name (:name file)
                    :is-shared false)

        files-stream (->> (rx/of {(:id file) file})
                          (rx/share))

        manifest-stream
        (->> files-stream
             (rx/map #(e/create-manifest (uuid/next) (:id file) :all % cfeat/default-features))
             (rx/map (fn [a]
                       (vector "manifest.json" a))))

        render-stream
        (->> files-stream
             (rx/merge-map vals)
             (rx/merge-map e/process-pages)
             (rx/observe-on :async)
             (rx/merge-map e/get-page-data)
             (rx/share))

        colors-stream
        (->> files-stream
             (rx/merge-map vals)
             (rx/map #(vector (:id %) (get-in % [:data :colors])))
             (rx/filter #(d/not-empty? (second %)))
             (rx/map e/parse-library-color))

        typographies-stream
        (->> files-stream
             (rx/merge-map vals)
             (rx/map #(vector (:id %) (get-in % [:data :typographies])))
             (rx/filter #(d/not-empty? (second %)))
             (rx/map e/parse-library-typographies))

        media-stream
        (->> files-stream
             (rx/merge-map vals)
             (rx/map #(vector (:id %) (get-in % [:data :media])))
             (rx/filter #(d/not-empty? (second %)))
             (rx/merge-map parse-library-media))

        components-stream
        (->> files-stream
             (rx/merge-map vals)
             (rx/filter #(d/not-empty? (ctkl/components-seq (:data %))))
             (rx/merge-map e/parse-library-components))

        pages-stream
        (->> render-stream
             (rx/map e/collect-page))]

    (rx/merge
     (->> render-stream
          (rx/map #(hash-map
                    :type :progress
                    :file (:id file)
                    :data (str "Render " (:file-name %) " - " (:name %)))))

     (->> (rx/merge
           manifest-stream
           pages-stream
           components-stream
           media-stream
           colors-stream
           typographies-stream)
          (rx/reduce conj [])
          (rx/with-latest-from files-stream)
          (rx/merge-map (fn [[data _]]
                          (->> (uz/compress-files data)
                               (rx/map #(vector file %)))))))))

(deftype File [^:mutable file]
  Object
  (addPage [_ name]
    (set! file (fb/add-page file {:name name}))
    (str (:current-page-id file)))

  (addPage [_ name options]
    (set! file (fb/add-page file {:name name :options (parse-data options)}))
    (str (:current-page-id file)))

  (closePage [_]
    (set! file (fb/close-page file)))

  (addArtboard [_ data]
    (set! file (fb/add-artboard file (parse-data data)))
    (str (:last-id file)))

  (closeArtboard [_]
    (set! file (fb/close-artboard file)))

  (addGroup [_ data]
    (set! file (fb/add-group file (parse-data data)))
    (str (:last-id file)))

  (closeGroup [_]
    (set! file (fb/close-group file)))

  (addBool [_ data]
    (set! file (fb/add-bool file (parse-data data)))
    (str (:last-id file)))

  (closeBool [_]
    (set! file (fb/close-bool file)))

  (createRect [_ data]
    (set! file (fb/create-rect file (parse-data data)))
    (str (:last-id file)))

  (createCircle [_ data]
    (set! file (fb/create-circle file (parse-data data)))
    (str (:last-id file)))

  (createPath [_ data]
    (set! file (fb/create-path file (parse-data data)))
    (str (:last-id file)))

  (createText [_ data]
    (set! file (fb/create-text file (parse-data data)))
    (str (:last-id file)))

  (createImage [_ data]
    (set! file (fb/create-image file (parse-data data)))
    (str (:last-id file)))

  (createSVG [_ data]
    (set! file (fb/create-svg-raw file (parse-data data)))
    (str (:last-id file)))

  (closeSVG [_]
    (set! file (fb/close-svg-raw file)))

  (addLibraryColor [_ data]
    (set! file (fb/add-library-color file (parse-data data)))
    (str (:last-id file)))

  (updateLibraryColor [_ data]
    (set! file (fb/update-library-color file (parse-data data)))
    (str (:last-id file)))

  (deleteLibraryColor [_ data]
    (set! file (fb/delete-library-color file (parse-data data)))
    (str (:last-id file)))

  (addLibraryMedia [_ data]
    (set! file (fb/add-library-media file (parse-data data)))
    (str (:last-id file)))

  (deleteLibraryMedia [_ data]
    (set! file (fb/delete-library-media file (parse-data data)))
    (str (:last-id file)))

  (addLibraryTypography [_ data]
    (set! file (fb/add-library-typography file (parse-data data)))
    (str (:last-id file)))

  (deleteLibraryTypography [_ data]
    (set! file (fb/delete-library-typography file (parse-data data)))
    (str (:last-id file)))

  (startComponent [_ data]
    (set! file (fb/start-component file (parse-data data)))
    (str (:current-component-id file)))

  (finishComponent [_]
    (set! file (fb/finish-component file)))

  (createComponentInstance [_ data]
    (set! file (fb/create-component-instance file (parse-data data)))
    (str (:last-id file)))

  (lookupShape [_ shape-id]
    (clj->js (fb/lookup-shape file (uuid/parse shape-id))))

  (updateObject [_ id new-obj]
    (let [old-obj (fb/lookup-shape file (uuid/parse id))
          new-obj (d/deep-merge old-obj (parse-data new-obj))]
      (set! file (fb/update-object file old-obj new-obj))))

  (deleteObject [_ id]
    (set! file (fb/delete-object file (uuid/parse id))))

  (getId [_]
    (:id file))

  (getCurrentPageId [_]
    (:current-page-id file))

  (asMap [_]
    (clj->js file))

  (newId [_]
    (uuid/next))

  (export [_]
    (p/create
     (fn [resolve reject]
       (->> (export-file file)
            (rx/filter #(not= (:type %) :progress))
            (rx/take 1)
            (rx/subs!
             (fn [value]
               (let [[_ export-blob] value]
                 (resolve export-blob)))
             reject))))))

(def Exception
  (obj/class
   :name "Exception"
   :extends js/Error
   :constructor
   (fn [this type code hint cause]
     (.call js/Error this hint)
     (set! (.-name this) (str "Exception: " hint))
     (set! (.-type this) type)
     (set! (.-code this) code)
     (set! (.-hint this) hint)

     (when (exists? js/Error.captureStackTrace)
       (.captureStackTrace js/Error this))

    (obj/add-properties!
     this
     {:name "cause"
      :enumerable true
      :this false
      :get (fn [] cause)}
     {:name "data"
      :enumerable true
      :this false
      :get (fn []
             (let [data (ex-data cause)]
               (when-let [explain (::sm/explain data)]
                 (json/->js (sm/simplify explain)))))}))))

(defn- handle-exception
  [cause]
  (let [data (ex-data cause)]
    (throw (new Exception
                (d/name (get data :type :unknown))
                (d/name (get data :code :unknown))
                (or (get data :hint) (ex-message cause))
                cause))))

(defn- default-uuid
  [v]
  (or v (uuid/next)))

(def ^:private decode-file
  (sm/decoder types.file/schema:file sm/json-transformer))

(def ^:private decode-page
  (sm/decoder types.page/schema:page sm/json-transformer))

(def ^:private decode-shape
  (sm/decoder types.shape/schema:shape-attrs sm/json-transformer))

(def ^:private decode-library-color
  (sm/decoder types.color/schema:color sm/json-transformer))

(defn- decode-params
  [params]
  (if (obj/plain-object? params)
    (json/->js params)
    params))

(defn- create-file*
  [file]
  (let [state* (volatile! file)]
    (obj/reify {:name "File"}
      :id
      {:get #(dm/str (:id @state*))}

      :addPage
      (fn [params]
        (try
          (let [params (-> params
                           (decode-params)
                           (decode-page))]
            (vswap! state* fb/add-page params)
            (dm/str (::fb/current-page-id @state*)))
          (catch :default cause
            (handle-exception cause))))

      :closePage
      (fn []
        (vswap! state* fb/close-page))

      :addArtboard
      (fn [params]
        (try
          (let [params (-> params
                           (json/->clj)
                           (assoc :type :frame)
                           (update :id default-uuid)
                           (decode-shape))]
            (vswap! state* fb/add-artboard params)
            (dm/str (::fb/current-page-id @state*)))
          (catch :default cause
            (handle-exception cause))))

      :closeArtboard
      (fn []
        (vswap! state* fb/close-artboard))

      :addGroup
      (fn [params]
        (try
          (let [params (-> params
                           (json/->clj)
                           (assoc :type :group)
                           (update :id default-uuid)
                           (decode-shape))]
            (vswap! state* fb/add-group params)
            (dm/str (::fb/current-page-id @state*)))
          (catch :default cause
            (handle-exception cause))))

      :closeGroup
      (fn []
        (vswap! state* fb/close-group))

      :addBool
      (fn [params]
        (try
          (let [params (-> params
                           (json/->clj)
                           (assoc :type :bool)
                           (update :id default-uuid)
                           (decode-shape))]
            (vswap! state* fb/add-bool params)
            (dm/str (::fb/current-page-id @state*)))
          (catch :default cause
            (handle-exception cause))))

      :closeBool
      (fn []
        (vswap! state* fb/close-bool))

      :addRect
      (fn [params]
        (try
          (let [params (-> params
                           (json/->clj)
                           (assoc :type :rect)
                           (update :id default-uuid)
                           (decode-shape))]
            (vswap! state* fb/create-rect params)
            (dm/str (::fb/current-page-id @state*)))
          (catch :default cause
            (handle-exception cause))))

      :addCircle
      (fn [params]
        (try
          (let [params (-> params
                           (json/->clj)
                           (assoc :type :circle)
                           (update :id default-uuid)
                           (decode-shape))]
            (vswap! state* fb/create-circle params)
            (dm/str (::fb/current-page-id @state*)))
          (catch :default cause
            (handle-exception cause))))

      :addPath
      (fn [params]
        (try
          (let [params (-> params
                           (json/->clj)
                           (assoc :type :path)
                           (update :id default-uuid)
                           (decode-shape))]
            (vswap! state* fb/create-path params)
            (dm/str (::fb/current-page-id @state*)))
          (catch :default cause
            (handle-exception cause))))

      :addText
      (fn [params]
        (try
          (let [params (-> params
                           (json/->clj)
                           (assoc :type :text)
                           (update :id default-uuid)
                           (decode-shape))]
            (vswap! state* fb/create-text params)
            (dm/str (::fb/current-page-id @state*)))
          (catch :default cause
            (handle-exception cause))))

      :addLibraryColor
      (fn [params]
        (try
          (let [params (-> params
                           (json/->clj)
                           (decode-library-color)
                           (d/without-nils))]
            (vswap! state* fb/add-library-color params)
            (dm/str (::fb/last-id @state*)))
          (catch :default cause
            (handle-exception cause))))

      :toMap
      (fn []
        (-> @state*
            (d/without-qualified)
            (json/->js))))))

(defn create-file
  [params]
  (try
    (let [params (-> params json/->clj decode-file)
          file   (fb/create-file params)]
      (create-file* file))
    (catch :default cause
      (handle-exception cause))))
