(ns clj-reload.core
  (:require
    [clj-reload.keep :as keep]
    [clj-reload.parse :as parse]
    [clj-reload.util :as util]
    [clojure.java.io :as io])
  (:import
   [java.util.concurrent.locks ReentrantLock]
   [java.io File]
   [java.net URL]))

; Config :: {:dirs        [<string> ...]       - where to look for files
;            :files       #"<regex>"           - which files to scan, defaults to #".*\.cljc?"
;            :no-unload   #{<symbol> ...}      - list of nses to skip unload
;            :no-reload   #{<symbol> ...}      - list of nses to skip reload
;            :reload-hook <symbol>             - if function with this name exists,
;                                                it will be called after reloading.
;                                                default: after-ns-reload
;            :unload-hook <symbol>}            - if function with this name exists,
;                                                it will be called before unloading.
;                                                default: before-ns-unload

(def ^:private ^:dynamic *config*)

; State :: {:since       <long>               - last time list of files was scanned
;           :files       {<file> -> File}     - all found files
;           :namespaces  {<symbol> -> NS}     - all found namespaces
;           :to-unload   #{<symbol> ...}      - list of nses pending unload
;           :to-load     #{<symbol> ...}}     - list of nses pending load
;
; File ::  {:namespaces #{<symbol> ...}       - nses defined in this file
;           :modified   <modified>}           - lastModified
;
; NS   ::  {:ns-files    #{<file> ...}        - files containing (ns ...) declaration
;           :in-ns-files #{<file> ...}        - files containing (in-ns ...) declaration
;           :requires    #{<symbol> ...}      - other nses this depends on
;           :meta        {}                   - metadata from ns symbol
;           :keep        {<symbol> -> Keep}}} - vars to keep between reloads
;
; Keep ::  {:tag      <symbol>                - type of value ('def, 'defonce etc)
;           :form     <any>                   - full source form, just in case
;                          
;           // stashed vars                   - one or more of these will contain
;                                               values remembered between reloads
;           :var      Var?
;           :ctor     Var?
;           :map-ctor Var?
;           :proto    Var?
;           :methods  {<symbol> Var}?}

(def ^:private *state
  (atom {}))

(def ^ReentrantLock lock
  (ReentrantLock.))

(defmacro with-lock [& body]
  `(try
     (.lock lock)
     ~@body
     (finally
       (.unlock lock))))

(defn- files->namespaces [files already-read]
  (let [*res (volatile! {})]
    (doseq [file files
            [name namespace] (or
                               (already-read file)
                               (parse/read-file file))
            :when (not (util/throwable? namespace))]
      (vswap! *res update name #(merge-with into % namespace)))
    @*res))

(defn- scan-impl [files-before since]
  (let [files-now        (->> (:dirs *config*)
                           (mapcat #(file-seq (io/file %)))
                           (filter util/file?)
                           (filter #(re-matches (:files *config*) (util/file-name %))))

        [files-modified
         files-broken]   (reduce
                           (fn [[modified broken] file]
                             (if (<= (util/last-modified file) since)
                               [modified broken]
                               (let [res (parse/read-file file)]
                                 (if (util/throwable? res)
                                   [modified (assoc broken file res)]
                                   [(assoc modified file res) broken]))))
                           [{} {}] files-now)

        files-deleted    (reduce disj (set (keys files-before)) files-now)
        
        nses-broken      (util/for-map [[file ex] files-broken
                                        ns (get-in files-before [file :namespaces])]
                           [ns ex])
        
        nses-unload      (reduce
                           #(into %1 (get-in files-before [%2 :namespaces]))
                           #{}
                           (concat (keys files-modified) files-deleted))
        
        nses-load        (util/for-set [[file namespaces] files-modified
                                        ns (keys namespaces)]
                           ns)
        
        files'           (as-> files-before %
                           (reduce dissoc % files-deleted)
                           (merge %
                             (util/for-map [[file namespaces] files-modified]
                               [file {:namespaces (set (keys namespaces))
                                      :modified   (util/last-modified file)}])))
        
        already-read     (merge
                           files-modified
                           (util/for-map [[file _] files-broken]
                             [file {}]))
        
        nses'            (files->namespaces (keys files') already-read)] ;; TODO don't parse second time
                           
    {:broken      nses-broken
     :files'      files'
     :namespaces' nses'
     :to-unload'  nses-unload
     :to-load'    nses-load}))

(defn find-namespaces
  "Returns namespaces matching regex, or all of them"
  ([]
   (find-namespaces #".*"))
  ([regex]
   (binding [util/*log-fn* nil]
     (let [{:keys [files]} @*state
           {:keys [namespaces']} (scan-impl files 0)]
       (into #{} (filter #(re-matches regex (name %)) (keys namespaces')))))))

(def ^{:doc "Returns dirs that are currently on classpath"
       :arglists '([])}
  classpath-dirs
  util/classpath-dirs)

(defn init
  "Options:
   
   :dirs        :: [<string> ...]  - where to look for files
   :files       :: #\"<regex>\"    - which files to scan, defaults to #\".*\\\\.cljc?\"
   :no-reload   :: #{<symbol> ...} - list of namespaces to skip reload entirely
   :no-unload   :: #{<symbol> ...} - list of namespaces to skip unload only.
                                     These will be loaded “on top” of previous state
   :unload-hook :: <symbol>        - if function with this name exists in a namespace,
                                     it will be called before unloading. Default: 'before-ns-unload
   :reload-hook :: <symbol>        - if function with this name exists in a namespace,
                                     it will be called after reloading. Default: 'after-ns-reload"
  [opts]
  (with-lock
    (binding [util/*log-fn* nil]
      (let [dirs  (vec (or (:dirs opts) (classpath-dirs)))
            files (or (:files opts) #".*\.cljc?")
            now   (util/now)]
        (alter-var-root #'*config*
          (constantly
            {:dirs        dirs
             :files       files
             :no-unload   (set (:no-unload opts))
             :no-reload   (set (:no-reload opts))
             :reload-hook (:reload-hook opts 'after-ns-reload)
             :unload-hook (:unload-hook opts 'before-ns-unload)}))
        (let [{:keys [files' namespaces']} (scan-impl nil 0)]
          (reset! *state {:since       now
                          :files       files'
                          :namespaces  namespaces'}))))))

(defn- topo-sort-fn
  "Accepts dependees map {ns -> #{downsteram-ns ...}},
   returns a fn that topologically sorts dependencies"
  [deps]
  (let [sorted (parse/topo-sort deps)]
    (fn [coll]
      (filter (set coll) sorted))))

(defn- add-unloaded [scan re loaded]
  (let [new (->> (keys (:namespaces' scan))
              (remove loaded)
              (filter #(re-matches re (str %))))]
    (update scan :to-load' into new)))

(defn carry-keeps [from to]
  (util/for-map [[ns-sym ns] to]
    [ns-sym (assoc ns :keep
              (merge-with merge
                (get-in from [ns-sym :keep])
                (:keep ns)))]))

(defn- scan [state opts]
  (let [{:keys [no-unload no-reload]} *config*
        {:keys [since to-load to-unload files namespaces]} state
        {:keys [only] :or {only :changed}} opts
        now              (util/now)
        loaded           @@#'clojure.core/*loaded-libs*
        {:keys [broken
                files'
                namespaces'
                to-unload'
                to-load']} (case only
                             :changed (scan-impl files since)
                             :loaded  (scan-impl files 0)
                             :all     (scan-impl files 0)
                             #_regex  (-> (scan-impl files since)
                                        (add-unloaded only loaded)))
        
        _                (doseq [[ns {:keys [exception]}] broken
                                 :when (loaded ns)
                                 :when (not (no-reload ns))]
                           (throw exception))
        
        since'           (transduce (map :modified) max (max since now) (vals files'))
        
        unload?          #(and
                            (loaded %)
                            (not (:clj-reload/no-unload (:meta (namespaces %))))
                            (not (:clj-reload/no-reload (:meta (namespaces %))))
                            (not (no-unload %))
                            (not (no-reload %)))
        deps             (parse/dependees namespaces)
        topo-sort        (topo-sort-fn deps) 
        to-unload''      (->> to-unload'
                           (filter unload?)
                           (parse/transitive-closure deps)
                           (filter unload?)
                           (concat to-unload)
                           (topo-sort)
                           (reverse))
        
        load?            #(and
                            (case only
                              :changed (loaded %)
                              :loaded  (loaded %)
                              :all     true
                              #_regex  (or (loaded %) (re-matches only (str %))))
                            (not (:clj-reload/no-reload (:meta (namespaces %))))
                            (not (no-reload %))
                            (namespaces' %))
        deps'            (parse/dependees namespaces')
        topo-sort'       (topo-sort-fn deps')
        to-load''        (->> to-load'
                           (filter load?)
                           (parse/transitive-closure deps')
                           (filter load?)
                           (concat to-load)
                           (topo-sort'))]
    (assoc state
      :since      since'
      :files      files'
      :namespaces (carry-keeps namespaces namespaces')
      :to-unload  to-unload''
      :to-load    to-load'')))

(defn- ns-unload [ns]
  (util/log "Unloading" ns)
  (try
    (when-some [unload-hook (:unload-hook *config*)]
      (when-some [ns-obj (find-ns ns)]
        (when-some [unload-fn (ns-resolve ns-obj unload-hook)]
          (unload-fn))))
    (catch Throwable t
      ;; eat up unload error
      ;; if we can’t unload there’s no way to fix that
      ;; because any changes would require reload first
      (util/log "  exception during unload hook" t)))
  (remove-ns ns)
  (dosync
    (alter @#'clojure.core/*loaded-libs* disj ns)))

(defn- ns-load [ns file-or-url keeps]
  (util/log "Loading" ns #_"from" #_(util/file-path file))
  (try
    (if (empty? keeps)
      (util/ns-load-file (slurp file-or-url) ns (if (instance? java.io.File file-or-url)
                                                  (.getName ^File file-or-url)
                                                  (.getFile ^URL  file-or-url)))
      (if (instance? java.io.File file-or-url)
        (keep/ns-load-patched ns file-or-url keeps)
        (throw (ex-info "Can only use keeps with java.io.File" {:ns ns
                                                                :file-or-url file-or-url
                                                                :keeps keeps}))))

    (when-some [reload-hook (:reload-hook *config*)]
      (when-some [reload-fn (ns-resolve (find-ns ns) reload-hook)]
        (reload-fn)))

    nil
    (catch Throwable t
      (util/log "  failed to load" ns t)
      t)))

(defn unload
  "Same as `reload`, but does not loads namespaces back"
  ([]
   (unload nil))
  ([opts]
   (binding [util/*log-fn* (:log-fn opts util/*log-fn*)]
     (swap! *state scan opts)
     (loop [unloaded []]
       (let [state @*state]
         (if (not-empty (:to-unload state))
           (let [[ns & to-unload'] (:to-unload state)
                 keeps             (keep/resolve-keeps ns (-> state :namespaces ns :keep))]
             (ns-unload ns)
             (swap! *state
               #(-> % 
                  (assoc :to-unload to-unload')
                  (update :namespaces update ns update :keep util/deep-merge keeps)))
             (recur (conj unloaded ns)))
           (do
             (when (empty? unloaded)
               (util/log "Nothing to unload"))
             {:unloaded unloaded})))))))

(defn reload
  "Options:
   
     :throw  :: true | false  - throw or return exception, default true
     :log-fn :: (fn [& args]) - fn to display unload/reload status
     :only   :: :changed      - default. Only reloads changed already loaded files
              | :loaded       - Reload all loaded files
              | <Pattern>     - Reload all nses matching this pattern
              | :all          - Reload everything it can find in dirs
   
   Returns map of what was reloaded
   
     {:unloaded [<symbol> ...]
      :loaded   [<symbol> ...]}
   
   If anything fails, throws. If :throw false, return value will also have keys
   
     {:failed    <symbol>
      :exception <Throwable>}
   
   Can be called multiple times. If reload fails, fix the error and call `reload` again"
  ([]
   (reload nil))
  ([opts]
   (with-lock
     (binding [util/*log-fn* (:log-fn opts util/*log-fn*)]
       (let [{:keys [unloaded]} (unload opts)]
         (loop [loaded []]
           (let [state @*state]
             (if (not-empty (:to-load state))
               (let [[ns & to-load'] (:to-load state)
                     files (-> state :namespaces ns :ns-files)]
                 (if-some [ex (some #(ns-load ns % (-> state :namespaces ns :keep)) files)]
                   (do
                     (swap! *state update :to-unload #(cons ns %))
                     (if (:throw opts true)
                       (throw
                         (ex-info
                           (str "Failed to load namespace: " ns)
                           {:unloaded  unloaded
                            :loaded    loaded
                            :failed    ns}
                           ex))
                       {:unloaded  unloaded
                        :loaded    loaded
                        :failed    ns
                        :exception ex}))
                   (do
                     (swap! *state #(-> %
                                      (assoc :to-load to-load')
                                      (update-in [:namespaces ns] dissoc :keep)))
                     (recur (conj loaded ns)))))
               (do
                 (when (empty? loaded)
                   (util/log "Nothing to reload"))
                 {:unloaded unloaded
                  :loaded   loaded})))))))))

(defn reload-all

  "Reload all loaded namespaces that contains at least one var, which matches
  regex, and any other namespaces depending on them."

  [regex]

  (let [{:keys [no-unload no-reload]} *config*
        ;; collect all loaded namespaces resources files paths set
        all-paths (->> (all-ns)
                       (reduce (fn [files ns]
                                 (reduce (fn [files' ns-var]
                                           (if-let [f-path (some-> ns-var meta :file)]
                                             (conj files' f-path)
                                             files'))
                                         files
                                         (vals (ns-interns ns))))
                               #{}))

        ;; build the namespaces map
        namespaces (reduce (fn [nss path]
                             (let [res (parse/read-resource path)]
                               ;; throwables here are caused for example by reading
                               ;; files which contains "#{`ns 'ns}". This is because
                               ;; of reading with clj-reload.util/dummy-resolver
                               (if-not (util/throwable? res)
                                 (merge nss res)
                                 nss)))
                           {}
                           all-paths)
        reload?          #(and
                           (not (:clj-reload/no-unload (:meta (namespaces %))))
                           (not (:clj-reload/no-reload (:meta (namespaces %))))
                           (not (no-unload %))
                           (not (no-reload %)))
        dependees  (parse/dependees namespaces)
        topo-sort  (topo-sort-fn dependees)
        matched-ns (->> (keys namespaces)
                        (filterv (fn [ns] (re-matches regex (name ns))))
                        (into #{}))
        to-reload   (->> (parse/deep-dependees-set matched-ns dependees)
                         topo-sort)
        to-unload (reverse to-reload)]

    (doseq [ns to-unload]
      (ns-unload ns))

    (doseq [ns to-reload]
      (doseq [ns-files (get-in namespaces [ns :ns-files])]
        (ns-load ns ns-files {})))))

(defmulti keep-methods
  (fn [tag]
    tag))

(defmethod keep-methods :default [tag]
  (throw
    (ex-info
      (str "Keeping " tag " forms is not implemented")
      {:tag tag})))
    
(defmethod keep-methods 'def [_]
  keep/keep-methods-defs)

(defmethod keep-methods 'defn [_]
  keep/keep-methods-defs)

(defmethod keep-methods 'defn- [_]
  keep/keep-methods-defs)

(defmethod keep-methods 'defonce [_]
  keep/keep-methods-defs)

(defmethod keep-methods 'deftype [_]
  keep/keep-methods-deftype)

(defmethod keep-methods 'defrecord [_]
  keep/keep-methods-defrecord)

(defmethod keep-methods 'defprotocol [_]
  keep/keep-methods-defprotocol)

;; Initialize with classpath-dirs to support “init-less” workflow
;; See https://github.com/tonsky/clj-reload/pull/4
;; and https://github.com/clojure-emacs/cider-nrepl/issues/849
(init
  {:dirs (classpath-dirs)})
