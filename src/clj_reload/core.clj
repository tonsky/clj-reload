(ns clj-reload.core
  (:require
    [clj-reload.keep :as keep]
    [clj-reload.parse :as parse]
    [clj-reload.util :as util]
    [clojure.java.classpath :as classpath]
    [clojure.java.io :as io]))

; State :: {// config
;           
;           :dirs        [<string> ...]       - where to look for files
;           :no-unload   #{<symbol> ...}      - list of nses to skip unload
;           :no-reload   #{<symbol> ...}      - list of nses to skip reload
;           :reload-hook <symbol>             - if function with this name exists,
;                                               it will be called after reloading.
;                                               default: after-ns-reload
;           :unload-hook <symbol>             - if function with this name exists,
;                                               it will be called before unloading.
;                                               default: before-ns-unload
;           // working state
;
;           :since       <long>               - last time list of files was scanned
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

(defn- files->namespaces [files already-read]
  (let [*res (volatile! {})]
    (doseq [file files
            [name namespace] (or
                               (already-read file)
                               (parse/read-file file))
            :when (not (util/throwable? namespace))]
      (vswap! *res update name #(merge-with into % namespace)))
    @*res))

(defn- scan-impl [files-before dirs since]
  (let [files-now        (->> dirs
                           (mapcat #(file-seq (io/file %)))
                           (filter util/file?)
                           (filter #(re-matches #".*\.cljc?" (util/file-name %))))

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

(defn- init-impl [opts]
  (let [dirs (vec (:dirs opts))
        now  (util/now)
        {:keys [files' namespaces']} (scan-impl nil dirs 0)]
    {:dirs        dirs
     :no-unload   (set (:no-unload opts))
     :no-reload   (set (:no-reload opts))
     :reload-hook (:reload-hook opts 'after-ns-reload)
     :unload-hook (:unload-hook opts 'before-ns-unload)
     :since       now
     :files       files'
     :namespaces  namespaces'}))

(defn init
  "Options:
   
   :dirs        :: [<string> ...]  - where to look for files
   :no-reload   :: #{<symbol> ...} - list of namespaces to skip reload entirely
   :no-unload   :: #{<symbol> ...} - list of namespaces to skip unload only.
                                     These will be loaded “on top” of previous state
   :unload-hook :: <symbol>        - if function with this name exists in a namespace,
                                     it will be called before unloading. Default: 'before-ns-unload
   :reload-hook :: <symbol>        - if function with this name exists in a namespace,
                                     it will be called after reloading. Default: 'after-ns-reload"
  [opts]
  (binding [util/*log-fn* nil]
    (reset! *state (init-impl opts))))

(defn- topo-sort-fn
  "Accepts dependees map {ns -> #{downsteram-ns ...}},
   returns a fn that topologically sorts dependencies"
  [deps]
  (let [sorted (parse/topo-sort deps)]
    (fn [coll]
      (filter (set coll) sorted))))

(defn- scan [state opts]
  (let [{:keys [dirs since to-load to-unload no-unload no-reload files namespaces]} state
        {:keys [only] :or {only :changed}} opts
        now              (util/now)
        loaded           (case only
                           :changed @@#'clojure.core/*loaded-libs*
                           :loaded  @@#'clojure.core/*loaded-libs*
                           :all     (constantly true))
        {:keys [broken
                files'
                namespaces'
                to-unload'
                to-load']} (case only
                             :changed (scan-impl files dirs since)
                             :loaded  (scan-impl files dirs 0)
                             :all     (scan-impl files dirs 0))
        
        _                (doseq [[ns {:keys [exception]}] broken
                                 :when (loaded ns)
                                 :when (not (no-reload ns))]
                           (throw exception))
        
        since'           (transduce (map :modified) max (max since now) (vals files'))
        
        unload?          #(and
                            (loaded %)
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
                            (loaded %)
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
      :namespaces namespaces'
      :to-unload  to-unload''
      :to-load    to-load'')))

(defn- ns-unload [ns unload-hook]
  (util/log "Unloading" ns)
  (try
    (when unload-hook
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

(defn- ns-load [ns file keeps reload-hook]
  (util/log "Loading" ns #_"from" #_(util/file-path file))
  (try
    (if (empty? keeps)
      (util/ns-load-file (slurp file) ns file)
      (keep/ns-load-patched ns file keeps))
    
    (when reload-hook
      (when-some [reload-fn (ns-resolve (find-ns ns) reload-hook)]
        (reload-fn)))
    
    nil
    (catch Throwable t
      (util/log "  failed to load" ns t)
      t)))

(defn reload
  "Options:
   
     :throw  :: true | false  - throw or return exception, default true
     :log-fn :: (fn [& args]) - fn to display unload/reload status
     :only   :: :changed      - default. Only reloads changed already loaded files
              | :loaded       - Reload all loaded files
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
   (binding [util/*log-fn* (:log-fn opts util/*log-fn*)]
     (if (empty? @*state)
       (do
         (init {:dirs (classpath/classpath-directories)})
         (swap! *state scan (assoc opts :only :all)))
       (swap! *state scan opts))
     (loop [unloaded []
            loaded   []
            state    @*state]
       (cond
         (not-empty (:to-unload state))
         (let [[ns & to-unload'] (:to-unload state)
               keeps             (keep/resolve-keeps ns (-> state :namespaces ns :keep))
               _                 (ns-unload ns (:unload-hook state))
               state'            (swap! *state #(-> % 
                                                  (assoc :to-unload to-unload')
                                                  (update :namespaces update ns assoc :keep keeps)))]
           (recur (conj unloaded ns) loaded state'))
        
         (not-empty (:to-load state))
         (let [[ns & to-load'] (:to-load state)
               files (-> state :namespaces ns :ns-files)] ;; TODO #3
           (if-some [ex (some #(ns-load ns % (-> state :namespaces ns :keep) (:reload-hook state)) files)]
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
             (let [state' (swap! *state assoc :to-load to-load')]
               (recur unloaded (conj loaded ns) state'))))
         
         :else
         (do
           (when (and (empty? unloaded) (empty? loaded))
             (util/log "Nothing to reload"))
           {:unloaded unloaded
            :loaded   loaded}))))))

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
