(ns clj-reload.keep-test
  (:require
    [clojure.string :as str]
    [clojure.test :refer [is are deftest testing use-fixtures]]
    [clj-reload.core :as reload]
    [clj-reload.keep :as keep]
    [clj-reload.test-util :as tu]))

(defn wrap-test [f]
  (binding [tu/*dir* "fixtures/keep_test"]
    (tu/reset
      '[clj-reload.keep-custom
        clj-reload.keep-defprotocol
        clj-reload.keep-defrecord
        clj-reload.keep-deftype
        clj-reload.keep-vars
        clj-reload.keep-downstream
        clj-reload.keep-upstream])
    (f)))

(use-fixtures :each wrap-test)

(deftest patch-file-test
  (is (=  "before (def *atom 888)     after"
        (keep/patch-file
          "before (defonce *atom 777) after"
          {'(defonce *atom) "(def *atom 888)"})))
  
  (is (=  "before (def *atom 1000000) after"
        (keep/patch-file
          "before (def *atom 1) after"
          {'(def *atom) "(def *atom 1000000)"})))
  
  (is (=  "before (def *atom 888)
       after"
        (keep/patch-file
          "before (defonce *atom
  777) after"
          {'(defonce *atom) "(def *atom 888)"})))
  
  (is (=  "before (def *atom 888) (reset! *atom nil) after"
        (keep/patch-file
          "before (def *atom 777) (reset! *atom nil) after"
          {'(def *atom) "(def *atom 888)"})))
  
  (is (=  "(ns keep)
           
              asdas
           
           8 10  (def *atom 777)
                        
           
           (def just-var 888)
                       "
        (keep/patch-file
          "(ns keep)
           
              asdas
           
           8 10  (defonce *atom
             (atom nil))
           
           (defonce just-var
             (Object.))"
          {'(defonce *atom) "(def *atom 777)"
           '(defonce just-var) "(def just-var 888)"}))))

(defn meta= [a b]
  (= (dissoc (meta a) :ns) (dissoc (meta b) :ns)))

(deftest keep-vars-test
  (tu/init 'clj-reload.keep-vars)
  (let [ns          (find-ns 'clj-reload.keep-vars)
        normal      @(ns-resolve ns 'normal)
        atom        (reset! @(ns-resolve ns '*atom) 100500)
        just-var    @(ns-resolve ns 'just-var)
        just-var-2  @(ns-resolve ns 'just-var-2)
        private-var @(ns-resolve ns 'private-var)
        dependent   @(ns-resolve ns 'dependent)
        meta-var    (ns-resolve ns 'meta-var)
        public-fn   (ns-resolve ns 'public-fn)
        private-fn  (ns-resolve ns 'private-fn)
        normal-2    @(ns-resolve ns 'normal-2)
        
        _           (tu/touch 'clj-reload.keep-vars)
        _           (tu/reload)
        ns'         (find-ns 'clj-reload.keep-vars)]
    
    (is (not= normal @(ns-resolve ns' 'normal)))
    (is (= atom @@(ns-resolve ns' '*atom)))
    (is (= just-var @(ns-resolve ns' 'just-var)))
    (is (= just-var-2 @(ns-resolve ns' 'just-var-2)))
    (is (= private-var @(ns-resolve ns' 'private-var)))
    (is (= (first dependent) (first @(ns-resolve ns' 'dependent))))
    (is (not= (second dependent) (second @(ns-resolve ns' 'dependent))))
    
    (is (= @meta-var @(ns-resolve ns' 'meta-var)))
    (is (meta= meta-var (ns-resolve ns' 'meta-var)))
    
    (is (= @public-fn @(ns-resolve ns' 'public-fn)))
    (is (meta= public-fn (ns-resolve ns' 'public-fn)))
    
    (is (= @private-fn @(ns-resolve ns' 'private-fn)))
    (is (meta= private-fn (ns-resolve ns' 'private-fn)))
    
    (is (not= normal-2 @(ns-resolve ns' 'normal-2)))))

(deftest keep-unsupported-test
  (tu/init 'clj-reload.keep-unsupported)
  (let [ns (find-ns 'clj-reload.keep-unsupported)
        v  @(ns-resolve ns 'v)]
    (tu/touch 'clj-reload.keep-unsupported)
    (is (thrown? Exception (tu/reload)))))

(deftest keep-type-test
  (tu/init 'clj-reload.keep-deftype)
  (let [ns             (find-ns 'clj-reload.keep-deftype)
        normal-new     @(ns-resolve ns 'type-normal-new)
        normal-factory @(ns-resolve ns 'type-normal-factory)
        keep-new       @(ns-resolve ns 'type-keep-new)
        keep-factory   @(ns-resolve ns 'type-keep-factory)
        _              (tu/touch 'clj-reload.keep-deftype)
        _              (tu/reload)
        ns'            (find-ns 'clj-reload.keep-deftype)]
    (is (not= normal-new @(ns-resolve ns' 'type-normal-new)))
    (is (not (identical? (class normal-new) (class @(ns-resolve ns' 'type-normal-new)))))
    
    (is (not= normal-factory @(ns-resolve ns' 'type-normal-factory)))
    (is (not (identical? (class normal-factory) (class @(ns-resolve ns' 'type-normal-factory)))))
    
    (is (not (identical? keep-new @(ns-resolve ns' 'type-keep-new))))
    (is (= keep-new @(ns-resolve ns' 'type-keep-new)))
    (is (identical? (class keep-new) (class @(ns-resolve ns' 'type-keep-new))))

    (is (not (identical? keep-factory @(ns-resolve ns' 'type-keep-factory))))
    (is (= keep-factory @(ns-resolve ns' 'type-keep-factory)))
    (is (identical? (class keep-factory) (class @(ns-resolve ns' 'type-keep-factory))))))

(deftest keep-record-test
  (tu/init 'clj-reload.keep-defrecord)
  (let [ns                 (find-ns 'clj-reload.keep-defrecord)
        normal-new         @(ns-resolve ns 'record-normal-new)
        normal-factory     @(ns-resolve ns 'record-normal-factory)
        normal-map-factory @(ns-resolve ns 'record-normal-map-factory)
        keep-new           @(ns-resolve ns 'record-keep-new)
        keep-factory       @(ns-resolve ns 'record-keep-factory)
        keep-map-factory   @(ns-resolve ns 'record-keep-map-factory)
        _                  (tu/touch 'clj-reload.keep-defrecord)
        _                  (tu/reload)
        ns'                (find-ns 'clj-reload.keep-defrecord)]
    (is (not= normal-new @(ns-resolve ns' 'record-normal-new)))
    (is (not (identical? (class normal-new) (class @(ns-resolve ns' 'record-normal-new)))))
    
    (is (not= normal-factory @(ns-resolve ns' 'record-normal-factory)))
    (is (not (identical? (class normal-factory) (class @(ns-resolve ns' 'record-normal-factory)))))
    
    (is (not= normal-map-factory @(ns-resolve ns' 'record-normal-map-factory)))
    (is (not (identical? (class normal-map-factory) (class @(ns-resolve ns' 'record-normal-map-factory)))))
    
    (is (not (identical? keep-new @(ns-resolve ns' 'record-keep-new))))
    (is (= keep-new @(ns-resolve ns' 'record-keep-new)))
    (is (identical? (class keep-new) (class @(ns-resolve ns' 'record-keep-new))))

    (is (not (identical? keep-factory @(ns-resolve ns' 'record-keep-factory))))
    (is (= keep-factory @(ns-resolve ns' 'record-keep-factory)))
    (is (identical? (class keep-factory) (class @(ns-resolve ns' 'record-keep-factory))))
    
    (is (not (identical? keep-map-factory @(ns-resolve ns' 'record-keep-map-factory))))
    (is (= keep-map-factory @(ns-resolve ns' 'record-keep-map-factory)))
    (is (identical? (class keep-map-factory) (class @(ns-resolve ns' 'record-keep-map-factory))))))

(defmethod reload/keep-methods 'deftype+ [_]
  (reload/keep-methods 'deftype))

(deftest keep-custom-def-test
  (tu/init 'clj-reload.keep-custom)
  (let [ns    (find-ns 'clj-reload.keep-custom)
        ctor  @(ns-resolve ns '->CustomTypeKeep)
        value @(ns-resolve ns 'custom-type-keep)
        _     (tu/touch 'clj-reload.keep-custom)
        _     (tu/reload)
        ns'   (find-ns 'clj-reload.keep-custom)]
    (is (identical? ctor @(ns-resolve ns' '->CustomTypeKeep)))
    (is (identical? (class value) (class @(ns-resolve ns' 'custom-type-keep))))))

(deftest keep-protocol-test
  (tu/init 'clj-reload.keep-defprotocol)
  (let [ns                (find-ns 'clj-reload.keep-defprotocol)
        proto             @(ns-resolve ns 'IProto)
        method            @(ns-resolve ns '-method)
        rec-inline        @(ns-resolve ns 'rec-inline)
        rec-extend-proto  @(ns-resolve ns 'rec-extend-proto)
        rec-extend-type   @(ns-resolve ns 'rec-extend-type)
        rec-extend        @(ns-resolve ns 'rec-extend)
        extend-meta       @(ns-resolve ns 'extend-meta)
        
        _                 (tu/touch 'clj-reload.keep-defprotocol)
        _                 (tu/reload)
        
        ns'               (find-ns 'clj-reload.keep-defprotocol)
        proto'            @(ns-resolve ns' 'IProto)
        method'           @(ns-resolve ns' '-method)
        rec-inline'       @(ns-resolve ns' 'rec-inline)
        rec-extend-proto' @(ns-resolve ns' 'rec-extend-proto)
        rec-extend-type'  @(ns-resolve ns' 'rec-extend-type)
        rec-extend'       @(ns-resolve ns' 'rec-extend)
        extend-meta'      @(ns-resolve ns' 'extend-meta)]
    
    ;; make sure reload happened
    (is (not (identical? rec-inline rec-inline')))
    (is (not (identical? (class rec-inline) (class rec-inline'))))
    
    (is (satisfies? proto rec-inline))
    (is (satisfies? proto rec-inline'))    
    (is (satisfies? proto' rec-inline))
    (is (satisfies? proto' rec-inline'))
    (is (= :rec-inline (method rec-inline)))
    (is (= :rec-inline (method rec-inline')))
    (is (= :rec-inline (method' rec-inline)))
    (is (= :rec-inline (method' rec-inline')))
    
    (is (satisfies? proto rec-extend-proto))
    ; (is (satisfies? proto rec-extend-proto'))
    (is (satisfies? proto' rec-extend-proto))
    (is (satisfies? proto' rec-extend-proto'))
    (is (= :rec-extend-proto (method rec-extend-proto)))
    ; (is (= :rec-extend-proto (method rec-extend-proto')))
    (is (= :rec-extend-proto (method' rec-extend-proto)))
    (is (= :rec-extend-proto (method' rec-extend-proto')))
    
    (is (satisfies? proto rec-extend-type))
    ; (is (satisfies? proto rec-extend-type'))
    (is (satisfies? proto' rec-extend-type))
    (is (satisfies? proto' rec-extend-type'))
    (is (= :rec-extend-type (method rec-extend-type)))
    ; (is (= :rec-extend-type (method rec-extend-type')))
    (is (= :rec-extend-type (method' rec-extend-type)))
    (is (= :rec-extend-type (method' rec-extend-type')))
    
    (is (satisfies? proto rec-extend))
    ; (is (satisfies? proto rec-extend'))
    (is (satisfies? proto' rec-extend))
    (is (satisfies? proto' rec-extend'))
    (is (= :rec-extend (method rec-extend)))
    ; (is (= :rec-extend (method rec-extend')))
    (is (= :rec-extend (method' rec-extend)))
    (is (= :rec-extend (method' rec-extend')))
    
    ; (is (satisfies? proto extend-meta))
    ; (is (satisfies? proto extend-meta'))
    ; (is (satisfies? proto' extend-meta))
    ; (is (satisfies? proto' extend-meta'))
    (is (= :extend-meta (method extend-meta)))
    (is (= :extend-meta (method extend-meta')))
    (is (= :extend-meta (method' extend-meta)))
    (is (= :extend-meta (method' extend-meta')))))

(deftest keep-dependent-test
  (tu/init 'clj-reload.keep-downstream)
  (let [downstream  @(ns-resolve (find-ns 'clj-reload.keep-downstream) 'downstream-var)
        upstream    @(ns-resolve (find-ns 'clj-reload.keep-upstream)   'upstream-var)
        _           (tu/touch 'clj-reload.keep-upstream)
        _           (tu/reload)
        downstream' @(ns-resolve (find-ns 'clj-reload.keep-downstream) 'downstream-var)
        upstream'   @(ns-resolve (find-ns 'clj-reload.keep-upstream)   'upstream-var)]
    (is (= downstream downstream'))
    (is (= upstream upstream'))))
        
(deftest keep-dependent-broken-test
  (tu/init 'clj-reload.keep-downstream)
  (let [downstream  @(ns-resolve (find-ns 'clj-reload.keep-downstream) 'downstream-var)
        upstream    @(ns-resolve (find-ns 'clj-reload.keep-upstream)   'upstream-var)]
    (tu/with-changed 'clj-reload.keep-upstream "(ns clj-reload.keep-upstream)
                                                oops!"
      (is (thrown? Exception (tu/reload))))
    (tu/reload)
    (let [downstream' @(ns-resolve (find-ns 'clj-reload.keep-downstream) 'downstream-var)
          upstream'   @(ns-resolve (find-ns 'clj-reload.keep-upstream)   'upstream-var)]
      (is (= downstream downstream'))
      (is (= upstream upstream')))))

(deftest keep-changing-test
  (tu/init 'clj-reload.keep-upstream)
  (let [upstream @(ns-resolve (find-ns 'clj-reload.keep-upstream) 'upstream-var)]
    (tu/with-changed 'clj-reload.keep-upstream "(ns clj-reload.keep-upstream)"
      ;; remove defonce
      (tu/reload))
    ;; add defonce
    (tu/reload)
    (let [upstream' @(ns-resolve (find-ns 'clj-reload.keep-upstream) 'upstream-var)]
      (is (not= upstream upstream')))))
