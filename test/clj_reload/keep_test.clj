(ns clj-reload.keep-test
  (:require
    [clojure.string :as str]
    [clojure.test :refer [is are deftest testing use-fixtures]]
    [clj-reload.core :as reload]
    [clj-reload.core-test :as core-test]))

(defn wrap-test [f]
  (binding [core-test/*dir* "test"]
    (core-test/reset
      'clj-reload.keep-test.keep-custom
      'clj-reload.keep-test.keep-defprotocol
      'clj-reload.keep-test.keep-defrecord
      'clj-reload.keep-test.keep-deftype
      'clj-reload.keep-test.keep-normal)
    (f)))

(use-fixtures :each wrap-test)

(deftest patch-file-test
  (is (=  "before (def *atom 888)     after"
        (reload/patch-file
          "before (defonce *atom 777) after" {'*atom "(def *atom 888)"})))
  
  (is (=  "before (def *atom 1000000) after"
        (reload/patch-file
          "before (def *atom 1) after" {'*atom "(def *atom 1000000)"})))
  
  (is (=  "before (def *atom 888)
       after"
        (reload/patch-file
          "before (defonce *atom
  777) after" {'*atom "(def *atom 888)"})))
  
  (is (=  #ml "(ns keep)
               
                  asdas
               
               8 10  (def *atom 777)
                            
               
               (def just-var 888)
                           "
        (reload/patch-file
          #ml "(ns keep)
               
                  asdas
               
               8 10  (defonce *atom
                 (atom nil))
               
               (defonce just-var
                 (Object.))"
          {'*atom "(def *atom 777)"
           'just-var "(def just-var 888)"}))))

(defn meta= [a b]
  (= (dissoc (meta a) :ns) (dissoc (meta b) :ns)))

(deftest keep-vars-test
  (require 'clj-reload.keep-test.keep-normal)
  (core-test/init)
  (let [ns         (find-ns 'clj-reload.keep-test.keep-normal)
        normal     @(ns-resolve ns 'normal)
        atom       (reset! @(ns-resolve ns '*atom) 100500)
        just-var   @(ns-resolve ns 'just-var)
        dependent  @(ns-resolve ns 'dependent)
        meta-var   (ns-resolve ns 'meta-var)
        public-fn  (ns-resolve ns 'public-fn)
        private-fn (ns-resolve ns 'private-fn)
        normal-2   @(ns-resolve ns 'normal-2)
        
        _          (core-test/touch 'clj-reload.keep-test.keep-normal)
        _          (core-test/reload)
        ns'        (find-ns 'clj-reload.keep-test.keep-normal)]
    
    (is (not= normal @(ns-resolve ns' 'normal)))
    (is (= atom @@(ns-resolve ns' '*atom)))
    (is (= just-var @(ns-resolve ns' 'just-var)))
    (is (= (first dependent) (first @(ns-resolve ns' 'dependent))))
    (is (not= (second dependent) (second @(ns-resolve ns' 'dependent))))
    
    (is (= @meta-var @(ns-resolve ns' 'meta-var)))
    (is (meta= meta-var (ns-resolve ns' 'meta-var)))
    
    (is (= @public-fn) @(ns-resolve ns' 'public-fn))
    (is (meta= public-fn (ns-resolve ns' 'public-fn)))
    
    (is (= @private-fn) @(ns-resolve ns' 'private-fn))
    (is (meta= private-fn (ns-resolve ns' 'private-fn)))
    
    (is (not= normal-2 @(ns-resolve ns' 'normal-2)))))

(deftest keep-type-test
  (require 'clj-reload.keep-test.keep-deftype)
  (core-test/init)
  (let [ns             (find-ns 'clj-reload.keep-test.keep-deftype)
        normal-new     @(ns-resolve ns 'type-normal-new)
        normal-factory @(ns-resolve ns 'type-normal-factory)
        keep-new       @(ns-resolve ns 'type-keep-new)
        keep-factory   @(ns-resolve ns 'type-keep-factory)
        _              (core-test/touch 'clj-reload.keep-test.keep-deftype)
        _              (core-test/reload)
        ns'            (find-ns 'clj-reload.keep-test.keep-deftype)]
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
  (require 'clj-reload.keep-test.keep-defrecord)
  (core-test/init)
  (let [ns                 (find-ns 'clj-reload.keep-test.keep-defrecord)
        normal-new         @(ns-resolve ns 'record-normal-new)
        normal-factory     @(ns-resolve ns 'record-normal-factory)
        normal-map-factory @(ns-resolve ns 'record-normal-map-factory)
        keep-new           @(ns-resolve ns 'record-keep-new)
        keep-factory       @(ns-resolve ns 'record-keep-factory)
        keep-map-factory   @(ns-resolve ns 'record-keep-map-factory)
        _                  (core-test/touch 'clj-reload.keep-test.keep-defrecord)
        _                  (core-test/reload)
        ns'                (find-ns 'clj-reload.keep-test.keep-defrecord)]
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

(defmethod reload/keep-methods 'deftype+ [tag]
  (reload/keep-methods 'deftype))

(deftest keep-custom-def-test
  (require 'clj-reload.keep-test.keep-custom)
  (core-test/init)
  (let [ns    (find-ns 'clj-reload.keep-test.keep-custom)
        ctor  @(ns-resolve ns '->CustomTypeKeep)
        value @(ns-resolve ns 'custom-type-keep)
        _     (core-test/touch 'clj-reload.keep-test.keep-custom)
        _     (core-test/reload)
        ns'   (find-ns 'clj-reload.keep-test.keep-custom)]
    (is (identical? ctor @(ns-resolve ns' '->CustomTypeKeep)))
    (is (identical? (class value) (class @(ns-resolve ns' 'custom-type-keep))))))

(comment
  (clojure.test/test-ns *ns*)
  (clojure.test/run-test-var #'keep-vars-test))
