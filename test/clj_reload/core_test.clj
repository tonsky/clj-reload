(ns clj-reload.core-test
  (:require
    [clojure.java.io :as io]
    [clojure.string :as str]
    [clojure.test :refer [is are deftest testing]]
    [clj-reload.core :as reload])
  (:import
    [java.io File PushbackReader StringReader StringWriter]))

(defn read-str [s]
  (reload/read-file (PushbackReader. (StringReader. s)) nil))

(deftest read-file-test
  (is (= '{x {:requires
              #{a.b.c
                a.b.d
                a.b.e
                a.b.f
                a.b.g
                a.b.h
                a.b.i
                a.b.j
                a.b.k
                a.b.l}
              :keep {x {:tag defonce
                        :form (defonce x 1)}
                     y {:tag defprotocol
                        :form (defprotocol y 2)}}}}
        (read-str #ml "(ns x
                         (:require
                           a.b.c
                           [a.b.d]
                           [a.b.e :as e]
                           [a.b f g]
                           [a.b [h :as h]])
                         (:require
                           a.b.i)
                         (:use
                           a.b.j))
                       ...
                       (defonce x 1)
                       ...
                       (require 'a.b.k)
                       ...
                       ^:clj-reload.core/keep
                       (defprotocol y 2)
                       ...
                       (use 'a.b.l)")))
  
  (is (= '{x nil}
        (read-str "(ns x)")))
  
  (is (= '{x nil}
        (read-str "(in-ns 'x)"))))

(deftest read-file-errors-test
  (let [file #ml "(ns x
                    (:require 123)
                    (:require [345])
                    (:require [567 :as a])
                    (:require [789 a b c]))"
        out  (StringWriter.)
        res  (binding [*out* out]
               (read-str file))]
    (is (= '{x nil} res))
    (is (= "Unexpected :require form: 123
Unexpected :require form: [345]
Unexpected :require form: [567 :as a]
Unexpected :require form: [789 a b c]
" (str out)))))

(deftest scan-impl-test
  (let [{files :files'
         nses  :namespaces'} (binding [reload/*log-fn* nil]
                               (@#'reload/scan-impl nil ["fixtures"] 0))]
    (is (= '#{}
          (get-in files [(io/file "fixtures/no_ns.clj") :namespaces])))
    
    (is (= '#{two-nses two-nses-second}
          (get-in files [(io/file "fixtures/two_nses.clj") :namespaces])))
    
    (is (= '#{split}
          (get-in files [(io/file "fixtures/split.clj") :namespaces])))
    
    (is (= '#{split}
          (get-in files [(io/file "fixtures/split_part.clj") :namespaces])))
        
    (is (= '#{clojure.string}
          (get-in nses ['two-nses :requires])))
    
    (is (= '#{clojure.set}
          (get-in nses ['two-nses-second :requires])))
    
    (is (= '#{clojure.string clojure.set}
          (get-in nses ['split :requires])))
    
    (is (= (io/file "fixtures/split.clj")
          (get-in nses ['split :main-file])))))

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

(def *trace
  (atom []))

(def *time
  (atom (System/currentTimeMillis)))

(defn reset []
  (reset! *trace [])
  (let [now (System/currentTimeMillis)]
    (reset! *time now)
    (doseq [^File file (next (file-seq (io/file "fixtures")))
            :when (> (.lastModified file) now)]
      (.setLastModified file now)))
  (doseq [ns '[two-nses two-nses-second split keep o n m l i j k f a g h d c e b]]
    (when (@@#'clojure.core/*loaded-libs* ns)
      (remove-ns ns)
      (dosync
        (alter @#'clojure.core/*loaded-libs* disj ns)))))

(defn touch [sym]
  (let [now  (swap! *time + 1000)
        file (io/file (str "fixtures/" sym ".clj"))]
    (.setLastModified ^File file now)))

(defn doeach [f xs]
  (doseq [x xs]
    (f x)))

(defn ^File sym->file [sym]
  (io/file (str "fixtures/" (str/replace (name sym) "-" "_") ".clj")))

(defmacro with-changed [sym content' & body]
  `(let [sym#     ~sym
         file#    (sym->file sym#)
         content# (slurp file#)]
     (try
       (spit file# ~content')
       (touch sym#)
       ~@body
       (finally
         (spit file# content#)
         (touch sym#)))))

(defmacro with-deleted [sym & body]
  `(let [sym#     ~sym
         file#    (sym->file sym#)
         content# (slurp file#)]
     (try
       (.delete file#)
       ~@body
       (finally
         (spit file# content#)
         (touch sym#)))))

(defn log-fn [type arg & _]
  (swap! *trace
    (fn [track]
      (let [last-type (->> track (filter string?) last)]
        (cond
          (= "fixtures/err_parse.clj" arg)          track
          (= "  exception during unload hook" type) (conj track type (.getName (class arg)))
          (not= type last-type)                     (conj track type arg)
          :else                                     (conj track arg))))))

(defn init
  ([]
   (init nil))
  ([opts]
   (reload/init
     (merge
       {:dirs ["fixtures"]}
       opts))))

(defn reload
  ([]
   (reload nil))
  ([opts]
   (reload/reload
     (merge {:log-fn log-fn} opts))))

(defn modify [& syms]
  (let [[opts syms] (if (map? (first syms))
                      [(first syms) (next syms)]
                      [nil syms])]
    (try
      (reset)
      (doeach require (:require opts))
      (init opts)
      (doeach touch syms)
      (reload opts)
      @*trace)))

;    a     f     i  l  m
;  / | \ /   \   |     |
; b  c  d  h  g  j     n
;     \ | /      |     |
;       e        k     o

(deftest reload-test
  (let [opts {:require '[b e c d h g a f k j i l]}]
    (is (= '["Unloading" a "Loading" a] (modify opts 'a)))
    (is (= '["Unloading" a b "Loading" b a] (modify opts 'b)))
    (is (= '["Unloading" a c "Loading" c a] (modify opts 'c)))
    (is (= '["Unloading" f a d "Loading" d a f] (modify opts 'd)))
    (is (= '["Unloading" h f a d c e "Loading" e c d a f h] (modify opts 'e)))
    (is (= '["Unloading" f "Loading" f] (modify opts 'f)))
    (is (= '["Unloading" f g "Loading" g f] (modify opts 'g)))
    (is (= '["Unloading" i "Loading" i] (modify opts 'i)))
    (is (= '["Unloading" i j "Loading" j i] (modify opts 'j)))
    (is (= '["Unloading" i j k "Loading" k j i] (modify opts 'k)))
    (is (= '["Unloading" l "Loading" l] (modify opts 'l)))
    (is (= '[] (modify opts)))
    (is (= '["Unloading" a c b "Loading" b c a] (modify opts 'a 'b 'c)))
    (is (= '["Unloading" l i j k h f a d c e "Loading" e c d a f h k j i l] (modify opts 'e 'k 'l)))
    (is (= '["Unloading" l i j k h f g a d c e b "Loading" b e c d a g f h k j i l] (modify opts 'a 'b 'c 'd 'e 'f 'g 'h 'i 'j 'k 'l)))))

(deftest return-value-ok-test
  (reset)
  (require 'a 'f 'h)
  (init)
  (is (= {:unloaded '[]
          :loaded   '[]} (reload)))
  (touch 'e)
  (is (= {:unloaded '[h f a d c e]
          :loaded   '[e c d a f h]} (reload))))

(deftest return-value-fail-test
  (reset)
  (require 'a 'f 'h)
  (init)
  (with-changed 'c "(ns c (:require e)) (/ 1 0)"
    (touch 'e)
    (try
      (reload)
      (is (= "Should throw" "Didn't throw"))
      (catch Exception e
        (is (= {:unloaded '[h f a d c e]
                :loaded   '[e]
                :failed   'c} (ex-data e))))))
  (is (= {:unloaded '[c]
          :loaded   '[c d a f h]} (reload))))

(deftest return-value-fail-safe-test
  (reset)
  (require 'a 'f 'h)
  (init)
  (with-changed 'c "(ns c (:require e)) (/ 1 0)"
    (touch 'e)
    (let [res (reload {:throw false})]
      (is (= {:unloaded '[h f a d c e]
              :loaded   '[e]
              :failed   'c} (dissoc res :exception)))))
  (is (= {:unloaded '[c]
          :loaded   '[c d a f h]} (reload))))

(deftest reload-active-test
  (is (= '["Unloading" a d c e "Loading" e c d a] (modify {:require '[a]} 'e)))
  (is (= '["Unloading" a d c e "Loading" e c d a] (modify {:require '[a]} 'e 'h 'g 'f 'k))))

(deftest reload-split-test
  (reset)
  (require 'split)
  (init)
  (is (= 1 @(resolve 'split/split-part)))
  (with-changed 'split-part "(in-ns 'split) (def split-part 2)"
    (reload)
    (is (= '["Unloading" split "Loading" split] @*trace))
    (is (= 2 @(resolve 'split/split-part)))))

(deftest exclude-test
  (let [opts {:require '[b e c d h g a f k j i l]}]
    (is (= '[] (modify (assoc opts :no-reload ['k]) 'k)))
    (is (= '["Unloading" h f a d e "Loading" e d a f h] (modify (assoc opts :no-reload ['c]) 'e)))
    (is (= '["Unloading" h f a d e "Loading" e c d a f h] (modify (assoc opts :no-unload ['c]) 'e)))))

(deftest reload-loaded-test
  (is (= '["Unloading" a d c e b "Loading" b e c d a] (modify {:require '[a] :only :loaded})))
  (is (= '["Unloading" f g a d c e b "Loading" b e c d a g f] (modify {:require '[a f] :only :loaded})))
  (is (= '["Unloading" h f g a d c e b "Loading" b e c d a g f h] (modify {:require '[a f h] :only :loaded}))))

(deftest reload-all-test
  (with-deleted 'err-runtime
    (with-deleted 'two-nses
      (is (= '["Unloading" split m n o l keep i j k h f g a d c e b
               "Loading" b e c d a g f h k j i keep l o n m split]
            (modify {:require '[] :only :all}))))))

(deftest reload-exception-test
  (reset)
  (require 'a)
  (init)
  (with-changed 'c "(ns c (:require e)) (/ 1 0)"
    (touch 'e)
    (is (thrown? Exception (reload)))
    (is (= '["Unloading" a d c e "Loading" e c "  failed to load" c] @*trace))
    (reset! *trace [])
    (is (thrown? Exception (reload)))
    (is (= '["Unloading" c "Loading" c "  failed to load" c] @*trace)))
  (reset! *trace [])
  (reload)
  (is (= '["Unloading" c "Loading" c d a] @*trace)))

(deftest reload-unknown-dep-test
  (reset)
  (require 'a)
  (init)
  (with-changed 'c "(ns c (:require e z))"
    (touch 'e)
    (is (thrown? Exception (reload)))
    (is (= '["Unloading" a d c e "Loading" e d c "  failed to load" c] @*trace)))
  (reset! *trace [])
  (reload)
  (is (= '["Unloading" c "Loading" c a] @*trace)))

(deftest reload-ill-formed-test
  (reset)
  (require 'a)
  (init)
  (with-changed 'c "(ns c (:require e"
    (touch 'e)
    (is (thrown? Exception (reload)))
    (is (= '["Failed to read" "fixtures/c.clj"] @*trace)))
  (reset! *trace [])
  (reload)
  (is (= '["Unloading" a d c e "Loading" e c d a] @*trace)))

(deftest reload-changed-test
  (reset)
  (require 'i)
  (init)
  (with-changed 'i "(ns i)"
    (with-changed 'j "(ns j (:require i))"
      (with-changed 'k "(ns k (:require j))"
        (reload)
        (is (= '["Unloading" i j k "Loading" i j k] @*trace)))))
  (reset! *trace [])
  (reload)
  (is (= '["Unloading" k j i "Loading" k j i] @*trace)))

(deftest reload-deleted-test
  (reset)
  (require 'l)
  (init)
  (with-deleted 'l
    (reload)
    (is (= '["Unloading" l] @*trace))))

(deftest reload-deleted-2-test
  (reset)
  (require 'i)
  (init)
  (with-changed 'j "(ns j)"
    (with-deleted 'k
      (reload)
      (is (= '["Unloading" i j k "Loading" j i] @*trace))))
  (reset! *trace [])
  (reload)
  (is (= '["Unloading" i j "Loading" j i] @*trace)))

(deftest reload-rename-ns
  (reset)
  (require 'i)
  (init)
  (with-changed 'i "(ns z)"
    (touch 'k)
    (reload)
    (is (= '["Unloading" i j k "Loading" k j] @*trace)))
  (reset! *trace [])
  (reload)
  (is (= '[] @*trace)))

(deftest reload-remove-ns
  (reset)
  (require 'i)
  (init)
  (with-changed 'i ""
    (touch 'k)
    (reload)
    (is (= '["Unloading" i j k "Loading" k j] @*trace)))
  (reset! *trace [])
  (reload)
  (is (= '[] @*trace)))

(deftest cycle-self-test
  (reset)
  (require 'l)
  (init)
  (with-changed 'l "(ns l (:require l))"
    (is (thrown-with-msg? Exception #"Cycle detected: l" (reload)))
    (is (= '[] @*trace)))
  (reset! *trace [])
  (reload)
  (is (= '["Unloading" l "Loading" l] @*trace)))

(deftest cycle-one-hop-test
  (reset)
  (require 'i)
  (init)
  (with-changed 'j "(ns j (:require i))"
    (is (thrown-with-msg? Exception #"Cycle detected: i, j" (reload)))
    (is (= '[] @*trace)))
  (reset! *trace [])
  (reload)
  (is (= '["Unloading" i j "Loading" j i] @*trace)))

(deftest cycle-two-hops-test
  (reset)
  (require 'i)
  (init)
  (with-changed 'k "(ns k (:require i))"
    (is (thrown-with-msg? Exception #"Cycle detected: i, j, k" (reload)))
    (is (= '[] @*trace)))
  (reset! *trace [])
  (reload)
  (is (= '["Unloading" i j k "Loading" k j i] @*trace)))

(deftest cycle-extra-nodes-test
  (reset)
  (require 'a 'f 'h)
  (init)
  (with-changed 'e "(ns e (:require h))"
    (is (thrown-with-msg? Exception #"Cycle detected: e, h" (reload)))
    (is (= '[] @*trace)))
  (reset! *trace [])
  (reload)
  (is (= '["Unloading" h f a d c e "Loading" e c d a f h] @*trace)))

(deftest hooks-test
  (reset)
  (is (= '["Unloading" m n "Loading" n m] (modify {:require '[o n m]} 'n)))
  (is (= [:unload-m :unload-n :reload-n :reload-m] @@(resolve 'o/*atom)))
  
  (reset)
  (is (= '["Unloading" m "Loading" m] (modify {:require '[o n m]} 'm)))
  (is (= [:unload-m :reload-m] @@(resolve 'o/*atom))))

(deftest unload-hook-fail-test
  (reset)
  (with-changed 'm "(ns m (:require n o))
                    (defn before-ns-unload []
                      (/ 1 0))"
    (require 'm)
    (init)
    (touch 'm)
    (reload)
    (is (= '["Unloading" m "  exception during unload hook" "java.lang.ArithmeticException" "Loading" m] @*trace)))
  (reset! *trace [])
  (reload)
  (is (= '["Unloading" m "  exception during unload hook" "java.lang.ArithmeticException" "Loading" m] @*trace)))

(deftest reload-hook-fail-test
  (reset)
  (require 'm)
  (init)
  (with-changed 'n "(ns n (:require o))
                    (defn after-ns-reload []
                      (/ 1 0))"
    (touch 'o)
    (is (thrown? Exception (reload)))
    (is (= '["Unloading" m n o "Loading" o n "  failed to load" n] @*trace)))
  (reset! *trace [])
  (reload)
  (is (= '["Unloading" n "Loading" n m] @*trace)))

(deftest keep-vars-test
  (reset)
  (require 'keep)
  (init)
  (let [normal    @(resolve 'keep/normal)
        atom      (reset! @(resolve 'keep/*atom) 100500)
        just-var  @(resolve 'keep/just-var)        
        dependent @(resolve 'keep/dependent)
        meta-var  @(resolve 'keep/meta-var)
        normal-2  @(resolve 'keep/normal-2)]
    (touch 'keep)
    (reload)
    (is (not= normal @(resolve 'keep/normal)))
    (is (= atom @@(resolve 'keep/*atom)))
    (is (= just-var @(resolve 'keep/just-var)))
    (is (= (first dependent) (first @(resolve 'keep/dependent))))
    (is (not= (second dependent) (second @(resolve 'keep/dependent))))
    (is (= meta-var @(resolve 'keep/meta-var)))
    (is (not= normal-2 @(resolve 'keep/normal-2)))))

(deftest keep-type-test
  (reset)
  (require 'keep)
  (init)
  (let [normal-new     @(resolve 'keep/type-normal-new)
        normal-factory @(resolve 'keep/type-normal-factory)
        keep-new       @(resolve 'keep/type-keep-new)
        keep-factory   @(resolve 'keep/type-keep-factory)]
    (touch 'keep)
    (reload)
    (is (not= normal-new @(resolve 'keep/type-normal-new)))
    (is (not (identical? (class normal-new) (class @(resolve 'keep/type-normal-new)))))
    
    (is (not= normal-factory @(resolve 'keep/type-normal-factory)))
    (is (not (identical? (class normal-factory) (class @(resolve 'keep/type-normal-factory)))))
    
    (is (not (identical? keep-new @(resolve 'keep/type-keep-new))))
    (is (= keep-new @(resolve 'keep/type-keep-new)))
    (is (identical? (class keep-new) (class @(resolve 'keep/type-keep-new))))

    (is (not (identical? keep-factory @(resolve 'keep/type-keep-factory))))
    (is (= keep-factory @(resolve 'keep/type-keep-factory)))
    (is (identical? (class keep-factory) (class @(resolve 'keep/type-keep-factory))))))

(deftest keep-record-test
  (reset)
  (require 'keep)
  (init)
  (let [normal-new         @(resolve 'keep/record-normal-new)
        normal-factory     @(resolve 'keep/record-normal-factory)
        normal-map-factory @(resolve 'keep/record-normal-map-factory)
        keep-new           @(resolve 'keep/record-keep-new)
        keep-factory       @(resolve 'keep/record-keep-factory)
        keep-map-factory   @(resolve 'keep/record-keep-map-factory)]
    (touch 'keep)
    (reload)
    (is (not= normal-new @(resolve 'keep/record-normal-new)))
    (is (not (identical? (class normal-new) (class @(resolve 'keep/record-normal-new)))))
    
    (is (not= normal-factory @(resolve 'keep/record-normal-factory)))
    (is (not (identical? (class normal-factory) (class @(resolve 'keep/record-normal-factory)))))
    
    (is (not= normal-map-factory @(resolve 'keep/record-normal-map-factory)))
    (is (not (identical? (class normal-map-factory) (class @(resolve 'keep/record-normal-map-factory)))))
    
    (is (not (identical? keep-new @(resolve 'keep/record-keep-new))))
    (is (= keep-new @(resolve 'keep/record-keep-new)))
    (is (identical? (class keep-new) (class @(resolve 'keep/record-keep-new))))

    (is (not (identical? keep-factory @(resolve 'keep/record-keep-factory))))
    (is (= keep-factory @(resolve 'keep/record-keep-factory)))
    (is (identical? (class keep-factory) (class @(resolve 'keep/record-keep-factory))))
    
    (is (not (identical? keep-map-factory @(resolve 'keep/record-keep-map-factory))))
    (is (= keep-map-factory @(resolve 'keep/record-keep-map-factory)))
    (is (identical? (class keep-map-factory) (class @(resolve 'keep/record-keep-map-factory))))))

(defmethod reload/keep-methods 'deftype+ [tag]
  (reload/keep-methods 'deftype))

(deftest keep-custom-def-test
  (reset)
  (require 'keep)
  (init)
  (let [ctor  @(resolve 'keep/->CustomTypeKeep)
        value @(resolve 'keep/custom-type-keep)]
    (touch 'keep)
    (reload)
    (is (identical? ctor @(resolve 'keep/->CustomTypeKeep)))
    (is (identical? (class value) (class @(resolve 'keep/custom-type-keep))))))

(comment
  (test/test-ns *ns*)
  (clojure.test/run-test-var #'keep-type-test))
