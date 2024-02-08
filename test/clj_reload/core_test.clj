(ns clj-reload.core-test
  (:require
    [clojure.java.io :as io]
    [clojure.string :as str]
    [clojure.test :refer [is are deftest testing]]
    [clj-reload.core :as reload])
  (:import
    [java.io File PushbackReader StringReader StringWriter]))

(defn read-str [s]
  (reload/read-file (PushbackReader. (StringReader. s))))

(deftest read-file-test
  (let [file "(ns x
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
              (require 'a.b.k)
              ...
              (use 'a.b.l)
              ...
              (ns y
                (:require
                  a.b.m))
              ...
              (in-ns 'z)
              ...
              (require 'a.b.n)"]
    (is (= '{x {:depends
                #{a.b.c
                  a.b.d
                  a.b.e
                  a.b.f
                  a.b.g
                  a.b.h
                  a.b.i
                  a.b.j
                  a.b.k
                  a.b.l}}
             y {:depends
                #{a.b.m}}
             z {:depends
                #{a.b.n}}}
          (read-str file)))))

(deftest read-file-errors-test
  (let [file "(ns x
                (:require 123)
                (:require [345])
                (:require [567 :as a])
                (:require [789 a b c]))"
        out  (StringWriter.)
        res  (binding [*out* out]
               (read-str file))]
    (is (= '{x {:depends #{}}} res))
    (is (= "Unexpected :require form: 123
Unexpected :require form: [345]
Unexpected :require form: [567 :as a]
Unexpected :require form: [789 a b c]
" (str out)))))

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
  (doseq [ns '[o n m l i j k f a g h d c e b]]
    (when (@@#'clojure.core/*loaded-libs* ns)
      (remove-ns ns)
      (dosync
        (alter @#'clojure.core/*loaded-libs* disj ns)))))

(defn touch [sym]
  (let [now  (swap! *time + 1000)
        file (io/file "fixtures" (str sym ".clj"))]
    (.setLastModified ^File file now)))

(defn doeach [f xs]
  (doseq [x xs]
    (f x)))

(defn ^File sym->file [sym]
  (io/file "fixtures" (str (str/replace (name sym) "-" "_") ".clj")))

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

(defn log-fn [op ns & _]
  (swap! *trace
    (fn [track]
      (let [last-op (->> track (filter keyword?) last)]
        (cond-> track
          (not= op last-op) (conj op)
          true              (conj ns))))))

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
   (reload []))
  ([opts]
   (binding [reload/*log-fn* log-fn
             reload/*stable?* true]
     (reload/reload opts))))

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
    (is (= '[:unload a :load a] (modify opts 'a)))
    (is (= '[:unload a b :load b a] (modify opts 'b)))
    (is (= '[:unload a c :load c a] (modify opts 'c)))
    (is (= '[:unload f a d :load d a f] (modify opts 'd)))
    (is (= '[:unload h f a d c e :load e c d a f h] (modify opts 'e)))
    (is (= '[:unload f :load f] (modify opts 'f)))
    (is (= '[:unload f g :load g f] (modify opts 'g)))
    (is (= '[:unload i :load i] (modify opts 'i)))
    (is (= '[:unload i j :load j i] (modify opts 'j)))
    (is (= '[:unload i j k :load k j i] (modify opts 'k)))
    (is (= '[:unload l :load l] (modify opts 'l)))
    (is (= '[] (modify opts)))
    (is (= '[:unload a c b :load b c a] (modify opts 'a 'b 'c)))
    (is (= '[:unload l i j k h f a d c e :load e c d a f h k j i l] (modify opts 'e 'k 'l)))
    (is (= '[:unload l i j k h f g a d c e b :load b e c d a g f h k j i l] (modify opts 'a 'b 'c 'd 'e 'f 'g 'h 'i 'j 'k 'l)))))

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
  (is (= '[:unload a d c e :load e c d a] (modify {:require '[a]} 'e)))
  (is (= '[:unload a d c e :load e c d a] (modify {:require '[a]} 'e 'h 'g 'f 'k))))

(deftest exclude-test
  (let [opts {:require '[b e c d h g a f k j i l]}]
    (is (= '[] (modify (assoc opts :no-reload ['k]) 'k)))
    (is (= '[:unload h f a d e :load e d a f h] (modify (assoc opts :no-reload ['c]) 'e)))
    (is (= '[:unload h f a d e :load e c d a f h] (modify (assoc opts :no-unload ['c]) 'e)))))

(deftest reload-loaded-test
  (is (= '[:unload a d c e b :load b e c d a] (modify {:require '[a] :only :loaded})))
  (is (= '[:unload f g a d c e b :load b e c d a g f] (modify {:require '[a f] :only :loaded})))
  (is (= '[:unload h f g a d c e b :load b e c d a g f h] (modify {:require '[a f h] :only :loaded}))))

(deftest reload-all-test
  (with-deleted 'err-runtime
    (is (= '[:unload m n o l i j k h f g a d c e b
             :load b e c d a g f h k j i l o n m]
          (modify {:require '[] :only :all})))))

(deftest reload-broken-test
  (doseq [[name body trace1 trace2]
          [["exception"
            "(ns c (:require e)) (/ 1 0)"
            '[:unload a d c e :load e :load-fail c]
            '[:unload c :load c d a]]
           
           ["unknown dep"
            "(ns c (:require e z))"
            '[:unload a d c e :load e d :load-fail c]
            '[:load c a]]
           
           ["ill-formed"
            "(ns c (:require e"
            '[:unload a d c e :load e d :load-fail a]
            '[:load a]]]]
    (testing name
      (reset)
      (require 'a)
      (init)
      (with-changed 'c body
        (touch 'e)
        (is (thrown? Exception (reload)))
        (is (= trace1 @*trace)))
      (reset! *trace [])
      (reload)
      (is (= trace2 @*trace)))))

(deftest reload-changed-test
  (reset)
  (require 'i)
  (init)
  (with-changed 'i "(ns i)"
    (with-changed 'j "(ns j (:require i))"
      (with-changed 'k "(ns k (:require j))"
        (reload)
        (is (= '[:unload i j k :load i j k] @*trace)))))
  (reset! *trace [])
  (reload)
  (is (= '[:unload k j i :load k j i] @*trace)))

(deftest reload-deleted-test
  (reset)
  (require 'l)
  (init)
  (with-deleted 'l
    (reload)
    (is (= '[:unload l] @*trace))))

(deftest reload-deleted-2-test
  (reset)
  (require 'i)
  (init)
  (with-changed 'j "(ns j)"
    (with-deleted 'k
      (reload)
      (is (= '[:unload i j k :load j i] @*trace))))
  (reset! *trace [])
  (reload)
  (is (= '[:unload i j :load j i] @*trace)))

(deftest cycle-self-test
  (reset)
  (require 'l)
  (init)
  (with-changed 'l "(ns l (:require l))"
    (is (thrown-with-msg? Exception #"Cycle detected: l" (reload)))
    (is (= '[] @*trace)))
  (reset! *trace [])
  (reload)
  (is (= '[:unload l :load l] @*trace)))

(deftest cycle-one-hop-test
  (reset)
  (require 'i)
  (init)
  (with-changed 'j "(ns j (:require i))"
    (is (thrown-with-msg? Exception #"Cycle detected: i, j" (reload)))
    (is (= '[] @*trace)))
  (reset! *trace [])
  (reload)
  (is (= '[:unload i j :load j i] @*trace)))

(deftest cycle-two-hops-test
  (reset)
  (require 'i)
  (init)
  (with-changed 'k "(ns k (:require i))"
    (is (thrown-with-msg? Exception #"Cycle detected: i, j, k" (reload)))
    (is (= '[] @*trace)))
  (reset! *trace [])
  (reload)
  (is (= '[:unload i j k :load k j i] @*trace)))

(deftest cycle-extra-nodes-test
  (reset)
  (require 'a 'f 'h)
  (init)
  (with-changed 'e "(ns e (:require h))"
    (is (thrown-with-msg? Exception #"Cycle detected: e, h" (reload)))
    (is (= '[] @*trace)))
  (reset! *trace [])
  (reload)
  (is (= '[:unload h f a d c e :load e c d a f h] @*trace)))

(deftest hooks-test
  (reset)
  (is (= '[:unload m n :load n m] (modify {:require '[o n m]} 'n)))
  (is (= [:unload-m :unload-n :reload-n :reload-m] @@(resolve 'o/*atom)))
  
  (reset)
  (is (= '[:unload m :load m] (modify {:require '[o n m]} 'm)))
  (is (= [:unload-m :reload-m] @@(resolve 'o/*atom))))

(deftest unload-hook-fail
  (reset)
  (with-changed 'm "(ns m (:require n o))
                    (defn before-ns-unload []
                      (/ 1 0))"
    (require 'm)
    (init)
    (touch 'm)
    (reload)
    (is (= '[:unload m :load m] @*trace)))
  (reset! *trace [])
  (reload)
  (is (= '[:unload m :load m] @*trace)))

(deftest reload-hook-fail
  (reset)
  (require 'm)
  (init)
  (with-changed 'n "(ns n (:require o))
                    (defn after-ns-reload []
                      (/ 1 0))"
    (touch 'o)
    (is (thrown? Exception (reload)))
    (is (= '[:unload m n o :load o :load-fail n] @*trace)))
  (reset! *trace [])
  (reload)
  (is (= '[:unload n :load n m] @*trace)))

(comment
  (test/test-ns *ns*)
  (clojure.test/run-test-var #'reload-deleted))
