(ns clj-reload.core-test
  (:require
    [clojure.java.io :as io]
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
  (doseq [ns '[l i j k f a g h d c e b]]
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

(defmacro with-changed [sym content' & body]
  `(let [sym#     ~sym
         file#    (io/file "fixtures" (str ~sym ".clj"))
         content# (slurp file#)]
     (try
       (spit file# ~content')
       (touch sym#)
       ~@body
       (finally
         (spit file# content#)
         (touch sym#)))))

(defn log-fn [op ns]
  (swap! *trace
    (fn [track]
      (let [last-op (->> track (filter keyword?) last)]
        (cond-> track
          (not= op last-op) (conj op)
          true              (conj ns))))))

(defn reload []
  (binding [reload/*log-fn* log-fn
            reload/*stable?* true]
    (reload/reload)))

(defn modify [& syms]
  (let [[opts syms] (if (map? (first syms))
                      [(first syms) (next syms)]
                      [nil syms])
        opts        (merge {:dirs ["fixtures"]}
                      opts)]
    (try
      (reset)
      (doeach require (:require opts '[b e c d h g a f k j i l]))
      (reload/init opts)
      (doeach touch syms)
      (reload)
      @*trace)))

;    a     f     i  l 
;  / | \ /   \   |    
; b  c  d  h  g  j    
;     \ | /      |    
;       e        k    

(deftest reload-test
  (is (= '[:unload a :load a] (modify 'a)))
  (is (= '[:unload a b :load b a] (modify 'b)))
  (is (= '[:unload a c :load c a] (modify 'c)))
  (is (= '[:unload f a d :load d a f] (modify 'd)))
  (is (= '[:unload h f a d c e :load e c d a f h] (modify 'e)))
  (is (= '[:unload f :load f] (modify 'f)))
  (is (= '[:unload f g :load g f] (modify 'g)))
  (is (= '[:unload i :load i] (modify 'i)))
  (is (= '[:unload i j :load j i] (modify 'j)))
  (is (= '[:unload i j k :load k j i] (modify 'k)))
  (is (= '[:unload l :load l] (modify 'l)))
  (is (= '[] (modify)))
  (is (= '[:unload a c b :load b c a] (modify 'a 'b 'c)))
  (is (= '[:unload l i j k h f a d c e :load e c d a f h k j i l] (modify 'e 'k 'l)))
  (is (= '[:unload l i j k h f g a d c e b :load b e c d a g f h k j i l] (modify 'a 'b 'c 'd 'e 'f 'g 'h 'i 'j 'k 'l))))

(deftest reload-active
  (is (= '[:unload a d c e :load e c d a] (modify {:require '[a]} 'e)))
  (is (= '[:unload a d c e :load e c d a] (modify {:require '[a]} 'e 'h 'g 'f 'k))))

(deftest reload-broken
  (reset)
  (require 'a)
  (reload/init {:dirs ["fixtures"]})
  (with-changed 'c "(ns c (:require e)) (/ 1 0)"
    (touch 'e)
    (is (thrown? Exception (reload)))
    (is (= '[:unload a d c e :load e :load-fail c] @*trace)))
  (reset! *trace [])
  (reload)
  (is (= '[:unload c :load c d a] @*trace)))

(deftest exclude-test
  (is (= '[] (modify {:no-load ['k]} 'k)))
  (is (= '[:unload h f a d e :load e d a f h] (modify {:no-load ['c]} 'e)))
  (is (= '[:unload h f a d e :load e c d a f h] (modify {:no-unload ['c]} 'e))))

(comment
  (test/test-ns *ns*)
  (clojure.test/run-test-var #'reload-broken))
