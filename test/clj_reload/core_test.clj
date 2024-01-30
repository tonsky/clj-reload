(ns clj-reload.core-test
  (:require
    [clojure.java.io :as io]
    [clojure.test :refer [is are deftest testing]]
    [clj-reload.core :as clj-reload])
  (:import
    [java.io File PushbackReader StringReader StringWriter]))

(defn read-str [s]
  (clj-reload/read-file (PushbackReader. (StringReader. s))))

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


(defn modify [& syms]
  (let [now    (System/currentTimeMillis)
        _      (doseq [^File file (next (file-seq (io/file "fixtures")))]
                 (when (> (.lastModified file) now)
                   (.setLastModified file now)))
        state  #p (clj-reload/first-scan ["fixtures"])
        now    (+ now 2000)
        _      (doseq [sym syms
                       :let [file (io/file "fixtures" (str sym ".clj"))]]
                 (.setLastModified ^File file now))
        *track (atom [])
        state' (binding [clj-reload/*log-fn* #(swap! *track conj (vec %&))]
                 (clj-reload/reload #p (clj-reload/scan #p state)))]
    @*track))

;    a     f     i  l 
;  / | \ /   \   |    
; b  c  d  h  g  j    
;     \ | /      |    
;       e        k    

(deftest reload-test
  (is (= [[:unload 'a] [:load 'a]] (modify 'a)))
  (is (= [[:unload 'a] [:unload 'b] [:load 'b] [:load 'a]] (modify 'b))))

(comment
  (modify 'a 'b))

; (test/test-ns *ns*)
; (test/run-test-var #'var)
