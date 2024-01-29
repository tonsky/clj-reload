(ns clj-reload.core-test
  (:require
    [clj-reload.core :as clj-reload]
    [clojure.test :refer [is are deftest testing]])
  (:import
    [java.io PushbackReader StringReader StringWriter]))

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

; (test/test-ns *ns*)
; (test/run-test-var #'var)
