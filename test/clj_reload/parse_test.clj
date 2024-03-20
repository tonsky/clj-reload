(ns clj-reload.parse-test
  (:require
    [clj-reload.core :as reload]
    [clj-reload.parse :as parse]
    [clj-reload.util :as util]
    [clojure.java.io :as io]
    [clojure.test :refer [is deftest testing use-fixtures]])
  (:import
    [java.io PushbackReader StringReader StringWriter]))

(defn read-str [s]
  (parse/read-file (PushbackReader. (StringReader. s)) nil))

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
        (read-str "(ns x
                     (:require
                       a.b.c
                       [a.b.d]
                       [a.b.e :as e]
                       [a.b f g]
                       [a.b [h :as h]]
                       [a.b.x :as-alias x]
                       [a.b [y :as-alias y]])
                     (:require
                       a.b.i)
                     (:use
                       a.b.j))
                   ...
                   (defonce x 1)
                   ...
                   (require 'a.b.k)
                   (require '[a.b.z :as-alias z])
                   ...
                   ^:clj-reload/keep
                   (defprotocol y 2)
                   ...
                   (use 'a.b.l)")))
  
  (is (= '{x nil}
        (read-str "(ns x)")))
  
  (is (= '{x {:meta {:clj-reload/no-reload true}}}
        (read-str "(ns ^:clj-reload/no-reload x)")))
  
  (is (= '{x nil}
        (read-str "(in-ns 'x)"))))

(deftest read-file-errors-test
  (let [file "(ns x
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
         nses  :namespaces'} (binding [util/*log-fn* nil]
                               (@#'reload/scan-impl nil ["fixtures"] 0))]
    (testing "no-ns"
      (is (= '#{}
            (get-in files [(io/file "fixtures/core_test/no_ns.clj") :namespaces]))))
    
    (testing "two-nses"
      (is (= '#{two-nses two-nses-second}
            (get-in files [(io/file "fixtures/core_test/two_nses.clj") :namespaces])))
    
      (is (= '#{clojure.string}
            (get-in nses ['two-nses :requires])))
    
      (is (= '#{clojure.set}
            (get-in nses ['two-nses-second :requires]))))
    
    (testing "split"
      (is (= '#{split}
            (get-in files [(io/file "fixtures/core_test/split.clj") :namespaces])))
    
      (is (= '#{split}
            (get-in files [(io/file "fixtures/core_test/split_part.clj") :namespaces])))
    
      (is (= '#{clojure.string clojure.set}
            (get-in nses ['split :requires])))
    
      (is (= #{(io/file "fixtures/core_test/split.clj")}
            (get-in nses ['split :ns-files])))
    
      (is (= #{(io/file "fixtures/core_test/split_part.clj")}
            (get-in nses ['split :in-ns-files]))))

    (testing "double"    
      (is (= '#{double}
            (get-in files [(io/file "fixtures/core_test/double.clj") :namespaces])))
      
      (is (= '#{double}
            (get-in files [(io/file "fixtures/core_test/double_b.clj") :namespaces])))
          
      (is (= '#{clojure.string clojure.set}
            (get-in nses ['double :requires])))

      (is (= #{(io/file "fixtures/core_test/double.clj") (io/file "fixtures/core_test/double_b.clj")}
            (get-in nses ['double :ns-files]))))))

(defn gen-graph [n]
  (reduce
   (fn [acc [k v]]
     (assoc acc k #{v}))
   {}
   (partition 2 (interleave (range 0 n) (range 1 (inc n))))))

; topo-sort-test will use the following dep graph.
; Top ones require bottom ones.
;
;    a     f     i  l  m
;  ╱ │ ╲ ╱   ╲   │     │
; b  c  d  h  g  j     n
;     ╲ │ ╱      │     │
;       e        k     o

(def test-graph
  {:e #{:c :d :h}
   :k #{:j}
   :o #{:n}
   :b #{:a}
   :c #{:a}
   :d #{:a :f}
   :h #{}
   :g #{:f}
   :j #{:i}
   :n #{:m}
   :a #{}
   :f #{}
   :i #{}
   :l #{}
   :m #{}})

(deftest topo-sort-test
  (is (= [0 1 2 3 4 5 6 7 8 9]
         (parse/topo-sort' (gen-graph 10))
         (parse/topo-sort (gen-graph 10))))
  ; while these functions have different orderings
  ; the order of dependencies is still correct in this case.
  (is (= [:b :e :c :d :a :g :f :h :k :j :i :l :o :n :m]
         (parse/topo-sort test-graph)))
  (is (= [:o :e :l :k :g :b :n :c :j :h :d :m :f :i :a]
         (parse/topo-sort' test-graph))))

(comment
  ; to see performance differences on a simple graph where each node only has one edge..
  (do
    (time
      (parse/topo-sort' (gen-graph 1000)))
    (time
      (parse/topo-sort (gen-graph 1000)))
    nil)
  )
