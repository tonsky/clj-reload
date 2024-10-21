(ns clj-reload.core-test
  (:require
    [clj-reload.core :as reload]
    [clj-reload.test-util :as tu]
    [clj-reload.util :as util]
    [clojure.string :as str]
    [clojure.test :refer [is are deftest testing use-fixtures]]))

(defn reset []
  (tu/reset '[two-nses-second two-nses split o n no-unload m l i j k f a g h d c e double b]))

(defn wrap-test [f]
  (binding [tu/*dir* "fixtures/core_test"]
    (reset)
    (f)))

(use-fixtures :each wrap-test)

(defn modify [& syms]
  (let [[opts syms] (if (map? (first syms))
                      [(first syms) (next syms)]
                      [nil syms])]
    (reset)
    (util/doeach require (:require opts))
    (tu/init opts)
    (util/doeach tu/touch syms)
    (tu/reload opts)
    (tu/trace)))

; Fixture namespaces dependency plan
; Top ones require bottom ones
; 
;    a     f     i  l  m
;  ╱ │ ╲ ╱   ╲   │     │
; b  c  d  h  g  j     n
;     ╲ │ ╱      │     │
;       e        k     o

(deftest find-namespaces-test
  (is (= '#{a b c d double e err-runtime f g h i j k l m n no-reload no-unload o split two-nses two-nses-second} (reload/find-namespaces)))
  (is (= '#{a b c d e f g h i j k l m n o} (reload/find-namespaces #"\w")))
  (is (= '#{a b c} (reload/find-namespaces #"[abc]"))))

(deftest reload-test
  (let [opts {:require '[b e c d h g a f k j i l]}]
    (is (= '["Unloading" a "Loading" a] (modify opts 'a)))
    (is (= '["Unloading" a b "Loading" b a] (modify opts 'b)))
    (is (= '["Unloading" a c "Loading" c a] (modify opts 'c)))
    (is (= '["Unloading" f a d "Loading" d a f] (modify opts 'd)))
    (is (= '["Unloading" f a h d c e "Loading" e c d h a f] (modify opts 'e)))
    (is (= '["Unloading" f "Loading" f] (modify opts 'f)))
    (is (= '["Unloading" f g "Loading" g f] (modify opts 'g)))
    (is (= '["Unloading" i "Loading" i] (modify opts 'i)))
    (is (= '["Unloading" i j "Loading" j i] (modify opts 'j)))
    (is (= '["Unloading" i j k "Loading" k j i] (modify opts 'k)))
    (is (= '["Unloading" l "Loading" l] (modify opts 'l)))
    (is (= '[] (modify opts)))
    (is (= '["Unloading" a c b "Loading" b c a] (modify opts 'a 'b 'c)))
    (is (= '["Unloading" i f a j h d c l k e "Loading" e k l c d h j a f i] (modify opts 'e 'k 'l)))
    (is (= '["Unloading" i f a j h d c l k g e b "Loading" b e g k l c d h j a f i] (modify opts 'a 'b 'c 'd 'e 'f 'g 'h 'i 'j 'k 'l)))))

(deftest reload-all-regex-test
  (let [*reloads (atom [])]
    (apply tu/init '[b e c d h g a f k j i l])
    (binding [util/*log-fn* (fn [& [x :as log-line]]
                              (when (#{"Loading" "Unloading" } x)
                                (swap! *reloads conj log-line)))]
      (reload/reload-all #"^e$")
      (is (= '[("Unloading" f)
               ("Unloading" a)
               ("Unloading" h)
               ("Unloading" d)
               ("Unloading" c)
               ("Unloading" e)
               ("Loading" e)
               ("Loading" c)
               ("Loading" d)
               ("Loading" h)
               ("Loading" a)
               ("Loading" f)]

             @*reloads)))))

(deftest return-value-ok-test
  (tu/init 'a 'f 'h)
  (is (= {:unloaded '[]
          :loaded   '[]} (tu/reload)))
  (tu/touch 'e)
  (is (= {:unloaded '[f a h d c e]
          :loaded   '[e c d h a f]} (tu/reload))))

(deftest return-value-fail-test
  (tu/init 'a 'f 'h)
  (tu/with-changed 'c "(ns c (:require e)) (/ 1 0)"
    (tu/touch 'e)
    (try
      (tu/reload)
      (is (= "Should throw" "Didn't throw"))
      (catch Exception e
        (is (= {:unloaded '[f a h d c e]
                :loaded   '[e]
                :failed   'c} (ex-data e))))))
  (is (= {:unloaded '[c]
          :loaded   '[c d h a f]} (tu/reload))))

(deftest return-value-fail-safe-test
  (tu/init 'a 'f 'h)
  (tu/with-changed 'c "(ns c (:require e)) (/ 1 0)"
    (tu/touch 'e)
    (let [res (tu/reload {:throw false})]
      (is (= {:unloaded '[f a h d c e]
              :loaded   '[e]
              :failed   'c} (dissoc res :exception)))))
  (is (= {:unloaded '[c]
          :loaded   '[c d h a f]} (tu/reload))))

(deftest reload-active-test
  (is (= '["Unloading" a d c e "Loading" e c d a] (modify {:require '[a]} 'e)))
  (is (= '["Unloading" a d c e "Loading" e c d a] (modify {:require '[a]} 'e 'h 'g 'f 'k))))

(deftest unload-test
  (tu/init 'a 'f 'h)
  (tu/touch 'e)
  (tu/unload)
  (is (= '["Unloading" f a h d c e] (tu/trace)))
  (tu/unload)
  (is (= '[] (tu/trace)))
  (tu/reload)
  (is (= '["Loading" e c d h a f] (tu/trace))))

(deftest reload-split-test
  (tu/init 'split)
  (is (= 1 @(resolve 'split/split-part)))
  (tu/with-changed 'split-part "(in-ns 'split) (def split-part 2)"
    (tu/reload)
    (is (= '["Unloading" split "Loading" split] (tu/trace)))
    (is (= 2 @(resolve 'split/split-part)))))

(deftest reload-double-test
  (tu/init 'double)
  (is (= :a @(resolve 'double/a)))
  (tu/touch 'double)
  (tu/reload)
  (is (= '["Unloading" double "Loading" double double] (tu/trace)))
  (is (= :a @(resolve 'double/a)))
  (is (= :b @(resolve 'double/b)))
  (tu/with-changed 'double "(ns double) (def a :a2)"
    (tu/reload)
    (is (= '["Unloading" double "Loading" double double] (tu/trace)))
    (is (= :a2 @(resolve 'double/a)))
    (is (= :b @(resolve 'double/b))))
  (tu/with-changed 'double-b "(ns double) (def b :b2)"
    (tu/reload)
    (is (= '["Unloading" double "Loading" double double] (tu/trace)))
    (is (= :a @(resolve 'double/a)))
    (is (= :b2 @(resolve 'double/b))))
  (tu/reload)
  (is (= '["Unloading" double "Loading" double double] (tu/trace)))
  (is (= :a @(resolve 'double/a)))
  (is (= :b @(resolve 'double/b))))

(deftest exclude-test
  (let [opts {:require '[b e c d h g a f k j i l]}]
    (is (= '[] (modify (assoc opts :no-reload ['k]) 'k)))
    (is (= '["Unloading" f a h d e "Loading" e d h a f] (modify (assoc opts :no-reload ['c]) 'e)))
    (is (= '["Unloading" f a h d e "Loading" e c d h a f] (modify (assoc opts :no-unload ['c]) 'e)))))

(deftest reload-loaded-test
  (is (= '["Unloading" a d c e b "Loading" b e c d a] (modify {:require '[a] :only :loaded})))
  (is (= '["Unloading" f a d c g e b "Loading" b e g c d a f] (modify {:require '[a f] :only :loaded})))
  (is (= '["Unloading" f a h d c g e b "Loading" b e g c d h a f] (modify {:require '[a f h] :only :loaded}))))

(deftest reload-regexp-test
  (is (= '["Loading" i] (modify {:require '[a f] :only #"(a|i)"})))
  (is (= '["Unloading" a "Loading" a i] (modify {:require '[a f] :only #"(a|i)"} 'a)))
  (is (= '["Loading" i] (modify {:require '[a f] :only #"(a|i)"} 'i)))
  (is (= '["Unloading" a "Loading" a i] (modify {:require '[a f] :only #"(a|i)"} 'a 'i)))
  (is (= '["Unloading" f a d c e "Loading" e c d a f i] (modify {:require '[a f] :only #"(a|i)"} 'e)))
  (is (= '["Loading" i] (modify {:require '[a f] :only #"(a|i)"} 'k)))
  (is (= '["Unloading" k "Loading" k i] (modify {:require '[a f k] :only #"(a|i)"} 'k))))

(deftest reload-all-test
  (tu/with-deleted 'err-runtime
    (is (= '["Loading" b double double e g k l no-unload o split two-nses two-nses-second c d h j n a f i m]
          (modify {:require '[] :only :all})))))

(deftest reload-exception-test
  (tu/init 'a)
  (tu/with-changed 'c "(ns c (:require e)) (/ 1 0)"
    (tu/touch 'e)
    (is (thrown? Exception (tu/reload)))
    (is (= '["Unloading" a d c e "Loading" e c "  failed to load" c] (tu/trace)))
    (is (thrown? Exception (tu/reload)))
    (is (= '["Unloading" c "Loading" c "  failed to load" c] (tu/trace))))
  (tu/reload)
  (is (= '["Unloading" c "Loading" c d a] (tu/trace))))

(deftest reload-unknown-dep-test
  (tu/init 'a)
  (tu/with-changed 'c "(ns c (:require e z))"
    (tu/touch 'e)
    (is (thrown? Exception (tu/reload)))
    (is (= '["Unloading" a d c e "Loading" e c "  failed to load" c] (tu/trace))))
  (tu/reload)
  (is (= '["Unloading" c "Loading" c d a] (tu/trace))))

(deftest reload-ill-formed-test
  (tu/init 'a)
  (tu/with-changed 'c "(ns c (:require e"
    (tu/touch 'e)
    (is (thrown? Exception (tu/reload)))
    (is (= '["Failed to read" "fixtures/core_test/c.clj"] (tu/trace))))
  (tu/reload)
  (is (= '["Unloading" a d c e "Loading" e c d a] (tu/trace))))

(deftest reload-changed-test
  (tu/init 'i)
  (tu/with-changed 'i "(ns i)"
    (tu/with-changed 'j "(ns j (:require i))"
      (tu/with-changed 'k "(ns k (:require j))"
        (tu/reload)
        (is (= '["Unloading" i j k "Loading" i j k] (tu/trace))))))
  (tu/reload)
  (is (= '["Unloading" k j i "Loading" k j i] (tu/trace))))

(deftest reload-deleted-test
  (tu/init 'l)
  (tu/with-deleted 'l
    (tu/reload)
    (is (= '["Unloading" l] (tu/trace)))))

(deftest reload-deleted-2-test
  (tu/init 'i)
  (tu/with-changed 'j "(ns j)"
    (tu/with-deleted 'k
      (tu/reload)
      (is (= '["Unloading" i j k "Loading" j i] (tu/trace)))))
  (tu/reload)
  (is (= '["Unloading" i j "Loading" j i] (tu/trace))))

(deftest reload-rename-ns
  (tu/init 'i)
  (tu/with-changed 'i "(ns z)"
    (tu/touch 'k)
    (tu/reload)
    (is (= '["Unloading" i j k "Loading" k j] (tu/trace))))
  (tu/reload)
  (is (= '[] (tu/trace))))

(deftest reload-remove-ns
  (tu/init 'i)
  (tu/with-changed 'i ""
    (tu/touch 'k)
    (tu/reload)
    (is (= '["Unloading" i j k "Loading" k j] (tu/trace))))
  (tu/reload)
  (is (= '[] (tu/trace))))

(deftest cycle-self-test
  (tu/init 'l)
  (tu/with-changed 'l "(ns l (:require l))"
    (tu/reload)
    (is (= '["Unloading" l "Loading" l] (tu/trace))))
  (tu/reload)
  (is (= '["Unloading" l "Loading" l] (tu/trace))))

(deftest cycle-one-hop-test
  (tu/init 'i)
  (tu/with-changed 'j "(ns j (:require i))"
    (is (thrown-with-msg? Exception #"Cycle detected: i, j" (tu/reload)))
    (is (= '[] (tu/trace))))
  (tu/reload)
  (is (= '["Unloading" i j "Loading" j i] (tu/trace))))

(deftest cycle-two-hops-test
  (tu/init 'i)
  (tu/with-changed 'k "(ns k (:require i))"
    (is (thrown-with-msg? Exception #"Cycle detected: i, j, k" (tu/reload)))
    (is (= '[] (tu/trace))))
  (tu/reload)
  (is (= '["Unloading" i j k "Loading" k j i] (tu/trace))))

(deftest cycle-extra-nodes-test
  (tu/init 'a 'f 'h)
  (tu/with-changed 'e "(ns e (:require h))"
    (is (thrown-with-msg? Exception #"Cycle detected: e, h" (tu/reload)))
    (is (= '[] (tu/trace))))
  (tu/reload)
  (is (= '["Unloading" f a h d c e "Loading" e c d h a f] (tu/trace))))

(deftest hooks-test
  (is (= '["Unloading" m n "Loading" n m] (modify {:require '[o n m]} 'n)))
  (is (= [:unload-m :unload-n :reload-n :reload-m] @@(resolve 'o/*atom))))

(deftest hooks-test-2
  (is (= '["Unloading" m "Loading" m] (modify {:require '[o n m]} 'm)))
  (is (= [:unload-m :reload-m] @@(resolve 'o/*atom))))

(deftest unload-hook-fail-test
  (tu/with-changed 'm "(ns m
                         (:require n o))
                       
                       (defn before-ns-unload []
                         (/ 1 0))"
    (tu/init 'm)
    (tu/touch 'm)
    (tu/reload)
    (is (= '["Unloading" m "  exception during unload hook" "java.lang.ArithmeticException" "Loading" m] (tu/trace))))
  (tu/reload)
  (is (= '["Unloading" m "  exception during unload hook" "java.lang.ArithmeticException" "Loading" m] (tu/trace))))

(deftest reload-hook-fail-test
  (tu/init 'm)
  (tu/with-changed 'n "(ns n
                         (:require o))
                       
                       (defn after-ns-reload []
                         (/ 1 0))"
    (tu/touch 'o)
    (is (thrown? Exception (tu/reload)))
    (is (= '["Unloading" m n o "Loading" o n "  failed to load" n] (tu/trace))))
  (tu/reload)
  (is (= '["Unloading" n "Loading" n m] (tu/trace))))

(deftest no-unload-meta-test
  (tu/init 'no-unload)
  (let [rand1 @(resolve 'no-unload/rand1)
        rand2 @(resolve 'no-unload/rand2)]
    (tu/with-changed 'no-unload "(ns ^:clj-reload/no-unload no-unload)
                                 
                                 (def rand1
                                   (rand-int Integer/MAX_VALUE))"
      (tu/reload)
      (let [rand1' @(resolve 'no-unload/rand1)
            rand2' @(resolve 'no-unload/rand2)]
        (is (not= rand1' rand1))
        (is (= rand2' rand2))))))

(deftest no-reload-meta-test
  (tu/init 'no-reload)
  (let [rand1  @(resolve 'no-reload/rand1)
        _      (tu/touch 'no-reload)
        _      (tu/reload)
        rand1' @(resolve 'no-reload/rand1)]
    (is (= rand1' rand1))))
