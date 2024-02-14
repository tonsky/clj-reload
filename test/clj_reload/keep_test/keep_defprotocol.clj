(ns clj-reload.keep-test.keep-defprotocol)

^:clj-reload.core/keep
(defprotocol IProto
  (-method-one [_])
  (-method-two [_ a b c]))
