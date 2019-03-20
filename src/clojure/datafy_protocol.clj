(in-ns 'clojure.core.protocols)

(defprotocol Datafiable
  (datafy [o] "return a representation of o as data (default identity)"))

(extend-protocol Datafiable
  nil
  (datafy [_] nil)

  Object
  (datafy [x] x))
