(ns test-runner
  (:require [cljs.test :as test :refer-macros [run-tests] :refer [report]]
            [text-justification.test.model-test]))
            ;[is-it-time.test.core-test]
            ;[is-it-time.test.compare-test]))

(enable-console-print!)

(defmethod report [::test/default :summary] [m]
  (println "\nRan" (:test m) "tests containing"
           (+ (:pass m) (:fail m) (:error m)) "assertions.")
  (println (:fail m) "failures," (:error m) "errors.")
  (aset js/window "test-failures" (+ (:fail m) (:error m))))


(defn runner []
  (if (cljs.test/successful?
        (run-tests
          'text-justification.test.model-test))
    0
    1))
