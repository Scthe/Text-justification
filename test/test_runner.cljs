(ns test-runner
  (:require [cljs.test :refer-macros [run-tests]]
            [text-justification.test.model-test]))
            ;[is-it-time.test.core-test]
            ;[is-it-time.test.compare-test]))

(enable-console-print!)

(defn runner []
  (if (cljs.test/successful?
        (run-tests
          'text-justification.test.model-test))
         ;'is-it-time.test.core-test
         ;'is-it-time.test.compare-test))
    0
    1))