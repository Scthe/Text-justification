(ns text-justification.test.model-test
  (:require
    [cljs.test :refer-macros [deftest testing is]]
    [text-justification.model :as model]))


(deftest badness
  ;; test note: badness() should freak out with negative numbers, but for perf. reasons it is not implemented
  ;; (ATM of writing profiler panics on has-memo? method with 3-6% time spend there)
  (testing "lines longer then max allowed length"
    (is (= js/Infinity (model/badness [41] 40)))
    (is (= js/Infinity (model/badness [1]   0)))
    (is (= js/Infinity (model/badness [20 20] 40))) ;; 40 chars + one space
    (is (= js/Infinity (model/badness [13 13 13] 40))) )
  (testing "lines of max allowed length"
    (is (zero? (model/badness [40]   40)))
    (is (zero? (model/badness [1]    1)))
    (is (zero? (model/badness [0]    0)))
    (is (zero? (model/badness [20 20] 41)))
    (is (zero? (model/badness [13 13 13] 41))) )
  (testing "cubes the difference between max line length and provided line"
    (is (= 1 (model/badness [1]   2)))    ;; 1^3
    (is (= 1  (model/badness [2]   3)))
    (is (= 1  (model/badness [2 2] 6)))
    (is (= 1  (model/badness [2 2 2] 9)))
    (is (= 27 (model/badness [8] 11)))    ;; 3^3
    (is (= 27 (model/badness [2 2] 8)))
    (is (= 27 (model/badness [2 2 2] 11))) ))

(deftest tj-inner
  (testing "example provided by Erik Demaine"
    ;; original example: "blah blah blah blah reallylongword"
    ;; should be justified like this:
    ;;   blah      blah
    ;;   blah      blah
    ;;   reallylongword
    ;;
    ;; (tj-inner operates on lengths of words and returns words per line)
    (is (= [2 2 1] (model/tj-inner 14 [4 4 4 4 14]))) ))

(deftest split-word
  (testing "words longer then max allowed length"
    (is (= 2 (count (model/split-word 3 "1234"))))  ;; "123" " -4"
    (is (= 3 (count (model/split-word 3 "12345")))) ;; "123" " -4" " -5"
    (is (= 2 (count (model/split-word 11 "aaa___aaa___")))) ;; ?
    (is (= 3 (count (model/split-word 11 "aaa___aaa___aaa___aaa___")))) ;; ?
    (is (= 4 (count (model/split-word 11 "aaa___aaa___aaa___aaa___aaa___aaa___")))) ) ;; ?
  (testing "words of max allowed length"
    (is (= 1 (count (model/split-word 1 "a"))))
    (is (= 1 (count (model/split-word 24 "aaa___aaa___aaa___aaa___")))) )
  (testing "words shorter then allowed"
    (is (= 1 (count (model/split-word 2 "a"))))
    (is (= 1 (count (model/split-word 13 "aaa___aaa___")))) ))


;; prepare-paragraph
;; prepare-text
;; text-justification
