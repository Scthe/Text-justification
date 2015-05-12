;;(ns is-it-time.test.compare-test
(ns text-justification.test.model-test
  (:require
    [cljs.test :refer-macros [deftest testing is]]))
  ;; require my own scripts too !

;;(deftest somewhat-less-wat
  ;;(is (= "{}[]" (+ {} []))))
  ;;(is (= "{}[]" (+ {} []))))

; (deftest javascript-allows-div0
  ; (is (= js/Infinity (/ 1 0) (/ (int 1) (int 0)))))

(deftest javascript-allows-div1
  (is (= 1 (/ 1 0) (/ (int 1) (int 0)))))

(comment
(with-test
  (defn pennies->dollar-string
    [pennies]
    {:pre [(integer? pennies)]}
    (str "$" (int (/ pennies 100)) "." (mod pennies 100)))
  (testing "assertions are nice"
    (is (thrown-with-msg? js/Error #"integer?" (pennies->dollar-string 564.2)))))
)
