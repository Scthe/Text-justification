(ns ^:figwheel-always text-justification.utils)

(defn exp
  "power: x^n"
  [x n]
  (reduce * (repeat n x)))

(defn sum-lengths
  "given iterable returns sum of lengths of all elements"
  [collection]
  (reduce (fn [acc e] (+ acc (count e))) 0 collection))

(defn line-length
  "given list of words return chars count"
  [line]
  (+ (sum-lengths line) (dec (count line)))) ;; characters + spaces_between_words

(defn call-if [predicate f] (fn [e] (if (predicate e) (f e) nil)))
