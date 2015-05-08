(ns ^:figwheel-always text-justification.model
    (:require [reagent.core :as reagent :refer [atom]]
              [goog.string :as gstring]))

(enable-console-print!)

;;;; TODO filter out empty words
;;;; TODO precalc badness to hash ?
;;;; TODO remove has-memo?

;;;;
;;;; consts
;;;;
(defonce invisible-char (gstring/unescapeEntities "&nbsp;")) ;; \u00A0 ?


;;;;
;;;; state
;;;;

(defonce state (atom []))

(defn- set-state [new-state]
  (swap! state (fn [] new-state)))
(defn- clear-state [] (set-state []))
(defn- update-state [new-lines]
  (set-state (concat @state new-lines)))
  ; (println "new state" @state))


;;;;
;;;; implementation
;;;;

(defn- badness
  "given line and page width returns measurement how `pretty` the line is"
  [line-lens page-width]
  (let [total-length (+ (reduce + line-lens) (count line-lens) -1)] ;; remember to count whitespaces
    (if (> total-length page-width)
      js/Infinity
      (let [a (- page-width total-length)] (* a a a)) )))


;; cache results (memo)
;; f.e. result for 'justify text after X word is: [lines]'
(def tj-memo (atom {}))
(defn- clear-memo [] (reset! tj-memo {}))
(defn- add-memo   [arg-val return-val] (swap! tj-memo #(assoc % arg-val return-val)))
(defn- has-memo?  [arg-val] (contains? @tj-memo arg-val))
(defn- get-memo   [arg-val] (get @tj-memo arg-val))

(defn- tj-inner ;;; text-justification-inner
  "fits words into lines so that whole text looks pretty
  @return [`pretty` metric value, [justified lines]]"
  ([word-lens page-width] ;; lens as in lengths, not actual lens
    (clear-memo)
    (second (tj-inner word-lens page-width 0)))
  ([word-lens page-width word-idx]
    (cond
      (empty? word-lens) [0.0 []]
      (has-memo? word-idx) (get-memo word-idx)
      :else
        (reduce
          (fn [acc word-id]
             (let [line-lens (subvec word-lens 0 word-id) rest-words-lengths (subvec word-lens word-id)
                   next-word-idx (+ word-idx word-id)
                   [sub-prob-badness sub-prob-lines] (tj-inner rest-words-lengths page-width next-word-idx)
                   ugliness (+ (badness line-lens page-width) sub-prob-badness)
                   ;; solution-proposal (cons line sub-prob-lines)]
                   solution-proposal (cons word-id sub-prob-lines)]
              ; (print "sub" line "|words in line" (count words))
              (add-memo next-word-idx [sub-prob-badness sub-prob-lines]) ;; write to memo
              (if (< ugliness (first acc)) [ugliness solution-proposal] acc)))
          [js/Infinity []]
          (map inc (range (count word-lens))) )))) ;; TODO (dec (count words)) ?


(defn- prepare-text
  "split by paragraphs or treat text as single text flow"
  [text separate-paragraphs]
  (if separate-paragraphs
    (seq (.split text "\n"))
    [(clojure.string/replace text #"\n" " ")]
    ))


(defn- prepare-word
  "remove whitespaces, split word if it is too long"
  [max-len word-raw]
  (let [word (gstring/trim word-raw) len (count word)]
    (cond
      (<= max-len 0) [word]
      (zero? len) [invisible-char]
      (<= len max-len) [word]
      :else (concat
          [(.substring word 0 max-len)]
          (prepare-word max-len (str " -" (.substring word max-len)))) )))
; (defn prepare-word-test [str]
  ; (let [res (prepare-word 11 str)]
    ; (println "test(" (count res) ")" res)))
; (prepare-word-test "aaabbbccc111")
; (prepare-word-test "aaabbbccc111aaabbbccc111")
; (prepare-word-test "aaabbbccc111aaabbbccc111aaabbbccc111")

(defn- word-lens-to-text [])
;;;;
;;;; public interface
;;;;

(defn text-justification
  "justify text provided as a collection of words to given page width"
  [text separate-paragraphs page-width]
  {:pre [(> page-width 0)]}
  (clear-state)
  (doseq [paragraph (prepare-text text separate-paragraphs)]
    ; (println "PARAGRAPH:" paragraph)
    (.profile js.console "text-justification")
    (let [words-raw (.split paragraph #" +")
          ;;words (vec (filter .blank? (flatten (map #(prepare-word page-width %) words-raw))))
          words (vec (flatten (map #(prepare-word page-width %) words-raw)))
          word-lens (vec (map count words))
          justified-text (tj-inner word-lens page-width)]
        ; (.log js/console "words" (count words) ":" words)
        ; (.log js/console "state" (count justified-text) ":" justified-text)
        (update-state
        (second (reduce
          (fn [[start-word-idx lines-atm] words-in-line]
            (let [next-word-start-idx (+ start-word-idx words-in-line)
                  words-in-this-line (subvec words start-word-idx next-word-start-idx)]
              ; (println words-in-this-line)
              [next-word-start-idx (conj lines-atm words-in-this-line)]
              ))
          [0 []]
          justified-text)))
      )
    (.profileEnd js.console)
  ))
