(ns ^:figwheel-always text-justification.model
    (:require [reagent.core :as reagent :refer [atom]]
              [goog.string :as gstring]
              [text-justification.utils :as utils]))

(enable-console-print!)


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

(defn- badness
  "given line and page width returns measurement how `pretty` the line is"
  [line page-width]
  (let [total-length (utils/line-length line)]
    (if (> total-length page-width)
      js/Infinity
      (utils/exp (- page-width total-length) 3))))

(defn- text-justification-inner
  "fits words into lines so that whole text looks pretty
  @return [`pretty` metric value, [justified lines]]"
  [words page-width]
  (if (empty? words)
    [0.0 []]
    (reduce
      (fn [acc word-id]
         (let [line (subvec words 0 word-id) rest-words (subvec words word-id)
               sub-problem-solution (text-justification-inner rest-words page-width)
               ugliness (+ (badness line page-width) (first sub-problem-solution))
               solution-proposal (cons line (second sub-problem-solution))]
          ; (print "sub" line "|words in line" (count words))
          (if (< ugliness (first acc)) [ugliness solution-proposal] acc)))
      [js/Infinity []]
      (map inc (range (count words))) )))

(defn- prepare-text
  "split by paragraphs or treat text as single text flow"
  [text separate-paragraphs]
  (if separate-paragraphs
    (seq (.split text "\n"))
    [(clojure.string/replace text #"\n" " ")]
    ))


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
    (let [words-raw (.split paragraph #" ")
          words (vec (flatten (map #(prepare-word page-width %) words-raw)))
          [_ justified-line] (text-justification-inner words page-width)]
        ; (.log js/console "words" (count words) ":" words)
        ; (.log js/console "state" (count justified-line) ":" justified-line)
        (update-state justified-line)
      )
  ))
