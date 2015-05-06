(ns ^:figwheel-always text-justification.model
    (:require [reagent.core :as reagent :refer [atom]]
              [goog.dom :as dom] ;; TODO remove
              [goog.string :as gstring]))

(enable-console-print!)

;; TODO move to view
(defonce space-char (gstring/unescapeEntities "&nbsp;")) ;; \u00A0 ?

;;
;; utils
;;

(defn prepare-word
  "remove whitespaces, split word if it is too long"
  [max-len word-raw]
  (let [word (gstring/trim word-raw) len (count word)]
    (cond
      (<= max-len 0) word
      (= len 0) space-char
      (<= len max-len) word
      ;;:else (str (.substring word 0 max-len) " _" (prepare-word max-len (.substring word max-len)))) ;; TODO return list and then flatten
      :else [(.substring word 0 max-len) (str " -" (prepare-word max-len (.substring word max-len)))] ;; TODO return list and then flatten
    )))
; (println "norm:" (prepare-word 3 "abc"))
; (println "trim:" (prepare-word 3 "  abc  "))
; (println "long:" (prepare-word 2 "abc"))
; (println "test:" (prepare-word 11 "aaabbbccc111"))


(defn exp
  "power: x^n"
  [x n]
  (reduce * (repeat n x)))

(defn sum-lengths
  "given iterable returns sum of lengths of all elements"
  [line]
  (reduce (fn [acc e] (+ acc (count e))) 0 line))



;;
;; main
;;


;; TODO make functions private

(defn badness
  "given line and page width and returns measurement how `preety` the line is"
  [line page_width]
  (let [total_length (sum-lengths line)]
    (if (> total_length page_width)
      js/Infinity
      (exp (- page_width total_length) 3))))

(defn text_justification
  "given words to fit and page width returns vector of `preety` measurement and vector of justified lines"
  [words page_width]
  (if (empty? words)
    [0.0 []]
    (reduce ( fn
      [acc word_id]
      (let [line (subvec words 0 word_id) rest_words (subvec words word_id)
            sub_problem_solution (text_justification rest_words page_width)
            ugliness (+ (badness line page_width) (first sub_problem_solution))
            solution_proposal (cons line (second sub_problem_solution))]
        ; (print "sub" line "|words in line" (count words))
        (if (< ugliness (first acc)) [ugliness solution_proposal] acc)))
      [js/Infinity []]
      (map inc (range (count words))) )))

;; TODO remove stretch_string from here - it is view related function
(defn stretch_string [line max_width]
  (let [str_len (count line)
        char_count (sum-lengths line) ;; total characters in provided string
        chars_left (- max_width char_count) ;; chars left in line
        spaces_per_break_f (float (/ chars_left (dec str_len)))
        spaces_per_break_i (int spaces_per_break_f)
        extra_space_every_X_words (/ 1 (- spaces_per_break_f spaces_per_break_i))]
    ; (if (> char_count max_width) ;; TODO restore this
      ; (throw (Exception. "Could not stretch string, it is already too long")))
    (second (reduce (fn [acc e] ;; run reduce and take second component
      (let [ word_id (first acc) res (second acc)
        space_count (if (>= word_id str_len) 0 ;; last word TODO add some padding here if needed -> (test ["b" "l" "e" "h"] 11)
          (+ spaces_per_break_i
            (if (== (mod word_id extra_space_every_X_words) 0) 1 0))) ;; add extra space every X words
        spaces (apply str (repeat space_count space-char))]
        [(inc word_id) (str res e spaces)]))
      [1 ""] line))
))

(defn execute [target-el lazy-text page-width]
  ;; clear target text
  (dom/removeChildren target-el)

  ;; paragraphs
  (doseq [paragraph (seq (.split (lazy-text) "\n"))] ;; TODO if paragraph is empty ? should leave it empty
    (println "PARAGRAPH:" paragraph)
    (let [words-raw  (.split paragraph #" ")
          words (vec (flatten (map #(prepare-word page-width %) words-raw)))
          ; justified_lines words] ;; debug
          [_ justified_lines] (text_justification words page-width)]
      ; (print ">>" words)
      ; (print "<<" justified_lines)
      ; (print (type words))
      (doseq [line_strs justified_lines] ;; TODO do not create view here
        (let [line (stretch_string line_strs page-width)
          el (.createElement js/document "div")]
          ; (println "line:" line)
          (.appendChild target-el el)
          (set! (.-innerText el) line)
          )))
    )
  )
