(ns ^:figwheel-always text-justification.core
    (:require [reagent.core :as reagent :refer [atom]]
              [goog.dom :as dom]))

(enable-console-print!)

(defonce text-id "text")
(defonce page-width 11)
(defonce target-el (dom/getElement "justified-text"))

(defn get-text-el []
  (dom/getElement "text"))

(defn get-text "get text to justify as string" []
  (.-value (get-text-el)))

; (.log js/console (.-value (dom/getElement "text")))
(.log js/console (get-text))

(defn exp "power: x^n" [x n]
  (reduce * (repeat n x)))

(defn count_chars "given array of string sum all lengths" [line]
  (reduce (fn [acc e] (+ acc (count e))) 0 line))

(defn badness [line page_width]
  (let [total_length (count_chars line)]
    (if (> total_length page_width) js/Infinity (exp (- page_width total_length) 3)
    )))

(defn text_justification[words page_width]
  (if (empty? words) [0.0 []]
  (reduce (fn[acc word_id]
    (let [ prev_ugliness (first acc) line (subvec words 0 word_id) rest_words (subvec words word_id)
      sub_problem_solution (text_justification rest_words page_width)
      ugliness (+ (badness line page_width) (first sub_problem_solution)) ; combined ugliness (line now + all lines under)
      solution_proposal (cons line (second sub_problem_solution))]
      (if (< ugliness prev_ugliness) [ugliness solution_proposal] acc))
    ) [js/Infinity []] (map inc (range (count words))) )))

;; TODO paragraphs

(defn stretch_string [line max_width]
  (let [ str_len (count line)
    char_count (count_chars line) ;; total characters in provided string
    chars_left (- max_width char_count) ;; chars left in line
    spaces_per_break_f (float (/ chars_left (dec str_len)))
    spaces_per_break_i (int spaces_per_break_f)
    extra_space_every_X_words (/ 1 (- spaces_per_break_f spaces_per_break_i))]
    ; (if (> char_count max_width)
      ; (throw (Exception. "Could not stretch string, it is already too long")))
    (second (reduce (fn [acc e] ;; run reduce and take second component
      (let [ word_id (first acc) res (second acc)
        space_count (if (>= word_id str_len) 0 ;; last word TODO add some padding here if needed -> (test ["b" "l" "e" "h"] 11)
          (+ spaces_per_break_i
            (if (== (mod word_id extra_space_every_X_words) 0) 1 0))) ;; add extra space every X words
        spaces (apply str (repeat space_count "_"))]
        [(inc word_id) (str res e spaces)]))
      [1 ""] line))
))


(defn execute []
  (.log js/console "Execute !!!")
  (set! (.-innerHtml target-el) "") ;; clear
  (let [strs (seq (.split (get-text) #" "))
    justified_lines (second (text_justification strs page-width))]
    ; (print justified_lines)
    (doseq [line_strs justified_lines]
      (let [line (stretch_string line_strs page-width)
        el (.createElement js/document "div")]
        (println "line:" line)
        (.appendChild target-el el)
        (set! (.-innerText el) line)
        ))))


;; remove listeners from text area
(let [ old-node (get-text-el) new-node (.cloneNode old-node true)]
  (.replaceChild (.-parentNode old-node) new-node old-node)
  )

;; add keyup listner to text-el
 (.addEventListener (get-text-el) "keyup" (fn []
  (execute)
  ))





(comment
(defn render-query [results]
  (str
    "<ul>"
    (apply str
      (for [result results]
        (str "<li>" result "</li>")))
    "</ul>"))

(let [strs (seq (.split (get-text) #" "))
  justified_lines (second (text_justification strs page-width))]
  ; (print justified_lines)
  (doseq [line_strs justified_lines]
    ; (println line_strs)
    ; (let [ll [:span "hi"]]
      ; (.appendChild target-el ll)
      ; )
    (set! (.-innerHTML results-view) (render-query results))
  (let [ul (render-query line_strs)]
   (println ul) )
    )
  ))

(comment
;; define your app data so that it doesn't get over-written on reload

(defonce app-state (atom {:text "Hello world!"}))

(defn hello-world []
  [:h1 (:text @app-state)])


(reagent/render-component [hello-world]
                          (. js/document (getElementById "app")))


; (defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
; ) 
)
