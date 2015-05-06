(ns ^:figwheel-always text-justification.core
    (:require [reagent.core :as reagent :refer [atom]]
              [goog.dom :as dom]
              [goog.string :as gstring]
              [text-justification.model :as model]))

(enable-console-print!)

(defonce page-width 11)
(defonce space-char (gstring/unescapeEntities "&nbsp;")) ;; \u00A0 ?
(defonce target-el (dom/getElement "justified-text"))

(defn get-text-el []
  (dom/getElement "text"))

(defn get-text "get text to justify as string" []
  (.-value (get-text-el)))

; (.log js/console (.-value (dom/getElement "text")))
; (.log js/console (get-text))


(defn stretch_string [line max_width]
  (let [str_len (count line)
        char_count (model/sum-lengths line) ;; total characters in provided string
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
          lines (model/execute words-raw page-width)]
      (doseq [line lines]
        (let [line-formatted (stretch_string line page-width)
          el (.createElement js/document "div")]
          ; (println "line:" line-formatted)
          (.appendChild target-el el)
          (set! (.-innerText el) line-formatted)
          )))
    )
  )


;; remove listeners from text area
(let [ old-node (get-text-el) new-node (.cloneNode old-node true)]
  (.replaceChild (.-parentNode old-node) new-node old-node)
  (set! (.-value new-node) "Bleh Bleh Bleh Bleh aaabbbccc\n\naaa bbb")
  ; (set! (.-value new-node) "aaabbbccc111 a")
  ; (set! (.-value new-node) "aaabbbccc11 -1 a")
  )

;; add keyup listner to text-el
 (.addEventListener (get-text-el) "keyup" (fn [] ;; TODO only characters, not f.e. arrows
  (execute target-el get-text page-width)
  ))

(execute target-el get-text page-width)


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
