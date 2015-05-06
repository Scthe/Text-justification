(ns ^:figwheel-always text-justification.core
    (:require [reagent.core :as reagent :refer [atom]]
              [goog.dom :as dom]
              [goog.string :as gstring]
              [text-justification.model :as model]
              [text-justification.utils :as utils]))

(enable-console-print!)

(defonce page-width 11)
; (defonce space-char (gstring/unescapeEntities "&nbsp;")) ;; \u00A0 ?
(defonce space-char "_") ;; \u00A0 ?
(defonce target-el (dom/getElement "justified-text"))

(defn get-text-el []
  (dom/getElement "text"))

(defn get-text "get text to justify as string" []
  (.-value (get-text-el)))

; (.log js/console (get-text))


(defn stretch-string
  "provide word separators between words so that line spans from beginning to end"
  [line max-width]
  (let [word-count (count line)
        char-count (utils/sum-lengths line) ;; total characters in provided string
        chars-left (- max-width char-count) ;; chars left in line
        spaces-per-break-f (float (/ chars-left (dec word-count)))
        spaces-per-break-i (int spaces-per-break-f)
        extra-space-every-X-words (/ 1 (- spaces-per-break-f spaces-per-break-i))]
    ; (if (> char-count max-width) ;; TODO restore this
      ; (throw (Exception. "Could not stretch string, it is already too long")))
    (second
      (reduce
        (fn [[word-id res] word](
          let[space-count
                (cond
                  (>= word-id word-count) 0 ; happens for last word
                  (= word-id (dec word-count)) (- max-width (count res) (count word) (count (last line)))
                  (zero? (mod word-id extra-space-every-X-words)) (inc spaces-per-break-i) ; add one more space the usual
                  :else spaces-per-break-i)
              spaces (apply str (repeat space-count space-char))]
             ; (println word "(" (= word-id (dec word-count)) "):" max-width "-" (count res) "-" (count word))
            [(inc word-id) (str res word spaces)]))
        [1 ""]
        line)) ))
; (let [test (stretch-string ["b" "l" "e" "ah"] 15)]
  ; (println "len" (count test) "; " test))

(defn execute
  "Justify provided text and write it to target element"
  [target-el text page-width]
  ; (dom/removeChildren target-el) ; clear target text
  ;; paragraphs
  (doseq [paragraph (seq (.split (text) "\n"))]
    (println "PARAGRAPH:" paragraph)
    (let [words-raw  (.split paragraph #" ")
          lines (model/text-justification words-raw page-width)]
          1
(comment
      (doseq [line lines]
        (let [line-formatted (stretch-string line page-width)
              el (.createElement js/document "div")]
          ; (println "line:" line-formatted)
          (.appendChild target-el el)
          (set! (.-innerText el) line-formatted) ))
)
           )))


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
; (println model/state)

(defonce text-state (atom ["line1" "line2" "--end--"]))

(defn line-component [id line]
  [:div.monospaced id ": " line])

; (defn text-justified-component []
  ; [:p.monospaced
  ; (for [line @text-state] [line-component line])])

(defn text-justified-component []
  [:p
  ; (for [[id line] (map-indexed vector @text-state)]
  (for [[id line] (map-indexed vector @model/state)]
    ^{:key id} [line-component id line])])

(reagent/render-component [text-justified-component]
                          (. js/document (getElementById "justified-text")))



; (defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
; ) 
