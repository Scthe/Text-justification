(ns ^:figwheel-always text-justification.core
    (:require [reagent.core :as reagent :refer [atom]]
              [goog.dom :as dom]
              [goog.string :as gstring]
              [text-justification.model :as model]))

(enable-console-print!)

(defonce page-width 11)
(defonce target-el (dom/getElement "justified-text"))

(defn get-text-el []
  (dom/getElement "text"))

(defn get-text "get text to justify as string" []
  (.-value (get-text-el)))

; (.log js/console (.-value (dom/getElement "text")))
; (.log js/console (get-text))





;; remove listeners from text area
(let [ old-node (get-text-el) new-node (.cloneNode old-node true)]
  (.replaceChild (.-parentNode old-node) new-node old-node)
  (set! (.-value new-node) "Bleh Bleh Bleh Bleh aaabbbccc\n\naaa bbb")
  ; (set! (.-value new-node) "aaabbbccc111 a")
  ; (set! (.-value new-node) "aaabbbccc11 -1 a")
  )

;; add keyup listner to text-el
 (.addEventListener (get-text-el) "keyup" (fn [] ;; TODO only characters, not f.e. arrows
  (model/execute target-el get-text page-width)
  ))

(model/execute target-el get-text page-width)


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
