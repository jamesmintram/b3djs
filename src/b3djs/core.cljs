(ns b3djs.core
  (:require [reagent.core :as reagent :refer [atom]]
            [b3djs.toker :as toker]
            [b3djs.test1 :as test]))

;; define your app data so that it doesn't get over-written on reload

(defonce app-state (atom {:text "Hello world!"}))

(defn recompile []
  (loop [state (toker/create test/code)
         tokens []] 
    
    (let [new-state (toker/consume state)]
      (if (= :eof (-> state :current :type))
        tokens
        (recur new-state (conj tokens (:current new-state)))))))

(comment 
  (print (recompile)))
 

(defn hello-world []
  [:div
   [:h1 (:text @app-state)]
   [:h3 "Edit this and watch it change!"]])

(defn start []
  (reagent/render-component [hello-world]
                            (. js/document (getElementById "app"))))

(defn ^:export init []
  ;; init is called ONCE when the page loads
  ;; this is called in the index.html and must be exported
  ;; so it is available even in :advanced release builds
  (start))

(defn stop []
  ;; stop is called before any code is reloaded
  ;; this is controlled by :before-load in the config
  (js/console.log "stop"))
