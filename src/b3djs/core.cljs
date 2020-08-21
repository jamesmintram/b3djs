(ns b3djs.core
  (:require [reagent.core :as reagent :refer [atom]]
            [reagent.dom :as rdom]
            [b3djs.toker :as toker]
            [b3djs.parser :as parser]
            [tests.print :as test]
            [cljs.pprint :as pprint]))
 
;; define your app data so that it doesn't get over-written on reload

(defonce app-state (atom {:text "Hello world!"}))

(defn print-tokens [lexer]
  (cljs.pprint/pprint 
   (loop [state lexer
          tokens []]
     (let [[_token new-state] (toker/consume state)]
       (if (= :eof (-> state :current :type))
         tokens
         (recur new-state (conj tokens (:current new-state))))))))

(defn recompile []
    (-> test/code
        (toker/create)
        (parser/build-ast)))

(defn hello-world []
  (print-tokens (toker/create test/code))
  [:div
   [:h1 (:text @app-state)]
   [:pre
    (let [ast (recompile)]
      (with-out-str (cljs.pprint/pprint ast)))]])

(defn start []
  (rdom/render [hello-world]
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
