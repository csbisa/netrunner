(ns i18n.en
  (:require
   [clojure.string :refer [join] :as s]
   [i18n.defs :refer [render-map]]))

(defmethod render-map "en"
  [_ input]
  (let [username (:username input)
        text (:raw-text input)]
    (if username
      (str username " " text ".")
      text)))
