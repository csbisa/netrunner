(ns i18n.en
  (:require
   [clojure.string :refer [join] :as s]
   [i18n.defs :refer [render-map]]))

(defn pluralize
  "Makes a string plural based on the number n. Takes specific suffixes for singular and plural cases if necessary."
  ([string n] (pluralize string "s" n))
  ([string suffix n] (pluralize string "" suffix n))
  ([string single-suffix plural-suffix n]
   (if (or (= 1 n)
           (= -1 n))
     (str string single-suffix)
     (str string plural-suffix))))

(defn quantify
  "Ensures the string is correctly pluralized based on the number n."
  ([n string] (str n " " (pluralize string n)))
  ([n string suffix] (str n " " (pluralize string suffix n)))
  ([n string single-suffix plural-suffix]
   (str n " " (pluralize string single-suffix plural-suffix n))))

(defn enumerate-str
  "Joins a collection to a string, seperated by commas and 'and' in front of
  the last item. If collection only has one item, justs returns that item
  without seperators. Returns an empty string if coll is empty."
  [strings]
  (if (<= (count strings) 2)
    (join " and " strings)
    (str (apply str (interpose ", " (butlast strings))) ", and " (last strings))))

(defn- render-single-cost
  [cost value]
  (case cost
    :click (str "spends " (apply str (repeat value "[Click]")))
    :credits (str "pays " value " [Credits]")
    :trash (str "trashes " value)       ; TODO
    :forfeit (str "forfeits " value)
    :gain-tag (str "takes " (quantify value "tag"))
    :tag (str "removes " (quantify value "tag"))
    :bad-pub (str "gains " value " bad publicity")
    default cost value)) ; TODO

(defn render-cost
  [cost]
  (when cost
    (str (enumerate-str (for [[c v] cost] (render-single-cost c v))) " to ")))

>>>>>>> ee5ffa200 (Convert costs to maps)
(defmethod render-map "en"
  [_ input]
  (let [username (:username input)
        text (:raw-text input)
        cost-str (render-cost (:cost input))]
    (if username
      (str username " " cost-str text ".")
      text)))
