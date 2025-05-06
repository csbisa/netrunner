(ns game.core.revealing
  (:require
   [clojure.string :as string]
   [game.core.eid :refer [effect-completed]]
   [game.core.engine :refer [queue-event checkpoint]]
   [game.core.say :refer [system-msg]]
   [game.core.servers :refer [name-zone]]
   [game.core.to-string :refer [card-str-map]]
   [game.utils :refer [enumerate-str]]
   [jinteki.utils :refer [other-side]]))

(defn reveal-hand
  "Reveals a side's hand to opponent and spectators."
  [state side]
  (swap! state assoc-in [side :openhand] true))

(defn conceal-hand
  "Conceals a side's revealed hand from opponent and spectators."
  [state side]
  (swap! state update side dissoc :openhand))

;; TODO - find a way to condense these into one fn
(defn reveal-and-queue-event
  [state side & targets]
  (let [cards (flatten targets)]
    (swap! state assoc :last-revealed cards)
    (queue-event state (if (= :corp side) :corp-reveal :runner-reveal) {:cards cards})))

(defn reveal
  "Trigger the event for revealing one or more cards."
  [state side eid & targets]
  (reveal-and-queue-event state side [targets])
  (checkpoint state side eid))

(defn reveal-loud
  "Trigger the event for revealing one or more cards, and also handle the log printout"
  [state side eid card {:keys [forced and-then no-event] :as args} & targets]
  (let [cards (flatten targets)
        cards-by-zone (group-by #(select-keys % [:side :zone]) cards)
        strs (map #(str (enumerate-str (map :title (get cards-by-zone %)))
                        " from " (name-zone (:side %) (:zone %)))
                  (keys cards-by-zone))]
    (system-msg state (if forced (other-side side) side)
                {:type :use :card (:title card) :force (boolean forced)
                 :effect (apply conj [[:reveal (mapv #(card-str-map state % {:visible true}) cards)]] and-then)})
    (if-not no-event
      (reveal state side eid targets)
      (effect-completed state side eid))))
