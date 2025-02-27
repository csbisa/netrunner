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
                  (keys cards-by-zone))
        ;; it's awkward to template a string that could refer to one or many
        ;; like "add it to the top of the stack" vs "add them to the top of the stack"
        ;; so I'm choosing to match the tokens [it] and [them] for this purpose
        plural-repr (if (< 1 (count cards)) "them" "it")
        follow-up (when (and and-then (string? and-then)) (string/replace and-then #"(\[it\])|(\[them\])" plural-repr))]
    (system-msg state (if forced (other-side side) side)
                (if (string? follow-up)
                  (str "uses " (:title card)
                       (if forced
                         (str " to force the " (string/capitalize (name side)))
                         "")
                       " to reveal " (enumerate-str strs) follow-up)
                  {:type :use :card (:title card) :force forced
                   ;; TODO if this isn't a sign that effects need to be ordered then i don't know what is
                   :effect (merge {:reveal (map #(card-str-map state % {:visible true}) cards)}
                                  follow-up)}))
    (if-not no-event
      (reveal state side eid targets)
      (effect-completed state side eid))))
