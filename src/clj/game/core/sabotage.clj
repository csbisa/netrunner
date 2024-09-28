(ns game.core.sabotage
  (:require
    [clojure.string :as string]
    [game.core.card :refer [corp? in-hand?]]
    [game.core.eid :refer [effect-completed]]
    [game.core.engine :refer [resolve-ability]]
    [game.core.moving :refer [trash-cards]]
    [game.core.say :refer [system-msg]]
    [game.macros :refer [req msg continue-ability]]
    [game.utils :refer [pluralize]]))

(defn choosing-prompt-req
  [n]
  (req
    (let [cards-rd (count (get-in @state [:corp :deck]))
          forced-hq (- n cards-rd)]
      (str "Choose"
           (when (pos? forced-hq)
             (str " at least " forced-hq " " (pluralize "card" forced-hq) " and"))
           " up to " n " " (pluralize "card" n)
           " to trash from HQ. Remainder will be trashed from top of R&D."))))

(defn trash-selected-req
  [n]
  (req
    (let [targets (if (nil? target) [] targets) ; catch cancel-effect that gives [nil] as targets
          selected-hq (count targets)
          selected-rd (min (count (:deck corp))
                           (- n selected-hq))
          to-trash (concat targets (take selected-rd (:deck corp)))]
      (system-msg state side
                  {:type :direct-effect
                   :effect (into [] (concat (when (pos? selected-hq) [[:trash-from-hq selected-hq]])
                                            (when (pos? selected-rd) [[:trash-rnd selected-rd]])))})
      (trash-cards state side eid to-trash {:unpreventable true}))))

(defn sabotage-ability
  [n]
  (let [choosing-ab (fn [forced-hq]
                      ^:ignore-async-check
                      {:waiting-prompt true
                       :player :corp
                       :prompt (choosing-prompt-req n)
                       :choices {:min forced-hq
                                 :max n
                                 :card #(and (corp? %)
                                             (in-hand? %))}
                       :async true
                       :cancel-effect (trash-selected-req n)
                       :effect (trash-selected-req n)})
        check-forcing-ab {:async true
                          :effect (req
                                    (let [cards-rd (count (:deck corp))
                                          cards-hq (count (:hand corp))
                                          forced-hq (- n cards-rd)]
                                      (if (>= n (+ cards-rd cards-hq))
                                        ((trash-selected-req n) state :corp eid card (:hand corp))
                                        (continue-ability state side
                                                          (choosing-ab forced-hq)
                                                          card nil))))}]
    {:req (req (pos? n))
     :msg (req [[:sabotage n]])
     :async true
     :effect (req
               (swap! state update-in [:stats :runner :cards-sabotaged] (fnil + 0) n)
               (continue-ability state side check-forcing-ab card targets))}))
