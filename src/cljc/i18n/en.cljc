(ns i18n.en
  (:require
   [clojure.string :refer [join split starts-with? ends-with?] :as s]
   [i18n.defs :refer [render-map try-catchall pprint-to-string] :include-macros true]
   [clojure.pprint :as pprint]
   [malli.core :as m]))

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

(defn build-spend-msg-suffix
  "Constructs the spend message  suffix for specified cost-str and verb(s)."
  ([cost-str verb] (build-spend-msg-suffix cost-str verb nil))
  ([cost-str verb verb2]
   (if (empty? cost-str)
     (str (or verb2 (str verb "s")) " ")
     (str verb " "))))

(defn render-credits
  [value]
  (if (map? value)
    (let [remainder-str (when-let [remainder (:pool value)]
                          (str remainder " [Credits]"))
          card-strs (when-let [cards (:cards value)]
                      (str (enumerate-str (map #(str (second %) " [Credits] from " (first %))
                                               cards))))
          message (str "pays "
                       card-strs
                       (when (and card-strs remainder-str)
                         " and ")
                       remainder-str
                       (when (and card-strs remainder-str)
                         " from [their] credit pool"))]
      message)
    (str "pays " value " [Credits]")))

;; okay so what should be coming in here is the server-side format, which is
;;   [<location> <server> <location>]
;; where
;; - location: :servers, :deck, :hand, :discard
;; - server: :hq :rd :archives :remoteN
;; - location: :content :ices
(defn to-zone-name
  ([zone] (to-zone-name zone :corp))
  ([[location server placement] side]
   (let [location (keyword location)
         server (keyword server)
         side (keyword side)]
     (case location
       :hand (if (= side :corp) "HQ" "the Grip")
       :deck (if (= side :corp) "R&D" "the Stack")
       :discard (if (= side :corp) "Archives" "the Heap")
       ;; TODO this is wrong, but messages don't carry enough context
       ;; e.g. IP enforcmenet is corp msg, but score area is runner side
       :scored "the Runner's score area"
       :servers
       (case server
         :hq "HQ"
         :rd "R&D"
         :archives "Archives"
         (str "Server " (last (split (str server) #":remote"))))
       (str "unhandled zone " location)))))

;; TODO fix it, jp is more comprehensive
;; TODO need 'root' wording here
(defn- render-card-internal
  [{:keys [card card-type server pos hosted]}]
  (str (if-not (empty? card)
         card
         (case (keyword card-type)
           :facedown "a facedown card"
           :ice "ice"
           :card "a card"
           :drawn-card (pprint/cl-format nil "the ~:R card drawn" pos)
           ""))
       (if hosted
         (str " hosted on " (render-card-internal hosted))
         (when server
           (if (not (nil? pos))
             (str " protecting "(to-zone-name server) " at position " pos)
             ;; so for better wording this is probably "from" for a non-root?
             ;; TODO need to confirm actual behavior today...
             (str (if (= (first server) :servers) " in the root of " " from ")
                  (to-zone-name server)))))))

(defn- render-card
  [card]
  (cond
    (string? card) card
    (nil? card) "itself"
    true (render-card-internal card)))

;; (render-card-list value "removes" "installed program" " from the game")
;; -> removes COUNT installed program(s) from the game (VAL1, VAL2, ...)
(defn- render-card-list
  ([cards action qualifier] (render-card-list cards action qualifier "" ""))
  ([cards action qualifier trailer] (render-card-list cards action qualifier trailer ""))
  ([cards action qualifier trailer suffix]
   (str action " " (quantify (count cards) qualifier) trailer
        " (" (enumerate-str (map render-card cards)) ")" suffix)))

(defn- to-duration
  [duration]
  (case (keyword duration)
    :end-of-run " for the remainder of the run"
    :end-of-turn " for the remainder of the turn"
    ""))

(defn- to-counter
  [counter]
  (case (keyword counter)
    :adv "advancement counter"
    :virus "virus counter"
    :power "power counter"
    ;; might do place-credits instead, or split this out
    :credit "[Credit]"
    ;; TODO stopgap
    :credits "[Credit]"))

;; TODO dumb name for now, i'm not sure if should be separate or unified
;; so this takes a list of titles and :unseen
(defn- render-card2
  [cards]
  (let [unseen (count (filter #(= "unseen" %) cards))
        self (some nil? cards)
        seen (remove nil? (filter #(not (= "unseen" %)) cards))]
    ;; TODO This is currently placing seen cards after the rest which is a bit unnatural
    (enumerate-str (remove nil? (conj seen
                                      (when (pos? unseen) (quantify unseen "unseen card"))
                                      (when self "itself"))))))

(defn- render-single-cost
  [cost value side]
  (let [hand (to-zone-name [:hand] (or side :corp))
        deck (to-zone-name [:deck] (or side :corp))]
    (case cost
      :click (str "spends " (apply str (repeat value "[Click]")))
      :lose-click (str "loses " (apply str (repeat value "[Click]")))
      :credits (render-credits value)
      :trash (str "trashes " value)
      ;; broken, in this case it's a list but printed as single
      ;; {:username "Runner", :type :install, :cost {:forfeit ("Vanity Project"), :credits 1}, :card "Chatterjee University", :origin [:deck], :raw-text nil}
      :forfeit (if (string? value)
                 (str "forfeits " value)
                 (str "forfeits " (quantify (count value) "agenda")
                      " (" (enumerate-str value) ")"))
      :reveal-and-trash (str "reveals and trashes " (quantify (count value) "card")
                             " (" (enumerate-str value) ") from " hand)
      :gain-tag (str "takes " (quantify value "tag"))
      :tag (str "removes " (quantify value "tag"))
      :bad-pub (str "gains " value " bad publicity")
      :return-to-hand (str "returns " value " to " hand)
      :remove-from-game (str "removes " value " from the game")
      :rfg-program (render-card-list value "removes" "installed program" " from the game")
      :trash-installed (render-card-list value "trashes" "installed card")
      :hardware (render-card-list value "trashes" "installed piece" " of hardware")
      :derez (render-card-list value "derezzes" "card")
      :program (render-card-list value "trashes" "installed program")
      :resource (render-card-list value "trashes" "installed resource")
      :connection (render-card-list value "trashes" "installed connection")
      ;; TODO this renders it as 'ices'
      :ice (render-card-list value "trashes" "installed rezzed ice")
      :trash-bioroid (render-card-list value "trashes" "rezed Bioroid")
      :trash-from-deck (str "trashes " (quantify value "card") " from the top of " deck)
      :trash-from-hand (if (int? value)
                         (str "trashes " (quantify value "card") " from " hand)
                         (render-card-list value "trashes" "card" "" (str " from " hand)))
      :randomly-trash-from-hand (str "trashes " (quantify value "card") " randomly from " hand)
      :trash-entire-hand (if (int? value)
                           (str "trashes all (" value ") cards in " hand)
                           (str "trashes all (" (count value) ") cards in " hand " (" (enumerate-str value) ")"))
      :trash-hardware-from-hand (render-card-list value "trashes" "piece" " of hardware" (str " from " hand))
      :trash-program-from-hand (render-card-list value "trashes" "program" "" (str " from " hand))
      :trash-resource-from-hand (render-card-list value "trashes" "resource" "" (str " from " hand))
      :take-net (str "suffers " value " net damage")
      :take-meat (str "suffers " value " meat damage")
      :take-core (str "suffers " value " core damage")
      :shuffle-installed-to-stack (render-card-list value "shuffles" "card" "" (str " into " deck))
      :add-installed-to-bottom-of-deck (render-card-list value "adds" "installed card" "" (str " to the bottom of " deck))
      :add-random-from-hand-to-bottom-of-deck (str "adds " (quantify value "random card") (str " from " hand " to the bottom of " deck))
      :hosted-to-hq (str "adds " (quantify (count value) "hosted card") " to HQ (" (enumerate-str value) ")")
      :agenda-counter (str "spends " (quantify (second value) "hosted agenda counter") " from on " (first value))
      ;; TODO is this a list?
      ;; yes, there's a path where this is a list of title-counts...
      ;; might need a generic mechanism
      :virus (let [[host count] value]
               (str "spends " (quantify count "hosted virus counter") " from on " host))
      :advancement (str "spends " (quantify (second value) "hosted advancement counter") " from on " (first value))
      :power (str "spends " (quantify (second value) "hosted power counter") " from on " (first value))
      :turn-hosted-matryoshka-facedown (str "turns "(quantify value "hosted cop" "y" "ies")
                                            " of Matryoshka facedown"))))

;; comes in as either
;; [[cost1 value1] [cost2 value2]]
;; [[[cost1 value1] [cost2 value2]] [cost3 value3]]
;; so cost is a vector, first cost is also a vector, first first cost is a keyword or vector?
(defn render-cost
  [cost side]
  (when cost
    (if (vector? (first (first cost)))
      (str
       (enumerate-str (for [[c v] (first cost)] (render-single-cost (keyword c) v side)))
       ", and then "
       (enumerate-str (for [[c v] (second cost)] (render-single-cost (keyword c) v side)))
       ",")
      (enumerate-str (for [[c v] cost] (render-single-cost (keyword c) v side))))))

(defn render-cost-str
  [{:keys [cost side]}]
  (when-not (empty? cost)
    (str (render-cost cost side)
         ;; TODO remove in order to do cost test
         ;;" to "
         " to "
         )))

(defmulti render-effect (fn [effect side & value] effect))
(defmethod render-effect :advance [effect side [value]] (str "advance " (render-card value)))
(defmethod render-effect :draw-cards [effect side [value]] (str "draw " (quantify value "card")))
(defmethod render-effect :gain-credits [effect side [value]] (str "gain " value " [Credits]"))
(defmethod render-effect :lose-credits [effect side [value]] (str "lose " value " [Credits]"))
(defmethod render-effect :gain-click [effect side [value]] (str "gain " (apply str (repeat value "[Click]"))))
(defmethod render-effect :lose-click [effect side [value]] (str "lose " (apply str (repeat value "[Click]"))))
;; TODO ideally part of force logic, but the problem is that these are effects
;; either bring back the silly keyword logic or just support the few cases that exist
(defmethod render-effect :lose-click-force [effect side [value]] (str "force the " (if (= (keyword side) :corp) "Runner" "Corp")" to lose " (apply str (repeat value "[Click]"))))
(defmethod render-effect :lose-credits-force [effect side [value]] (str "force the " (if (= (keyword side) :corp) "Runner" "Corp")" to lose " value " [Credits]"))
(defmethod render-effect :give-tag [effect side [value]] (str "give the Runner " (quantify value "tag")))
(defmethod render-effect :take-tag [effect side [value]] (str "take " (quantify value "tag")))
(defmethod render-effect :remove-tag [effect side [value]] (str "remove " (quantify value "tag")))
(defmethod render-effect :take-bp [effect side [value]] (str "take " value " bad publicity"))
;; TODO this is a clusterfuck, figure out how to unify it later
(defmethod render-effect :add-from-stack [effect side [value]] (str "add " value " from the stack to the grip and shuffle the stack"))
(defmethod render-effect :add-from-rnd [effect side [value]] (str  "reveal " value " from R&D and add it to HQ"))
(defmethod render-effect :add-from-rnd-to-rnd [effect side [value]] (str  "reveal " value " from R&D, shuffle R&D, and place it on top of R&D"))
;; TODO this is probably the best generic mechanism, need to squash it with several others here
(defmethod render-effect :add-card [effect side [card from to]]
            (str "add " (or card "itself")
                 (when from) (str " from " (to-zone-name from))
                 (when to) (str " to " (to-zone-name to))))
(defmethod render-effect :add-to-hq [effect side [value]] (str "add " (render-card value) " to HQ"))
;; TODO not sure if this should be string or rendered card
(defmethod render-effect :add-to-grip [effect side [value]] (str "add " (render-card value) " to the Grip"))
(defmethod render-effect :add-to-hq-unseen [effect side [value]] (str "add " (quantify value "card") " to HQ"))
;; TODO making this add would be more consistent? Working Prototype uses "add", ??? uses "move"
(defmethod render-effect :move-to-top-stack [effect side [value]] (str "move " (or value "them") " to the top of the Stack"))
(defmethod render-effect :shuffle-rnd [effect side [value]] (str "shuffle R&D"))
(defmethod render-effect :reveal-and-add [effect side [card from to]]
                  (str "add " (or card "itself") " from " (to-zone-name from) " to " (to-zone-name to)))
(defmethod render-effect :reveal-from-hq [effect side [value]] (str "reveal " (enumerate-str value) " from HQ"))
(defmethod render-effect :make-run [effect side [value]] (str "make a run on " (to-zone-name value)))
(defmethod render-effect :end-run [effect side [value]] "end the run")
;; TODO probably need a duration here, others are encounter-only IIRC
(defmethod render-effect :gain-type [effect side [card type duration]] (str "make " (render-card card) " gain " (enumerate-str type) (to-duration duration)))
(defmethod render-effect :place-counter [effect side [type count target]]
                 (str "place "
                      (if (or (= (keyword type) :credit) (= (keyword type) :credits))
                        (str count " [Credits]")
                        (quantify count (to-counter type)))
                      " on "
                      (if target (render-card target) "itself")))
(defmethod render-effect :remove-counter [effect side [type count target]]
  (str "remove "
       (quantify count (to-counter type))
       " from "
       (if target (render-card target) "itself")))
(defmethod render-effect :move-counter [effect side [type count source target]]
  (str "move "
       (quantify count (to-counter type))
       " from "
       (if source (render-card source) "itself")
       " to "
       (if target (render-card target) "itself")))
;; TODO need to fix hq/blah
(defmethod render-effect :trash-from-hand [effect side [value]] (if (int? value)
                   (str "trashes " (quantify value "card") " from " "HQ")
                   (render-card-list value "trashes" "card" "" (str " from " "HQ"))))
(defmethod render-effect :add-str [effect side [card count]] (str "add " count " strength to " card))
(defmethod render-effect :reduce-str [effect side [card count]] (str "give -" count " strength to " (render-card card) " for the remainder of the encounter"))
;; TODO combine hq/rnd?
(defmethod render-effect :access-additional-from-hq [effect side [value]] (str "access " (quantify value "additional card") " from HQ"))
(defmethod render-effect :access-additional-from-rnd [effect side [value]] (str "access " (quantify value "additional card") " from R&D"))
;; TODO similar, combine?
(defmethod render-effect :deal-net [effect side [value]] (str "deal " value " net damage"))
(defmethod render-effect :deal-meat [effect side [value]] (str "deal " value " meat damage"))
(defmethod render-effect :take-meat [effect side [value]] (str "suffer " value " meat damage"))
(defmethod render-effect :deal-core [effect side [value]] (str "deal " value " core damage"))
(defmethod render-effect :install [effect side [value]] (str "install " value))
(defmethod render-effect :rez [effect side [value]] (str "rez " (render-card value)))
(defmethod render-effect :install-and-rez-free [effect side [value]] (str "install and rez " value ", ignoring all costs"))
(defmethod render-effect :host [effect side [value]] (str "host " (render-card value)))
(defmethod render-effect :host-on [effect side [card host]] (str "host " (render-card card) " on " (render-card host)))
(defmethod render-effect :bypass [effect side [value]] (str "bypass " (render-card value)))
(defmethod render-effect :trash-free [effect side [value]] (str "trash " value " at no cost"))
(defmethod render-effect :str-pump [effect side [base-str target-str duration]]
            (str "increase its strength from " base-str " to " target-str (to-duration duration)))
(defmethod render-effect :lower-ice-str [effect side [strength card]]
                 (str "lower the strength of "
                      (or card "each installed icebreaker")
                      " by " strength))
(defmethod render-effect :shuffle-into-rnd [effect side [value]] (str "shuffle " (render-card2 value) " into R&D"))
(defmethod render-effect :shuffle-from-hq-into-rnd [effect side [value]] (str "shuffle " (quantify value "card") " from HQ into R&D"))
(defmethod render-effect :rearrange-rnd [effect side [value]] (str "rearrange the top " (quantify value "card") " of R&D"))
(defmethod render-effect :reveal-from-rnd [effect side [value]] (str "reveal " value " from the top of R&D"))
(defmethod render-effect :look-top-rnd [effect side [value]] (str "look at the top " (quantify value "card") " of R&D"))
;; TODO merge with above
(defmethod render-effect :look-top-stack [effect side [value]] (str "look at the top " (quantify value "card") " of the Stack"))
(defmethod render-effect :move-hq-rnd [effect side [value]] (str "add " (quantify value "card") " from HQ to to the top of R&D"))
(defmethod render-effect :play [effect side [card zone]] (str "play " card (when zone (str " from " (to-zone-name zone)))))
#_(defmethod render-effect :move-server
  ([effect side server] (render-effect effect side server nil))
  ([effect side server card]
   (str "move " (or card "itself") " to " (to-zone-name server))))
(defmethod render-effect :move-server
  [effect side [server card]]
  (str "move " (or card "itself") " to " (to-zone-name server)))
(defmethod render-effect :prevent-access [effect side [type card]]
  (str "prevent the runner from accessing "
       (case (keyword type)
         :target card
         :exclusive (str "cards other than " card))))
(defmethod render-effect :trash-stack [effect side [value]] (str "trash " (enumerate-str value) " from the top of the stack"))
(defmethod render-effect :prevent-net [effect side [value]] (str "prevent " value " net damage"))
(defmethod render-effect :prevent-encounter-ability [effect side [card ability]]
                             (str "prevent the encounter ability on " card (when ability (str " (" ability ")"))))
(defmethod render-effect :prevent-etr [effect side [value]] (str "prevent the run from ending"))
(defmethod render-effect :prevent-etr-effect [effect side [value]] (str "prevent " (render-card value) " from ending the run this encounter"))
(defmethod render-effect :gain-str [effect side [strength duration]] (str "gain " strength " strength" (to-duration duration)))
(defmethod render-effect :breach-server [effect side [value]] (str "breach " (to-zone-name value)))
(defmethod render-effect :derez [effect side [value]] (str "derez "
            (if (coll? value)
              (enumerate-str (map render-card value))
              (render-card value))))
(defmethod render-effect :rez-free [effect side [value]] (str "rez " (enumerate-str value) ", ignoring all costs"))
(defmethod render-effect :encounter-ice [effect side [value]] (str "make the Runner encounter " (render-card value)))
(defmethod render-effect :reveal-self [effect side [value]] (str "reveal itself from " (to-zone-name value)))
(defmethod render-effect :add-from-hq-to-score [effect side [value]] (str "add " value " from HQ to [their] score area"))
(defmethod render-effect :turn-faceup [effect side [value]] (str "turn " value " in Archives faceup"))
(defmethod render-effect :add-self-to-hq [effect side [value]] (str "add itself to HQ"))
(defmethod render-effect :trash [effect side [value]] (str "trash " (if (coll? value) (enumerate-str (map render-card value)) (render-card value))))
(defmethod render-effect :add-str-new [effect side [card count duration]] (str "give " (render-card card) " +" count " strength" (to-duration duration)))
;; TODO could spruce this up but it follows current thunderbolt format
(defmethod render-effect :add-sub [effect side [value]] (str "add " value " after its other subroutines"))
(defmethod render-effect :trash-rnd [effect side [value]] (str "trash the top " (quantify value "card") " of R&D"))
(defmethod render-effect :trash-rnd-and-add [effect side [value]] (pprint/cl-format nil "trash the ~:R from R&D and add the rest to HQ" value))
(defmethod render-effect :remove-click-next-turn [effect side [value]] (str "give the Runner -" value " allotted [Click] for [their] next turn"))
(defmethod render-effect :move-grip-to-stack [effect side [value]] (str "add " (enumerate-str value) " from the Grip to the top of the Stack"))
(defmethod render-effect :shuffle-into-stack [effect side [value]] (str "shuffle " (or value "them") " into the stack"))
(defmethod render-effect :remove-all-virus-counters [effect side [value]] (str "remove all virus counters from " (render-card value)))
(defmethod render-effect :trash-from-hq [effect side [value]] (str "trash " value " from HQ"))
(defmethod render-effect :reveal-from-grip [effect side [value]] (str "reveal " (enumerate-str value) " from the Grip"))
(defmethod render-effect :add-to-top-rnd [effect side [value]] (str "add " value " to the top of R&D"))
;; so extend this to render cards, and render cards using other sources too?
(defmethod render-effect :add-to-bottom-rnd [effect side [value]] (str "add " (render-card value) " to the bottom of R&D"))
(defmethod render-effect :add-to-bottom-stack [effect side [value]] (str "add " (render-card value) " to the bottom of the Stack"))
(defmethod render-effect :force-reveal [effect side [value]] (str "reveal " (quantify value "random card") " from HQ"))
(defmethod render-effect :shuffle-zone-into [effect side [value]] (str "shuffle " (enumerate-str (map to-zone-name value)) " into " (to-zone-name [:deck])))
(defmethod render-effect :rfg [effect side [value]] (str "remove " (enumerate-str value) " from the game"))
(defmethod render-effect :reveal-from-stack [effect side [value]] (str "reveal " (enumerate-str value) " from the top of the stack"))
(defmethod render-effect :host-on-self [effect side [value]] (str "host " value " on itself"))
(defmethod render-effect :host-instead-of-access [effect side [value]] (str "host " value " on itself instead of accessing it"))
(defmethod render-effect :shuffle-stack [effect side [value]] (str "shuffle the stack"))
(defmethod render-effect :trash-self [effect side [value]] (str "trash itself"))
(defmethod render-effect :credits [effect side [value]] (str "pay " value " [Credits]"))
(defmethod render-effect :draw-additional [effect side [value]] (str "draw " (quantify value "additional card")))
(defmethod render-effect :purge [effect side [value]] "purge virus counters")
(defmethod render-effect :reveal [effect side [value]] (let [groups (group-by :server value)]
          (str "to reveal "
               (enumerate-str (map #(str (enumerate-str (map :card (second %)))
                                         " from " (to-zone-name (first %)))
                                   groups)))))
(defmethod render-effect :choose-server [effect side [value]] (str "target " (to-zone-name value)))
(defmethod render-effect :choose-subtype [effect side [value]] (str "choose " value))
(defmethod render-effect :choose-ice [effect side [value]] (str "choose " (render-card value)))
(defmethod render-effect :choose-card-type [effect side [value]] (str "choose " value))
(defmethod render-effect :add-to-score [effect side [card kind points]]
                (str "add " (or card "itself") " to the score area"
                     (when points
                       (str " as an "
                            (when (= (keyword kind) :assassination) "assassination ")
                            "agenda worth " (quantify points "agenda point")))))
(defmethod render-effect :prevent-steal-trash [effect side [value]] (str "prevent the Runner from stealing or trashing Corp cards" (to-duration value)))
(defmethod render-effect :swap-ice-from-hand [effect side [value]] (str "swap " (render-card value) " with a piece of ice from HQ"))
;; TODO
(defmethod render-effect :swap-ice [effect side [value]] (throw "foo"))
(defmethod render-effect :gain-click-next-turn [effect side [value]] (str "get +" value " allotted [Click] for [their] next turn"))
(defmethod render-effect :redirect-run [effect side [value]] (str "make the Runner continue the run on " (to-zone-name value)))
(defmethod render-effect :access [effect side [server cards]] (str "access " (quantify cards "card") " from " (to-zone-name server)))
(defmethod render-effect :resolve-subroutine [effect side [ice subroutine]] (str "resolve the subroutine (\"[subroutine]" subroutine "\") from " (render-card ice)))
(defmethod render-effect :turn-faceup [effect side [value]] (str "turn " (render-card value) " faceup"))
(defmethod render-effect :flip-id [effect side [value]] (str "flips [their] identity to " value))
(defmethod render-effect :change-server [effect side [value]] (str "change the attacked server to " (to-zone-name value)))
(defmethod render-effect :reveal-and-host [effect side [value]] (str "reveal and host " value " from HQ"))
(defmethod render-effect :sabotage [effect side [value]] (str "sabotage " value))
(defmethod render-effect :prevent-rez [effect side [card duration]] (str "prevent the Corp from rezzing " (render-card card) (to-duration duration)))

(defn render-single-effect-force-check
  [effect value side forced]
  (str
   (when forced
     (str "force the "
          ;; This is inverted -- corp forcing effect means it's forcing runner to take the effect.
          (if (= (keyword side) :corp) "Runner" "Corp")
          " to "))
   #_(apply render-effect (keyword effect) side value)
   (render-effect (keyword effect) side value)))

(defn render-effects
  [effects side forced]
  (when effects
    (if (vector? (first (first effects)))
      (str
       (enumerate-str (remove nil? (for [[c & v] (first effects)]
                                     (render-single-effect-force-check c v side forced))))
       ", and then "
       (enumerate-str (remove nil? (for [[c & v] (second effects)]
                                     (render-single-effect-force-check c v side forced))))
       ",")
      (enumerate-str (remove nil? (for [[c & v] effects]
                                    (render-single-effect-force-check c v side forced)))))))

(defn render-effect-str
  [{:keys [effect side forced]}]
  (when-not (empty? effect)
    (str " to " (render-effects effect side forced))))

(defmulti render-text (fn [input] (or (keyword (:type input)) :raw-text)))

(defmethod render-text :create-game [_] "has created the game")
(defmethod render-text :join-game [_] "has joined the game")
(defmethod render-text :leave-game [_] "has left the game")
(defmethod render-text :watch-game [_] "has joined the game as a spectator")
(defmethod render-text :keep-hand [_] "keeps [their] hand")
(defmethod render-text :mulligan-hand [_] "takes a mulligan")
(defmethod render-text :mandatory-draw [_] "makes [their] mandatory start of turn draw")

(defmethod render-text :no-action [_] "has no further action")

(defmethod render-text :turn-state
  [input]
  (let [state (:state input)
        pre (if (= (:phase state) "start-turn") "started" "is ending")
        turn (:turn state)
        credits (:credits state)
        cards (:cards state)
        hand (if (= (:side state) "runner") "[their] Grip" "HQ")]
    (str pre " [their] turn " turn " with " credits " [Credit] and " (quantify cards "card") " in " hand)))

(defmethod render-text :play
  [{:keys [card cost zone]}]
  (let [cost-spend-msg (build-spend-msg-suffix cost "play")]
    (str cost-spend-msg card
         (when zone (str "from " (to-zone-name zone))))))

(defn- cost-discount-str
  [{:keys [ignore-all-costs ignore-cost ignore-install-costs cost-bonus alternative-cost]}]
  (cond
    ignore-cost " at no cost" ;; TODO this is redundant but currently wording isn't consistent
    ignore-all-costs " (ignoring all costs)"
    ignore-install-costs " (ignoring its install cost)"
    alternative-cost " (by paying its alternative cost)"
    (and cost-bonus (pos? cost-bonus)) (str " (paying " cost-bonus " [Credits] more)")
    (and cost-bonus (neg? cost-bonus)) (str " (paying " (* -1  cost-bonus) " [Credits] less)")))

;; TODO this should probably be squashed with render-card-internal somehow
;; TODO need to handle the "... as a facedown card" logic. i don't think 'unseen' here is ever used
(defmethod render-text :install
  [{:keys [card card-type server new-remote origin install-source cost host hosted side no-cost] :as input}]
  (let [card-type (keyword card-type)]
    (str (if install-source
           (str (build-spend-msg-suffix cost "use") install-source " to install ")
           (build-spend-msg-suffix cost "install"))
         (when hosted "hosted ")
         (if (= card-type :ice)
           (str (or card "ice"))
           (str (or card (if (= card-type :facedown)
                           "an unseen card"
                           "a card"))))
         (when origin
           (str " from " (to-zone-name origin side)))
         (when server
           (str (if (= card-type :ice)
                  " protecting "
                  " in the root of ")
                (to-zone-name server)
                (when new-remote " (new remote)")))
         (cost-discount-str input)
         (when host
           (str " on " (render-card host)))
         (when no-cost " at no cost"))))

(defmethod render-text :rez
  [{:keys [card alternative-cost ignore-cost cost-bonus rez-source cost] :as input}]
  (str (if rez-source
         (str (build-spend-msg-suffix cost "use") rez-source " to rez ")
         (if cost "rez " "rezzes "))
       (if (string? card) card (render-card card))
       (cost-discount-str input)))

(defmethod render-text :use
  [{:keys [card cost effect]}]
  (let [cost-spend-msg (build-spend-msg-suffix cost "use")]
    ;; TODO this is a stopgap until everything is covnerted to effects
    ;; if there's no effect, it's assumed this is followed by raw text
    (str cost-spend-msg card (when-not effect " to "))))

(defmethod render-text :advance
  [input]
  (str "advance " (render-card input)))

(defmethod render-text :score
  [{:keys [card cost points]}]
  (str (if cost "score " "scores ") card
       (when points (str " and gains " points " agenda points"))))

(defmethod render-text :steal
  [{:keys [card cost points]}]
  (str (if cost "steal " "steals ") card
       (when points (str " and gains " points " agenda points"))))

(defmethod render-text :start-run
  [{:keys [server ignore-costs cost]}]
  (str (if cost "make " "makes ")
       "a run on " (to-zone-name server)
       (when ignore-costs ", ignoring all costs")))

(defmethod render-text :continue-run
  [_]
  (str "will continue the run"))

(defmethod render-text :jack-out
  [{:keys [cost]}]
  (str (if cost "jack" "jacks") " out"))

(defmethod render-text :approach-ice
  [{:keys [ice]}]
  (str "approaches " (render-card ice)))

(defmethod render-text :bypass-ice
  [{:keys [ice]}]
  (str "bypasses " ice))

(defmethod render-text :encounter-ice
  [{:keys [ice]}]
  (str "encounters " (render-card ice)))

;; TODO this is relying on costs but costs always adds "to"
;; might need some refactoring...
(defmethod render-text :encounter-effect
  [{:keys [card]}]
  (str "on encountering " card))

(defmethod render-text :pass-ice
  [{:keys [ice]}]
  (str "passes " (render-card ice)))

;; TODO cost can be {} so need a better check for that one
;; TODO this is redundant, should just reuse the effect. would have to fix stalone cost+effect though
(defmethod render-text :break-subs
  [{:keys [card ice subtype subs break-type sub-count str-boost cost]}]
  (let [sub-count (or sub-count (count subs))]
    (str (if str-boost
           (str (if cost "increase " "increases ")
                "the strength of " card
                " to " str-boost " and break ")
           (str (if cost "use " "uses ")
                card
                " to break "))
         (case (keyword break-type)
           :all (str "all " sub-count " subroutines")
           :remaining (str "the remaining " sub-count " subroutines")
           (quantify sub-count (str (when subtype (str subtype " ")) "subroutine")))
         " on " ice
         (when-not break-type
           (str " (\"[subroutine] "
                (join "\" and \"[subroutine] " subs)
                "\")")))))

;; TODO similar to above
(defmethod render-text :str-boost
  [{:keys [card strength]}]
  (str "increase the strength of " card " to " strength))

;; TODO red-headed stepchild here, burying stuff into :resolved unlike everything else
(defmethod render-text :resolve-subs
  [input]
  (let [info (:resolved input)
        ice (:ice info)
        resolved-subs (:subs info)]
    (str "resolves " (quantify (count resolved-subs) "unbroken subroutine")
         " on " ice
         " (\"[subroutine] "
         (join "\" and \"[subroutine] " resolved-subs)
         "\")")))

(defmethod render-text :approach-server
  [{:keys [server]}]
  (str "approaches " (to-zone-name server)))

(defmethod render-text :breach-server
  [{:keys [server]}]
  (str "breaches " (to-zone-name server)))

(defmethod render-text :access
  [{:keys [card server]}]
  ;; TODO review this for any cleanup, it shouldn't be needed with card maps anymore
  (str "accesses " (or card
                       (if (or (= server [:deck]) (= server ["deck"]))
                         "an unseen card"
                         "a card"))
       " from " (to-zone-name server)))

(defmethod render-text :access-all
  [_]
  "accesses everything else in Archives")

(defmethod render-text :trash
  [{:keys [card server]}]
  (str "trashes " (render-card card)
       (when (string? card) (when server (str " from " (to-zone-name server))))))

(defmethod render-text :take-damage
  [{:keys [cards cause]}]
  (str "trashes " (enumerate-str cards) " due to "
       (case (keyword cause)
         :net "net damage"
         :meat "meat damage"
         :brain "core damage")))

(defmethod render-text :rfg
  [{:keys [card]}]
  (str "removes " card " from the game"))

(defmethod render-text :discard
  [{:keys [card side reason]}]
  (let [not-map (or (string? card) (number? card) (coll? card))]
    (str "discards "
         (cond
           (string? card) card
           (number? card) (quantify card "card")
           (coll? card) (enumerate-str card)
           true (render-card card))
         (when not-map (str " from " (to-zone-name [:hand] side)))
         (when reason
           ;; TODO only end of turn is supported here, so...
           " at end of turn"))))

(defmethod render-text :increase-trace-link
  [{:keys [strength side]}]
  (str "increase " (if (= (keyword side) :corp) "trace" "link") " strength to " strength))

(defmethod render-text :win-reason
  [{:keys [cause]}]
  (case (keyword cause)
    :concession "concedes"
    :decked "is decked"
    :flatline "is flatlined"))

(defmethod render-text :win-game
  [_]
  "wins the game")

(defmethod render-text :direct-effect
  [{:keys [effect]}]
  ;; doesn't quite work yet, prevents doubling at least but there's a stray ' to'
  ;(render-effect effect)
  )

(defmethod render-text :fire-unbroken
  [{:keys [card]}]
  (str "indicates to fire all unbroken subroutines on " card))

(defmethod render-text :use-command
  [{:keys [command]}]
  (str "uses a command: " command))

(defmethod render-text :raw-text
  [input]
  (:raw-text input))

;; TODO still experimental
(defmethod render-text :force
  [{:keys [card side]}]
  (str "satisfy " card))

(defmethod render-text :default
  [input]
  (str "unknown type " input))

(defmethod render-map "en"
  [_ {:keys [username raw-text cost effect urgent] :as input}]
  (println input)
  (try-catchall
    (let [cost-str (render-cost-str input)
          effect-str (render-effect-str input)]
      (let [output (str (when urgent "[!]")
                        (if username
                          (str username " " cost-str (render-text input) effect-str ".")
                          raw-text))]
        (println output)
        output))
    (catch e# ::exception
      #_(throw e#)
      (str "BUG" (pprint-to-string input)))))

#_(defmethod render-map "en"
  [_ input]
  (str input))
