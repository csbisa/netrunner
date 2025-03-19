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
       :servers
       (case server
         :hq (if (= side :corp) "HQ" "the Grip")
         :rd (if (= side :corp) "R&D" "the Stack")
         :archives (if (= side :corp) "Archives" "the Heap")
         (str "Server " (last (split (str server) #":remote"))))
       (str "unhandled server " location)))))

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
        " (" (enumerate-str (map #(if (string? %) % (render-card %)) cards)) ")" suffix)))

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
      ;; TODO not sure if this makes sense. should be number and never revealed?
      :add-random-from-hand-to-bottom-of-deck (str "adds " (quantify (count value) "random card") (str " from " hand " to the bottom of " deck))
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
       (enumerate-str (for [[c v] (first cost)] (render-single-cost c v side)))
       ", and then "
       (enumerate-str (for [[c v] (second cost)] (render-single-cost c v side)))
       ",")
      (enumerate-str (for [[c v] cost] (render-single-cost c v side))))))

(defn render-cost-str
  [{:keys [cost side]}]
  (when-not (empty? cost)
    (str (render-cost cost side)
         ;; TODO remove in order to do cost test
         ;;" to "
         " to "
         )))

;; ? {:username Runner, :type :use, :cost {:click 1}, :effect {:make-run HQ}, :card Red Team, :forced false, :raw-text nil}
;; ? Runner spends [Click] to use Red Team to  to make a run on unknown server HQ.
(defn- render-single-effect
  [effect value side]
  (when-not (and (number? value) (zero? value))
    (case effect
      :advance (str "advance " (render-card value))
      :draw-cards (str "draw " (quantify value "card"))
      :gain-credits (str "gain " value " [Credits]")
      :gain-click (str "gain " (apply str (repeat value "[Click]")))
      :lose-click (str "lose " (apply str (repeat value "[Click]")))
      ;; TODO ideally part of force logic, but the problem is that these are effects
      ;; either bring back the silly keyword logic or just support the few cases that exist
      :lose-click-force (str "force the " (if (= (keyword side) :corp) "Runner" "Corp")" to lose " (apply str (repeat value "[Click]")))
      :lose-credits-force (str "force the " (if (= (keyword side) :corp) "Runner" "Corp")" to lose " value " [Credits]")
      :give-tag (str "give the Runner " (quantify value "tag"))
      :take-tag (str "take " (quantify value "tag"))
      :remove-tag (str "remove " (quantify value "tag"))
      :take-bp (str "take " value " bad publicity")
      ;; TODO this is a clusterfuck, figure out how to unify it later
      :add-from-stack (str "add " value " from the stack to the grip and shuffle the stack")
      :add-from-rnd (str  "reveal " value " from R&D and add it to HQ")
      ;; TODO this is probably the best generic mechanism, need to squash it with several others here
      :add-card (let [[card from to] value]
                  (str "add " (or card "itself")
                       (when from) (str " from " (to-zone-name from))
                       (when to) (str " to " (to-zone-name to))))
      :add-to-hq (str "add " (render-card value) " to HQ")
      ;; TODO not sure if this should be string or rendered card
      :add-to-grip (str "add " (render-card value) " to the Grip")
      :add-to-hq-unseen (str "add " (quantify value "card") " to HQ")
      ;; TODO making this add would be more consistent? Working Prototype uses "add", ??? uses "move"
      :move-to-top-stack (str "move " (or value "them") " to the top of the Stack")
      :shuffle-rnd (str "shuffle R&D")
      :reveal-and-add (let [[card from to] value]
                        (str "add " (or card "itself") " from " (to-zone-name from) " to " (to-zone-name to)))
      :reveal-from-hq (str "reveal " (enumerate-str value) " from HQ")
      :make-run (str "make a run on " (to-zone-name value))
      :end-run "end the run"
      ;; TODO probably need a duration here, others are encounter-only IIRC
      :gain-type (let [[card type] value] (str "make " card " gain " (enumerate-str type) " until the end of the run"))
      :place-counter (let [[type count target] value]
                       (str "place "
                            (if (or (= (keyword type) :credit) (= (keyword type) :credits))
                              (str count " [Credits]")
                              (quantify count (to-counter type)))
                            " on "
                            (if target (render-card target) "itself")))
      :remove-counter (let [[type count target] value]
                       (str "remove "
                            (quantify count (to-counter type))
                            " from "
                            (if target (render-card target) "itself")))
      :move-counter (let [[type count source target] value]
                      (str "move "
                           (quantify count (to-counter type))
                           " from "
                           (if source (render-card source) "itself")
                           " to "
                           (if target (render-card target) "itself")))
      ;; TODO need to fix hq/blah
      :trash-from-hand (if (int? value)
                         (str "trashes " (quantify value "card") " from " "HQ")
                         (render-card-list value "trashes" "card" "" (str " from " "HQ")))
      :add-str (let [[card count] value] (str "add " count " strength to " card))
      :reduce-str (let [[card count] value] (str "give -" count " strength to " (render-card card) " for the remainder of the encounter"))
      ;; TODO combine hq/rnd?
      :access-additional-from-hq (str "access " (quantify value "additional card") " from HQ")
      :access-additional-from-rnd (str "access " (quantify value "additional card") " from R&D")
      ;; TODO similar, combine?
      :deal-net (str "deal " value " net damage")
      :deal-meat (str "deal " value " meat damage")
      :deal-core (str "deal " value " core damage")
      :install (str "install " value)
      :rez (str "rez " (render-card value))
      :install-and-rez-free (str "install and rez " value ", ignoring all costs")
      :host (str "host " (render-card value))
      :host-on (str "host " (render-card (first value)) " on " (render-card (second value)))
      :bypass (str "bypass " (render-card value))
      :trash-free (str "trash " value " at no cost")
      :str-pump (let [[base-str target-str duration] value]
                  (str "increase its strength from " base-str " to " target-str (to-duration duration)))
      :lower-ice-str (let [[strength card] value]
                       (str "lower the strength of "
                            (or card "each installed icebreaker")
                            " by " strength))
      :shuffle-into-rnd (str "shuffle " (render-card2 value) " into R&D")
      :shuffle-from-hq-into-rnd (str "shuffle " (quantify value "card") " from HQ into R&D")
      :rearrange-rnd (str "rearrange the top " (quantify value "card") " of R&D")
      :reveal-from-rnd (str "reveal " value " from the top of R&D")
      :look-top-rnd (str "look at the top " (quantify value "card") " of R&D")
      :move-hq-rnd (str "add " (quantify value "card") " from HQ to to the top of R&D")
      :play (str "play " value)
      :move-server (let [[server card] value]
                     (str "move " (or card "itself") " to " (to-zone-name server)))
      :prevent-access (let [[type card] value]
                        (str "prevent the runner from accessing "
                             (case (keyword type)
                               :target card
                               :exclusive (str "cards other than " card))))
      :trash-stack (str "trash " (enumerate-str value) " from the top of the stack")
      :prevent-net (str "prevent " value " net damage")
      :prevent-encounter-ability (let [[card ability] value]
                                   (str "prevent the encounter ability on " card (when ability (str " (" ability ")"))))
      :prevent-etr (str "prevent " (render-card value) " from ending the run this encounter")
      :gain-str (let [[strength duration] value] (str "gain " strength " strength" (to-duration duration)))
      :breach-server (str "breach " (to-zone-name value))
      :derez (str "derez "
                  (if (coll? value)
                    (enumerate-str (map render-card value))
                    (render-card value)))
      :rez-free (str "rez " (enumerate-str value) ", ignoring all costs")
      :encounter-ice (str "make the Runner encounter " (render-card value))
      :reveal-self (str "reveal itself from " (to-zone-name value))
      :add-from-hq-to-score (str "add " value " from HQ to [their] score area")
      :turn-faceup (str "turn " value " in Archives faceup")
      :add-self-to-hq (str "add itself to HQ")
      :trash (str "trash " (render-card value))
      ;; TODO this needs a duration?
      :add-str-new (let [[card count] value] (str "give " (render-card card) " +" count " strength"))
      ;; TODO could spruce this up but it follows current thunderbolt format
      :add-sub (str "add " value " after its other subroutines")
      :trash-rnd (str "trash the top " (quantify value "card") " of R&D")
      :remove-click-next-turn (str "give the Runner -" value " allotted [Click] for [their] next turn")
      :move-grip-to-stack (str "add " (enumerate-str value) " from the Grip to the top of the Stack")
      :shuffle-into-stack (str "shuffle " (or value "them") " into the stack")
      :remove-all-virus-counters (str "remove all virus counters from " (render-card value))
      :trash-from-hq (str "trash " value " from HQ")
      :reveal-from-grip (str "reveal " (enumerate-str value) " from the Grip")
      :add-to-top-rnd (str "add " value " to the top of R&D")
      ;; so extend this to render cards, and render cards using other sources too?
      :add-to-bottom-rnd (str "add " (render-card value) " to the bottom of R&D")
      :force-reveal (str "reveal " (quantify value "random card") " from HQ")
      :shuffle-zone-into (str "shuffle " (enumerate-str (map to-zone-name value)) " into " (to-zone-name [:deck]))
      :rfg (str "remove " (enumerate-str value) " from the game")
      :reveal-from-stack (str "reveal " (enumerate-str value) " from the top of the stack")
      :host-on-self (str "host " value " on itself")
      :host-instead-of-access (str "host " value " on itself instead of accessing it")
      :shuffle-stack (str "shuffle the stack")
      :trash-self (str "trash itself")
      :credits (str "pay " value " [Credits]")
      :draw-additional (str "draw " (quantify value "additional card"))
      :purge "purge virus counters"
      :reveal (let [groups (group-by :server value)]
                (str "to reveal "
                     (enumerate-str (map #(str (enumerate-str (map :card (second %)))
                                               " from " (to-zone-name (first %)))
                                         groups))))
      :choose-server (str "target " (to-zone-name value))
      :choose-subtype (str "choose " value)
      :choose-ice (str "choose " (render-card value))
      :add-to-score (let [[card kind points] value]
                      (str "add " (or card "itself") " to the score area"
                           (when points
                             (str " as an "
                                  (when (= (keyword kind) :assassination) "assassination ")
                                  "agenda worth " (quantify points "agenda point")))))
      ;; TODO
      :swap-ice-from-hand (str "swap " (render-card value) " with a piece of ice from HQ")
      :swap-ice (throw "foo"))))

(defn render-single-effect-force-check
  [effect value side forced]
  (str
   (when forced
     (str "force the "
          ;; This is inverted -- corp forcing effect means it's forcing runner to take the effect.
          (if (= (keyword side) :corp) "Runner" "Corp")
          " to "))
   (render-single-effect (keyword effect) value side)))

(defn render-effect
  [effects side]
  (when effects
    (enumerate-str (remove nil? (for [[c v] effects]
                                  (render-single-effect-force-check c v side forced))))))

(defn render-effect-str
  [{:keys [effect side]}]
  (when-not (empty? effect)
    (str " to " (render-effect effect side))))

(defmulti render-text (fn [input] (or (keyword (:type input)) :raw-text)))

(defmethod render-text :create-game [_] "has created the game")
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
  [{:keys [card cost]}]
  (let [cost-spend-msg (build-spend-msg-suffix cost "play")]
    (str cost-spend-msg card)))

(defn- cost-discount-str
  [{:keys [ignore-all-costs ignore-install-costs cost-bonus alternative-cost]}]
  (cond
    ignore-all-costs " (ignoring all costs))"
    ignore-install-costs " (ignoring its install cost)"
    alternative-cost " (by paying its alternative cost)"
    (and cost-bonus (pos? cost-bonus)) (str " (paying " cost-bonus " [Credits] more)")
    (and cost-bonus (neg? cost-bonus)) (str " (paying " (* -1  cost-bonus) " [Credits] less)")))

;; TODO this should probably be squashed with render-card-internal somehow
;; TODO need to handle the "... as a facedown card" logic. i don't think 'unseen' here is ever used
(defmethod render-text :install
  [{:keys [card card-type server new-remote origin install-source cost host hosted side ignore-all-costs ignore-install-costs cost-bonus no-cost] :as input}]
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
  [{:keys [card points]}]
  (str "scores " card " and gains " points " agenda points"))

(defmethod render-text :steal
  [{:keys [card points]}]
  (str "steals " card " and gains " points " agenda points"))

(defmethod render-text :start-run
  [{:keys [server ignore-costs cost]}]
  (str (if cost "make " "makes ")
       "a run on " (to-zone-name server)
       (when ignore-costs ", ignoring all costs")))

(defmethod render-text :continue-run
  [_]
  (str "will continue the run"))

(defmethod render-text :jack-out
  [input]
  ;; TODO also jack/jacks here
  (str (if (:cost input) "jack" "jacks") " out"))

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
           ;; N.B. a space is included in the passed down subtype currently...
           (quantify sub-count (str subtype "subroutine")))
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

;; TODO need to support "everything else in archives"
(defmethod render-text :access
  [{:keys [card server]}]
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
      (throw e#)
      #_(str "BUG" (pprint-to-string input)))))

#_(defmethod render-map "en"
  [_ input]
  (str input))
