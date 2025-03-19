(ns game.core.msg_schema
  (:require
   [malli.core :as m]))

#_(require '[malli.dev])
#_(malli.dev/start!)

(def Card
  :string)

(def strength number?)
(def duration [:enum {:title "duration"} :end-of-encounter :end-of-run :end-of-turn])
(def counter-type [:enum :adv :virus :power :credit :credits])
(def server [:or :keyword [:vector :keyword]])
(def side [:enum :corp :runner])
(def card-map :map)
;; TODO fix this
#_(def card-map
    [:schema
     {:registry
      {::card-map
       [:map
        [:card {:optional true} :string]
        [:card-type {:optional true} [:enum :facedown :ice :card :drawn-card]]
        [:hosted {:optional true} [:ref ::card-map]]
        [:server {:optional true} server]
        [:pos {:optional true} number?]]}}])
(def card
  [:maybe ;; Self-references use nil so accept this too.
   [:or
    :string
    card-map]])
(def card-list [:or [:vector card] [:sequential card]])
(def maybe-unseen-cards [:vector [:or string? [:enum :unseen]]])
(def count-or-card-list [:or number? card-list])

;; TODO any :sequential here is just a workaround, it's because something is passing down
;; a list instead of a vector
(def MapCost
  [:maybe
   [:multi {:dispatch first}
    [:click [:tuple keyword? number?]]
    [:lose-click [:tuple keyword? number?]]
    [:credits [:tuple keyword? [:or number? [:map [:cards [:sequential [:tuple string? number?]]]]]]]
    [:trash [:tuple keyword? card]]
    [:forfeit [:tuple keyword? count-or-card-list]]
    [:gain-tag [:tuple keyword? number?]]
    [:tag [:tuple keyword? number?]]
    [:bad-pub [:tuple keyword? number?]]
    [:return-to-hand [:tuple keyword? number?]]
    [:remove-from-game [:tuple keyword? card]]
    [:rfg-program [:tuple keyword? card-list]]
    [:trash-installed [:tuple keyword? card-list]]
    [:hardware [:tuple keyword? card-list]]
    [:derez [:tuple keyword? card-list]]
    [:program [:tuple keyword? card-list]]
    [:resource [:tuple keyword? card-list]]
    [:connection [:tuple keyword? card-list]]
    [:ice [:tuple keyword? card-list]]
    [:trash-from-deck [:tuple keyword? count-or-card-list]]
    [:trash-from-hand [:tuple keyword? count-or-card-list]]
    [:randomly-trash-from-hand [:tuple keyword? count-or-card-list]]
    [:trash-entire-hand [:tuple keyword? count-or-card-list]]
    [:trash-hardware-from-hand [:tuple keyword? card-list]]
    [:trash-program-from-hand [:tuple keyword? card-list]]
    [:trash-resource-from-hand [:tuple keyword? card-list]]
    [:take-net [:tuple keyword? number?]]
    [:take-meat [:tuple keyword? number?]]
    [:take-core [:tuple keyword? number?]]
    [:shuffle-installed-to-stack [:tuple keyword? card-list]]
    [:add-installed-to-bottom-of-deck [:tuple keyword? card-list]]
    [:add-random-from-hand-to-bottom-of-deck [:tuple keyword? number?]]
    [:agenda-counter [:cat keyword? [:+ [:tuple card number?]]]]
    [:virus [:cat keyword? [:+ [:tuple card number?]]]]
    [:advancement [:cat keyword? [:+ [:tuple card number?]]]]
    [:power [:cat keyword? [:+ [:tuple card number?]]]]
    [:turn-hosted-matryoshka-facedown [:tuple keyword? number?]]]])

(def MapCosts
  [:vector MapCost])

(def MapEffect
  [:multi {:dispatch first ;:error/message {:en "unknown value in :type"}
           :error/fn {:en (fn [{:keys [value]} _] (str (first value) " is not a valid effect"))}}
   [:advance [:tuple keyword? card]]
   [:draw-cards [:tuple keyword? number?]]
   [:gain-credits [:tuple keyword? number?]]
   [:gain-click [:tuple keyword? number?]]
   [:lose-click [:tuple keyword? number?]]
   [:lose-click-force [:tuple keyword? number?]]
   [:lose-credits-force [:tuple keyword? number?]]
   [:give-tag [:tuple keyword? number?]]
   [:take-tag [:tuple keyword? number?]]
   [:remove-tag [:tuple keyword? number?]]
   [:take-bp [:tuple keyword? number?]]
   [:add-from-stack [:tuple keyword? string?]]
   [:add-from-rnd [:tuple keyword? string?]]
   [:add-card [:tuple keyword? card server server]] ;; TODO optional + nils?
   [:add-to-hq [:tuple keyword? card]]
   [:add-to-grip [:tuple keyword? card]]
   [:add-to-hq-unseen [:tuple keyword? number?]]
   [:move-to-top-stack [:tuple keyword? card]]
   [:shuffle-rnd [:tuple keyword?]]
   [:reveal-and-add [:tuple keyword? card server server]]
   [:reveal-from-hq [:tuple keyword? card-list]]
   [:make-run [:tuple keyword? server]]
   [:end-run [:tuple keyword? boolean?]]
   [:gain-type [:tuple keyword? string? [:vector string?]]]
   [:place-counter [:or [:tuple keyword? counter-type number?]
                    [:tuple keyword? counter-type number? card]]]
   [:remove-counter [:tuple keyword? counter-type number? card]]
   [:move-counter [:tuple keyword? counter-type number? card]]
   [:trash-from-hand [:tuple keyword? [:or number? card-list]]]
   [:add-str [:tuple keyword? card number?]]
   [:reduce-str [:tuple keyword? card number?]]
   [:access-additional-from-hq [:tuple keyword? number?]]
   [:access-additional-from-rnd [:tuple keyword? number?]]
   [:deal-net [:tuple keyword? number?]]
   [:deal-meat [:tuple keyword? number?]]
   [:deal-core [:tuple keyword? number?]]
   [:install [:tuple keyword? string?]]
   [:rez [:tuple keyword? card]]
   [:install-and-rez-free [:tuple keyword? string?]]
   [:host [:tuple keyword? card]]
   [:host-on [:tuple keyword? card card]]
   [:bypass [:tuple keyword? card]]
   [:trash-free [:tuple keyword? string?]]
   [:str-pump [:tuple keyword? [:int {:title "from-str"}] [:int {:title "to-str"}] duration]]
   [:lower-ice-str [:tuple keyword? number? card]]
   [:shuffle-into-rnd [:tuple keyword? maybe-unseen-cards]]
   [:rearrange-rnd [:tuple keyword? number?]]
   [:reveal-from-rnd [:tuple keyword? number?]]
   [:look-top-rnd [:tuple keyword? number?]]
   [:move-hq-rnd [:tuple keyword? number?]]
   [:play [:tuple keyword? string?]]
   [:move-server [:tuple keyword? server card]]
   [:prevent-access [:tuple keyword? [:enum :target :exclusive] card]]
   [:trash-stack [:tuple keyword? number?]]
   [:prevent-net [:tuple keyword? number?]]
   [:prevent-encounter-ability [:tuple keyword? card string?]]
   [:prevent-etr [:tuple keyword? card]]
   [:gain-str [:tuple keyword? number? duration]]
   [:breach-server [:tuple keyword? server]]
   [:derez [:tuple keyword? [:or card card-list]]]
   [:rez-free [:tuple keyword? card-list]]
   [:encounter-ice [:tuple keyword? card]]
   [:reveal-self [:tuple keyword? server]]
   [:add-from-hq-to-score [:tuple keyword? string?]]
   [:turn-faceup [:tuple keyword? string?]]
   [:add-self-to-hq [:tuple keyword?]]
   [:trash [:tuple keyword? card]]
   [:add-str-new [:tuple keyword? card number?]]
   [:add-sub [:tuple keyword? string?]]
   [:trash-rnd [:tuple keyword? number?]]
   [:remove-click-next-turn [:tuple keyword? number?]]
   [:move-grip-to-stack [:tuple keyword? card-list]]
   [:shuffle-into-stack [:tuple keyword? card-list]]
   [:remove-all-virus-counters [:tuple keyword? card]]
   [:trash-from-hq [:tuple keyword? string?]]
   [:reveal-from-grip [:tuple keyword? card-list]]
   [:add-to-top-rnd [:tuple keyword? string?]]
   [:add-to-bottom-rnd [:tuple keyword? card]]
   [:force-reveal [:tuple keyword? number?]]
   [:shuffle-zone-into [:tuple keyword? server server]]
   [:rfg [:tuple keyword? card-list]]
   [:reveal-from-stack [:tuple keyword? card-list]]
   [:host-on-self [:tuple keyword? string?]]
   [:host-instead-of-access [:tuple keyword? string?]]
   [:shuffle-stack [:tuple keyword?]]
   [:trash-self [:tuple keyword?]]
   [:credits [:tuple keyword? number?]]
   [:draw-additional [:tuple keyword? number?]]
   [:purge [:tuple keyword?]]
   [:reveal [:tuple keyword? [:+ card]]]
   [:choose-server [:tuple keyword? server]]
   [:choose-subtype [:tuple keyword? string?]]
   [:choose-ice [:tuple keyword? card]]
   [:add-to-score [:tuple keyword? card [:enum :assassination nil] number?]]
   [:swap-ice-from-hand [:tuple keyword? card]]
   ])

(def MapEffects
  [:vector MapEffect])

;; TODO string is only to deal with empty string which still comes down in some cases
;; can be removed after chasing down whatever's causing that
(def costs [:cost {:optional true} [:or MapCosts string?]])
(def effects [:effect {:optional true} MapEffects])

#_(def cost-effect-map
    [:map [:type keyword?]])
(def MapMsg
  [:multi {:dispatch :type ;:error/message {:en "unknown value in :type"}
           :error/fn {:en (fn [{:keys [value]} _] (str (:type value) " is not a valid type"))}}
   [:create-game [:map [:type keyword?]]]
   [:keep-hand [:map [:type keyword?]]]
   [:mulligan-hand [:map [:type keyword?]]]
   [:mandatory-draw [:map [:type keyword?]]]
   [:no-action [:map [:type keyword?]]]
   [:turn-state [:map [:type keyword?]]] ;; TODO args
   [:play [:map [:type keyword?] costs [:card string?]]]
   [:install [:map [:type keyword?] [:card {:optional true} [:or string? nil?]]
              [:card-type {:optional true} [:or nil? [:enum :ice :facedown :unknown]]]
              [:server {:optional true} server] [:new-remote {:optional true} boolean?]
              [:origin {:optional true} server] [:install-source {:optional true} string?]
              costs [:host {:optional true} card] [:hosted {:optional true} boolean?]
              [:side {:optional true} side] [:ignore-all-costs {:optional true} boolean?]
              [:ignore-install-costs {:optional true} boolean?]
              [:cost-bonus {:optional true} number?] [:no-cost  {:optional true} boolean?]]]
   [:rez [:map [:type keyword?] [:card card] [:alternative-cost {:optional true} boolean?]
          [:ignore-cost {:optional true} boolean?] [:cost-bonus {:optional true} number?]
          [:rez-source {:optional true} string?] costs]]
   [:use [:map [:type keyword?] [:card card] costs effects]]
   [:advance [:map [:type keyword?] [:card card]]]
   [:score [:map [:type keyword?] [:card string?] [:points number?]]]
   [:steal [:map [:type keyword?] [:card string?] [:points number?]]]
   [:start-run [:map [:type keyword?]]]
   [:continue-run [:map [:type keyword?]]]
   [:jack-out [:map [:type keyword?]]]
   [:approach-ice [:map [:type keyword?]]]
   [:bypass-ice [:map [:type keyword?]]]
   [:encounter-ice [:map [:type keyword?]]]
   [:encounter-effect [:map [:type keyword?]]]
   [:pass-ice [:map [:type keyword?]]]
   [:break-subs [:map [:type keyword?]]]
   [:str-boost [:map [:type keyword?]]]
   [:resolve-subs [:map [:type keyword?]]]
   [:approach-server [:map [:type keyword?] [:server server]]]
   [:breach-server [:map [:type keyword?] [:server server]]]
   [:access [:map [:type keyword?]]]
   [:access-all [:map [:type keyword?]]]
   [:trash [:map [:type keyword?]]]
   [:take-damage [:map [:type keyword?]]]
   [:rfg [:map [:type keyword?]]]
   [:discard [:map [:type keyword?]]]
   [:increase-trace-link [:map [:type keyword?]]]
   [:win-game [:map [:type keyword?]]]
   [:direct-effect [:map [:type keyword?]]]
   [:fire-unbroken [:map [:type keyword?]]]
   [:use-command [:map [:type keyword?]]]
   ;; TODO default for testing for now
   ;; okay, so maybe we won't have a type, in which case it's *just* raw-text
   #_[::m/default [:map [:type keyword?]]]
   [::m/default [:or [:map [:raw-text string?]]
                 [:map [:cost MapCosts]]]]])

(def MapMsgOrString
  [:or MapMsg :string])

;; TESTS from here on out
(require '[malli.error :as me])

(and
 (m/validate MapMsg {:type :use :cost nil})
 (m/validate MapMsg {:type :access-all :hello nil}))

(let [text {:type :access-alld :hello nil}]
  (when-not (m/validate MapMsgOrString text)
    (me/humanize (m/explain MapMsgOrString text))
    #_(throw (Exception. (ex-info (-> MapMsgOrString (m/explain text) (me/humanize))
                                  {:input text})))))

(me/humanize (m/explain MapCosts (m/validate MapCosts [[:credits 1 #_{:cards [["Cold Read" 1]]}]])))
(m/validate MapCosts [[:credits {:cards (list ["Cold Read" 1])}]])

(and
 (m/validate MapEffects [[:str-pump 3 4 :end-of-turn]])
 (m/validate MapEffects [[:host-instead-of-access "bar"]])
 (m/validate MapEffects [[:str-pump 3 4 :end-of-turn] [:host-instead-of-access "bar"]])
 )

;; just showing we can shove map through seq and have the same effect
;; it might be too ambiguous, some of them are vectors that should be squashed, others should not
(and
 (m/validate MapEffects (apply vector (seq {:str-pump [3 4 :end-of-turn]})))
 (m/validate MapEffects (apply vector (seq {:host-instead-of-access "bar"})))
 )

(and
 (m/validate MapCosts [[:click 1]])
 (m/validate MapCosts [[:rfg-program ["foo"]]])
 (m/validate MapCosts [[:trash-from-deck ["foo"]]])
 (m/validate MapCosts [[:trash-from-deck 5]])
 )

(-> MapMsg
    (m/explain {:type :use :cost nil})
    (me/humanize))

(m/explain MapMsg {:type :use :card "foo" :cost [[:click nil]]})
(me/humanize (m/explain MapMsg {:type :use :card "foo" :cost [[:click nil]]}))

(require '[malli.instrument :as mi])
(mi/collect!)
(m/function-schemas)
(mi/instrument!)
