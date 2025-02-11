(ns i18n.ja
  (:require
   [clojure.string :refer [join split starts-with?] :as s]
   [i18n.defs :refer [render-map try-catchall pprint-to-string] :include-macros true]))

;; so...
;; <card>から<value>[Credits]を支払って
;; card can be クレジットプール
(defn render-credits
  [value]
  (println value)
  (if (map? value)
    (let [remainder-str (when-let [remainder (:pool value)]
                          (str remainder " [Credits]"))
          card-strs (when-let [cards (:cards value)]
                      (join "" (map #(str (first %) "から" (second %) " [Credits]を支払って")
                                    cards)))
          message (str card-strs
                       (when (and card-strs remainder-str)
                         "クレジットプールから")
                       remainder-str
                       (when (and card-strs remainder-str)
                         "支払って"))]
      message)
    (str value " [Credits]を支払って")))

(defn to-zone-name
  ([zone] (to-zone-name zone :corp))
  ([[location server placement] side]
   (let [location (keyword location)
         server (keyword server)
         side (keyword side)]
     (case location
       :hand (if (= side :corp) "ＨＱ" "グリップ")
       :deck (if (= side :corp) "Ｒ＆Ｄ" "スタック")
       :discard (if (= side :corp) "アーカイブ" "ヒープ")
       :servers
       (case server
         :hq "HQ"
         :rd "R&D"
         :archives "アーカイブ"
         (str "サーバー" (last (split (str server) #":remote"))))
       (str "unhandled server " location)))))

(defn- to-duration
  [duration]
  (case (keyword duration)
    :end-of-run " ランの終了まで"
    :end-of-turn "ターンの終了まで"
    ""))

(defn- to-counter
  [counter]
  (case (keyword counter)
    :adv "アドバンスカウンター"
    :virus "ウィルスカウンター"
    :power "パワーカウンター"
    ;; might do place-credits instead, or split this out
    :credit " [Credits]"
    ;; TODO stopgap
    :credits " [Credit]"))

; {:username "test", :type "use", :cost {:click 1, :credits 1}, :effect {:advance {:card-type "card", :server "remote1"}}, :card "Corp Basic Action Card", :forced false, :raw-text nil}
(defn- render-card
  [{:keys [card card-type server pos hosted]}]
  (str (if hosted
         (str (render-card hosted) "に搭載される")
         (if server
           (if (not (nil? pos))
             (str (to-zone-name server) "を位置" pos "に守っている")
             (str (to-zone-name server) "に"))
           ""))
       (if-not (empty? card)
         card
         (case (keyword card-type)
           :facedown "裏向きのカード"
           :ice "アイス"
           :card "カード"))))

; need to figure how how to combine them from here
(defn- render-single-cost
  [cost value last]
  (let [hand (to-zone-name [:hand] :corp)
        deck (to-zone-name [:deck] :corp)]
    (case cost
      :click (str (apply str (repeat value "[Click]")) "を消費" (if last "する" "して"))
      :lose-click (str (apply str (repeat value "[Click]")) "を失って")
      :credits (render-credits value)
      :trash (str value "をトラッシュして")
      :forfeit (if (string? value)
                 (str value "を放棄して")
                 (str (count value) "枚の計画書を破棄して (" (join "と" value) ")"))
      :gain-tag (str "タグを" value "つ受けて")
      :tag (str "タグを" value "つ取り除いて")
      :bad-pub (str "悪名を" value "つ受けて")
      :return-to-hand (str hand "に" value "を" "加えて")
      :remove-from-game (str value "を取り除いて")
      :rfg-program (str "インストール状態のプログラムを" (count value) "つ取り除いて (" (join "と" value) ")")
      :trash-installed (str "インストール状態のカードを" (count value) "つトラッシュして (" (join "と" value) ")")
      :hardware (str "つインストール状態のハードウェアを" (count value) "つトラッシュして (" (join "と" value) ")")
      :derez (str (count value) "つカードをデレゾして (" (join "と" value) ")")
      :program (str (count value) "つインストール状態のプログラムをトラッシュして (" (join "と" value) ")")
      :resource (str (count value) "つインストール状態のリソースをトラッシュして (" (join "と" value) ")")
      :connection (str (count value) "つインストール状態のコネをトラッシュして (" (join "と" value) ")")
      ;; TODO missing 'rezzed'
      :ice (str (count value) "つインストール状態のアイスをトラッシュして (" (join "と" value) ")")
      :trash-from-deck (str deck "の一番上から" value "枚のカードからをトラッシュして")
      :trash-from-hand (if (int? value)
                         (str hand "から" value "枚のカードをトラッシュして")
                         (str hand "から" (count value) "枚のカードをトラッシュして (" (join "と" value) ")"))
      :randomly-trash-from-hand (str hand "から" value "枚ンダムにトラッシュして")
      :trash-entire-hand (if (int? value)
                           (str hand "の" value "枚のカードをすべてトラッシュして")
                           (str hand "の" value "枚カードをすべてトラッシュして (" (join "と" value) ")"))
      :trash-hardware-from-hand (str hand "から" value "枚のハードウェアをトラッシュして (" (join "と" value) ")")
      :trash-program-from-hand (str hand "から" value "枚のプログラムをトラッシュして (" (join "と" value) ")")
      :trash-resource-from-hand (str hand "から" value "枚のリソースをトラッシュして (" (join "と" value) ")")
      :take-net (str value "ネットダメージを受けて")
      :take-meat (str value "ミートダメージを受けて")
      :take-core (str value "コアダメージを受けて")
      :shuffle-installed-to-stack (str (count value) "枚のカードを" deck "に加えシャフルして (" (join "と" value) ")")
      :add-installed-to-bottom-of-deck (str (count value) "枚のインストール状態のカードを" deck "の一番下に加えて (" (join "と" value) ")")
      ;; TODO not sure if this makes sense. should be number and never revealed?
      :add-random-from-hand-to-bottom-of-deck (str hand "の" (count value) "枚のランダムなカードを" deck "の一番下に加えて")
      :agenda-counter (let [[host count] value]
                        (str host "の搭載計画カウンターを" count "つ消費して"))
      ;; TODO is this a list?
      ;; yes, there's a path where this is a list of title-counts...
      ;; might need a generic mechanism
      :virus (let [[host count] value]
               (str host "の搭載ウィルスカウンターを" count "つ消費して"))
      :advancement (let [[host count] value]
                     (str host "の搭載アドバンスカウンターを" count "つ消費して"))
      :power (let [[host count] value]
               (str host "の搭載パワーカウンターを" count "つ消費して"))
      default cost value))) ; TODO

(defn render-cost
  [cost]
  (when cost
    (join "" (for [[c v] cost] (render-single-cost c v false)))))

(defn- render-single-effect
  [effect value last]
  (when-not (and (number? value) (zero? value))
    (case effect
      :advance (str (render-card value) "をアドバンス" (if last "する" "して"))
      :draw-cards (str value "枚を" (if last "引く" "引いて"))
      :gain-credits (str value " [Credits]を" (if last "得る" "得て"))
      :gain-click (str (apply str (repeat value "[Click]")) "を得る")
      :lose-click (str (apply str (repeat value "[Click]")) "を失う")
      :lose-credits (str value " [Credits]を失う")
      :give-tag (str "ランナーに" value "つタグを与える")
      :take-tag (str value "つタグを受ける")
      :remove-tag (str value "つタグを取り" (if last "除く" "除いて"))
      :take-bp (str "悪名を" value "つ受ける")
      :add-from-stack (str "スタックから" value "をグリップに加えてスタックをシャッフルする")
      :add-from-rnd (str  "R&Dから " value "を公開してHQに加える")
      :add-to-hq (str "HQに" (render-card value) "を加える")
      :add-to-grip (str "グリップに" (render-card value) "を加える")
      :add-to-hq-unseen (str "HQにカードを" value "枚加える")
      :move-to-top-stack (str "スタックの一番上に" value "を加える")
      :shuffle-rnd (str "R&Dをシャフルする")
      :reveal-and-add (let [[card from to] value]
                        (str (to-zone-name from) "から" (to-zone-name to) "に" card "を加える"))
      :reveal-from-hq (str "HQから" (join "と" value) "を公開する")
      :make-run (str (to-zone-name value) "にランする")
      :end-run "ランを終了する"
      ;; TODO probably need a duration here, others are encounter-only IIRC
      :gain-type (let [[card type] value] (str "ランの終了時まで" card "が" type "を得る"))
      :place-counter (let [[type count target] value]
                       ;; TODO ????
                       (str (if target (render-card target) "それ")
                            "に"
                            (to-counter type)
                            "を" count "つ置く"))
      :remove-counter (let [[type count target] value]
                       (str (if target (render-card target) "それ")
                            "から"
                            (to-counter type)
                            "を" count "つ取り除く"))
      :move-counter (let [[type count source target] value]
                      (str (if source (render-card source) "それ")
                           "から"
                           (if target (render-card target) "それ")
                           "に"
                           (to-counter type)
                           "を" count "つ移動する"))
      :add-str (let [[card count] value] (str card "が強度＋" count "する"))
      :reduce-str (str (render-card value) "を強度−１する")
      :access-additional-from-hq (str "HQからの追加で" value "枚のカードにアクセスする")
      :access-additional-from-rnd (str "R&Dからの追加で" value "枚のカードにアクセスする")
      :deal-net (str value "ネットダメージを与える")
      :deal-meat (str value "ミートダメージを与える")
      :deal-core (str value "コアダメージを与える")
      :install (str value "をインストールする")
      :rez (str value "レゾする")
      :install-and-rez-free (str value "をすべてのコストを無視してインストールしてレゾする")
      ;; TODO
      :host (str (render-card value) "をホストする")
      :bypass (str (render-card value) "を迂回する")
      :trash-free (str "無料で" value "をトラッシュする")
      :str-pump (let [[base-str target-str duration] value]
                  (str (to-duration duration) "強度" base-str "から強度" target-str "に" base-str " to " target-str (to-duration duration)))
      ;; TODO ignore for now and figure it out later. wildcat strike
      :force nil
      (str "TODO " effect " " value)))) ; TODO

(defn render-effect
  [effect]
  (when effect
    (println effect)
    (join "" (for [[e v] effect] (render-single-effect e v true)))))

(defmulti render-text (fn [input] (or (keyword (:type input)) :raw-text)))

(defmethod render-text :create-game [_] "ゲームを作りました")
(defmethod render-text :keep-hand [_] "手札をキープする")
(defmethod render-text :mulligan-hand [_] "手札をマリガンする")
(defmethod render-text :mandatory-draw [_] "強制ドローする")

;; TODO
(defmethod render-text :no-action [_] "has no further action TODO")

(defmethod render-text :turn-state
  [input]
  (let [state (:state input)
        phase (:phase state)
        turn (:turn state)
        side (:side state)
        credits (:credits state)
        cards (:cards state)
        phase (if (= (:phase state) "start-turn") "開始" "終了")]
    (str (to-zone-name [:hand] side) "に"
     cards "枚のカードと" credits " [Credits]で" "ターン" turn "目が" phase "する")))

(defmethod render-text :play
  [input]
  (str (:card input) "をプレイする"))

(defmethod render-text :install
  [{:keys [card card-type server new-remote origin install-source cost side]}]
  (let [card-type (keyword card-type)]
    (str (when install-source
           (str install-source "で"))
         (when origin
           (str (to-zone-name origin side) "から"))
         (when server
           (str (when new-remote "新しい") ;; i don't like this
                (to-zone-name server)
                "に"
                (when (= card-type :ice) "守っている")))
         (if (= card-type :ice)
           (str (or card "アイス"))
           (str (or card (if (= card-type :facedown)
                           "未知のカード"
                           "カード"))))
         "をインストールする")))

(defmethod render-text :rez
  [{:keys [card alternative-cost ignore-cost]}]
  ;; TODO :alternative-cost and ignore-cost
  (str (if (string? card) card (render-card card)) "をレゾする"))

(defmethod render-text :use
  [input]
  (str (:card input) "で"))

(defmethod render-text :advance
  [input]
  (str (render-card input) "をアドバンスする"))

(defmethod render-text :score
  [{:keys [card points]}]
  (str card "を得点して計画点" points "を得る"))

(defmethod render-text :steal
  [{:keys [card points]}]
  (str card "を盗んで計画点" points "を得る"))

(defmethod render-text :start-run
  [{:keys [server ignore-costs cost]}]
  (str (when ignore-costs
         "すべてのコストを無視して")
       (to-zone-name server) "にランする"))

(defmethod render-text :continue-run
  [_]
  (str "ランを続ける"))

(defmethod render-text :jack-out
  [_]
  (str "ジャックアウトする"))

(defmethod render-text :approach-ice
  [{:keys [ice]}]
  (str (render-card ice) "にアプローチする"))

(defmethod render-text :bypass-ice
  [{:keys [ice]}]
  (str ice "を迂回する"))

(defmethod render-text :encounter-ice
  [{:keys [ice]}]
  (str (render-card ice) "にエンカウントする"))

;; TODO similar problem... i think the right format is
;; <ice>のエンカウンターのために3 [Credit]を支払う
(defmethod render-text :encounter-effect
  [{:keys [card]}]
  (str "on encountering " card))

(defmethod render-text :pass-ice
  [{:keys [ice]}]
  (str (render-card ice) "を通過する"))

(defmethod render-text :break-subs
  [{:keys [card ice subtype subs break-type sub-count str-boost cost]}]
  ;; TODO str boost
  (str card "で"
       (when str-boost (str card "の強度" str-boost "にして")) ;; TODO wording
       ice "の"
       (when subtype (str subtype "の"))
       "サブルーチンを"
       (case (keyword break-type)
         ;; TODO now inconsistent, english has the number of subs too
         :all "すべて"
         :remaining "remaining subs"   ; TODO
         (str (count subs) "つ"))
       "ブレイクする"
       (when subs
         (str " (\"[subroutine] "
              (join "\" and \"[subroutine] " subs)
              "\")"))))

;; resolves 2 unbroken subroutines on (" Make the Runner lose 3 " and " End the run if the Runner has 6 or less").
;; <ice>の未ブレイクのサブルーチンを解決する (..sub..)
(defmethod render-text :resolve-subs
  [input]
  (let [info (:resolved input)
        ice (:ice info)
        resolved-subs (:subs info)]
    (str ice "の未ブレイクのサブルーチンを" (count resolved-subs) "つ解決する"
         " (\"[subroutine] "
         (join "\"と\"[subroutine] " resolved-subs)
         "\")")))

(defmethod render-text :approach-server
  [{:keys [server]}]
  (str (to-zone-name server) "にアプローチする"))

(defmethod render-text :breach-server
  [{:keys [server]}]
  (str (to-zone-name server) "に侵入する"))

(defmethod render-text :access
  [{:keys [card server]}]
  (str (to-zone-name server)
       "から"
       (or card
           (if (or (= server [:deck]) (= server ["deck"]))
             "未知のカード"
             "カード"))
       "をアクセスする"))

(defmethod render-text :trash
  [{:keys [card server cards]}]
  (str (when server (str (to-zone-name server) "から")) card "をトラッシュする"))

(defmethod render-text :take-damage
  [{:keys [cards cause]}]
  (str (case (keyword cause)
         :net "ネットダメージ"
         :meat "ミートダメージ"
         :brain "コアダメージ")
       "を受けるので" (join "と" cards) "をトラッシュする"))

(defmethod render-text :rfg
  [{:keys [card]}]
  (str  card "を取り除く"))

(defmethod render-text :discard
  [{:keys [card side reason]}]
  (str
   (when reason
    ;; TODO only end of turn is supported here, so...
    "ターンの終了に")
   (to-zone-name [:hand] side) "から"
   (cond
     (string? card) card
     (number? card) (str "カード" card "枚")
     true (join "と" card))
   "を捨てる"))

(defmethod render-text :win-game
  [_]
  "対戦を勝つ")

(defmethod render-text :raw-text
  [input]
  (:raw-text input))

(defmethod render-text :default
  [input]
  (str "unknown type " input))

(defmethod render-map "ja"
  [_ {:keys [username raw-text cost effect] :as input}]
  (println input)
  (try-catchall
    (let [cost-str (render-cost (:cost input))
          effect-str (render-effect (:effect input))]
      (let [output (if username
                     (str username "は" cost-str (render-text input) effect-str "。")
                     raw-text)]
        (println output)
        output))
    (catch e# ::exception (render-map "en" input))))
