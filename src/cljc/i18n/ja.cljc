(ns i18n.ja
  (:require
   [clojure.string :refer [join split starts-with? ends-with?] :as s]
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
    :end-of-run "ランの終了時まで"
    :end-of-turn "ターンの終了時まで"
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
(defn- render-card-internal
  [{:keys [card card-type server pos hosted]}]
  (str (if hosted
         (str (render-card-internal hosted) "に搭載される")
         (if server
           (if (not (nil? pos))
             (str (to-zone-name server) "を位置" pos "を守っている")
             (str (to-zone-name server) "に"))
           ""))
       (if-not (empty? card)
         card
         (case (keyword card-type)
           :facedown "裏向きのカード"
           :ice "アイス"
           :card "カード"
           :drawn-card (str pos "枚目に引いたカード")
           ""))))

(defn- render-card
  [card]
  (cond
    (string? card) card
    (nil? card) "それ自体"
    true (render-card-internal card)))

(defn- render-card2
  [cards]
  (let [unseen (count (filter #(= "unseen" %) cards))
        self (some nil? cards)
        seen (remove nil? (filter #(not (= "unseen" %)) cards))]
    (join "と" (remove nil? (conj seen
                                  (when (pos? unseen) (str "未知カード" unseen "枚"))
                                  (when self "それ自体"))))))

; need to figure how how to combine them from here
(defn- render-single-cost
  [cost value]
  (let [hand (to-zone-name [:hand] :corp)
        deck (to-zone-name [:deck] :corp)]
    (case cost
      :click (str (apply str (repeat value "[Click]")) "を消費して")
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
      :rfg-program (str "インストール状態のプログラムを" (count value) "つ取り除いて (" (join "と" (map render-card value)) ")")
      :trash-installed (str "インストール状態のカードを" (count value) "つトラッシュして (" (join "と" (map render-card value)) ")")
      :hardware (str "インストール状態のハードウェアを" (count value) "つトラッシュして (" (join "と" (map render-card value)) ")")
      ;; TODO this is only harmonic in costs... is something else using this?
      :derez (str (count value) "つカードをデレゾして (" (join "と" (map render-card value)) ")")
      :program (str (count value) "つインストール状態のプログラムをトラッシュして (" (join "と" (map render-card value)) ")")
      :resource (str (count value) "つインストール状態のリソースをトラッシュして (" (join "と" (map render-card value)) ")")
      :connection (str (count value) "つインストール状態のコネをトラッシュして (" (join "と" (map render-card value)) ")")
      :ice (str (count value) "つインストールとレゾ状態のアイスをトラッシュして (" (join "と" (map render-card value)) ")")
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
      :add-installed-to-bottom-of-deck (str (count value) "枚のインストール状態のカードを" deck "の一番下に加えて (" (join "と" (map render-card value)) ")")
      ;; TODO card name
      :turn-hosted-matryoshka-facedown (str "搭載されたMatryoshkaの" value "枚を裏向きにする")
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
               (str host "の搭載パワーカウンターを" count "つ消費して")))))

(defn render-cost
  [cost]
  (when cost
    (join "" (for [[c v] cost] (render-single-cost c v)))))

(defn- render-single-effect
  [effect value side]
  (when-not (and (number? value) (zero? value))
    (case effect
      :advance (str (render-card value) "をアドバンスする")
      :draw-cards (str "カード" value "枚を引く")
      :gain-credits (str value " [Credits]を得る")
      :gain-click (str (apply str (repeat value "[Click]")) "を得る")
      :lose-click (str (apply str (repeat value "[Click]")) "を失う")
      :lose-click-force (str (if (= (keyword side) :corp) "ランナー" "コーポ") "に" (apply str (repeat value "[Click]")) "を失うことをさせる")
      :lose-credits-force (str (if (= (keyword side) :corp) "ランナー" "コーポ") "に" value " [Credits]を失うことをさせる")
      :give-tag (str "ランナーに" value "つタグを与える")
      :take-tag (str value "つタグを受ける")
      :remove-tag (str value "つタグを取り除く")
      :take-bp (str "悪名を" value "つ受ける")
      :add-from-stack (str "スタックから" value "をグリップに加えてスタックをシャッフルする")
      :add-from-rnd (str  "R&Dから " value "を公開してHQに加える")
      :add-card (let [[card from to] value]
                  (str (when from (str (to-zone-name from) "から"))
                       (when to (str (to-zone-name to) "に"))
                       (or  card "それ自体") "を加える"))
      :add-to-hq (str "HQに" (render-card value) "を加える")
      :add-to-grip (str "グリップに" (render-card value) "を加える")
      :add-to-hq-unseen (str "HQにカードを" value "枚加える")
      :move-to-top-stack (str "スタックの一番上に" value "を加える")
      :shuffle-rnd (str "R&Dをシャフルする")
      :reveal-and-add (let [[card from to] value]
                        (str (to-zone-name from) "から" (to-zone-name to) "に" (or  card "それ自体") "を加える"))
      :reveal-from-hq (str "HQから" (join "と" value) "を公開する")
      :make-run (str (to-zone-name value) "にランする")
      :end-run "ランを終了する"
      ;; TODO probably need a duration here, others are encounter-only IIRC
      :gain-type (let [[card type] value] (str "ランの終了時まで" card "が" (join "と" type) "を得る"))
      :place-counter (let [[type count target] value]
                       (str (if target (render-card target) "それ自体")
                            "に"
                            (if (or (= (keyword type) :credit) (= (keyword type) :credits))
                              (str count " [Credits]を置く")
                              (str (to-counter type) "を" count "つ置く"))))
      :remove-counter (let [[type count target] value]
                       (str (if target (render-card target) "それ自体")
                            "から"
                            (to-counter type)
                            "を" count "つ取り除く"))
      :move-counter (let [[type count source target] value]
                      (str (if source (render-card source) "それ自体")
                           "から"
                           (if target (render-card target) "それ自体")
                           "に"
                           (to-counter type)
                           "を" count "つ移動する"))
      ;; TODO need to fix hq/blah
      :trash-from-hand (if (int? value)
                         (str "HQ" "から" value "枚のカードをトラッシュする")
                         (str "HQ" "から" (count value) "枚のカードをトラッシュする (" (join "と" value) ")"))
      :add-str (let [[card count] value] (str card "が強度＋" count "する"))
      :reduce-str (let [[card count] value] (str "エンカウンターの終了時までに" (render-card card) "を強度ー" count "する"))
      :access-additional-from-hq (str "HQからの追加で" value "枚のカードにアクセスする")
      :access-additional-from-rnd (str "R&Dからの追加で" value "枚のカードにアクセスする")
      :deal-net (str value "ネットダメージを与える")
      :deal-meat (str value "ミートダメージを与える")
      :deal-core (str value "コアダメージを与える")
      :install (str value "をインストールする")
      :rez (str (render-card value) "をレゾする")
      :install-and-rez-free (str value "をすべてのコストを無視してインストールしてレゾする")
      :host (str (render-card value) "を搭載する")
      :host-on (str (render-card (second value)) "に" (render-card (first value)) "を搭載する")
      :bypass (str (render-card value) "を迂回する")
      :trash-free (str "無料で" value "をトラッシュする")
      :str-pump (let [[base-str target-str duration] value]
                  (str (to-duration duration) "強度" base-str "から強度" target-str "にする"))
      :lower-ice-str (let [[strength card] value]
                       (or card "各インストール状態のアイスブレイカー")
                       "を強度ー" strength "する")
      :shuffle-into-rnd (str "Ｒ＆Ｄに" (render-card2 value) "を加えシャフルする")
      :shuffle-from-hq-into-rnd (str "Ｒ＆ＤにＨＱのカードを" value "枚加えシャフルする")
      :rearrange-rnd (str "Ｒ＆Ｄの一番上のカード" value "枚を並べ替える")
      :reveal-from-rnd (str "Ｒ＆Ｄの一番上から" value "を公開する")
      :look-top-rnd (str "Ｒ＆Ｄの一番上のカード" value "枚を見る")
      :move-hq-rnd (str"Ｒ＆Ｄの一番上にＨＱのカード" value "枚を加える")
      :play (str value "をプレイする")
      :move-server (let [[server card] value]
                     (str (to-zone-name server) "に" (or card "それ自体") "を動かす"))
      :prevent-access (let [[type card] value]
                        (str (case (keyword type)
                               :target card
                               :exclusive (str card "以外"))
                             "にアクセスすることを妨害する"))
      :trash-stack (str "スタックの一番上から" (join "と" value) "をトラッシュする")
      :prevent-net (str value "ネットダメージを妨害する")
      :prevent-encounter-ability (let [[card ability] value]
                                   (str card "のエンカウントした時能力を妨害する"
                                        (when ability (str " (" ability ")"))))
      :prevent-etr (str (render-card value) "でランを終了することを妨害する")
      :gain-str (let [[strength duration] value] (str (to-duration duration) "強度+" strength "する"))
      :breach-server (str (to-zone-name value) "に侵入する")
      :derez (str (if (list? value)
                    (join "と" (map render-card value))
                    (render-card value))
                  "をデレゾする")
      :rez-free (str "すべてのコストを無視して" (join "と" value) "をレゾする")
      :encounter-ice (str "ランナーに" (render-card value) "をエンカウントさせる")
      :reveal-self (str (to-zone-name value) "からそれ自体を公開する")
      :add-from-hq-to-score (str "得点エリアにＨＱから" value "を加える")
      :turn-faceup (str "アーカイブに" value "を表向きにする")
      :add-self-to-hq (str "ＨＱにそれ自体を加える")
      :trash (str (if (string? value) value (render-card value)) "をトラッシュする")
      ;; TODO this needs a duration?
      :add-str-new (let [[card count] value] (str (render-card card) "居度＋" count "与える"))
      :add-sub (str "「[subroutine] " value "」を他のサブルーチンの後に与える")
      :trash-rnd (str "Ｒ＆Ｄの一番上のカード" value "枚をトラッシュする")
      :remove-click-next-turn (str "ランナーの次のターンの割当[Click]をー" value "する")
      :move-grip-to-stack (str "グリップから" (join "と" value) "をスタックに加える")
      :shuffle-into-stack (str value "をスタックに加えシャフルする")
      :remove-all-virus-counters (str (render-card value) "からウィルスカウンターを取り除く")
      :trash-from-hq (str "ＨＱから" value "をトラッシュする")
      :reveal-from-grip (str "グリップから" (join "と" value) "を公開する")
      :add-to-top-rnd (str "Ｒ＆Ｄの一番上に" value "を加える")
      :add-to-bottom-rnd (str "Ｒ＆Ｄの一番下に" (render-card value) "を加える")
      :force-reveal (str "ＨＱのランダムなカード" value "枚を公開する")
      :shuffle-zone-into (str "スタックに" (join "と" (map to-zone-name value)) "に加えシャフルする")
      :rfg (str (join "と" value) "を取り除く")
      :reveal-from-stack (str "スタックの一番上から" (join "と" value) "を公開する")
      :host-on-self (str "それ自体に" value "を搭載する")
      :host-instead-of-access (str "アクセスの代わりに" value "をそれ自体に搭載する")
      :shuffle-stack (str "スタックをシャフルする")
      :trash-self (str "それ自体をトラッシュする")
      :credits (str value " [Credits]を支払う")
      :draw-additional (str "追加で" value "枚カードを引く")
      :purge "ウィルスカウンター破棄する"
      :reveal (let [groups (group-by :zone value)]
                (str (join "、" (map #(str (to-zone-name (first %)) "から"
                                           (join "と" (map :card (second %))))
                                     groups))
                     "を公開する"))
      :target-server (str (to-zone-name value) "を選ぶ")
      :choose-subtype (str value "を選ぶ")
      :choose-ice (str (render-card value) "を選ぶ")
      :add-to-score (let [[card kind points] value]
                      (str (or card "それ自体") "を"
                           (when points
                             (str points "価値を持つ"
                                  (when (= (keyword kind) :assassination) "暗殺の")
                                  "計画書として自身の得点エリア"))
                       "に加える"))      ;; TODO
      :swap-ice-from-hand (str (render-card value) "とHQにあるアイスを交換する")
      :swap-ice (throw "foo"))))

(defn render-single-effect-force-check
  [effect value side forced]
  (str
   (when forced
     (str (if (= (keyword side) :corp) "ランナー" "コーポ") "に"))
   (render-single-effect (keyword effect) value side)
   (when forced "ことをさせる")))

(defn do-conj
  [input]
  (-> input
      (s/replace #"する$" "して")
      (s/replace #"与える$" "与えて")
      (s/replace #"引く$" "引いて")
      (s/replace #"得る$" "得て")
      (s/replace #"失う$" "失って")
      (s/replace #"受ける$" "受けて")
      (s/replace #"除く$" "除いて")
      (s/replace #"加える$" "加えて")
      (s/replace #"置く$" "置いて")
      (s/replace #"替える$" "替えて")
      (s/replace #"見る$" "見て")
      (s/replace #"動かす$" "動かして")
      (s/replace #"させる$" "させて")))

(defn render-effect
  [effects side forced]
  (when effects
    (let [effect-strs (remove nil? (for [[c v] effects]
                                     (render-single-effect-force-check c v side forced)
                                     #_(render-single-effect c v)))]
      (str (apply str (map do-conj (butlast effect-strs))) (last effect-strs)))))

(defmulti render-text (fn [input] (or (keyword (:type input)) :raw-text)))

(defmethod render-text :create-game [_] "ゲームを作りました")
(defmethod render-text :keep-hand [_] "手札をキープする")
(defmethod render-text :mulligan-hand [_] "手札をマリガンする")
(defmethod render-text :mandatory-draw [_] "強制ドローする")

(defmethod render-text :no-action [_] "これ以上アクションがない")

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
  [{:keys [card]}]
  (str card "をプレイする"))

(defmethod render-text :install
  [{:keys [card card-type server new-remote origin install-source cost host side]}]
  (let [card-type (keyword card-type)]
    (str (when install-source
           (str install-source "で"))
         (when origin
           (str (to-zone-name origin side) "から"))
         (when host
           (str (render-card host) "に"))
         (if (= card-type :ice)
           (str (or card "アイス"))
           (str (or card (if (= card-type :facedown)
                           "未知のカード"
                           "カード"))))
         "を"
         (when server
           (str (when new-remote "新しい") ;; i don't like this
                (to-zone-name server)
                (when (= card-type :ice) "を守っている位置")))
         "にインストールする")))

(defmethod render-text :rez
  [{:keys [card alternative-cost ignore-cost]}]
  (str (if alternative-cost "代替コストを支払って"
           (when ignore-cost "コストを無視して"))
       (if (string? card) card (render-card card))
       "をレゾする"))

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
  (let [sub-count (or sub-count (count subs))]
    (str card "で"
         (when str-boost (str card "の強度" str-boost "にして"))
         ice "の"
         (when subtype (str subtype "の"))
         "サブルーチンを"
         (case (keyword break-type)
           :all "すべて"
           :remaining "残り"
           "")
         sub-count "つ"
         "ブレイクする"
         (when-not break-type
           (str " (「[subroutine]"
                (join "」と「[subroutine]" subs)
                "」)")))))

(defmethod render-text :str-boost
  [{:keys [card strength]}]
  (str card "の強度を" strength "まで上げる"))

(defmethod render-text :resolve-subs
  [input]
  (let [info (:resolved input)
        ice (:ice info)
        resolved-subs (:subs info)]
    (str ice "の未ブレイクのサブルーチンを" (count resolved-subs) "つ解決する"
         " 「[subroutine] "
         (join "」と「[subroutine] " resolved-subs)
         "」)")))

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
       "にアクセスする"))

(defmethod render-text :access-all
  [_]
  "アーカイブの残りのカードにすべてアクセスする")

(defmethod render-text :trash
  [{:keys [card server]}]
  (str (when (string? card)
         (when server (str (to-zone-name server) "から")))
       (render-card card) "をトラッシュする"))

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
  (let [not-map (or (string? card) (number? card) (coll? card))]
    (str (when reason
           ;; TODO only end of turn is supported here, so...
           "ターンの終了に")
         (when not-map (str (to-zone-name [:hand] side) "から"))
         (cond
           (string? card) card
           (number? card) (str "カード" card "枚")
           (coll? card) (join "と" card)
           true (render-card card))
         "を捨てる")))

(defmethod render-text :increase-trace-link
  [{:keys [strength side]}]
  (str (if (= (keyword side) :corp) "トレース" "リンク") "強度を" strength "に上げる"))

(defmethod render-text :win-game
  [_]
  "対戦を勝つ")

(defmethod render-text :fire-unbroken
  [{:keys [card]}]
  (str card "の未ブレイクのサブルーチンを解決することを許可する"))

(defmethod render-text :use-command
  [{:keys [command]}]
  (str "コマンドを使う: " command))

(defmethod render-text :force
  [{:keys [card side]}]
  (str card "の条件を満たす"))

(defmethod render-text :raw-text
  [input]
  (:raw-text input))

(defmethod render-text :default
  [input]
  (str "unknown type " input))

(defmethod render-map "ja"
  [_ {:keys [username raw-text cost effect forced side urgent] :as input}]
  (println input)
  (try-catchall
    (let [cost-str (render-cost cost)
          effect-str (render-effect effect side forced)]
      (let [output (str (when urgent "[!]")
                        (if username
                          (str username "は"
                               (when-not (empty? cost-str) (str cost-str "、"))
                               (render-text input) effect-str "。")
                          raw-text))]
        (println output)
        output))
    (catch e# ::exception (throw e#) #_(render-map "en" input))))
