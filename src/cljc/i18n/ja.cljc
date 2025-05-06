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
       ;; TODO this is wrong, but messages don't carry enough context
       ;; e.g. IP enforcmenet is corp msg, but score area is runner side
       :scored "ランナーの得点エリア"
       :servers
       (case server
         :hq "HQ"
         :rd "R&D"
         :archives "アーカイブ"
         (str "サーバー" (last (split (str server) #":remote"))))
       (str "unhandled zone " location)))))

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
  [cost value side]
  (let [hand (to-zone-name [:hand] (or side :corp))
        deck (to-zone-name [:deck] (or side :corp))]
    (case cost
      :click (str (apply str (repeat value "[Click]")) "を消費して")
      :lose-click (str (apply str (repeat value "[Click]")) "を失って")
      :credits (render-credits value)
      :trash (str value "をトラッシュして")
      :forfeit (if (string? value)
                 (str value "を放棄して")
                 (str (count value) "枚の計画書を破棄して (" (join "と" value) ")"))
      :reveal-and-trash (str hand "からカードの" (count value) "枚を公開しトラッシュして (" (join "と" (map render-card value)) ")")
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
      :trash-bioroid (str (count value) "つレゾ状態のバイオロイドをトラッシュして (" (join "と" (map render-card value)) ")")
      :trash-from-deck (str deck "の一番上から" value "枚のカードからをトラッシュして")
      :trash-from-hand (if (int? value)
                         (str hand "から" value "枚のカードをトラッシュして")
                         (str hand "から" (count value) "枚のカードをトラッシュして (" (join "と" (map render-card value)) ")"))
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
      :add-random-from-hand-to-bottom-of-deck (str hand "の" value "枚のランダムなカードを" deck "の一番下に加えて")
      :hosted-to-hq (str "搭載されたカードの" (count value) "枚をHQに加えて (" (join "と" value) ")")
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
  [{:keys [cost side]}]
  (when cost
    (join "" (for [[c v] cost] (render-single-cost (keyword c) v side)))))

(defmulti render-effect (fn [effect side & value] effect))
(defmethod render-effect :advance [effect side [value]] (str (render-card value) "をアドバンスする"))
(defmethod render-effect :draw-cards [effect side [value]] (str "カードを" value "枚引く"))
(defmethod render-effect :gain-credits [effect side [value]] (str value " [Credits]を得る"))
(defmethod render-effect :lose-credits [effect side [value]] (str value " [Credits]を失う"))
(defmethod render-effect :gain-click [effect side [value]] (str (apply str (repeat value "[Click]")) "を得る"))
(defmethod render-effect :lose-click [effect side [value]] (str (apply str (repeat value "[Click]")) "を失う"))
(defmethod render-effect :lose-click-force [effect side [value]] (str (if (= (keyword side) :corp) "ランナー" "コーポ") "に" (apply str (repeat value "[Click]")) "を失うことをさせる"))
(defmethod render-effect :lose-credits-force [effect side [value]] (str (if (= (keyword side) :corp) "ランナー" "コーポ") "に" value " [Credits]を失うことをさせる"))
(defmethod render-effect :give-tag [effect side [value]] (str "ランナーにタグを" value "つ与える"))
(defmethod render-effect :take-tag [effect side [value]] (str value "つタグを受ける"))
(defmethod render-effect :remove-tag [effect side [value]] (str value "つタグを取り除く"))
(defmethod render-effect :take-bp [effect side [value]] (str "悪名を" value "つ受ける"))
(defmethod render-effect :add-from-stack [effect side [value]] (str "スタックから" value "をグリップに加えてスタックをシャッフルする"))
(defmethod render-effect :add-from-rnd [effect side [value]] (str  "R&Dから " value "を公開してHQに加える"))
(defmethod render-effect :add-from-rnd-to-rnd [effect side [value]] (str  "R&Dから " value "を公開してR&DをシャフルしてR&Dの一番上に加える"))
(defmethod render-effect :add-card [effect side [card from to]]
  (str (when from (str (to-zone-name from) "から"))
       (when to (str (to-zone-name to) "に"))
       (or  card "それ自体") "を加える"))
(defmethod render-effect :add-to-hq [effect side [value]] (str "HQに" (render-card value) "を加える"))
(defmethod render-effect :add-to-grip [effect side [value]] (str "グリップに" (render-card value) "を加える"))
(defmethod render-effect :add-to-hq-unseen [effect side [value]] (str "HQにカードを" value "枚加える"))
(defmethod render-effect :move-to-top-stack [effect side [value]] (str "スタックの一番上に" value "を加える"))
(defmethod render-effect :shuffle-rnd [effect side [value]] (str "R&Dをシャフルする"))
(defmethod render-effect :reveal-and-add [effect side [card from to]]
                  (str (to-zone-name from) "から" (to-zone-name to) "に" (or  card "それ自体") "を加える"))
(defmethod render-effect :reveal-from-hq [effect side [value]] (str "HQから" (join "と" value) "を公開する"))
(defmethod render-effect :make-run [effect side [value]] (str (to-zone-name value) "にランする"))
(defmethod render-effect :end-run [effect side [value]] "ランを終了する")
;; TODO probably need a duration here, others are encounter-only IIRC
(defmethod render-effect :gain-type [effect side [card type duration]] (str (to-duration duration) (render-card card) "が" (join "と" type) "を得る"))
(defmethod render-effect :place-counter [effect side [type count target]]
                 (str (if target (render-card target) "それ自体")
                      "に"
                      (if (or (= (keyword type) :credit) (= (keyword type) :credits))
                        (str count " [Credits]を置く")
                        (str (to-counter type) "を" count "つ置く"))))
(defmethod render-effect :remove-counter [effect side [type count target]]
                  (str (if target (render-card target) "それ自体")
                       "から"
                       (to-counter type)
                       "を" count "つ取り除く"))
(defmethod render-effect :move-counter [effect side [type count source target]]
                (str (if source (render-card source) "それ自体")
                     "から"
                     (if target (render-card target) "それ自体")
                     "に"
                     (to-counter type)
                     "を" count "つ移動する"))
;; TODO need to fix hq/blah
(defmethod render-effect :trash-from-hand [effect side [value]] (if (int? value)
                   (str "HQ" "から" value "枚のカードをトラッシュする")
                   (str "HQ" "から" (count value) "枚のカードをトラッシュする (" (join "と" value) ")")))
(defmethod render-effect :add-str [effect side [value]] (let [[card count] value] (str card "が強度＋" count "する")))
(defmethod render-effect :reduce-str [effect side [value]] (let [[card count] value] (str "エンカウンターの終了時までに" (render-card card) "を強度ー" count "する")))
(defmethod render-effect :access-additional-from-hq [effect side [value]] (str "HQからの追加で" value "枚のカードにアクセスする"))
(defmethod render-effect :access-additional-from-rnd [effect side [value]] (str "R&Dからの追加で" value "枚のカードにアクセスする"))
(defmethod render-effect :deal-net [effect side [value]] (str value "ネットダメージを与える"))
(defmethod render-effect :deal-meat [effect side [value]] (str value "ミートダメージを与える"))
(defmethod render-effect :take-meat [effect side [value]] (str value "ミートダメージを受ける"))
(defmethod render-effect :deal-core [effect side [value]] (str value "コアダメージを与える"))
(defmethod render-effect :install [effect side [value]] (str value "をインストールする"))
(defmethod render-effect :rez [effect side [value]] (str (render-card value) "をレゾする"))
(defmethod render-effect :install-and-rez-free [effect side [value]] (str value "をすべてのコストを無視してインストールしてレゾする"))
(defmethod render-effect :host [effect side [value]] (str (render-card value) "を搭載する"))
(defmethod render-effect :host-on [effect side [value]] (str (render-card (second value)) "に" (render-card (first value)) "を搭載する"))
(defmethod render-effect :bypass [effect side [value]] (str (render-card value) "を迂回する"))
(defmethod render-effect :trash-free [effect side [value]] (str "無料で" value "をトラッシュする"))
(defmethod render-effect :str-pump [effect side [base-str target-str duration]]
            (str (to-duration duration) "強度" base-str "から強度" target-str "にする"))
(defmethod render-effect :lower-ice-str [effect side [strength card]]
  (str (or card "各インストール状態のアイスブレイカー")
       "を強度ー" strength "する"))
(defmethod render-effect :shuffle-into-rnd [effect side [value]] (str "Ｒ＆Ｄに" (render-card2 value) "を加えシャフルする"))
(defmethod render-effect :shuffle-from-hq-into-rnd [effect side [value]] (str "Ｒ＆ＤにＨＱのカードを" value "枚加えシャフルする"))
(defmethod render-effect :rearrange-rnd [effect side [value]] (str "Ｒ＆Ｄの一番上のカード" value "枚を並べ替える"))
(defmethod render-effect :reveal-from-rnd [effect side [value]] (str "Ｒ＆Ｄの一番上から" value "を公開する"))
(defmethod render-effect :look-top-rnd [effect side [value]] (str "Ｒ＆Ｄの一番上のカード" value "枚を見る"))
(defmethod render-effect :look-top-stack [effect side [value]] (str "スタックの一番上のカード" value "枚を見る"))
(defmethod render-effect :move-hq-rnd [effect side [value]] (str"Ｒ＆Ｄの一番上にＨＱのカード" value "枚を加える"))
(defmethod render-effect :play [effect side [card zone]] (str (when zone (str (to-zone-name zone) "から")) card "をプレイする"))
(defmethod render-effect :move-server [effect side [server card]]
               (str (to-zone-name server) "に" (or card "それ自体") "を動かす"))
(defmethod render-effect :prevent-access [effect side [type card]]
                  (str "ランナーに"
                       (case (keyword type)
                         :target card
                         :exclusive (str card "以外"))
                       "にアクセスすることを妨害する"))
(defmethod render-effect :trash-stack [effect side [value]] (str "スタックの一番上から" (join "と" value) "をトラッシュする"))
(defmethod render-effect :prevent-net [effect side [value]] (str value "ネットダメージを妨害する"))
(defmethod render-effect :prevent-encounter-ability [effect side [card ability]]
                             (str card "のエンカウントした時能力を妨害する"
                                  (when ability (str " (" ability ")"))))
(defmethod render-effect :prevent-etr [effect side [value]] (str "ランを終了することを妨害する"))
;; TODO missing encounter... kind of redundant here
(defmethod render-effect :prevent-etr-effect [effect side [value]] (str (render-card value) "でランを終了することを妨害する"))
(defmethod render-effect :gain-str [effect side [value]] (let [[strength duration] value] (str (to-duration duration) "強度+" strength "する")))
(defmethod render-effect :breach-server [effect side [value]] (str (to-zone-name value) "に侵入する"))
(defmethod render-effect :derez [effect side [value]] (str (if (vector? value)
              (join "と" (map render-card value))
              (render-card value))
            "をデレゾする"))
(defmethod render-effect :rez-free [effect side [value]] (str "すべてのコストを無視して" (join "と" value) "をレゾする"))
(defmethod render-effect :encounter-ice [effect side [value]] (str "ランナーに" (render-card value) "をエンカウントさせる"))
(defmethod render-effect :reveal-self [effect side [value]] (str (to-zone-name value) "からそれ自体を公開する"))
(defmethod render-effect :add-from-hq-to-score [effect side [value]] (str "得点エリアにＨＱから" value "を加える"))
(defmethod render-effect :turn-faceup [effect side [value]] (str "アーカイブに" value "を表向きにする"))
(defmethod render-effect :add-self-to-hq [effect side [value]] (str "ＨＱにそれ自体を加える"))
(defmethod render-effect :trash [effect side [value]]
  (str (if (or (vector? value) (list? value)) (join "と" (map render-card value)) (render-card value)) "をトラッシュする"))
(defmethod render-effect :add-str-new [effect side [card count duration]] (str (to-duration duration) (render-card card) "居度＋" count "与える"))
(defmethod render-effect :add-sub [effect side [value]] (str "「[subroutine] " value "」を他のサブルーチンの後に与える"))
(defmethod render-effect :trash-rnd [effect side [value]] (str "Ｒ＆Ｄの一番上のカード" value "枚をトラッシュする"))
(defmethod render-effect :trash-rnd-and-add [effect side [value]] (str "Ｒ＆Ｄの一番上から" value "枚目のカードをトラッシュし残りをHQに加える"))
(defmethod render-effect :remove-click-next-turn [effect side [value]] (str "ランナーの次のターンの割当[Click]をー" value "する"))
(defmethod render-effect :move-grip-to-stack [effect side [value]] (str "グリップから" (join "と" value) "をスタックに加える"))
(defmethod render-effect :shuffle-into-stack [effect side [value]] (str (when value (str (join "と" value) "を")) "スタックに加えシャフルする"))
(defmethod render-effect :remove-all-virus-counters [effect side [value]] (str (render-card value) "からウィルスカウンターを取り除く"))
(defmethod render-effect :trash-from-hq [effect side [value]] (str "ＨＱから" value "をトラッシュする"))
(defmethod render-effect :reveal-from-grip [effect side [value]] (str "グリップから" (join "と" value) "を公開する"))
(defmethod render-effect :add-to-top-rnd [effect side [value]] (str "Ｒ＆Ｄの一番上に" value "を加える"))
(defmethod render-effect :add-to-bottom-rnd [effect side [value]] (str "Ｒ＆Ｄの一番下に" (render-card value) "を加える"))
(defmethod render-effect :add-to-bottom-stack [effect side [value]] (str "スタックの一番下に" (render-card value) "を加える"))
(defmethod render-effect :force-reveal [effect side [value]] (str "ＨＱのランダムなカード" value "枚を公開する"))
(defmethod render-effect :shuffle-zone-into [effect side [value]] (str "スタックに" (join "と" (map to-zone-name value)) "に加えシャフルする"))
(defmethod render-effect :rfg [effect side [value]] (str (join "と" value) "をゲームから取り除く"))
(defmethod render-effect :reveal-from-stack [effect side [value]] (str "スタックの一番上から" (join "と" value) "を公開する"))
(defmethod render-effect :host-on-self [effect side [value]] (str "それ自体に" value "を搭載する"))
(defmethod render-effect :host-instead-of-access [effect side [value]] (str "アクセスの代わりに" value "をそれ自体に搭載する"))
(defmethod render-effect :shuffle-stack [effect side [value]] (str "スタックをシャフルする"))
(defmethod render-effect :trash-self [effect side [value]] (str "それ自体をトラッシュする"))
(defmethod render-effect :credits [effect side [value]] (str value " [Credits]を支払う"))
(defmethod render-effect :draw-additional [effect side [value]] (str "追加で" value "枚カードを引く"))
(defmethod render-effect :purge [effect side [value]] "ウィルスカウンター破棄する")
(defmethod render-effect :reveal [effect side [value]] (let [groups (group-by :zone value)]
          (str (join "、" (map #(str (to-zone-name (first %)) "から"
                                     (join "と" (map :card (second %))))
                               groups))
               "を公開する")))
(defmethod render-effect :target-server [effect side [value]] (str (to-zone-name value) "を選ぶ"))
(defmethod render-effect :choose-subtype [effect side [value]] (str value "を選ぶ"))
(defmethod render-effect :choose-ice [effect side [value]] (str (render-card value) "を選ぶ"))
(defmethod render-effect :choose-card-type [effect side [value]] (str value "を選ぶ"))
(defmethod render-effect :add-to-score [effect side [card kind points]]
                (str (or card "それ自体") "を"
                     (when points
                       (str points "価値を持つ"
                            (when (= (keyword kind) :assassination) "暗殺の")
                            "計画書として自身の得点エリア"))
                     "に加える"))
;; TODO weird. also :prevent-access needs a revisit
(defmethod render-effect :prevent-steal-trash [effect side [value]] (str (to-duration value) "ランナーにコーポのカードを盗むかトラッシュすることを妨害する"))
(defmethod render-effect :swap-ice-from-hand [effect side [value]] (str (render-card value) "とHQにあるアイスを交換する"))
;; TODO
(defmethod render-effect :swap-ice [effect side [value]] (throw "foo"))
(defmethod render-effect :gain-click-next-turn [effect side [value]] (str "自身の次のターンに割当[Click]を＋" value "する"))
(defmethod render-effect :redirect-run [effect side [value]] (str "ランナーに" (to-zone-name value) "の最外の位置に移動することをさせる"))
(defmethod render-effect :access [effect side [server cards]] (str (to-zone-name server) "から" cards "枚のカードにアクセスする"))
(defmethod render-effect :resolve-subroutine [effect side [ice subroutine]] (str (render-card ice) "のサブルーチン（[subroutine]" subroutine "）を解決する"))
(defmethod render-effect :turn-faceup [effect side [value]] (str (render-card value) "を裏向きにする"))
(defmethod render-effect :flip-id [effect side [value]] (str "IDを" value "にめくる"))
(defmethod render-effect :change-server [effect side [value]] (str "攻撃されているサーバーを" (to-zone-name value) "に変更する"))
(defmethod render-effect :reveal-and-host [effect side [value]] (str "HQから" value "を公開し搭載する"))
(defmethod render-effect :sabotage [effect side [value]] (str "破壊工作" value "を行う"))

(defn render-single-effect-force-check
  [effect value side forced]
  (str
   (when forced
     (str (if (= (keyword side) :corp) "ランナー" "コーポ") "に"))
   (render-effect (keyword effect) side value)
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
      (s/replace #"させる$" "させて")
      (s/replace #"めくる$" "めくって")))

(defn render-effects
  [effects side forced]
  (when effects
    (if (vector? (first (first effects)))
      (let [effect-strs1 (remove nil? (for [[c & v] (first effects)]
                                        (render-single-effect-force-check c v side forced)))
            effect-strs2 (remove nil? (for [[c & v] (second effects)]
                                        (render-single-effect-force-check c v side forced)))]
        (str
         (str (apply str (map do-conj effect-strs1)))
         "、"
        (str (apply str (map do-conj (butlast effect-strs2))) (last effect-strs2))))
      (let [effect-strs (remove nil? (for [[c & v] effects]
                                       (render-single-effect-force-check c v side forced)))]
        (str (apply str (map do-conj (butlast effect-strs))) (last effect-strs))))))

(defmulti render-text (fn [input] (or (keyword (:type input)) :raw-text)))

(defmethod render-text :create-game [_] "ゲームを作成した")
(defmethod render-text :join-game [_] "ゲームを参加した")
(defmethod render-text :leave-game [_] "ゲームを退出した")
;; TODO :side
(defmethod render-text :watch-game [_] "観戦としてゲームを参加した")
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
  [{:keys [card zone]}]
  (str (when zone (str (to-zone-name zone) "から"))
       card "をプレイする"))

(defn- cost-discount-str
  [{:keys [ignore-all-costs ignore-install-costs cost-bonus alternative-cost]}]
  (cond
    ignore-all-costs "（すべてのコストを無視して）"
    ignore-install-costs "（インストールのコストを無視して）"
    alternative-cost " （代替コストを支払って）"
    (and cost-bonus (pos? cost-bonus)) (str "（支払いを" cost-bonus " [Credits]増して）")
    (and cost-bonus (neg? cost-bonus)) (str "（支払いを" (* -1  cost-bonus) " [Credits]減らして）")))

(defmethod render-text :install
  [{:keys [card card-type server new-remote origin install-source cost host hosted side no-cost] :as input}]
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
           (str (when new-remote "新しい遠隔")
                (to-zone-name server)
                (when (= card-type :ice) "を守っている位置")))
         "に"
         (cost-discount-str input)
         "インストールする")))

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
  (str  card "をゲームから取り除く"))

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

(defmethod render-text :win-reason
  [{:keys [cause]}]
  (case (keyword cause)
    :concession "降参する"
    :decked "デッキが切れた"
    :flatline "フラットラインされた"))

(defmethod render-text :win-game
  [_]
  "対戦を勝つ")

;; This is basically a no-op, current effect behavior will just print it correctly.
(defmethod render-text :direct-effect
  [{:keys [effect]}])

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
    (let [cost-str (render-cost input)
          effect-str (render-effects effect side forced)]
      (let [output (str (when urgent "[!]")
                        (if username
                          (str username "は"
                               (when-not (empty? cost-str) (str cost-str "、"))
                               (render-text input) effect-str "。")
                          raw-text))]
        (println output)
        output))
    (catch e# ::exception #_(throw e#) (render-map "en" input))))
