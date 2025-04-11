(ns nr.landing)

(def landing-content
  [:div.landing.panel.content-page.blue-shade
   [:h2 "ネットランナーへようこそ"]
   [:p "このウェブサイトはオンラインでネットランナーの対戦ができます。ゲームのルールを完全に実装するものではありませんのでご注意ください。"]
   [:p "日本語対応は開発中です。"]
   [:p "どのようなフィードバックをDiscordサーバーAlways Be Runningで#フィードバックに歓迎します。"]
   [:p "Currently, many things are hardcoded in Japanese and switching to English will not update these to English."]])

(defn landing []
  [:div.page-container
   [:div.worlds2020]
   [:div.landing-message
    [:h4 "ルールや情報は" [:a {:href "https://wikiwiki.jp/netrunner/" :target "_blank"} "Netrunner日本語wiki"] "をご覧ください"]]
   landing-content])

