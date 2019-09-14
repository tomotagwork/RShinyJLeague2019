tabItem_info <- tabItem("tab_info",
                        div(p("[2019/09/14, ver1.0]"),align="right"),
                        div(img(src="shika02_maru.gif", width="75px", height="75px"),align="center"),
                        h1("Jリーグ 2019 集計"),
                        hr(),
                        h2("Information"),
                        a("J.League Data Site", href="https://data.j-league.or.jp/SFMS01/", target="_blank"), 
                        p("上のから情報を取得してデータを加工しています。PC前提のためスマホだとみにくくなってしまうと思います。"),
                        p("コードについての詳細は以下の記事をご参照下さい。"),
                        a("Qiita: R - ShinyアプリでJリーグの勝点推移グラフを作成してみた", 
                          href="https://qiita.com/tomotagwork/items/3162cfef78c71ef4b977",
                          target="_blank"),
                        hr(),
                        h3("勝点推移"),

                        p("個人的には\"勝ち点\"というのが大事だと思っています。順位が1つ違うとしても、勝ち点差が1ポイントなのか3ポイントなのかで大きく状況は異なります。"),
                        p("順位の推移のグラフは公式サイトなどにもありますが、勝ち点の推移についてはきちんとしたグラフが見当たらないので作ってみました。"),
                        
                        h3("日程/結果"),
                        p("Jリーグデータサイトでも似たような表を出力することはできますが、ボタンでスクロールする方式で使いにくいので作ってみました。"),
                        p("試合結果の詳細はJリーグデータサイトのページにリンクで飛ぶようにしています。"),
                        hr()
                        

)