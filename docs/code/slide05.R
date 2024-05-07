### 第5講 サンプルコード

#' 最初に一度だけ以下のいずれかを実行しておく
#'  - Package タブから tidyverse をインストール
#'  - コンソール上で次のコマンドを実行 'install.packages("tidyverse")'
#' tidyverse パッケージの読み込み
library(tidyverse)

jp_data <- read_csv(file = "data/jpdata1.csv")
jp_item <- read_csv(file = "data/jpdata2.csv")
jp_area <- read_csv(file = "data/jpdata3.csv")

tw_data <- read_csv(file = "data/tokyo_weather.csv")

tc_data <- read_csv(file="data/tokyo_covid19_2021.csv")

#' @exercise 折れ線グラフの描画
#' 東京の5月の気温と日射量の推移

tw_data |> filter(month == 5) |> # 5月を抽出
  ggplot(aes(x = day)) + # day をx軸に指定
  geom_line(aes(y = temp), colour = "blue") + # 気温を青
  geom_line(aes(y = solar), colour = "red") + # 日射量を赤
  labs(y = "temp.(blue) / solar rad.(red)") # y軸のラベルを変更

#' 前例の別の書き方

tw_data |> filter(month == 5) |>
  pivot_longer(c(temp, solar)) |> # 集約する列を指定
  ggplot(aes(x = day, y = value, colour = name)) + 
  geom_line() + # index ごとに定義されたカラーパレットの異なる色が用いられる
  labs(title = "Weather in May")

#' @notes
#' 描画に必要な情報は ggplot の中で指定されるが
#' 以下のように必要な列を選択してもよい
tw_data |> filter(month == 5) |> select(c(day, temp, solar)) |>
  pivot_longer(!day) |> # day 以外の列を集約
  ggplot(aes(x = day, y = value, colour = name)) +
  geom_line() + labs(title = "Weather in May")
#' 色は theme で設定されているカラーパレットに従って自動的に選択されるが
#' 自身で設定することも可能．詳細は例えば以下を参照
#' https://ggplot2-book.org/scales-colour

tw_data |> filter(month == 5) |> 
  pivot_longer(c(temp, solar)) |> 
  ggplot(aes(x = day, y = value, colour = name)) +
  geom_line(show.legend  =  FALSE) + # 凡例は不要なので消す
  labs(title = "Weather in May") +
  facet_grid(rows = vars(name)) # name ごとに行に並べる (rowsは省略可)

#' @notes
#' 属性ごとに描いた異なるグラフを並べる場合には
#' 関数 ggplot2::facet_grid() (属性ごとに行・列を構成)や
#' 関数 ggplot2::facet_wrap() (行数・列数を指定)を用いる
foo <- # 基本となるグラフオブジェクトを保存
  tw_data |> filter(month %in% c(5,6,7,8)) |>
  select(c(month, day, temp, solar, wind)) |>
  pivot_longer(!c(month, day)) |> 
  ggplot(aes(x = day, y = value, colour = name)) +
  geom_line(show.legend  =  FALSE) 
foo + facet_grid(rows = vars(name), cols = vars(month)) 
foo + facet_grid(name ~ month) # 同上
foo + facet_wrap(vars(name, month), nrow = 4, ncol = 3) # 4x3 に並べる
foo + facet_wrap(name ~ month, nrow = 4, ncol = 3) # 同上

#' ---------------------------------------------------------------------------
#' @practice 基本的なグラフの描画
#' 
#' データの読み込み
tw_data <- read_csv(file = "data/tokyo_weather.csv")
#'
#' 6月の気温と湿度の折線グラフ
#' 同じグラフに描いてみる
tw_data |>
  filter(month == 6) |> # 6月を選択
  select(c(day, temp, humid)) |> # 必要な列を選択
  pivot_longer(!day) |> # long format に変換
  ggplot(aes(x = day, y = value, colour = name)) + # 審美的属性を指定
  geom_line() # 折線グラフの描画
#' 物理的に異なる量なので facet を分ける
tw_data |>
  filter(month == 6) |> 
  select(c(day, temp, humid)) |> 
  pivot_longer(!day, names_to = "index") |> # 列名を "index" に変更
  ggplot(aes(x = day, y = value, colour = index)) + # こちらも "index"
  geom_line() + # 凡例も "index" になっている
  facet_grid(rows = vars(index)) # index ごとに facet を行に並べる
#' 値域が異なるので facet ごとにy軸を調整する
tw_data |>
  filter(month == 6) |> 
  select(c(day, temp, humid)) |> 
  pivot_longer(!day, names_to = "index") |> 
  ggplot(aes(x = day, y = value, colour = index)) +
  geom_line() +
  facet_grid(rows = vars(index),
             scales = "free_y") # y軸を個別に自動調整
#' 不要な凡例の削除とタイトルの追加
tw_data |>
  filter(month == 6) |> 
  pivot_longer(c(temp, humid)) |> # 集約する列を指定(余計な列も存在)
  ggplot(aes(x = day, y = value, colour = name)) +
  geom_line(show.legend = FALSE) + # 凡例の削除
  facet_grid(rows = vars(name), scales = "free_y") +
  labs(title = "Weather in June") # タイトルの追加
#'
#' 1年間の気温と湿度の折線グラフ
tw_data |>
  select(c(temp, humid)) |> # 必要な列を抽出
  rowid_to_column(var = "day") |> # 行番号を ID として列 day を作る
  pivot_longer(!day) |> 
  ggplot(aes(x = day, y = value, colour = name)) +
  geom_line(show.legend = FALSE) +
  facet_grid(rows = vars(name), scales = "free_y") +
  labs(title = "Weather in Tokyo") 
#' x軸として日付を用いる
tw_data |>
  mutate(date = as_date(paste(year, month, day, sep = "-"))) |> # 日付
  select(c(date, temp, humid)) |> # 必要な列を抽出
  pivot_longer(!date) |> 
  ggplot(aes(x = date, y = value, colour = name)) +
  geom_line(show.legend = FALSE) +
  facet_grid(rows = vars(name), scales = "free_y") +
  labs(title = "Weather in Tokyo") 
#'
#' 各月の平均気温と湿度の折線グラフを描け
tw_data |>
  group_by(month) |> # 月毎にまとめる
  summarize(across(c(temp, humid), mean)) |> # 目的の指標を集計
  pivot_longer(!month) |> 
  ggplot(aes(x = month, y = value, colour = name)) +
  geom_line(show.legend = FALSE) +
  facet_grid(vars(name), scales = "free_y") +
  labs(title = "Weather in Tokyo") 
#' x軸の目盛を指定
tw_data |>
  group_by(month) |> 
  summarize(across(c(temp, humid), mean)) |>
  pivot_longer(!month) |> 
  ggplot(aes(x = month, y = value, colour = name)) +
  geom_line(show.legend = FALSE) +
  facet_grid(vars(name), scales = "free_y") +
  scale_x_continuous(breaks = 1:12) + # 1:12 の目盛を描く
  labs(title = "Weather in Tokyo") 
#' ---------------------------------------------------------------------------

#' @exercise 散布図の描画
#' 夏季の日射量と気温の関係

tw_data |> filter(month %in% 7:9) |> # 7月-9月を抽出
  ggplot(aes(x = solar, y = temp)) + # x軸を日射量，y軸を気温に設定
  geom_point(colour = "blue", shape = 19) + # 色と形を指定(点の形は '?points' を参照)
  labs(x = "solar radiation", y = "temperature") # 軸の名前を指定

#' 湿度の情報を点のサイズとして追加

tw_data |> filter(month %in% 7:9) |> 
  ggplot(aes(x = solar, y = temp, size = humid)) + # 湿度を点の大きさで表示
  geom_point(colour = "blue", shape = 19) + 
  labs(x = "solar radiation", y = "temperature")

#' 各軸を対数表示に変更

tw_data |> filter(month %in% 7:9) |> 
  ggplot(aes(x = solar, y = temp, size = humid)) + 
  geom_point(colour = "blue", shape = 19) + 
  labs(x = "solar radiation", y = "temperature") +
  scale_x_log10() + scale_y_log10() # x軸，y軸を対数表示

#' @exercise 散布図行列の描画
#' 夏季の気温と日射量と湿度の関係
library(GGally)

tw_data |> filter(month %in% 7:9) |> 
  select(c(temp, solar, humid)) |> # 必要な列を選択
  ggpairs() # 標準の散布図行列 (上三角は相関，対角は密度，下三角は散布図)

#' 月ごとに色分けして表示する

tw_data |> filter(month %in% 7:9) |> select(c(month, temp, solar, humid)) |>
  mutate(month = as_factor(month)) |> # 月を因子化(ラベルとして扱う)
  ggpairs(columns = 2:4, legend = c(1,1), # 表示する列．凡例の雛型
          aes(colour = month), # 月ごとに色づける
          diag = list(continuous = "barDiag")) + # 対角をヒストグラム
  theme(legend.position = "top") # 凡例(上で指定した1行1列の凡例)の位置

#' @notes
#' 事例ベースの使い方は以下のコマンドで見ることができる
vig_ggally("ggpairs")

#' 最初に一度だけ以下のいずれかを実行しておく
#'  - Package タブから plotly をインストール
#'  - コンソール上で次のコマンドを実行 'install.packages("plotly")'
#' plotly パッケージの読み込み
library(plotly)

#' @exercise 対話型のグラフへの変換

#' 5月の気温と日射量の例
tw_data |> filter(month == 5) |> select(c(day, temp, solar)) |>
  pivot_longer(!day, names_to = "index") |> 
  ggplot(aes(x = day, y = value, colour = index)) +
  geom_line() + labs(title = "Weather in May")
ggplotly() # 最後に描いた ggplot オブジェクトを変換して 右下 Viewer タブに表示

#' 夏季の日射量と温度と湿度の例
bar <- # ggplot オブジェクトを保存
  tw_data |> filter(month %in% 7:9) |> 
  ggplot(aes(x = solar, y = temp, size = humid,
             text = paste0("date: ", month, "/", day))) + # 日付を付加
  geom_point(colour = "blue", shape = 19) + 
  labs(x = "solar radiation", y = "temperature")
ggplotly(bar) # 保存した ggplot オブジェクトを変換

#' ---------------------------------------------------------------------------
#' @practice 散布図の描画
#' 
#' データの読み込み
jp_data <- read_csv(file = "data/jpdata1.csv")
jp_area <- read_csv(file = "data/jpdata3.csv")
#'
#' @notes
#' 日本語を用いる場合は macOS ではフォントの指定が必要
if(Sys.info()["sysname"] == "Darwin") { # macOS か調べて日本語フォントを指定
  theme_update(text = element_text(family = "HiraginoSans-W4"))
  update_geom_defaults("text", list(family = theme_get()$text$family))
  update_geom_defaults("label", list(family = theme_get()$text$family))}
#'
#' 単純な散布図
jp_data |> # データフレームを指定
  ggplot(aes(x = 婚姻, y = 離婚)) + # xy軸を設定
  geom_point(colour = "blue", # 表示する色
             shape = 19)      # 表示する形．'?graphics::points' を参照
#' 軸やタイトルを変更
jp_data |> 
  ggplot(aes(x = 婚姻, y = 離婚)) + 
  geom_point(colour = "blue", 
             shape = 19) +    
  labs(x = "1000人あたりの婚姻数",      # x軸のラベル
       y = "1000人あたりの離婚数",      # y軸のラベル
       title = "婚姻数と離婚数の散布図") # タイトル
#' 県名を追加
jp_data |> 
  ggplot(aes(x = 婚姻, y = 離婚)) + 
  geom_point(colour = "blue",
             shape = 19) + 
  geom_text(aes(label = 県名), # ラベルとして県名を指定
            size = 3,         # サイズは適宜調整
            vjust = -1) +     # ラベルに位置(縦)の調整  
  labs(x = "1000人あたりの婚姻数",
       y = "1000人あたりの離婚数",
       title = "婚姻数と離婚数の散布図")
#'
#' 地方ごとに色と点の形を変える
jp_data |> 
  mutate(地方 = as_factor(jp_area[["地方"]])) |> # 地方区分を追加
  ggplot(aes(x = 婚姻, y = 離婚)) + 
  geom_point(aes(colour = 地方,   # 地方ごとに色を変える
                 shape = 地方)) + # 地方ごとに形を変える
  geom_text(aes(label = 県名), 
            size = 2,
            vjust = -1) +
  labs(x = "1000人あたりの婚姻数",
       y = "1000人あたりの離婚数",
       title = "婚姻数と離婚数の散布図")
#' @notes
#' shape 属性は6種類までが推奨されている．
#' そのままでは表示されないが，手動で設定すれば表示される
ggplot2::last_plot() + # 最後に描いたグラフオブジェクトに追加
  scale_shape_manual(values = 1:8) # 形は '?points' を参照
#' @notes
#' 図中に入れる文字を自動的に調整するパッケージもある
#' 重なりが多いところはラベルを削除するので注意は必要
library(ggrepel)
if(Sys.info()["sysname"] == "Darwin") { # maxOS のための日本語フォントの設定
  update_geom_defaults("text_repel", list(family = theme_get()$text$family))
  update_geom_defaults("label_repel", list(family = theme_get()$text$family))}
jp_data |> 
  mutate(地方 = as_factor(jp_area[["地方"]])) |>
  ggplot(aes(x = 婚姻, y = 離婚)) + 
  geom_point(aes(colour = 地方,
                 shape = 地方)) +
  geom_text_repel(aes(label = 県名), # geom_text の拡張
                  size = 2) +       # サイズは適宜調整 
  labs(x = "1000人あたりの婚姻数",
       y = "1000人あたりの離婚数",
       title = "婚姻数と離婚数の散布図")
#' 上記と同様に shape については警告が出る
ggplot2::last_plot() + 
  scale_shape_manual(values = LETTERS) # 文字を指定することもできる
#' ---------------------------------------------------------------------------

#' @exercise ヒストグラムの描画
#' 各日の全天日射量の頻度分布

tw_data |>
  ggplot(aes(x = solar)) + # 分布を描画する列を指定
  geom_histogram(bins = 30, fill = "pink", colour = "red") +
  labs(x = expression(MJ/m^2), # 数式の表示は '?plotmath' を参照
       title = "Solar Radiation in Tokyo")

#' @notes
#' 関数 geom_text() を用いて各ビンの頻度を表示することができる
tw_data |>
  ggplot(aes(x = solar)) + 
  geom_histogram(bins = 30, fill = "pink", colour = "red") +
  geom_text(stat="bin", bins = 30, colour = "darkblue", size = 3, 
            aes(label = after_stat(count), y = after_stat(count) + 1)) +
  labs(x = expression(paste("intensity [",MJ/m^2,"]")), # 文字列と数式を結合
       y = "frequency", # geom_text で y軸のラベルが書き換えられるため
       title = "Solar Radiation in Tokyo")

#' @exercise 密度の描画
#' 各日の全天日射量の密度推定

tw_data |>
  ggplot(aes(x = solar)) + 
  geom_density(fill = "pink", colour = "red") + 
  labs(x = expression(MJ/m^2), 
       title = "Solar Radiation in Tokyo")

#' @notes
#' 関数 geom_function() を用いて理論曲線と比較することができる
tw_data |>
  ggplot(aes(x = solar)) + 
  geom_density(fill = "pink", colour = "red") +
  geom_function(fun = dnorm, # 正規分布と比較
                args = with(tw_data, # 標本平均と標準偏差を計算
                            list(mean = mean(solar),
                                 sd = sd(solar))),
                color = "blue") + # 色を指定
  labs(x = expression(paste("intensity [",MJ/m^2,"]")),
       y = "density", # geom_function で y軸のラベルが書き換えられるため
       title = "Solar Radiation in Tokyo")
#' ヒストグラムも理論曲線と重ねることができるが，
#' 標準ではy軸は頻度なので，以下のように書き換える必要がある
tw_data |>
  ggplot(aes(x = solar)) + 
  geom_histogram(aes(y = after_stat(density)), # y軸を密度表示
                bins = 30, fill = "pink", colour = "red") +
  geom_function(fun = dnorm, # 正規分布と比較
                args = with(tw_data, # 標本平均と標準偏差を計算
                            list(mean = mean(solar),
                                 sd = sd(solar))),
                color = "blue") + # 色を指定
  labs(x = expression(paste("intensity [",MJ/m^2,"]")),
       y = "density", # geom_function で y軸のラベルが書き換えられるため
       title = "Solar Radiation in Tokyo")

#' @exercise 箱ひげ図の描画
#' 月ごとの日射量の分布

tw_data |>
  mutate(month = as_factor(month)) |> # 月を因子化
  ggplot(aes(x = month, y = solar)) + # 月毎に集計する
  geom_boxplot(fill = "orange") + # 塗り潰しの色を指定
  labs(title = "Solar Radiation in Tokyo")

#' @notes
#' 関数 geom_violin() を用いると密度関数を表示することができる
tw_data |>
  mutate(month = as_factor(month)) |> 
  ggplot(aes(x = month, y = solar)) + 
  geom_violin(colour = "red", fill = "pink") + # 色を指定
  labs(title = "Solar Radiation in Tokyo")
#' 表示する幅を適切に調整すれば両者を重ねることもできる
tw_data |>
  mutate(month = as_factor(month)) |> 
  ggplot(aes(x = month, y = solar)) + 
  geom_violin(width = 1.5, colour = "red", fill = "pink") + 
  geom_boxplot(width = 0.1, fill = "orange") + 
  labs(title = "Solar Radiation in Tokyo")

#' @exercise 棒グラフの描画
#' 月ごとの日射量・降水量・降雪量の合計値の推移

tw_data |> 
  mutate(month = as_factor(month)) |> group_by(month) |>
  summarize(across(c(solar, rain, snow), sum)) |> # 月ごとに集計
  pivot_longer(!month) |> # long format に変更
  ggplot(aes(x = name, y = value, fill = month)) +
  geom_bar(stat = "identity", position = "dodge", na.rm = TRUE) +
  theme(legend.position = "top") + guides(fill = guide_legend(nrow = 2))

#' @notes
#' 並べ方の指定を変えてみるには以下のようにすればよい
foo <- # 共通部分を保存
  tw_data |> 
  mutate(month = as_factor(month)) |> group_by(month) |>
  summarize(across(c(solar, rain, snow), sum)) |> 
  pivot_longer(!month) |>
  mutate(name = as_factor(name)) |> # name を出現順に処理するために因子化
  ggplot(aes(x = name, y = value, fill = month))
#' 積み上げ (stack)
foo + geom_bar(stat = "identity", position = "stack") + labs(x = NULL)
#' 横並び (dodge) 
foo + geom_bar(stat = "identity", position = "dodge") + labs(x = NULL)
#' 比率の表示 (fill)
foo + geom_bar(stat = "identity", position = "fill") + labs(x = NULL)

#' ---------------------------------------------------------------------------
#' @practice いろいろなグラフの描画
#' 東京都の感染動向データによる例
#' (書き方はいろいろあるので，以下はあくまで一例)
#'
#' データの読み込み
tc_data <- read_csv(file = "data/tokyo_covid19_2021.csv") |>
  rename(年月日 = ...1) # CSVファイルの1列目の名前が空白なので定義しておく
#'
#' 陽性患者数の推移 (折れ線グラフ)
tc_data |>
  ggplot(aes(x = 年月日, y = 陽性者数)) +
  geom_line(colour = "red") +
  labs(title = "陽性患者数の推移")
#' 陽性率の推移 (折れ線グラフ)
tc_data |>
  mutate(陽性率 = 陽性者数/総検査実施件数) |>
  ggplot(aes(x = 年月日, y = 陽性率)) +
  geom_line(colour = "blue") +
  labs(title = "陽性率の推移")
#' 両者を同時に表示する
tc_data |>
  mutate(陽性率 = 陽性者数/総検査実施件数) |>
  pivot_longer(c(陽性者数,陽性率)) |>
  ggplot(aes(x = 年月日, y = value, colour = name)) +
  geom_line(show.legend = FALSE) +
  facet_grid(rows = vars(name), scales = "free_y") +
  labs(title = "陽性者数・陽性率の推移")
#'
#' 月ごとの検査実施件数の推移 (棒グラフ)
tc_data |> 
  mutate(月 = as_factor(月)) |> group_by(月) |>
  summarize(検査実施件数合計 = sum(総検査実施件数)) |>
  ggplot(aes(x = 月, y = 検査実施件数合計)) +
  geom_bar(stat = "identity", position = "dodge",
           colour = "blue", fill = "lightblue") +
  labs(title = "月ごとの検査実施人数の推移")
#'
#' 曜日ごとの検査実施件数の分布 (箱ひげ図)
tc_data |> 
  ggplot(aes(x = 曜日, y = 総検査実施件数)) +
  geom_boxplot(colour = "blue", fill = "lightblue") +
  labs(title = "曜日ごとの検査実施件数の分布")
#' 曜日を順序付きの因子に変換してグラフの表示順を制御する
tc_data |> 
  mutate(曜日 = factor(曜日, # levels で順序を指定．labels で名称を変更
                       levels=c("日曜日","月曜日","火曜日","水曜日","木曜日","金曜日","土曜日"), 
                       labels=c("日曜","月曜","火曜","水曜","木曜","金曜","土曜"))) |>
  ggplot(aes(x = 曜日, y = 総検査実施件数)) +
  geom_boxplot(colour = "blue", fill = "lightblue") +
  labs(title = "曜日ごとの検査実施件数の分布")
#' ---------------------------------------------------------------------------
