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
  pivot_longer(cols = c(temp, solar)) |> # 集約する列を指定
  ggplot(aes(x = day, y = value, colour = name)) + 
  geom_line() + # index ごとに定義されたカラーパレットの異なる色が用いられる
  labs(title = "Weather in May")

#' @notes
#' 描画に必要な情報は ggplot の中で指定されるが
#' 以下のように必要な列のみを選択してもよい
tw_data |> filter(month == 5) |> select(day, temp, solar) |>
  pivot_longer(!day) |> # day 以外の列を集約．引数名 cols は省略できる
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
  select(month, day, temp, solar, wind) |>
  pivot_longer(!c(month, day)) |> 
  ggplot(aes(x = day, y = value, colour = name)) +
  geom_line(show.legend  =  FALSE) 
foo + facet_grid(rows = vars(name), cols = vars(month)) 
foo + facet_grid(name ~ month) # 同上
foo + facet_wrap(vars(name, month), nrow = 4, ncol = 3) # 4x3 に並べる
foo + facet_wrap(name ~ month, nrow = 4, ncol = 3) # 同上

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
  select(temp, solar, humid) |> # 必要な列を選択
  ggpairs() # 標準の散布図行列 (上三角は相関，対角は密度，下三角は散布図)

#' 月ごとに色分けして表示する

tw_data |> filter(month %in% 7:9) |> select(month, temp, solar, humid) |>
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
tw_data |> filter(month == 5) |> select(day, temp, solar) |>
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

#' @exercise ヒストグラムの描画
#' 各日の全天日射量の頻度分布

tw_data |>
  ggplot(aes(x = solar)) + # 分布を描画する列を指定
  geom_histogram(bins = 30, fill = "pink", colour = "red") +
  labs(x = expression(MJ/m^2), # 数式の表示は '?plotmath' を参照
       title = "Solar Radiation in Tokyo")

#' @notes
#' 横向きにする方法はいくつかある
tw_data |>
  ggplot(aes(y = solar)) + # 分布を描画する列をy軸に指定する
  geom_histogram(bins = 30, fill = "pink", colour = "red") +
  labs(y = expression(MJ/m^2), 
       title = "Solar Radiation in Tokyo")
tw_data |>
  ggplot(aes(x = solar)) + 
  geom_histogram(bins = 30, fill = "pink", colour = "red") +
  labs(x = expression(MJ/m^2),
       title = "Solar Radiation in Tokyo") +
  coord_flip() # 座標を反転する
#'
#' 関数 geom_text() を用いると各ビンの頻度を表示することができる
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
                colour = "blue") + # 色を指定
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
                colour = "blue") + # 色を指定
  labs(x = expression(paste("intensity [",MJ/m^2,"]")),
       y = "density", # geom_function で y軸のラベルが書き換えられるため
       title = "Solar Radiation in Tokyo")

#' @exercise 箱ひげ図の描画
#' 月ごとの日射量の分布

tw_data |>
  mutate(month = as_factor(month)) |> # 月を因子(ラベル)化
  ggplot(aes(x = month, y = solar)) + # 月毎に集計する
  geom_boxplot(fill = "orange") + # 塗り潰しの色を指定
  labs(title = "Solar Radiation in Tokyo")

#' @notes
#' 因子があれば各因子で箱ひげ図を作成するが
#' 関数 mutate() を使わずに以下のように書くこともできる
tw_data |>
  ggplot(aes(x = as_factor(month), y = solar)) + # 月を因子化して集計する
  geom_boxplot(fill = "orange") + 
  labs(title = "Solar Radiation in Tokyo",
       x = "month") # 因子化のため書き変わるラベルを修正
tw_data |>
  ggplot(aes(x = month, y = solar)) + 
  geom_boxplot(aes(group = month), fill = "orange") + # 月毎に集計
  labs(title = "Solar Radiation in Tokyo") +
  scale_x_continuous(breaks = 1:12) # x軸の目盛を明示的に指定
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
  geom_violin(width = 1.2, colour = "red", fill = "pink") + 
  geom_boxplot(width = 0.1, fill = "orange") + 
  labs(title = "Solar Radiation in Tokyo")

#' @exercise 棒グラフの描画
#' 月ごとの日射量・降水量・風速の平均値の推移

tw_data |> 
  mutate(month = as_factor(month)) |> group_by(month) |>
  summarize(across(c(solar, rain, wind), mean)) |> # 月ごとに集計
  pivot_longer(!month) |> # long format に変更
  ggplot(aes(x = name, y = value, fill = month)) +
  geom_bar(stat = "identity", position = "dodge", na.rm = TRUE) +
  theme(legend.position = "top") + guides(fill = guide_legend(nrow = 2))

#' @notes
#' 並べ方の指定を変えてみるには以下のようにすればよい
foo <- # 共通部分を保存
  tw_data |> 
  mutate(month = as_factor(month)) |> group_by(month) |>
  summarize(across(c(solar, rain, wind), mean)) |> 
  pivot_longer(!month) |>
  mutate(name = as_factor(name)) |> # name を出現順に処理するために因子化
  ggplot(aes(x = name, y = value, fill = month))
#' 積み上げ (stack)
foo + geom_bar(stat = "identity", position = "stack") + labs(x = NULL)
#' 横並び (dodge) 
foo + geom_bar(stat = "identity", position = "dodge") + labs(x = NULL)
#' 比率の表示 (fill)
foo + geom_bar(stat = "identity", position = "fill") + labs(x = NULL)
#' 向きを変えるのはヒストグラムなどと同様
tw_data |> 
  mutate(month = as_factor(month)) |> group_by(month) |>
  summarize(across(c(solar, rain, wind), mean)) |> 
  pivot_longer(!month) |> 
  mutate(name = fct_reorder(name, value)) |> # value の小さい順に並べる
  ggplot(aes(y = name, x = value, fill = month)) + # xy軸を入れ替える
  geom_bar(stat = "identity", position = "dodge", na.rm = TRUE) +
  theme(legend.position = "top") + guides(fill = guide_legend(nrow = 2))
tw_data |> 
  mutate(month = as_factor(month)) |> group_by(month) |>
  summarize(across(c(solar, rain, wind), mean)) |> 
  pivot_longer(!month) |> 
  mutate(name = fct_reorder(name, value, .desc = TRUE)) |> # value の大きい順
  ggplot(aes(x = name, y = value, fill = month)) + 
  geom_bar(stat = "identity", position = "dodge", na.rm = TRUE) +
  theme(legend.position = "top") + guides(fill = guide_legend(nrow = 2)) +
  coord_flip()
