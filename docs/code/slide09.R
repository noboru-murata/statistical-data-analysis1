### 第9講 サンプルコード
library(tidyverse)
#' 以下，日本語を用いるため macOS では以下の設定を行う
if(Sys.info()["sysname"] == "Darwin") { # macOS か調べて日本語フォントを指定
  theme_update(text = element_text(family = "HiraginoSans-W4"))
  update_geom_defaults("text", list(family = theme_get()$text$family))
  update_geom_defaults("label", list(family = theme_get()$text$family))}

#' ---------------------------------------------------------------------------
#' @practice 平均・分散・標準偏差の計算
#' 
#' データの読み込み
tw_data <- read_csv("data/tokyo_weather.csv")
#'
#' 全データによる計算
tw_data |>
  summarise(across(c(temp,solar,wind), mean))
tw_data |>
  summarise(across(c(temp,solar,wind), var))
tw_data |>
  summarise(across(c(temp,solar,wind), sd))
(tw_summary <- # まとめて計算して保存しておく
   tw_data |> 
   summarise(across(c(temp,solar,wind),
                    list(mean = mean, var = var, sd =sd))))
#'
#' 毎月5日のデータによる計算
tw_data |>
  filter(day == 5) |>
  summarise(across(c(temp,solar,wind),
                   list(mean = mean, var = var, sd =sd)))
#'
#' 5の付く日のデータによる計算
tw_data |>
  filter(day %in% c(5,15,25)) |>
  summarise(across(c(temp,solar,wind),
                   list(mean = mean, var = var, sd =sd)))
#'
#' ランダムに選択した36日で推定した場合のばらつきを調べる
mc <- 5000 # 実験回数を指定
n <- 36 # ランダムに選択する日数を指定
#'
#' 気温の標本平均による例
my_trial <- function(){ 
  tw_data |>
    slice(sample(nrow(tw_data), n)) |>
    summarise(mean = mean(temp)) |>
    as.numeric() # tibble形式ではなく単なる数値として返す
}
xbars <- replicate(mc, my_trial())
tibble(平均気温の推定値 = xbars) |>
  ggplot(aes(x = 平均気温の推定値)) +
  geom_histogram(aes(y = after_stat(density)), 
                 bins = 30,                    
                 fill ="lightblue",          
                 colour = "blue") + 
  geom_vline(xintercept = tw_summary[["temp_mean"]], # 全体の平均
             colour = "red") 
#'
#' @notes
#' 上記の例では保存しておいた平均の値を参照しているが
#' geom_vline の中で計算し直すこともできる
#' 
tibble(平均気温の推定値 = xbars) |>
  ggplot(aes(x = 平均気温の推定値)) +
  geom_histogram(aes(y = after_stat(density)), 
                 bins = 30,                    
                 fill ="lightblue",          
                 colour = "blue") + 
  geom_vline(xintercept = tw_data |> 
               summarise(mean(temp)) |>
               as.numeric(), # tibble形式を数値に変換
             colour = "red") 
#'
#' @notes
#' 例えば以下のようにすれば
#' 項目および統計量を総当たりで調べることができる
#' 
my_trial <- function(n){ # 日数を指定してまとめて計算するように変更する
  tmp <- # 計算結果を一時保存
    tw_data |>
    slice(sample(nrow(tw_data), n)) |>
    summarise(across(c(temp,solar,wind),
                     list(mean = mean, var = var, sd =sd)))
  return(t(tmp)[,1,drop = TRUE]) # データフレームではなくベクトルとして返す
}
my_data <- # 実験データをデータフレームに直しておく
  replicate(mc, my_trial(n = n)) |> t() |> as_tibble()
#'
#' 図示する部分を関数として定義しておく
my_plot <- function(data,    # データ
                    summary, # データ全体から計算した真値
                    item,    # 項目名
                    func){  # 集計関数名
  name <- sym(paste(item, func, sep = "_")) # シンボルを作成
  gg <- data |>
    ggplot(aes(x = !!name)) + # シンボルを !! によって unquote
    geom_histogram(aes(y = after_stat(density)), 
                   bins = 30,                    
                   fill ="lightblue",          
                   colour = "blue") + 
    geom_vline(xintercept = summary[[name]],
               colour = "red") +
    labs(title = paste(item, "の", func, "の推定"))
  print(gg)
}
#'
#' 全ての組み合わせは for 文で実行可能
for(item in c("temp","solar","wind")){ # 項目を指定
  for(func in c("mean","var","sd")){   # 関数を指定
    my_plot(my_data, tw_summary, item, func)
  }
}
#'
#' 標準偏差の推定量は偏りがあることが確認できる
#' 風速の分散の推定量の分布が他と異なり正規分布に近くないので，
#' サンプル数を3倍に増やしてみる
my_plot(replicate(mc, my_trial(n = 108)) |> t() |> as_tibble()，
        tw_summary, "wind", "var")
#'
#' 推定量の分散がちいさくなるとともに形状が正規分布に近づいたことが確認できる
#' ---------------------------------------------------------------------------

#' ---------------------------------------------------------------------------
#' @practice 歪度と超過尖度の計算
#' 
library("e1071")
#'
#' データの読み込み
tw_data <- read_csv("data/tokyo_weather.csv")
#'
#' 全データによる計算
tw_data |> 
  summarise(across(c(temp,solar,wind),
                   list(skew = skewness, kurt = kurtosis)))
#'
#' @notes
#' パッケージの一部の関数しか利用しない場合は
#' 関数 library() でパッケージを読み込まなくても
#' パッケージ名とともに関数を指定することで利用することができる
#' ただし，関数内で他の関数に依存する場合は注意する必要がある
#' 
tw_data |> 
  summarise(across(c(temp,solar,wind),
                   list(skew = e1071::skewness,
                        kurt = e1071::kurtosis)))
#'
#' 5の付く日のデータによる計算
tw_data |>
  filter(day %in% c(5,15,25)) |>
  summarise(across(c(temp,solar,wind),
                   list(skew = skewness, kurt = kurtosis)))
#'
#' @notes
#' 歪度や尖度は3次・4次の計算なので，少数のデータでは計算結果が不安定になりやすい
#'
#' 各データのヒストグラムと正規分布の比較
tw_summary <- # 前の練習問題の結果を利用 
  tw_data |> 
  summarise(across(c(temp,solar,wind),
                   list(mean = mean, var = var, sd =sd)))
for(item in c("temp","solar","wind")){ # 項目を指定
  item <- sym(item)
  gg <- tw_data |>
    ggplot(aes(x = !!item)) + 
    geom_histogram(aes(y = after_stat(density)), 
                   bins = 30,                    
                   fill ="lightblue",          
                   colour = "blue") + 
    geom_function(fun = dnorm,
                  args = list(mean = tw_summary[[paste0(item,"_mean")]],
                              sd = tw_summary[[paste0(item,"_sd")]]),
                  colour = "red") +
    labs(title = paste(item, "と正規分布の比較"))
  print(gg)
}
#' ---------------------------------------------------------------------------

#' ---------------------------------------------------------------------------
#' @practice 共分散と相関の計算
#' 
#' データの読み込み
tw_data <- read_csv("data/tokyo_weather.csv")
#'
#' 共分散・相関行列の計算
tw_cov <-
  tw_data |>
  select(temp,rain,solar,wind,press,humid) |>
  cov()
tw_cor <-
  tw_data |>
  select(temp,rain,solar,wind,press,humid) |>
  cor()
tw_cor == min(tw_cor) # 負の最大相関 (temp,press)
tw_cor == max(tw_cor-diag(diag(tw_cor))) # 対角を除く最大相関 (temp,humid)
abs(tw_cor) == min(abs(tw_cor)) # 最小相関(0に近い) (rain,wind)
#'
#' 散布図の描画
tw_data |> # 対象データを全て表示してみる
  select(temp,rain,solar,wind,press,humid) |>
  GGally::ggpairs() # packageを読み込まずに直接指定する
tw_data |> # columnsを指定すれば関数selectを使わなくてもよい
  GGally::ggpairs(columns = c("temp","rain","solar","wind","press","humid"))
tw_data |> # 月ごとに色を変えることもできる
  GGally::ggpairs(columns = c("temp","rain","solar","wind","press","humid"),
                  lower = list(mapping = aes(colour = as_factor(month))))
tw_data |> # 負の最大相関
  GGally::ggpairs(columns = c("temp","press"))
tw_data |> # 最大相関
  GGally::ggpairs(columns = c("temp","humid"))
tw_data |> # 最小相関
  GGally::ggpairs(columns = c("rain","wind"))
#'
#' @notes
#' 数値項目を抽出するのであれば以下のように簡潔に書ける
#' 
tw_data |>
  select(where(is.numeric))
#' ただし不要な項目も含まれるので
tw_data |>
  select(where(is.numeric) & !year:day & !c(snow,cloud))
#' などの工夫は必要となる
#' ---------------------------------------------------------------------------

#' ---------------------------------------------------------------------------
#' @practice 分位点と最頻値の計算
#' 
#' データの読み込み
tw_data <- read_csv("data/tokyo_weather.csv")
#'
#' 気温の分位点
#' 全データによる計算
(tw_temp_summary <-
   tw_data |>
   pull(temp) |> # 1列のみ抽出してベクトルにする
   summary())
#'
#' 5の付く日のデータによる計算
tw_data |>
  filter(day %in% c(5,15,25)) |>
  pull(temp) |>
  summary()
#'
#' ランダムに選択した36日で推定した場合のばらつき
mc <- 5000 # 実験回数を指定
my_trial <- function(){
  tw_data |>
    slice(sample(nrow(tw_data),36)) |> # 重複なしで36行選ぶ
    pull(temp) |>
    summary()
}
my_data <-
  replicate(mc, my_trial()) |> t() |> as_tibble()
#'
#' ヒストグラムの表示
for(name in names(my_data)){
  name <- sym(name)
  gg <- my_data |>
    ggplot(aes(x = !!name)) +
    geom_histogram(aes(y = after_stat(density)),
                   bins = 40,
                   fill = "lightblue",
                   colour = "blue") +
    geom_vline(xintercept = tw_temp_summary[[name]],
               colour = "red") +
    labs(x = "気温",
         title = paste("気温の", name, "の推定"))
  print(gg)
}
#'
#' ヒストグラムの表示 (定義域とビンを揃えて表示する)
breaks <- # 適切なビンの計算して固定する
  tw_data |> pull(temp) |> pretty(n = 40)
for(name in names(my_data)){
  name <- sym(name)
  gg <- my_data |>
    ggplot(aes(x = !!name)) +
    geom_histogram(aes(y = after_stat(density)),
                   breaks = breaks, # ビンを固定
                   fill = "lightblue",
                   colour = "blue") +
    geom_vline(xintercept = tw_temp_summary[[name]],
               colour = "red") +
    ylim(0,0.5) + # y軸の表示を適当に固定する
    labs(x = "気温",
         title = paste("気温の", name, "の推定"))
  print(gg)
}
#'
#' 最多風向の最頻値
(my_table <-
   tw_data |>
   pull(wdir) |>
   table()) # 頻度表
max(my_table) # 最大値 
which.max(my_table) # 最頻値の表の位置
names(which.max(my_table)) # 最頻値の項目名
#'
#' @notes
#' 例えば以下のようにすれば16方位の頻度をグラフ化することができる
#' 
cardinal_directions <- c("N","NNE","NE","ENE",
                         "E","ESE","SE","SSE",
                         "S","SSW","SW","WSW",
                         "W","WNW","NW","NNW")
tw_data |>
  mutate(wdir = factor(wdir, levels = cardinal_directions)) |>
  group_by(wdir, .drop = FALSE) |> # 0も含めて16方位全て計算する
  summarize(count = n()) |> # 関数 table() だと頻度 0 の項目はないことに注意
  ggplot(aes(x = wdir, y = count)) +
  geom_bar(stat = "identity", fill = alpha("blue", 0.4)) +
  coord_polar(start = 0) # 極座標表示にする
#'
#' 中心を空洞にするには負の値で下駄を履かせればよい
tw_data |>
  mutate(wdir = factor(wdir, levels = cardinal_directions)) |>
  group_by(wdir, .drop = FALSE) |> # 0も含めて16方位全て計算する
  summarize(count = n()) |>
  ggplot(aes(x = wdir, y = count)) +
  geom_bar(stat = "identity", fill = alpha("blue", 0.4)) +
  ylim(-40,80) + # 負の側を表示範囲に加える
  coord_polar(start = -pi/16) + # 北を真上に配置
  labs(x = NULL, y = NULL, title = "風向きの頻度") # 不要なラベルを削除
#' ---------------------------------------------------------------------------
