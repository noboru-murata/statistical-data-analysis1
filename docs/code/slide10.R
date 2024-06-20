### 第10講 サンプルコード
library(tidyverse)
#' 日本語を用いる場合 macOS では以下の設定を行うと良い
if(Sys.info()["sysname"] == "Darwin") { # macOS か調べて日本語フォントを指定
  theme_update(text = element_text(family = "HiraginoSans-W4"))
  update_geom_defaults("text", list(family = theme_get()$text$family))
  update_geom_defaults("label", list(family = theme_get()$text$family))}

#' ---------------------------------------------------------------------------
#' @practice 一様分布の平均の推定
#' 
set.seed(1234) # 乱数のシード値の指定
#'
#' 平均値の推定を行う関数(標本平均，中央値，最大最小の平均)
estimate_means <- function(n, min, max){
  x <- runif(n, min = min, max = max)
  return(c(xbar = mean(x),           # 標本平均
           med = median(x),          # 中央値
           mid = (max(x)+min(x))/2)) # 最大最小の平均
}
#'
#' 実験の設定 
n <- 10         # 観測データのサンプル数
mc <- 10000     # 実験回数
a <- 0; b <- 50 # 一様乱数の区間
mc_data <- 
  replicate(mc, estimate_means(n, min = a, max = b)) |> # 3 x mc 型行列
  t() |>      # 転置 mc x 3 型行列
  as_tibble() # データフレーム化
mc_data # 実験結果の一部を確認
mc_data |> # 集計 (各推定値の平均と分散を計算)
  summarize_all(list(mean = mean, var = var))
#'
#' @notes
#' 関数 summarize_all() は以下と等価
#' 
mc_data |> 
  summarize(across(everything(),
                   list(mean = mean, var = var)))
#'
#' もう少し詳しくみてみる
mc_data |> summary() # 四分位点を表示 (簡便な方法)
mc_data |> # データフレームとして取得する方法 (以下は一例)
  pivot_longer(everything()) |> # long format に変更
  group_by(name) |> # 推定量ごとに集計
  summarize(as_tibble_row(quantile(value))) # 計算結果を行に並べる
#'
#' それぞれのヒストグラムを書いてみる
for(name in c("xbar","med","mid")) {
  name <- sym(name)
  gg <-
    mc_data |>
    ggplot(aes(x = !!name)) +
    geom_histogram(bins = 40,
                   fill = "pink",
                   colour = "brown") +
    xlim(a,b) + ylim(0,2500) + # 同じ大きさの図にする
    labs(x = "estimate",
         title = paste(name, "の分布"))
  print(gg)
}
#'
#' @notes
#' 違う分布で試してみる
#' 
estimate_means2 <- function(n){
  x <- rt(n, df = 2) # 自由度2のt分布 (裾が重く平均が推定しにくい分布)
  return(c(xbar = mean(x), med = median(x), mid = (max(x)+min(x))/2))
}
mc_data2 <- 
  replicate(mc, estimate_means2(n)) |> t() |> as_tibble()
#'
#' それぞれの分布を書いてみる
for(name in c("xbar","med","mid")) { 
  name <- sym(name)
  gg <-
    mc_data2 |>
    ggplot(aes(x = !!name)) +
    geom_histogram(aes(y = after_stat(density)), # 密度表示
                   bins = 40,
                   fill = "pink",
                   colour = "brown") +
    xlim(-10,10) + # x軸を同じにする
    labs(x = "estimate",
         title = paste(name, "の分布"))
  print(gg)
}
#'
#' この分布では中央値が良い推定となることがわかる
#'
#' @notes
#' Rで用いることのできる色の名前は関数 colors()/colours() で確認することができる
#' 例えば以下のようにすると視覚的に確認することができる
cols <- colors()[grep("(grey|[0-1,3-9])",  # greyおよび2以外の数を含む色を排除
                      colors(), invert = TRUE)]
ncols <- 6; nrows <- ceiling(length(cols)/ncols) # 必要なタイルの枚数の計算
tibble(x = rep(1:ncols, nrows),
       y = rep(1:nrows, each = ncols),
       z = factor(1:(nrows*ncols))) |>   # タイルの配置のためのデータフレーム
  slice_head(n = length(cols)) |>        # 必要な行のみ残す
  ggplot(aes(x = x, y = y, fill = z)) +
  scale_fill_manual(values = cols) +     # fillの色を指定
  geom_tile(show.legend = FALSE) +       # 色のタイルを配置
  geom_text(aes(label = cols), size = 2) # 色名を記入
#' ---------------------------------------------------------------------------

library("stats4") # 関数mleを利用するため
## 数値最適化のためには尤度関数を最初に評価する初期値が必要
mle.gamma <- function(x, # 観測データ
                      nu0=1, alpha0=1){ # nu, alphaの初期値
    ## 負の対数尤度関数を定義 (最小化を考えるため)
    ll <- function(nu, alpha) # nuとalphaの関数として定義 
        suppressWarnings(-sum(dgamma(x, nu, alpha, log=TRUE)))
    ## suppressWarnings は定義域外で評価された際の警告を表示させない
    ## 最尤推定(負の尤度の最小化)
    est <- mle(minuslogl=ll, # 負の対数尤度関数
               start=list(nu=nu0, alpha=alpha0), # 初期値
               method="BFGS", # 最適化方法 (選択可能)
               nobs=length(x)) # 観測データ数
    return(coef(est)) # 推定値のみ返す
}

#' ---------------------------------------------------------------------------
#' @practice ガンマ分布による風速データのモデル化
#' 
#' データの取得
tw_data <- read_csv("data/tokyo_weather.csv")
#'
#' ヒストグラムの描画
tw_data |>
  ggplot(aes(x = wind)) +
  geom_histogram(aes(y = after_stat(density)), # 密度表示
                 bins = 30,
                 fill = "skyblue",
                 colour = "slateblue") +
  labs(x = "風速 [m/s]", y = "密度", title = "風速のヒストグラム")
#'
#' ガンマ分布の最尤推定量
library("stats4") # 関数mleを利用
mle.gamma <- function(x,                # 観測データ
                      nu0 = 1,          # nuの初期値
                      alpha0 = 1,       # alphaの初期値
                      verbose = FALSE){ # debug用に追加
  ## 負の対数尤度関数を定義 (最小化を考えるため)
  ll <- function(nu, alpha) # nuとalphaの関数として定義 
    suppressWarnings(-sum(dgamma(x, nu, alpha, log=TRUE)))
  ## suppressWarnings は定義域外で評価された際の警告を表示させない
  ## 最尤推定(負の尤度の最小化)
  est <- mle(minuslogl=ll, # 負の対数尤度関数
             start=list(nu=nu0, alpha=alpha0), # 初期値
             method="BFGS", # 最適化方法 (選択可能)
             nobs=length(x)) # 観測データ数
  if(verbose) { 
    return(est) # verbose=TRUEならmleの結果を全て返す
  } else {
    return(coef(est)) # 推定値のみ返す
  }
}
#' 最尤推定の計算
(theta <- with(tw_data, mle.gamma(wind))) 
#'
#' 結果の重ね描き
last_plot() +
  geom_function(fun = dgamma,
                args = list(shape = theta[1],
                            rate = theta[2]),
                colour = "orange",
                linewidth = 1.2)
#'
#' @notes
#' シミュレーションによる一致性の検証
#' 
set.seed(5678) # 乱数のシード値の指定
nu <- 5; alpha <- 2 # 真のパラメタ
mc <- 1000 # 実験回数 (計算が重いので少なめにしている)
for(n in c(10, 50, 100)){ # データ数を変えて実験
  ## Monte-Carlo実験
  mc_data <- # 推定値のデータフレーム
    replicate(mc, mle.gamma(rgamma(n, nu, alpha))) |>
    t() |> as_tibble()
  ## 結果を密度推定で表示
  gg <-
    mc_data |>
    ggplot(aes(x = nu)) + # nu の推定値の分布
    geom_density(fill = "skyblue1",
                 colour = "skyblue4") +
    geom_vline(xintercept = nu, # nu の真値
               colour = "tomato",
               linewidth = 1.2) +
    xlim(0,20) +
    labs(x = expression(nu), title = paste("n =",n))
  print(gg)
  gg <- 
    mc_data |>
    ggplot(aes(x = alpha)) + # alpha の推定値の分布
    geom_density(fill = "seagreen1",
                 colour = "seagreen4") +
    geom_vline(xintercept = alpha, # alpha の真値
               colour = "tomato",
               linewidth = 1.2) +
    xlim(0,10) +
    labs(x = expression(alpha), title = paste("n =",n)) 
  print(gg)
}
#' ---------------------------------------------------------------------------

#' ---------------------------------------------------------------------------
#' @practice 日射量データの区間推定
#' 
#' データの取得
tw_data <- read_csv("data/tokyo_weather.csv")
#'
#' 全データによる平均値の計算
(mu <- tw_data |> pull(solar) |> mean()) # 真値
#'
#' 50点による90%信頼区間
set.seed(1357) # 乱数のシード値の指定
n <- 50
idx <- sample(nrow(tw_data),n) # データの抽出
(xbar <- tw_data |> slice(idx) |> pull(solar) |> mean()) # 標本平均
(sigma <- tw_data |> slice(idx) |> pull(solar) |> sd())  # 標本標準偏差
z95 <- qnorm(0.95) # 標準正規分布の0.95分位点
(ci <- c(L = xbar-z95*sigma/sqrt(n),
         U = xbar+z95*sigma/sqrt(n))) # 信頼区間
#'
#' 信頼区間の正答率の評価
mc <- 100
my_trial <- function(n){ # nを変えて実験できるように
  idx <- sample(nrow(tw_data),n)
  xbar <- tw_data |> slice(idx) |> pull(solar) |> mean() # 標本平均
  sigma <- tw_data |> slice(idx) |> pull(solar) |> sd()  # 標本標準偏差
  return(c(L = xbar-z95*sigma/sqrt(n),
           U = xbar+z95*sigma/sqrt(n))) # 信頼区間
}
mc_data <- # 信頼区間のMonte-Carlo実験
  replicate(mc, my_trial(n)) |> t() |> as_tibble() |>
  mutate(answer = L < mu & mu < U) # 真値が信頼区間に含まれるか
mc_data |> pull(answer) |> table()
#'
#' @notes
#' 信頼区間について多数で評価する
#' 
mc <- 2000
mc_data <- # 信頼区間のMonte-Carlo実験
  replicate(mc, my_trial(n)) |> t() |> as_tibble() |>
  mutate(answer = L < mu & mu < U) # 真値が信頼区間に含まれるか
mc_data |> pull(answer) |> table()/mc # 確率を見る
#'
#' @notes
#' グラフを描いてみる
#' 
k <- 20 
idx <- sample(nrow(mc_data), k) 
mc_data |>
  slice(sample(nrow(mc_data), k)) |> # k個ランダムに選んで描く
  rowid_to_column(var = "index") |>
  ggplot(aes(x = index)) +
  geom_errorbar(aes(ymin = L, ymax = U),
                colour = "blue",
                width = 0.2) +
  geom_hline(yintercept = mu, # 観測データを全て使った推定値
             colour = "orange",
             linewidth = 1.1)
#' 日射量の平均の推定における
#' 観測データを全て使った推定値(真値に相当)と信頼区間の関係
#' ---------------------------------------------------------------------------
