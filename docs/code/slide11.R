### 第11講 サンプルコード
library(tidyverse)
#' 以下，日本語を用いるため macOS では以下の設定を行う
if(Sys.info()["sysname"] == "Darwin") { # macOS か調べて日本語フォントを指定
  theme_update(text = element_text(family = "HiraginoSans-W4"))
  update_geom_defaults("text", list(family = theme_get()$text$family))
  update_geom_defaults("label", list(family = theme_get()$text$family))}

#' ---------------------------------------------------------------------------
#' @practice t検定のMonte-Carlo実験
#' 
n <- 10
mu0 <- 5
sd0 <- 3
mc <- 10000
alpha <- 0.05
#'
#' 検定統計量の分布を調べてみる
mc_trial <- function(n){ 
  result <- t.test(rnorm(n, mean = mu0, sd = sd0), mu = mu0)
  return(result$statistic)} # 検定統計量を取り出す
mc_data <- replicate(mc, mc_trial(n)) |> as_tibble()
mc_data |>
  ggplot(aes(x = value)) +
  geom_histogram(aes(y = after_stat(density)), # 密度表示
                 bins = 40,
                 fill = "lightgreen",
                 colour = "seagreen") +
  geom_function(fun = dnorm,
                colour = "red",
                linewidth = 1.2) +
  geom_function(fun = dt,
                args = list(df = n-1),
                colour = "blue",
                linewidth = 1.2) +
  labs(x = "t統計量", title = "検定統計量の分布")
#' 自由度 n-1 の t分布が良く当て嵌まることが確認できる
#'
#' p 値の分布を見る
mc_trial <- function(n){ 
  result <- t.test(rnorm(n, mean = mu0, sd = sd0), mu = mu0)
  return(result$p.value)} # p値を取り出す
mc_data <- replicate(mc, mc_trial(n)) |> as_tibble()
mc_data |>
  ggplot(aes(x = value)) +
  geom_histogram(fill = "plum",
                 colour = "orchid") +
  labs(x = "p値", title = "p値の分布")
table(mc_data < alpha)/mc # alpha以下の結果の数を調べる = サイズ
#' p値は一様に分布し，有意水準と第一種の過誤の関係が確認できる
#'
#' 帰無仮説が成り立たない場合の p 値の分布を見る
mc_trial <- function(n){ 
  result <- t.test(rnorm(n, mean = mu0+1, sd = sd0), mu = mu0)
  return(result$p.value)} # p値を取り出す
replicate(mc, mc_trial(n)) |>
  as_tibble() |>
  ggplot(aes(x = value)) +
  geom_histogram(fill = "khaki",
                 colour = "tan") +
  labs(x = "p値", title = "帰無仮説が成り立たない場合")
#' p値は一様に分布しなくなることが確認できる
#'
#' @notes
#' たとえば以下のようにすると関数 t.test() で計算される量をまとめて取り出すことができる
#' まとめて計算できるが，計算が若干重い
#' 
mc_trial <- function(n){ 
  t.test(rnorm(n, mean = mu0, sd = sd0), mu = mu0) |>
    broom::tidy() |> # tidyverse のパッケージ．様々なオブジェクトを tibble 形式に変換
    select(where(is.numeric)) |> # 数値のみ取り出す
    unlist() # 名前付の数値ベクトルに変換
}
mc <- 2000
replicate(mc, mc_trial(n)) |> t() |> as_tibble() |>
  pivot_longer(c(statistic.t, p.value)) |>
  ggplot(aes(x = value, fill = name)) +
  geom_histogram(show.legend = FALSE) +
  facet_wrap(vars(name), ncol = 2, scales = "free") # 横に並べる
#' ---------------------------------------------------------------------------

#' ---------------------------------------------------------------------------
#' @practice 視聴率の検定
#'
#' X は正規分布ではないが，nが十分大きい場合には
#' 区間推定と同様に正規分布(自由度の大きなt分布)で近似される
n <- 600
mu0 <- 0.1 # 越えたい視聴率
mu1 <- 0.11 # 真の視聴率(11%)
x <- sample(0:1, n, replace = TRUE, prob = c(1-mu1,mu1))
table(x)
t.test(x,mu = mu0, alternative = "greater")
#'
#' この設定でMonte-Carlo実験を行う
mc <- 10000
alpha <- 0.05 # 自由に設定せよ
mc_trial <- function(n){ 
  x <- sample(0:1, n, replace = TRUE, prob = c(1-mu1,mu1))
  result <- t.test(x, mu = mu0, alternative = "greater")
  return(result$p.value)}
mc_data <- replicate(mc, mc_trial(n)) |> as_tibble()
mc_data |>
  ggplot(aes(x = value)) +
  geom_histogram(fill = "plum",
                 colour = "orchid") +
  labs(x = "p値", title = paste0("p値の分布 (n=",n,")"))
table(mc_data < alpha)/mc # alpha以下のデータの数を調べる=検出力
#'
#' n を変えて実験してみる
n <- 5000
mc_data <- replicate(mc, mc_trial(n)) |> as_tibble()
mc_data |>
  ggplot(aes(x = value)) +
  geom_histogram(fill = "plum",
                 colour = "orchid") +
  labs(x = "p値", title = paste0("p値の分布 (n=",n,")"))
table(mc_data < alpha)/mc # alpha以下のデータの数を調べる=検出力
#' ---------------------------------------------------------------------------

#' ---------------------------------------------------------------------------
#' @practice 気温の分散の検定
#'
#' データの読み込み
tw_data <- read_csv("data/tokyo_weather.csv") |>
  mutate(month = as_factor(month)) # 月毎の表示のために因子化しておく
#'
#' 月毎の分布を確認する
tw_data |> 
  ggplot(aes(x = month, y = temp)) + 
  geom_boxplot(fill = "lightblue", colour = "blue") +
  labs(x = "月", y = "気温", title = "東京の気温の分布")
#'
#' 月毎の分散を計算する
tw_data |> group_by(month) |> summarize(var(temp)) |>
  ggplot(aes(x = month, # 
             y = `var(temp)`)) +
  geom_bar(stat = "identity", fill = "royalblue") +
  labs(x = "月", y = "気温の分散", title = "東京の気温のばらつき")
#'
#' @notes
#' 関数 summarize() は列名を指定しなければ計算内容に``を付けて列名とする
#' これは "()" が特殊な文字(関数の引数を表す)のためである
#' このような指定が面倒であれば列名を指定すればよい
#' 
tw_data |> group_by(month) |> summarize(var = var(temp)) |>
  ggplot(aes(x = month, y = var)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "月", y = "気温の分散", title = "東京の気温のばらつき")
#' 検定統計量の計算
v0 <- tw_data |> group_by(month) |>
  summarize(var(temp)) |> pull(`var(temp)`) |> mean()
x <- tw_data |> filter(month == 6) |> pull(temp) # 6月の気温のベクトル
(n <- length(x)) # データ数
(chi2 <- (n-1)*var(x)/v0) # 検定統計量
p0 <- pchisq(chi2, df=n-1)
(p <- 2*min(p0,1-p0))
#' 分散が小さな8月を検定してみる
x <- tw_data |> filter(month == 8) |> pull(temp) # 8月の気温のベクトル
(n <- length(x)) # データ数
(chi2 <- (n-1)*var(x)/v0) # 検定統計量
p0 <- pchisq(chi2, df=n-1)
(p <- 2*min(p0,1-p0))
#' ---------------------------------------------------------------------------

#' ---------------------------------------------------------------------------
#' @practice 平均の差の検定
#'
#' データの読み込み
tw_data <- read_csv("data/tokyo_weather.csv") |>
  mutate(month = as_factor(month))
#'
#' 7月と8月の平均気温を比較する
tw_data |>
  filter(month %in% c(7,8)) |> # 月を限定
  ggplot(aes(x = month, y = temp)) +
  geom_boxplot(fill = "pink") +
  geom_jitter(colour = "red", width = 0.2) + # データを重ねて表示
  labs(x = "月", y = "気温", title = "気温の比較")
#' ばらつき方は大きく異なるが平均気温はばらつきの範囲内の違いのように見える
#' 
#' 7月と8月の平均気温の差を検定する
x <- tw_data |> filter(month == 7) |> pull(temp)
y <- tw_data |> filter(month == 8) |> pull(temp)
t.test(x,y)
#'
#' 2月と12月でも試してみる
tw_data |>
  filter(month %in% c(2,12)) |> 
  ggplot(aes(x = month, y = temp)) +
  geom_boxplot(fill = "lightblue") + # 色を変える
  geom_jitter(colour = "blue", width = 0.2) +
  labs(x = "月", y = "気温", title = "気温の比較")
#' 平均気温の差はばらつきと同程度のように見える
#' 
x <- tw_data |> filter(month == 2) |> pull(temp)
y <- tw_data |> filter(month == 12) |> pull(temp)
t.test(x,y)
#'
#' @notes
#' 分布を視覚的に捉えるためには
#' 関数 geom_jitter() 以外にもさまざまなパッケージが利用できる
#' 例えば以下のようなものがある
library(see)
tw_data |>
  filter(month %in% 6:9) |>
  ggplot(aes(x = month, y = temp, fill = month)) +
  geom_violindot(fill_dots = "grey") +
  labs(x = "月", y = "気温", title = "気温の比較")
#' ---------------------------------------------------------------------------

#' ---------------------------------------------------------------------------
#' @practice 分散の比の検定
#' 
#' データの読み込み
tw_data <- read_csv("data/tokyo_weather.csv") |>
  mutate(month = as_factor(month))
#'
#' 3月と6月の気温の分散を比較する
tw_data |>
  filter(month %in% c(3,6)) |> 
  ggplot(aes(x = month, y = temp)) +
  geom_boxplot(fill = "lightgreen") + # 色を変える
  geom_jitter(colour = "seagreen", width = 0.2) +
  labs(x = "月", y = "気温", title = "気温の比較")
#' 平均気温は大きく異なるがばらつきは同等のように見える
#'
x <- tw_data |> filter(month == 3) |> pull(temp)
y <- tw_data |> filter(month == 6) |> pull(temp)
var.test(x,y)
#' 
#' 3月と11月でも試してみる
tw_data |>
  filter(month %in% c(3,11)) |> 
  ggplot(aes(x = month, y = temp)) +
  geom_boxplot(fill = "lightgreen") + # 色を変える
  geom_jitter(colour = "seagreen", width = 0.2) +
  labs(x = "月", y = "気温", title = "気温の比較")
#' 平均気温は似通っているがばらつきは異なるように見える
#' 
x <- tw_data |> filter(month == 3) |> pull(temp)
y <- tw_data |> filter(month == 11) |> pull(temp)
var.test(x,y)
#' ---------------------------------------------------------------------------
