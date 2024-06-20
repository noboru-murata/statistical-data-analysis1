### 第8講 サンプルコード
library(tidyverse)
#' 日本語を用いる場合 macOS では以下の設定を行うと良い
if(Sys.info()["sysname"] == "Darwin") { # macOS か調べて日本語フォントを指定
  theme_update(text = element_text(family = "HiraginoSans-W4"))
  update_geom_defaults("text", list(family = theme_get()$text$family))
  update_geom_defaults("label", list(family = theme_get()$text$family))}

#' @exercise 離散一様分布で用いた例

#' @exercise 二項分布で用いた例

rbinom(10, size=1, prob=0.2) # Bernoulli分布(10個)
rbinom(20, size=5, prob=0.6) # 二項分布(20個)

#' ---------------------------------------------------------------------------
#' @practice 二項分布
#' 
mc <- 10000 # 実験回数を指定
n <- 16 
p <- 0.6
my_random <- function(){ # Bernolli分布をm個生成して合計
  sum(rbinom(n, size=1, prob=p))}
my_data <- replicate(mc, my_random())
my_table <- table(my_data)/mc # 出現確率ごとの表(度数分布表)を作成
tibble(値 = as.numeric(names(my_table)),
       観測値 = as.numeric(my_table),
       理論値 = dbinom(値, size = n, prob = p)) |>
  pivot_longer(!値, values_to = "確率") |>
  ggplot(aes(x = 値, y = 確率, fill = name)) +
  geom_bar(stat = "identity",
           width = 0.8, # 並びがわかりやすいように幅を調整
           position = "dodge") +
  labs(fill = NULL, # 凡例のfillの名称(name)を消去
       title = paste0("二項分布(試行回数", n, ", 成功確率", p, ")"))
#'
#' @notes
#' データフレームはいろいろな方法で作成できる
#' 
my_table |>
  as_tibble() |> # 2列のデータフレーム (my_table<文字列>, n<数値>)
  magrittr::set_colnames(c("値","観測値")) |> # 列名を全て変更
  mutate(値 = as.integer(値), # 整数(数値でも可)に変換
         理論値 = dbinom(値, size = n, prob = p)) |>
  pivot_longer(!値, values_to = "確率") |>
  ggplot(aes(x = 値, y = 確率, fill = name)) +
  geom_bar(stat = "identity",
           width = 0.8, 
           position = "dodge") +
  labs(fill = NULL, 
       title = paste0("二項分布(試行回数", n, ", 成功確率", p, ")"))
#' ---------------------------------------------------------------------------

#' @exercise Poisson 分布で用いた例

rpois(15, lambda=1) # 強度1の Poisson 分布(15個)
rpois(15, lambda=10) # 強度10の Poisson 分布(15個)

#' ---------------------------------------------------------------------------
#' @practice Poisson 分布
#' 
mc <- 10000 
lambda1 <- 5
lambda2 <- 12
my_random <- function(){ # 2つの Poisson 分布の和
  rpois(1, lambda=lambda1)+rpois(1, lambda=lambda2)}
my_data <- replicate(mc, my_random())
my_table <- table(my_data)/mc
tibble(値 = as.numeric(names(my_table)),
       観測値 = as.numeric(my_table),
       理論値 = dpois(値, lambda=lambda1+lambda2)) |>
  pivot_longer(!値, values_to = "確率") |>
  ggplot(aes(x = 値, y = 確率, fill = name)) +
  geom_bar(stat = "identity",
           width = 0.8, 
           position = "dodge") +
  labs(fill = NULL, 
       title = paste0("Poisson 分布(強度", lambda1+lambda2, ")"))
#' ---------------------------------------------------------------------------

#' @exercise 幾何分布で用いた例

rgeom(15, prob=0.1) # 成功確率0.1の幾何分布(15個)

#' @exercise 一様分布で用いた例

runif(8) # 区間(0,1)上の一様乱数(8個)
runif(8,min=-1,max=1) # 区間(-1,1)上の一様乱数(8個)

#' @exercise 正規分布で用いた例

rnorm(8) # 標準正規乱数(8個)
rnorm(8,mean=1,sd=2) # 平均1分散4=2^2の正規乱数

#' ---------------------------------------------------------------------------
#' @practice 正規分布
#' 
mc <- 10000 # 実験回数を指定
my_random <- function(){ # 一方の分布を確認する
  u1 <- runif(1)
  u2 <- runif(1)
  return(sqrt(-2*log(u1))*cos(2*pi*u2))}
my_data <- replicate(mc, my_random()) # Monte-Carlo実験
tibble(x = my_data) |>
  ggplot(aes(x = x)) +
  geom_histogram(aes(y = after_stat(density)), 
                 bins = 30,                    
                 fill ="lightblue",          
                 colour = "white") + 
  geom_function(fun = dnorm, 
                colour = "red") +
  labs(title = "標準正規分布")
#'
#' Box-Muller法で作られる2つの確率変数の関係を調べる
#' 
boxmuller <- function(){
  u1 <- runif(1)
  u2 <- runif(1)
  x1 <- sqrt(-2*log(u1))*cos(2*pi*u2)
  x2 <- sqrt(-2*log(u1))*sin(2*pi*u2)
  return(c(x1,x2))
}
x <- replicate(mc, boxmuller()) # 2行xmc列の行列が得られる
#'
#' 散布図を描く
#' 
tibble(x1 = x[1,], x2 = x[2,]) |>
  ggplot(aes(x = x1, y = x2)) +
  geom_point(colour = "royalblue")
#'
#' x1,x2は同じ分布に従う独立な変数なので以下ではまとめて扱う
#' 個別に扱う場合は x を x[1,] などとすれば良い
#' 
mu <- mean(x)
sigma <- sd(x)
tibble(x = as.vector(x)) |>
  ggplot(aes(x = x)) +
  geom_histogram(aes(y = after_stat(density)), 
                 bins = 30,                    
                 fill ="lightblue",          
                 colour = "white") + 
  geom_function(fun = dnorm,
                args = list(mean = mu, sd = sigma),
                colour = "red") +
  labs(title = paste0("正規分布(平均", round(mu,2), # 2桁で四捨五入
                      ", 分散", round(sigma^2,2), ")"))
#'
#' @notes
#' 散布図を描くためのデータフレームは以下のようにして作ることもできる
#' 高次元の場合には関数を利用した方が簡単で確実
#' 
x |> t() |>
  as_tibble(.name_repair = "minimal") |>
  magrittr::set_colnames(paste0("x", 1:2)) |>
  ggplot(aes(x = x1, y = x2)) +
  geom_point(colour = "royalblue")
#' ---------------------------------------------------------------------------

#' @exercise ガンマ分布で用いた例

rgamma(8, shape=3, rate=1) # ガンマ分布(8個)
rgamma(8, shape=1, rate=3) # 異なるパラメタのガンマ分布(8個)

rexp(8) # レート1の指数分布(8個)
rexp(8, rate=0.5) # レート0.5の指数分布(8個)

rchisq(8, df=1) # 自由度1のカイ二乗分布(8個)
rchisq(8, df=4) # 自由度4のカイ二乗分布(8個)

#' ---------------------------------------------------------------------------
#' @practice カイ二乗分布
#' 
mc <- 10000 # 実験回数を指定
k <- 8 # 自由度を設定
my_random <- function(){ 
  sum(rnorm(k)^2)} # k個の標準正規乱数の二乗和
my_data <- replicate(mc, my_random()) # Monte-Carlo実験
tibble(x = my_data) |>
  ggplot(aes(x = x)) +
  geom_histogram(aes(y = after_stat(density)), 
                 bins = 30,                    
                 fill ="lightblue",          
                 colour = "white") + 
  geom_function(fun = dchisq,
                args = list(df = k),
                colour = "red") +
  labs(title = bquote(paste(chi^2,"分布 (自由度",.(k),")")))
#'
#' @notes
#' 関数 bquote() は関数 expression() と同様な働きをするが
#' 自由度の k を .(k) で評価して取り込むことができる
#' 以下と比較してみよう
#' 
tibble(x = my_data) |>
  ggplot(aes(x = x)) +
  geom_histogram(aes(y = after_stat(density)), 
                 bins = 30,                    
                 fill ="lightblue",          
                 colour = "white") + 
  geom_function(fun = dchisq,
                args = list(df = k),
                colour = "red") +
  labs(title = expression(paste(chi^2,"分布 (自由度",k,")")))
#' ---------------------------------------------------------------------------

#' @exercise t 分布で用いた例

rt(8, df=1) # 自由度1のt分布(8個)
rt(8, df=4) # 自由度4のt分布(8個)

#' ---------------------------------------------------------------------------
#' @practice t分布
#' 
mc <- 10000 # 実験回数を指定
k <- 7
my_random <- function(){ 
  y <- rchisq(1, df=k) # 自由度kのカイ2乗分布
  ## y <- sum(rnorm(k)^2) # 正規乱数を用いてもよい
  z <- rnorm(1) # 標準正規乱数
  return(z/sqrt(y/k))}
my_data <- replicate(mc, my_random()) # Monte-Carlo実験
tibble(x = my_data) |>
  ggplot(aes(x = x)) +
  geom_histogram(aes(y = after_stat(density)), 
                 bins = 30,                    
                 fill ="lightblue",          
                 colour = "white") + 
  geom_function(fun = dt,
                args = list(df = k),
                colour = "red") +
  labs(title = bquote(paste(Z/sqrt(Y/k)," (",k==.(k),")")))
#' ---------------------------------------------------------------------------

#' @exercise F 分布で用いた例

rf(10, df1=4, df2=7) # 自由度4,7のF分布(10個)
rf(10, df1=7, df2=12) # 自由度7,12のF分布(10個)

#' ---------------------------------------------------------------------------
#' @practice F分布
#' 
mc <- 10000 # 実験回数を指定
k1 <- 20
k2 <- 10
my_random <- function(){ 
  y1 <- rchisq(1, df=k1) # 自由度20のカイ二乗分布
  y2 <- rchisq(1, df=k2) # 自由度10のカイ二乗分布
  ## y1 <- sum(rnorm(k1)^2) # 正規乱数を用いてもよい
  ## y2 <- sum(rnorm(k2)^2) 
  return((y1/k1)/(y2/k2))}
my_data <- replicate(mc, my_random()) # Monte-Carlo実験
tibble(x = my_data) |>
  ggplot(aes(x = x)) +
  geom_histogram(aes(y = after_stat(density)), 
                 bins = 30,                    
                 fill ="lightblue",          
                 colour = "white") + 
  geom_function(fun = df,
                args = list(df1 = k1, df2 = k2),
                colour = "red") +
  labs(title = bquote(paste(frac(Y[1]/k[1],Y[2]/k[2]),
                            " (",k[1]==.(k1),
                            ", ",k[2]==.(k2),")")))
#'
#' @notes
#' グラフの一部に着目したい場合は xlim/ylim で表示を調整することができる
#' 
last_plot() + # 直前のプロット
  xlim(0,6) + ylim(0,0.9) # 表示領域を指定する(表示しない部分について警告が出る)
#' ---------------------------------------------------------------------------
