### 第7講 サンプルコード
library(tidyverse)

#' @exercise 基本事項の確認

#' 平均と分散の計算
p <- rep(c(1/9,2/9),3) # 確率の値 (1/9 と 2/9 を交互に3回繰り返す)
x <- 1:6 # サイコロの目の値
(mu <- sum(x*p)) # 平均値の計算
(v <- sum((x-mu)^2*p)) # 分散の計算
sqrt(v) # 標準偏差

#' 正規化しないで計算する方法もある
w <- rep(1:2,3) # 1,2 の繰り返し (確率ではない)
weighted.mean(x,w)
weighted.mean(x^2,w)-weighted.mean(x,w)^2

#' ---------------------------------------------------------------------------
#' @practice 大数の法則
#' 
set.seed(121212) # 乱数のシード値の指定
#'
#' 試行(離散分布の標本平均の計算)を定義する
omega <- 1:6 # サイコロの目 (標本空間．以下では固定)
my_mean <- function(n, p){ # 歪んだサイコロの標本平均を計算する関数
  mean(sample(omega, size = n, prob = p, replace = TRUE))}
#' 以下の実験で明示的に変えるものを関数の引数としている
#' * n はサイコロを振る回数
#' * p は歪み具合
#'(n,p ともに omega と同じように関数の外側で定義することもできる)
#'
#' 実験の設定
(p <- rep(1:2, 3)) # 出現確率の比(奇数1:偶数2)を設定
(mu <- weighted.mean(omega, p)) # 理論上の平均
roll <- 1:6 # サイコロの目 
probability <- p/sum(p) # 確率値に変換
#'
#' n毎に図を作る場合
for(n in c(10,100,1000)){ # サンプル数を変えて実験
  xbar <- my_mean(n, p) # 1回実験を行う
  foo <- tibble(roll, probability) |>
    ggplot(aes(x = roll, y = probability)) +
    geom_bar(stat = "identity", fill = "orange", width = 0.2) +
    geom_vline(xintercept = xbar, # 標本平均 (1回の実験による推定値)
               colour = "blue", linewidth = 1.1) +
    geom_vline(xintercept = mu,   # 真の平均
               colour = "red", linewidth = 1.1) +
    scale_x_continuous(breaks = roll) +
    labs(title = paste("n =", n))
  print(foo) # ggplot オブジェクトはブロックの中では明示的にprintする必要がある
}
#'
#' 多数のnと標本平均を1つの図で比較する場合
nseq <- seq(10, 1000, by = 30) # 10から1000まで30おきに調べる
xbars <- sapply(nseq, # 各値について以下の関数を計算
                function(x) my_mean(x, p)) # '?sapply'を参照
tibble(n = nseq, xbar = xbars) |>
  ggplot(aes(x = n, y = xbar)) +
  geom_point(colour = "blue") + # n毎の標本平均
  geom_hline(yintercept = mu,   # 真の平均
             colour = "red", linewidth = 1.1) +
  labs(y = expression(bar(X))) # help(expression), help(plotmath) 参照
#'
#' サンプル数の違いによる標本平均の分布を比較
#' n毎に図を作る場合
mc <- 1000 # Monte-Carlo実験の回数
for(n in c(10,100,1000)){ # サンプル数を変えて実験
  xbars <- replicate(mc, my_mean(n, p)) # mc回繰り返し
  foo <- tibble(x = xbars) |>
    ggplot(aes(x = x)) +
    geom_histogram(binwidth = 0.6/sqrt(n), # ビンの幅
                   fill ="azure",          # 塗り潰しの色
                   colour = "lightblue") + # 線の色
    geom_vline(xintercept = mu,
               colour = "pink",       # 線の色
               linetype = "dashed") + # 線の種類
    geom_hline(yintercept = 0,
               colour = "grey",
               linetype = "solid") +
    xlim(1,6) + ylim(0,200) + # 複数の図を同じ領域で描画(適宜調整が必要)
    labs(x = expression(bar(X)[n]), 
         title = paste("n =", n))
  print(foo)
}
#' @notes
#' 警告は xlim で指定した範囲のbinにはデータが無いため
#' 
#' 多数のnと標本平均を1つの図で比較する場合
my_data <- tibble(n = NULL, xbar = NULL) # 空のデータフレームを作成
for(n in nseq){ # サンプル数を変えて実験して追加する
  xbars <- replicate(mc, my_mean(n, p))
  my_data <- bind_rows(my_data,
                       tibble(n = rep(n,mc), xbar = xbars))
}
my_data |>
  ggplot(aes(x = n, y = xbar)) +
  geom_boxplot(aes(group = n),       # 標本平均の分
               fill = "royalblue") + 
  geom_hline(yintercept = mu,   # 真の平均
             colour = "pink",
             linewidth = 1.1) +
  labs(x = "n",
       y = expression(bar(X))) 
#'
#' @notes
#' 統計的性質を見るための実験
p <- rep(1:2, 3) # 出現確率の比(奇数1:偶数2)
mu <- weighted.mean(omega, p) # 理論上の平均
mc <- 1000 # Monte-Carlo実験の回数
n <- 10 # 標本数
xbars <- replicate(mc, my_mean(n, p)) 
tibble(x = xbars) |>
  ggplot(aes(x = x)) +
  geom_histogram(binwidth = 0.6/sqrt(n), 
                 fill ="azure",          
                 colour = "lightblue") + 
  geom_vline(xintercept = mu,
             colour = "pink",       
             linetype = "dashed") + 
  geom_hline(yintercept = 0,
             colour = "grey",
             linewidth = 1.1,
             linetype = "solid") +
  xlim(1,6) + ylim(0,200) + 
  labs(x = expression(bar(X)[n]), 
       title = paste("n =", n))
#'
#' 乱数のシードによる違いを比較
n <- 100 # 標本数
for(i in c(12,34,56,78,90)){ # シード値を指定
  set.seed(i)
  xbars <- replicate(mc, my_mean(n, p))
  foo <- tibble(x = xbars) |>
    ggplot(aes(x = x)) +
    geom_histogram(binwidth = 0.6/sqrt(n), 
                   fill ="azure",          
                   colour = "lightblue") + 
    geom_vline(xintercept = mu,
               colour = "pink",       
               linetype = "dashed") + 
    geom_hline(yintercept = 0,
               colour = "grey",
               linetype = "solid") +
    xlim(1,6) + ylim(0,200) + 
    labs(x = expression(bar(X)[n]), 
         title = paste("n =", n, "seed =", i))
  print(foo)
}
#'
#' 出現確率の違いを比較
n <- 100 # 標本数
for(i in 1:5){ # pをランダムに設定して実験
  p <- runif(length(omega)) # 一様乱数で出現確率を設定
  mu <- weighted.mean(omega, p) # 理論上の平均
  xbars <- replicate(mc, my_mean(n, p))
  foo <- tibble(x = xbars) |>
    ggplot(aes(x = x)) +
    geom_histogram(binwidth = 0.6/sqrt(n), 
                   fill ="azure",          
                   colour = "lightblue") + 
    geom_vline(xintercept = mu,
               colour = "pink",       
               linetype = "dashed") + 
    geom_hline(yintercept = 0,
               colour = "grey",
               linetype = "solid") +
    xlim(1,6) + ylim(0,200) + 
    labs(x = expression(bar(X)[n]), 
         title = paste("n =", n, "p =", paste(round(p, 2), collapse = "/")))
  print(foo)
}
#' ---------------------------------------------------------------------------

#' ---------------------------------------------------------------------------
#' @practice 中心極限定理
set.seed(232323)    # 乱数のシード値の指定
#' 試行(離散分布の標本平均の計算)を定義する
omega <- 1:6 # 以下サイコロの場合で実験
my_mean <- function(n, p){ # 標本平均を計算する関数
  mean(sample(omega, size = n, prob = p, replace=TRUE))}
mc <- 10000 # Monte-Carlo実験の繰り返し回数(大数の法則より多めに実験する)

#' サンプル数の違いによる分布の比較
p <- rep(1:2, 3) # 出現確率の比(奇数1:偶数2) (9で割らなくても大丈夫)
(mu <- weighted.mean(omega, p)) # 理論上の平均
(sigma <- sqrt( # 理論上の標準偏差(分散の平方根)
   weighted.mean(omega^2,p)-mu^2)) 
for(n in c(2,10,100,1000)){ # サンプル数を変えて実験
  xbars <- replicate(mc, my_mean(n, p))
  foo <- tibble(x = sqrt(n)*(xbars - mu)/sigma) |>
    ggplot(aes(x = x)) +
    geom_histogram(aes(y = after_stat(density)), # 密度で表示
                   bins = 30,                    # ビンの数を指定
                   fill ="orange",          
                   colour = "orchid") + 
    geom_function(fun = dnorm, # 理論曲線(標準正規分布)と比較
                  colour = "lightblue") +
    xlim(-4,4) + ylim(0,0.7) + # 比較のため表示領域を限定する
    labs(x = expression(sqrt(n)*(bar(X)[n]-mu)/sigma),
         title = paste("n =", n))
  print(foo)
}
#'
#' 出現確率の違いによる分布の比較
n <- 100 # 標本数 (いろいろ変えて実験せよ．nが小さいとpの影響が大きい)
for(i in 1:5){ # pをランダムに設定して実験
  p <- runif(length(omega)) # 一様乱数で出現確率を設定
  mu <- weighted.mean(omega, p) # 理論上の平均
  sigma <- sqrt( # 理論上の標準偏差(分散の平方根)
    weighted.mean(omega^2,p)-mu^2)
  xbars <- replicate(mc, my_mean(n,p)) 
  foo <- tibble(x = sqrt(n)*(xbars - mu)/sigma) |>
    ggplot(aes(x = x)) +
    geom_histogram(aes(y = after_stat(density)), 
                   bins = 30,                    
                   fill ="orange",          
                   colour = "orchid") + 
    geom_function(fun = dnorm, 
                  colour = "lightblue") +
    xlim(-4,4) + ylim(0,0.7) + 
    labs(x = expression(sqrt(n)*(bar(X)[n]-mu)/sigma),
         title = paste("n =", n, "p =", paste(round(p, 2), collapse = "/")))
  print(foo)
}
#' ---------------------------------------------------------------------------

#' ---------------------------------------------------------------------------
#' @practice 少数の法則
set.seed(343434) # 乱数のシード値の指定
#'
#' 以下，日本語を用いるため macOS では以下の設定を行う
if(Sys.info()["sysname"] == "Darwin") { # macOS か調べて日本語フォントを指定
  theme_update(text = element_text(family = "HiraginoSans-W4"))
  update_geom_defaults("text", list(family = theme_get()$text$family))
  update_geom_defaults("label", list(family = theme_get()$text$family))
}
#'
#' 基本の実験
n <- 5000    # 1日の総生産量
p <- 0.002   # 不良品の発生確率
N <- 5*50*2  # 観測日数(週5日x50週間x2年操業に対応)
x <- rbinom(N,n,p) # 不良品数
#' x <- replicate(N, rbinom(1,n,p)) # 同上 (書き方はいろいろある)
(my_data <- table(x)) # 不良品数の度数分布表を作成
#' それぞれの不良品数が生じた日数の割合のグラフを作成
tibble(不良品数 = as.numeric(names(my_data)),
       観測値 = as.numeric(my_data/N),
       理論値 = dpois(不良品数, n*p)) |>
  pivot_longer(!不良品数, values_to = "発生割合") |>
  ggplot(aes(x = 不良品数, y = 発生割合, fill = name)) +
  geom_bar(stat = "identity",
           width = 0.8, # 並びがわかりやすいように幅を調整
           position = "dodge") +
  labs(fill = NULL) # 凡例のfillの名称(name)を消去
#'
#' nの違いを比較
p <- 0.002
for(n in c(500,1000,5000,10000)){
  x <- rbinom(N,n,p)
  my_data <- table(x)
  foo <- tibble(不良品数 = as.numeric(names(my_data)),
                観測値 = as.numeric(my_data/N),
                理論値 = dpois(不良品数, n*p)) |>
    pivot_longer(!不良品数, values_to = "発生割合") |>
    ggplot(aes(x = 不良品数, y = 発生割合, fill = name)) +
    geom_bar(stat = "identity",
             width = 0.8,
             position = "dodge") +
    labs(fill = NULL,
         title = paste("n =", n, "p =", p))
  print(foo)
}
#'
#' pの違いを比較
n <- 5000
for(p in c(0.001,0.002,0.005,0.01)){
  x <- rbinom(N,n,p)
  my_data <- table(x)
  foo <- tibble(不良品数 = as.numeric(names(my_data)),
                観測値 = as.numeric(my_data/N),
                理論値 = dpois(不良品数, n*p)) |>
    pivot_longer(!不良品数, values_to = "発生割合") |>
    ggplot(aes(x = 不良品数, y = 発生割合, fill = name)) +
    geom_bar(stat = "identity",
             width = 0.8,
             position = "dodge") +
    labs(fill = NULL,
         title = paste("n =", n, "p =", p))
  print(foo)
}
#' @notes
#' n が大きくなるに従い，正規分布の形に近付いていくことが観測できる
#' 中心極限定理がここでも成り立っていることがわかる
#' ---------------------------------------------------------------------------
