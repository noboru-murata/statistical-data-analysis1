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
