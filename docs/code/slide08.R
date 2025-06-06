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

#' @exercise Poisson 分布で用いた例

rpois(15, lambda=1) # 強度1の Poisson 分布(15個)
rpois(15, lambda=10) # 強度10の Poisson 分布(15個)

#' @exercise 幾何分布で用いた例

rgeom(15, prob=0.1) # 成功確率0.1の幾何分布(15個)

#' @exercise 一様分布で用いた例

runif(8) # 区間(0,1)上の一様乱数(8個)
runif(8,min=-1,max=1) # 区間(-1,1)上の一様乱数(8個)

#' @exercise 正規分布で用いた例

rnorm(8) # 標準正規乱数(8個)
rnorm(8,mean=1,sd=2) # 平均1分散4=2^2の正規乱数

#' @exercise ガンマ分布で用いた例

rgamma(8, shape=3, rate=1) # ガンマ分布(8個)
rgamma(8, shape=1, rate=3) # 異なるパラメタのガンマ分布(8個)

rexp(8) # レート1の指数分布(8個)
rexp(8, rate=0.5) # レート0.5の指数分布(8個)

rchisq(8, df=1) # 自由度1のカイ二乗分布(8個)
rchisq(8, df=4) # 自由度4のカイ二乗分布(8個)

#' @exercise t 分布で用いた例

rt(8, df=1) # 自由度1のt分布(8個)
rt(8, df=4) # 自由度4のt分布(8個)

#' @exercise F 分布で用いた例

rf(10, df1=4, df2=7) # 自由度4,7のF分布(10個)
rf(10, df1=7, df2=12) # 自由度7,12のF分布(10個)
