### 第10講 練習問題解答例

### 練習問題 一様分布の平均の推定
set.seed(1234) # 乱数のシード値の指定
## 平均値の推定を行う関数(標本平均，x1，最大最小)
my_mean_est <- function(n, min, max){
    x <- runif(n, min=min, max=max)
    return(c(xbar=mean(x), med=median(x), mid=(max(x)+min(x))/2))
}
## 実験の設定 
n <- 10 # 観測データのサンプル数
mc <- 10000 # 実験回数
a <- 0; b <- 50 # 一様乱数の区間
my_data <- as.data.frame(t( # mc行 x 3種に変換(転置)
    replicate(mc, my_mean_est(n, min=a, max=b))
))
## replicate は3行 x mc 列の行列を返すことに注意
## ここでは data.frame として扱うために型を変換している
head(my_data) # 実験結果の表示
apply(my_data,2,mean) # 推定値の平均 (colMeans(my_data)も可)
apply(my_data,2,var)  # 推定値の分散
## もう少し詳しくみてみる
summary(my_data) # 四分位点を表示
for(i in 1:3) { # それぞれのヒストグラムを書いてみる
    hist(my_data[,i], breaks=40, 
         xlim=c(a,b), ylim=c(0,2000), # 同じ大きさの図にする
         col="pink", border="brown",
         xlab="est", main=names(my_data)[i])
}

### 練習問題 ガンマ分布による風速データのモデル化
## データの取得
tw_data <- read.csv("data/tokyo_weather.csv")
## 以下では関数 with を利用しているが，tw_data から風速を取り出しておいても良い

## ヒストグラムの描画
if(Sys.info()["sysname"]=="Darwin"){par(family="HiraginoSans-W4")}
with(tw_data, hist(wind, breaks=30, freq=FALSE, # 密度
                   col="skyblue", border="blue",
                   main="風速のヒストグラム", xlab="風速 [m/s]", ylab="密度"))

## ガンマ関数による最尤推定
library("stats4") # 関数mleを利用
## ガンマ分布の最尤推定量
mle.gamma <- function(x, # 観測データ
                      nu0=1, # nuの初期値
                      alpha0=1, # alphaの初期値
                      verbose=FALSE){ # debug用に追加
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
(theta <- with(tw_data, mle.gamma(wind))) # 最尤推定

## 結果の重ね描き
curve(dgamma(x, theta[1], theta[2]), # あてはめたガンマ分布の密度関数
      add=TRUE, col="orange", lwd=3) 

### 補遺: シミュレーションによる一致性の検証
set.seed(5678) # 乱数のシード値の指定
nu <- 5; alpha <- 2 # 真のパラメタ
mc <- 2000 # 実験回数 (計算が重いので少なめにしている)
for(n in c(10, 100, 300)){ # データ数を変えて実験
    ## Monte-Carlo実験
    my_est <- data.frame(t( # 推定値のdata.frame
        replicate(mc, mle.gamma(rgamma(n, nu, alpha)))
    ))
    ## 結果をヒストグラムで表示
    hist(my_est$nu, breaks=30, col="skyblue1", border="skyblue4",
         xlim=c(0, 20), main=paste0("n=",n), xlab=expression(nu))
    abline(h=0, lwd=2, lty="dotted") # x軸 (y=0) を表示
    abline(v=nu, col="tomato", lwd=2, lty="solid") # nuの真値
    hist(my_est$alpha, breaks=30, col="seagreen1", border="seagreen4",
         xlim=c(0, 10), main=paste0("n=",n), xlab=expression(alpha))
    abline(h=0, lwd=2, lty="dotted")  # x軸 (y=0) を表示
    abline(v=alpha, col="tomato", lwd=2, lty="solid") # alphaの真値
}

### 練習問題 日射量データの区間推定
## データの取得
tw_data <- read.csv("data/tokyo_weather.csv")
## 以下では関数 with を利用しているが，tw_data から日射量を取り出しておいても良い

## 全データによる平均値の計算
(mu <- with(tw_data, mean(solar))) # 真値

## 50点による90%信頼区間
set.seed(1357) # 乱数のシード値の指定
n <- 50
idx <- sample(nrow(tw_data),n) # データの抽出
(xbar <- with(tw_data[idx,], mean(solar))) # 標本平均
(s <- with(tw_data[idx,], sd(solar))) # 標本標準偏差
z95 <- qnorm(0.95) # 標準正規分布の0.95分位点
(ci <- c(L=xbar-z95*s/sqrt(n),U=xbar+z95*s/sqrt(n))) # 信頼区間

## 信頼区間の正答率の評価
mc <- 100
my_trial <- function(n){ # nを変えて実験できるように
    idx <- sample(nrow(tw_data),n)
    xbar <- with(tw_data[idx,], mean(solar)) # 標本平均
    s <- with(tw_data[idx,], sd(solar)) # 標本標準偏差
    return(c(L=xbar-z95*s/sqrt(n),U=xbar+z95*s/sqrt(n))) # 信頼区間
}
my_ci <- as.data.frame(t( # confidence intervals
    replicate(mc,my_trial(n))
))
apply(my_ci,1,function(x)(x["L"]<mu & mu<x["U"]))
table(apply(my_ci,1,function(x)(x["L"]<mu & mu<x["U"])))

### 補遺: 信頼区間について多数で評価する
mc <- 2000
my_ci <- as.data.frame(t(
    replicate(mc,my_trial(n))
))
table(apply(my_ci,1,function(x)(x["L"]<mu & mu<x["U"])))/mc # 確率を見る
### 補遺: グラフを描いてみる
k <- 20 
idx <- sample(nrow(my_ci),k) # k個選んで描く
plot(x=1:k,type="n",
     ylim=c(min(my_ci),max(my_ci)),
     xlab="index", ylab="confidence intervals") # 枠だけ描く
abline(h=mu, col="orange", lwd=2)
with(my_ci[idx,],
     arrows(x0=1:k,y0=U,y1=L,code=3,
            angle=90,length=1/20,col="blue"))
