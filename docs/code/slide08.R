### 第08回 練習問題解答例

### 離散一様分布で用いた例

a <- 1:6 # サンプリング対象の集合をベクトルとして定義
sample(a, size=20, replace=TRUE) # 離散一様分布(20個)

### 二項分布で用いた例

rbinom(10, size=1, prob=0.2) # Bernoulli分布(10個)
rbinom(20, size=5, prob=0.6) # 二項分布(20個)

### 練習問題 二項分布
mc <- 10000 # 実験回数を指定
n <- 16 
p <- 0.6
myRandom <- function(){ # Bernolli分布をm個生成して合計
  sum(rbinom(n, size=1, prob=p))}
myData <- replicate(mc, myRandom())
myTable <- table(myData)/mc # 出現確率ごとの表(度数分布表)を作成
if(Sys.info()["sysname"]=="Darwin"){par(family="HiraginoSans-W4")}
plot(myTable, type="h", lwd=5, col="royalblue",
     xlab="値", ylab="確率",
     main=paste0("二項分布(試行回数", n, ", 成功確率", p, ")"))
myRange <- min(myData):max(myData) # 範囲を取得
lines(myRange + 0.3, dbinom(myRange, size=n, prob=p),
      type="h", col="red", lwd=5) # 理論上の出現確率
legend("topright", inset=0.05, legend=c("観測値", "理論値"), 
       col=c("royalblue", "red"), lwd=5) # 凡例を作成

### Poisson 分布で用いた例

rpois(15, lambda=1) # 強度1の Poisson 分布(15個)
rpois(15, lambda=10) # 強度10の Poisson 分布(15個)

### 練習問題 Poisson 分布
mc <- 10000 
lambda1 <- 5
lambda2 <- 12
myRandom <- function(){ # 2つの Poisson 分布の和
  rpois(1, lambda=lambda1)+rpois(1, lambda=lambda2)}
myData <- replicate(mc, myRandom())
myTable <- table(myData)/mc 
if(Sys.info()["sysname"]=="Darwin"){par(family="HiraginoSans-W4")}
plot(myTable, type="h", lwd=5, col="royalblue",
     xlab="値", ylab="確率",
     main=paste0("Poisson 分布(強度", lambda1+lambda2, ")"))
myRange <- min(myData):max(myData) 
lines(myRange + 0.3,
      dpois(myRange, lambda=lambda1+lambda2), 
      type="h", col="red", lwd=5) # 理論上の出現確率
legend("topright", inset=0.05, legend=c("観測値", "理論値"), 
       col=c("royalblue", "red"), lwd=5)

### 幾何分布で用いた例

rgeom(15, prob=0.1) # 成功確率0.1の幾何分布(15個)

### 一様分布で用いた例

runif(8) # 区間(0,1)上の一様乱数(8個)
runif(8,min=-1,max=1) # 区間(-1,1)上の一様乱数(8個)

### 正規分布で用いた例

rnorm(8) # 標準正規乱数(8個)
rnorm(8,mean=1,sd=2) # 平均1分散4=2^2の正規乱数

### 練習問題 正規分布
mc <- 10000 # 実験回数を指定
myRandom <- function(){ # 一方の分布を確認する
  u1 <- runif(1)
  u2 <- runif(1)
  return(sqrt(-2*log(u1))*cos(2*pi*u2))}
myData <- replicate(mc, myRandom()) # Monte-Carlo実験
if(Sys.info()["sysname"]=="Darwin"){par(family="HiraginoSans-W4")}
hist(myData, freq=FALSE, breaks=40,
     col="lightblue", border="white", 
     xlab="x", main=paste0("標準正規分布")) # ヒストグラム(密度表示)
curve(dnorm(x, mean=0, sd=1), add=TRUE, 
      col="red", lwd=3) # 理論上の確率密度関数
legend("topright", inset=0.05, legend=c("観測値", "理論値"), 
       col=c("lightblue", "red"), lwd=3) # 凡例を作成
## Box-Muller法で作られる2つの確率変数の関係を調べる
boxmuller <- function(){
  u1 <- runif(1)
  u2 <- runif(1)
  x1 <- sqrt(-2*log(u1))*cos(2*pi*u2)
  x2 <- sqrt(-2*log(u1))*sin(2*pi*u2)
  return(c(x1,x2))
}
x <- replicate(mc, boxmuller()) # 2行xmc列の行列が得られる
par(family="HiraginoSans-W4") # 日本語フォントの指定
plot(x[1,],x[2,],xlab="x1",ylab="x2") # 散布図を描く
## 以下では同じ分布なのでx1,x2はまとめて計算
mu <- round(mean(x),2)
sigma <- round(sd(x),2)
hist(x, freq=FALSE, breaks=40, col="lightblue", border="white",

     main=paste0("正規分布(平均", mu, ", 分散", sigma^2, ")")) # ヒストグラム(密度表示)
curve(dnorm(x, mean=mu, sd=sigma), add=TRUE, 
      col="red", lwd=3) # 理論上の確率密度関数
legend("topright", inset=0.05, legend=c("観測値", "理論値"), 
       col=c("lightblue", "red"), lwd=3) # 凡例を作成

### ガンマ分布で用いた例

rgamma(8, shape=3, rate=1) # ガンマ分布(8個)
rgamma(8, shape=1, rate=3) # 異なるパラメタのガンマ分布(8個)

rexp(8) # レート1の指数分布(8個)
rexp(8, rate=0.5) # レート0.5の指数分布(8個)

rchisq(8, df=1) # 自由度1のカイ二乗分布(8個)
rchisq(8, df=4) # 自由度4のカイ二乗分布(8個)

### 練習問題 カイ二乗分布
mc <- 10000 # 実験回数を指定
k <- 8 # 自由度を設定
myRandom <- function(){ 
  sum(rnorm(k)^2)} # k個の標準正規乱数の二乗和
myData <- replicate(mc, myRandom()) # Monte-Carlo実験
if(Sys.info()["sysname"]=="Darwin"){par(family="HiraginoSans-W4")}
hist(myData, freq=FALSE, breaks=25,
     col="lightblue", border="white", 
     xlab="x", main=bquote(paste(chi^2,"分布(自由度",.(k),")"))) 
curve(dchisq(x, k), # 理論曲線(確率密度)
      add=TRUE, col="red", lwd=3) 
legend("topright", inset=0.05, legend=c("観測値", "理論値"), 
       col=c("lightblue", "red"), lwd=3)

### t-分布で用いた例

rt(8, df=1) # 自由度1のt分布(8個)
rt(8, df=4) # 自由度4のt分布(8個)

### 練習問題 t分布
mc <- 10000 # 実験回数を指定
k <- 7
myRandom <- function(){ 
  y <- rchisq(1, df=k) # 自由度kのカイ2乗分布
  ## y <- sum(rnorm(k)^2) # 正規乱数を用いてもよい
  z <- rnorm(1) # 標準正規乱数
  return(z/sqrt(y/k))}
myData <- replicate(mc, myRandom()) # Monte-Carlo実験
if(Sys.info()["sysname"]=="Darwin"){par(family="HiraginoSans-W4")}
hist(myData, freq=FALSE, breaks=40,
     col="lightblue", border="white",  
     xlab="x", main=bquote(paste(Z/sqrt(Y/k)," (",k==.(k),")")))
curve(dt(x, df=k), # 確率密度関数(理論)
      add=TRUE, col="red", lwd=3) 
legend("topright", inset=0.05, legend=c("観測値", "理論値"), 
       col=c("lightblue", "red"), lwd=3)

### F-分布で用いた例

rf(10, df1=4, df2=7) # 自由度4,7のF分布(10個)
rf(10, df1=7, df2=12) # 自由度7,12のF分布(10個)

### 練習問題 F分布
mc <- 10000 # 実験回数を指定
k1 <- 20
k2 <- 10
myRandom <- function(){ 
  y1 <- rchisq(1, df=k1) # 自由度20のカイ二乗分布
  y2 <- rchisq(1, df=k2) # 自由度10のカイ二乗分布
  ## y1 <- sum(rnorm(k1)^2) # 正規乱数を用いてもよい
  ## y2 <- sum(rnorm(k2)^2) 
  return((y1/k1)/(y2/k2))}
myData <- replicate(mc, myRandom()) # Monte-Carlo実験
if(Sys.info()["sysname"]=="Darwin"){par(family="HiraginoSans-W4")}
hist(myData, freq=FALSE, breaks=40,
     col="lightblue", border="white",
     xlab="x",
     main=bquote(paste(frac(Y[1]/k[1],Y[2]/k[2]),
                       " (",k[1]==.(k1),
                       ", ",k[2]==.(k2),")"))) 
curve(df(x, df1=k1, df2=k2), # 確率密度関数(理論)
      add=TRUE, col="red", lwd=3) 
legend("topright", inset=0.05, legend=c("観測値", "理論値"), 
       col=c("lightblue", "red"), lwd=3)
