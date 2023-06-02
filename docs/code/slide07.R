### 第7講 練習問題解答例

### 基本事項の確認で用いた例

### 平均と分散の計算
p <- rep(c(1/9,2/9),3) # 確率の値 (1/9 と 2/9 を交互に3回繰り返す)
x <- 1:6 # サイコロの目の値
(mu <- sum(x*p)) # 平均値の計算
(var <- sum((x-mu)^2*p)) # 分散の計算
sqrt(var) # 標準偏差

## 正規化しないで計算する方法もある
w <- rep(1:2,3) # 1,2 の繰り返し
weighted.mean(x,w)
weighted.mean(x^2,w)-weighted.mean(x,w)^2

### 練習問題 大数の法則
set.seed(121212) # 乱数のシード値の指定
## 試行(離散分布の標本平均の計算)を定義する
my_mean <- function(n,p){ # 歪んだサイコロの標本平均を計算する関数
  mean(sample(omega, size=n, prob=p, replace=TRUE))}
omega <- 1:6 # サイコロの目 (以下では固定)
## 以下の実験で明示的に変えるものを関数の引数としている
## * n はサイコロを振る回数
## * p は歪み具合
## (n,p ともに omega と同じように関数の外側で定義することもできる)

## 基本の実験 
(p <- rep(1:2, 3)) # 出現確率の比(奇数1:偶数2)を設定
(mu <- weighted.mean(omega, p)) # 理論上の平均
## n毎に図を作る場合
for(n in c(10,100,1000)){ # サンプル数を変えて実験
  xbar <- my_mean(n,p) # 1回実験を行う
  plot(p/sum(p), type="h", col="orange", lwd=4,
       ylim=c(0,0.3), ylab="probability",
       main=paste("n =",n)) # 各目の確率を表示
  abline(v=mu, col="red", lwd=2) # 真の平均
  abline(v=xbar, col="blue", lwd=2) # 標本平均(1つの観測にもとづく)
}
## 多数のnと標本平均を1つの図で比較する場合
nseq <- seq(10,1000,by=30) # 10から1000まで50おきに調べる
xbars <- sapply(nseq,function(x)my_mean(x,p))
plot(nseq, xbars, col="blue", 
     xlab="n", ylab=expression(bar(X))) # 標本平均(1つの観測にもとづく)
abline(h=mu, col="red", lwd=2) # 真の平均

## サンプル数の違いによる標本平均の分布を比較
## n毎に図を作る場合
mc <- 1000 # Monte-Carlo実験の回数
for(n in c(10,100,1000)){ # サンプル数を変えて実験
  xbars <- replicate(mc, my_mean(n,p)) # mc回繰り返し
  hist(xbars, breaks=20,
       col="azure", border="lightblue",
       xlim=c(1,6), ylim=c(0,200), # 複数の図を同じ領域で描画
       xlab=expression(bar(X)[n]), # help(expression), help(plotmath) 参照
       main=paste0("n=",n)) 
  abline(v=mu, col="red", lwd=2, lty="dotted")
  abline(h=0, col="grey", lwd=2, lty="solid")
}
## 多数のnと標本平均を1つの図で比較する場合
my_data <- data.frame(n=NULL,xbar=NULL) # 空のデータフレームを作成
for(n in nseq){ # サンプル数を変えて実験
  xbars <- replicate(mc, my_mean(n,p)) # mc回繰り返し
  my_data <- rbind(my_data,data.frame(n=rep(n,mc),xbar=xbars))
}
boxplot(xbar ~ n, data=my_data, col="blue", 
        xlab="n", ylab=expression(bar(X))) # 標本平均(1つの観測にもとづく)
abline(h=mu, col="red", lwd=2) # 真の平均

## 参考
## 統計的性質を見るための実験
p <- rep(1:2, 3) # 出現確率の比(奇数1:偶数2)
mu <- weighted.mean(omega, p) # 理論上の平均
mc <- 1000 # Monte-Carlo実験の回数
n <- 10 # 標本数
xbars <- replicate(mc, my_mean(n,p)) 
hist(xbars, breaks=20,
     col="azure", border="lightblue",
     xlim=c(1,6), ylim=c(0,200), # 描画範囲を指定
     xlab=expression(bar(X)[n]), 
     main=paste0("n=",n)) # 回数をタイトルに表示
abline(v=mu, col="red", lwd=2, lty="dotted") # 理論値
abline(h=0, col="grey", lwd=2, lty="solid")

## 乱数のシードによる違いを比較
n <- 100 # 標本数
for(i in c(12,34,56,78,90)){ # シード値を指定
  set.seed(i)
  xbars <- replicate(mc, my_mean(n,p)) # mc回繰り返し
  hist(xbars, breaks=20,
       col="azure", border="lightblue",
       xlim=c(1,6), ylim=c(0,200), 
       xlab=expression(bar(X)[n]), 
       main=paste0("n=",n)) 
  abline(v=mu, col="red", lwd=2, lty="dotted")
  abline(h=0, col="grey", lwd=2, lty="solid")
}

## 出現確率の違いを比較
n <- 100 # 標本数
for(i in 1:5){ # pをランダムに設定して実験
  p <- runif(length(omega)) # 一様乱数で出現確率を設定
  mu <- weighted.mean(omega, p) # 理論上の平均
  xbars <- replicate(mc, my_mean(n,p)) # mc回繰り返し
  hist(xbars, breaks=20,
       col="azure", border="lightblue",
       xlim=c(1,6), ylim=c(0,200), 
       xlab=expression(bar(X)[n]), 
       main=paste0("n=",n)) 
  abline(v=mu, col="red", lwd=2, lty="dotted")
  abline(h=0, col="grey", lwd=2, lty="solid")
}

### 練習問題 中心極限定理
set.seed(232323)    # 乱数のシード値の指定
## 試行(離散分布の標本平均の計算)を定義する
my_mean <- function(n,p){ # 標本平均を計算する関数
  mean(sample(omega, size=n, prob=p, replace=TRUE))}
omega <- 1:6 # 以下サイコロの場合で実験
mc <- 10000 # Monte-Carlo実験の繰り返し回数(大数の法則より多めに実験する)

## サンプル数の違いによる分布の比較
p <- rep(1:2, 3) # 出現確率の比(奇数1:偶数2) (9で割らなくても大丈夫)
(mu <- weighted.mean(omega, p)) # 理論上の平均
(sigma <- sqrt( # 理論上の標準偏差(分散の平方根)
   weighted.mean(omega^2,p)-mu^2)) 
for(n in c(2,10,100,1000)){ # サンプル数を変えて実験
  xbars <- replicate(mc, my_mean(n,p)) # mc回繰り返し
  hist(sqrt(n)*(xbars - mu)/sigma, breaks=30,
       freq=FALSE, # 密度で表示
       xlim=c(-3, 3), ylim=c(0,0.55),  
       col="orange", border="orchid",
       xlab=expression(sqrt(n)*(bar(X)[n]-mu)/sigma),
       main=paste0("n=",n))
  curve(dnorm, add=TRUE, col="red", lwd=2) # 理論曲線
}

## 出現確率の違いによる分布の比較
n <- 100 # 標本数 (いろいろ変えて実験せよ．nが小さいとpの影響が大きい)
for(i in 1:5){ # pをランダムに設定して実験
  p <- runif(length(omega)) # 一様乱数で出現確率を設定
  mu <- weighted.mean(omega, p) # 理論上の平均
  sigma <- sqrt( # 理論上の標準偏差(分散の平方根)
    weighted.mean(omega^2,p)-mu^2)
  xbars <- replicate(mc, my_mean(n,p)) 
  hist(sqrt(n)*(xbars - mu)/sigma, breaks=30,
       freq=FALSE, 
       xlim=c(-3, 3), ylim=c(0,0.55),  
       col="orange", border="orchid",
       xlab=expression(sqrt(n)*(bar(X)[n]-mu)/sigma),
       main=paste0("p=",toString(round(p,2))))
  curve(dnorm, add=TRUE, col="red", lwd=2) 
}

### 練習問題 少数の法則
set.seed(343434) # 乱数のシード値の指定

## 基本の実験
n <- 5000    # 1日の総生産量
p <- 0.002   # 不良品の発生確率
N <- 5*50*2  # 観測日数(週5日x50週間x2年操業に対応)
x <- rbinom(N,n,p) # 不良品数
## x <- replicate(N, rbinom(1,n,p)) # これでも同じ
(my_data <- table(x)) # 不良品数の度数分布表を作成
## それぞれの不良品数が生じた日数の割合のグラフを作成
if(Sys.info()["sysname"]=="Darwin"){par(family="HiraginoSans-W4")} # 日本語表示
plot(my_data/N, type="h", col="royalblue", lwd=6, 
     xlab="不良品数", ylab="発生割合")
lines(min(x):max(x)+0.4, dpois(min(x):max(x),n*p),
      type="h", col="red", lwd=6) # 理論上の割合
legend("topright", inset=1/20, # 右上に配置
       legend=c("観測値", "理論値"), 
       col=c("royalblue", "red"), lwd=4) # 凡例

## nの違いを比較
p <- 0.002
for(n in c(500,1000,5000,10000)){
  x <- rbinom(N,n,p) # 不良品数
  my_data <- table(x)/N # 集計
  if(Sys.info()["sysname"]=="Darwin"){par(family="HiraginoSans-W4")} # 日本語表示
  plot(my_data, type="h", col="royalblue", lwd=6, 
       xlab="不良品数", ylab="発生割合",
       main=paste("n =",n,"p =",p))
  lines(min(x):max(x)+0.4, dpois(min(x):max(x),n*p),
        type="h", col="red", lwd=6) 
}

## pの違いを比較
n <- 5000
for(p in c(0.001,0.002,0.005,0.01)){
  x <- rbinom(N,n,p) # 不良品数
  my_data <- table(x)/N # 集計
  if(Sys.info()["sysname"]=="Darwin"){par(family="HiraginoSans-W4")} # 日本語表示
  plot(my_data, type="h", col="royalblue", lwd=6, 
       xlab="不良品数", ylab="発生割合",
       main=paste("n =",n,"p =",p))
  lines(min(x):max(x)+0.4, dpois(min(x):max(x),n*p),
        type="h", col="red", lwd=6) 
}
## n が大きくなるに従い，正規分布の形に近付いていくことが観測できる
## 中心極限定理がここでも成り立っていることがわかる
