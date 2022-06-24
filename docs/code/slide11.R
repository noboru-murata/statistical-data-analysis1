### 第11回 練習問題解答例

### 練習問題 t検定のMonte-Carlo実験
n <- 10
mu0 <- 5
sd0 <- 3
mc <- 10000
alpha <- 0.05
myTrial <- function(n){ 
    result <- t.test(rnorm(n,mean=mu0,sd=sd0),mu=mu0)
    return(result$p.value)}
myData <- replicate(mc, myTrial(n))
hist(myData,xlab="p value") # p.valueの分布を見る
table(myData<alpha)/mc # alpha以下の結果の数を調べる=サイズ

### 練習問題 視聴率の検定
### X は正規分布ではないが，nが十分大きい場合には
### 区間推定と同様に正規分布(自由度の大きなt分布)で近似される
n <- 600
mu0 <- 0.1 # 越えたい視聴率
mu1 <- 0.11 # 真の視聴率(11%)
x <- sample(0:1,n,replace=TRUE,prob=c(1-mu1,mu1))
table(x)
t.test(x,mu=mu0,alternative="greater")
## この設定でMonte-Carlo実験を行う
mc <- 10000
alpha <- 0.03 # 
myTrial <- function(n){ 
    x <- sample(0:1,n,replace=TRUE,prob=c(1-mu1,mu1))
    result <- t.test(x,mu=mu0,alternative="greater")
    return(result$p.value)}
myData <- replicate(mc, myTrial(n))
hist(myData,xlab="p value") # p.valueの分布を見る
table(myData<alpha)/mc # alpha以下のデータの数を調べる=検出力
## n を変えて実験してみる
n <- 5000
myData <- replicate(mc, myTrial(n))
hist(myData,xlab="p value") # p.valueの分布を見る
table(myData<alpha)/mc # alpha以下のデータの数を調べる=検出力

### 練習問題 6月の気温の分散の検定
TW.data <- read.csv("data/tokyo_weather.csv")
if(Sys.info()["sysname"]=="Darwin"){par(family="HiraginoSans-W4")}
boxplot(temp ~ month, data=TW.data, col="lightblue")
mySummary <- aggregate(temp ~ month, data=TW.data, FUN=var)
plot(mySummary,type="h",col="blue",lwd=3,
     ylim=c(0,max(mySummary$temp)+1),ylab="気温の分散")
v0 <- mean(mySummary$temp) # sigma0^2
x <- subset(TW.data, subset=month==6, select=temp, drop=TRUE)
(n <- length(x)) # データ数
(chi2 <- (n-1)*var(x)/v0) # 検定統計量
p0 <- pchisq(chi2, df=n-1)
(p <- 2*min(p0,1-p0))
## 分散が最も大きな9月を検定してみる
x <- subset(TW.data, subset=month==9, select=temp, drop=TRUE)
(n <- length(x)) # データ数
(chi2 <- (n-1)*var(x)/v0) # 検定統計量
p0 <- pchisq(chi2, df=n-1)
(p <- 2*min(p0,1-p0))

### 練習問題 7月と8月の気温の平均の差の検定
## install.packages("beeswarm") # consoleからinstallする場合
library(beeswarm) # boxplot上に散布図を表示するため
TW.data <- read.csv("data/tokyo_weather.csv")
boxplot(temp~month, col="pink",
        data=TW.data, subset=month%in%7:8) # 月を限定
beeswarm(temp~month, add=TRUE, col="red",
         data=TW.data, subset=month%in%7:8) # 同じ書き方で良い
x <- subset(TW.data,subset=month==7,select=temp,drop=TRUE) 
y <- subset(TW.data,subset=month==8,select=temp,drop=TRUE)
t.test(x,y)
## 2月と12月でも試してみる
boxplot(temp~month, col="lightblue",
        data=TW.data, subset=month%in%c(2,12)) # 月を限定
beeswarm(temp~month, add=TRUE, col="blue",
         data=TW.data, subset=month%in%c(2,12)) # 同じ書き方で良い
x <- subset(TW.data,subset=month==2, select=temp,drop=TRUE) 
y <- subset(TW.data,subset=month==12,select=temp,drop=TRUE)
t.test(x,y)

### 練習問題 3月と6月の気温の分散の比の検定
boxplot(temp~month, col="lightgreen",
        data=TW.data, subset=month%in%c(3,6)) # 月を限定
beeswarm(temp~month, add=TRUE, col="seagreen",
         data=TW.data, subset=month%in%c(3,6)) # 同じ書き方で良い
x <- subset(TW.data,subset=month==3,select=temp,drop=TRUE) 
y <- subset(TW.data,subset=month==6,select=temp,drop=TRUE)
var.test(x,y)
## 2月と6月でも試してみる
boxplot(temp~month, col="lightgreen",
        data=TW.data, subset=month%in%c(2,6)) # 月を限定
beeswarm(temp~month, add=TRUE, col="seagreen",
         data=TW.data, subset=month%in%c(2,6)) # 同じ書き方で良い
x <- subset(TW.data,subset=month==2,select=temp,drop=TRUE) 
var.test(x,y)
