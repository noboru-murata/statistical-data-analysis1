### 練習1.1
### t検定のMonte-Carlo実験
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

### 練習1.2
### 視聴率の検定
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

### 練習2.1
### 6月の気温の分散の検定
myData <- read.csv("data/tokyo_weather.csv",
                   fileEncoding="utf8")
par(family="HiraginoSans-W4") 
boxplot(気温~月,data=myData, col="lightblue")
mySummary <- aggregate(気温~月,data=myData,FUN=var)
plot(mySummary,type="h",col="blue",lwd=3,
     ylim=c(0,max(mySummary$気温)+1),ylab="気温の分散")
v0 <- mean(mySummary$気温) # sigma0^2
x <- subset(myData, subset=月==6, select=気温, drop=TRUE)
(n <- length(x)) # データ数
(chi2 <- (n-1)*var(x)/v0) # 検定統計量
(p <- 1-pchisq(chi2, df=n-1))
## 分散が最も大きな3月を検定してみる
x <- subset(myData, subset=月==3, select=気温, drop=TRUE)
(n <- length(x)) # データ数
(chi2 <- (n-1)*var(x)/v0) # 検定統計量
(p <- 1-pchisq(chi2, df=n-1))

### 練習3.1
### 7月と8月の気温の平均の差の検定
## install.packages("beeswarm") # consoleからinstallする場合
library(beeswarm) # boxplot上に散布図を表示するため
myData <- read.csv("data/tokyo_weather.csv",
                   fileEncoding="utf8")
boxplot(気温~月, col="pink",
        data=myData, subset=月%in%7:8) # 月を限定
beeswarm(気温~月, add=TRUE, col="red",
         data=myData, subset=月%in%7:8) # 同じ書き方で良い
x <- subset(myData,subset=月==7,select=気温,drop=TRUE) 
y <- subset(myData,subset=月==8,select=気温,drop=TRUE)
t.test(x,y)
## 2月と3月でも試してみる
boxplot(気温~月, col="lightblue",
        data=myData, subset=月%in%2:3) # 月を限定
beeswarm(気温~月, add=TRUE, col="blue",
         data=myData, subset=月%in%2:3) # 同じ書き方で良い
x <- subset(myData,subset=月==2,select=気温,drop=TRUE) 
y <- subset(myData,subset=月==3,select=気温,drop=TRUE)
t.test(x,y)

### 練習3.2
### 3月と6月の気温の分散の比の検定
boxplot(気温~月, col="lightgreen",
        data=myData, subset=月%in%c(3,6)) # 月を限定
beeswarm(気温~月, add=TRUE, col="seagreen",
         data=myData, subset=月%in%c(3,6)) # 同じ書き方で良い
x <- subset(myData,subset=月==3,select=気温,drop=TRUE) 
y <- subset(myData,subset=月==6,select=気温,drop=TRUE)
var.test(x,y)
## 2月と6月でも試してみる
boxplot(気温~月, col="lightgreen",
        data=myData, subset=月%in%c(2,6)) # 月を限定
beeswarm(気温~月, add=TRUE, col="seagreen",
         data=myData, subset=月%in%c(2,6)) # 同じ書き方で良い
x <- subset(myData,subset=月==2,select=気温,drop=TRUE) 
var.test(x,y)
