### 第09回 練習問題解答例

### 練習問題 平均・分散・標準偏差の計算
## データの読み込み
TW.data <- read.csv("data/tokyo_weather.csv")
## 全データによる計算
myData <- subset(TW.data, 
                 select = c(temp,solar,wind))
(mu <- apply(myData,2,FUN=mean))
(s2 <- apply(myData,2,FUN=var))
(s  <- apply(myData,2,FUN=sd))
## 毎月5日のデータによる計算
myData <- subset(TW.data,
                 subset = day==5,
                 select = c(temp,solar,wind))
apply(myData,2,FUN=mean)
apply(myData,2,FUN=var)
apply(myData,2,FUN=sd)
## 5の付く日のデータによる計算
myData <- subset(TW.data,
                 subset = day%in%c(5,15,25),
                 select=c(temp,solar,wind))
apply(myData,2,FUN=mean)
apply(myData,2,FUN=var)
apply(myData,2,FUN=sd)
## ランダムに選択した36日で推定した場合のばらつき
mc <- 5000 # 実験回数を指定
myItems <- c("temp","solar","wind") # 文字列のベクトルとして定義する場合は""が必要
myFuncs <- c("mean","var","sd")
myTruth <- list(mu,s2,s) # 各変数ごとの平均，分散，標準偏差ベクトルをまとめておく
inum <- 1; fnum <- 1 # 気温の標本平均の例
myTrial <- function(){ # return と明示していないが，最後の行が返される
    idx <- sample(1:nrow(TW.data),36) # 重複なしで36行選ぶ
    apply(subset(TW.data[idx,],select=myItems[inum]),2,FUN=myFuncs[fnum])}
xbars <- replicate(mc,myTrial())
if(Sys.info()["sysname"]=="Darwin"){par(family="HiraginoSans-W4")}
hist(xbars, breaks=40, freq=FALSE,
     col="lightblue", border="blue",
     xlab=myItems[inum], main=paste(myItems[inum],"の",myFuncs[fnum],"の推定"))
abline(v=myTruth[[fnum]][inum],col="red",lwd=2)
## 全ての組み合わせは for 文で実行可能
for(inum in 1:3){
    for(fnum in 1:3){
        myTrial <- function(){
            idx <- sample(1:nrow(TW.data),36) # 重複なしで36行選ぶ
            apply(subset(TW.data[idx,],select=myItems[inum]),2,FUN=myFuncs[fnum])}
        xbars <- replicate(mc,myTrial())
        if(Sys.info()["sysname"]=="Darwin"){par(family="HiraginoSans-W4")}
        hist(xbars, breaks=40, freq=FALSE,
             col="lightblue", border="blue",
             xlab=myItems[inum], main=paste(myItems[inum],"の",myFuncs[fnum],"の推定"))
        abline(v=mean(xbars),col="green",lwd=2) # 推定量の平均
        abline(v=myTruth[[fnum]][inum],col="red",lwd=2) # 真の値
    }
}

### 練習問題 歪度と超過尖度の計算
library(e1071)
## データの読み込み
TW.data <- read.csv("data/tokyo_weather.csv")
## 全データによる計算
myData <- subset(TW.data,
                 select = c(temp,solar,wind))
(skew <- apply(myData,2,FUN=skewness))
(kurt <- apply(myData,2,FUN=kurtosis))
## 5の付く日のデータによる計算
myData <- subset(TW.data,
                 subset = day%in%c(5,15,25),
                 select = c(temp,solar,wind))
apply(myData,2,FUN=skewness)
apply(myData,2,FUN=kurtosis)
## ヒストグラムの描画と
myItems <- c("temp","solar","wind")
if(Sys.info()["sysname"]=="Darwin"){par(family="HiraginoSans-W4")}
## 全ての組み合わせは for 文で実行可能
for(inum in 1:3){
  foo <- TW.data[,myItems[inum]] # ベクトルにする必要がある
  ## foo <- subset(TW.data, select=myItems[inum], drop=TRUE) # でもよい
  if(Sys.info()["sysname"]=="Darwin"){par(family="HiraginoSans-W4")}
  hist(foo, breaks=30, freq=FALSE,
       col="lightgreen", border="green",
       xlab=myItems[inum], main=myItems[inum])
  curve(dnorm(x,mean=mean(foo),sd=sd(foo)), # x は curve の変数
        add=TRUE, col="orange", lwd=2)
}

### 練習問題 共分散と相関の計算
## データの読み込み
TW.data <- read.csv("data/tokyo_weather.csv")
## 共分散・相関行列の計算
myData <- subset(TW.data,
                 select = c(temp,rain,solar,snow,wind,press,humid)) # 数値データ
(myCov <- cov(myData))
(myCor <- cor(myData))
myCor==min(myCor) # 負の最大相関の位置を確認 (solar,humid)
myCor==max(myCor-diag(diag(myCor))) # 対角を除く最大相関の位置を確認 (temp,humid)
abs(myCor)==min(abs(myCor)) # 最小相関の位置を確認 (snow,wind)
## 散布図の描画
if(Sys.info()["sysname"]=="Darwin"){par(family="HiraginoSans-W4")}
pairs(~ solar + humid, data=myData, col="blue")
pairs(~ temp + humid, data=myData, col="red")
pairs(~ snow + wind, data=myData, col="grey")
pairs(myData, col="orange") # 数値データを全部表示してみる

### 練習問題 分位点と最頻値の計算
## データの読み込み
TW.data <- read.csv("data/tokyo_weather.csv")
## 気温の分位点
## 全データによる計算
(myTruth <- summary(subset(TW.data,select=temp,drop=TRUE)))
## 5の付く日のデータによる計算
summary(subset(TW.data,subset=day%in%c(5,15,25),select=temp,drop=TRUE))
## ランダムに選択した36日で推定した場合のばらつき
mc <- 5000 # 実験回数を指定
myFuncs <- c("min","1Q","median","mean","3Q","max")
myTrial <- function(){
  idx <- sample(1:nrow(TW.data),36) # 重複なしで36行選ぶ
  summary(subset(TW.data[idx,],select=temp,drop=TRUE))}
quants <- replicate(mc,myTrial())
## ヒストグラムの表示
for(fnum in 1:6) {
  if(Sys.info()["sysname"]=="Darwin"){par(family="HiraginoSans-W4")}
  hist(quants[fnum,], breaks=40, freq=FALSE,
       col="lightblue", border="blue",
       xlab="気温", main=paste("気温の",myFuncs[fnum],"の推定"))
  abline(v=myTruth[fnum],col="red",lwd=2)
}
## ヒストグラムの表示 (定義域とビンを揃えて表示する)
for(fnum in 1:6) {
  if(Sys.info()["sysname"]=="Darwin"){par(family="HiraginoSans-W4")}
  hist(quants[fnum,],
       breaks=pretty(subset(TW.data,select=temp,drop=TRUE),n=50), # 固定
       freq=FALSE,
       col="lightblue", border="blue",
       xlab="気温", main=paste("気温の",myFuncs[fnum],"の推定"))
  abline(v=myTruth[fnum],col="red",lwd=2)
}
## 最多風向の最頻値
(myTable <- table(subset(TW.data,select=wdir))) # 頻度表
max(myTable) # 最大値 
which.max(myTable) # 最頻値の表の位置
names(which.max(myTable)) # 最頻値の項目名
