### 練習1
### 平均・分散・標準偏差の計算
## データの読み込み
myData <- read.csv("data/tokyo_weather.csv", fileEncoding="utf8")
## 全データによる計算
(mu <- apply(subset(myData,select=c(気温,日射量,風速)),2,FUN=mean))
(s2 <- apply(subset(myData,select=c(気温,日射量,風速)),2,FUN=var))
(s <- apply(subset(myData,select=c(気温,日射量,風速)),2,FUN=sd))
## 毎月5日のデータによる計算
apply(subset(myData,subset=日==5,select=c(気温,日射量,風速)),2,FUN=mean)
apply(subset(myData,subset=日==5,select=c(気温,日射量,風速)),2,FUN=var)
apply(subset(myData,subset=日==5,select=c(気温,日射量,風速)),2,FUN=sd)
## 5の付く日のデータによる計算
apply(subset(myData,subset=日%in%c(5,15,25),select=c(気温,日射量,風速)),2,FUN=mean)
apply(subset(myData,subset=日%in%c(5,15,25),select=c(気温,日射量,風速)),2,FUN=var)
apply(subset(myData,subset=日%in%c(5,15,25),select=c(気温,日射量,風速)),2,FUN=sd)
## ランダムに選択した36日で推定した場合のばらつき
mc <- 5000 # 実験回数を指定
myItems <- c("気温","日射量","風速")
myFuncs <- c("mean","var","sd")
myTruth <- list(mu,s2,s)
inum <- 1; fnum <- 1 # 気温の標本平均の例
myTrial <- function(){
    idx <- sample(1:nrow(myData),36) # 重複なしで36行選ぶ
    apply(subset(myData[idx,],select=myItems[inum]),2,FUN=myFuncs[fnum])}
xbars <- replicate(mc,myTrial())
par(family="HiraginoSans-W4") 
hist(xbars, breaks=40, freq=FALSE,
     col="lightblue", border="blue",
     xlab=myItems[inum], main=paste(myItems[inum],"の",myFuncs[fnum],"の推定"))
abline(v=myTruth[[fnum]][inum],col="red",lwd=2)
## 全ての組み合わせは for 文で実行可能
for(inum in 1:3){
    for(fnum in 1:3){
        myTrial <- function(){
            idx <- sample(1:nrow(myData),36) # 重複なしで36行選ぶ
            apply(subset(myData[idx,],select=myItems[inum]),2,FUN=myFuncs[fnum])}
        xbars <- replicate(mc,myTrial())
        par(family="HiraginoSans-W4") 
        hist(xbars, breaks=40, freq=FALSE,
             col="lightblue", border="blue",
             xlab=myItems[inum], main=paste(myItems[inum],"の",myFuncs[fnum],"の推定"))
        abline(v=myTruth[[fnum]][inum],col="red",lwd=2)
    }
}

### 練習2
### 歪度と超過尖度の計算
library(e1071)
## データの読み込み
myData <- read.csv("data/tokyo_weather.csv", fileEncoding="utf8")
## 全データによる計算
(skew <- apply(subset(myData,select=c(気温,日射量,風速)),2,FUN=skewness))
(kurt <- apply(subset(myData,select=c(気温,日射量,風速)),2,FUN=kurtosis))
## 5の付く日のデータによる計算
apply(subset(myData,subset=日%in%c(5,15,25),select=c(気温,日射量,風速)),2,FUN=skewness)
apply(subset(myData,subset=日%in%c(5,15,25),select=c(気温,日射量,風速)),2,FUN=kurtosis)
## ヒストグラムの描画と
myItems <- c("気温","日射量","風速")
par(family="HiraginoSans-W4") 
## 全ての組み合わせは for 文で実行可能
for(inum in 1:3){
    x <- myData[,myItems[inum]] # ベクトルにする必要がある
    ## x <- subset(myData, select=myItems[inum], drop=TRUE) # でもよい
    par(family="HiraginoSans-W4") 
    hist(x, breaks=30, freq=FALSE,
         col="lightgreen", border="green",
         xlab=myItems[inum], main=myItems[inum])
    curve(dnorm(x,mean=mean(x),sd=sd(x)), add=TRUE,
          col="orange", lwd=2)
}

### 練習3
### 共分散と相関の計算
## データの読み込み
myData <- read.csv("data/tokyo_weather.csv", fileEncoding="utf8")
## 共分散・相関行列の計算
x <- subset(myData,select=c(気温,降水量,日射量,降雪量,風速,気圧,湿度)) # 数値データ
(myCov <- cov(x))
(myCor <- cor(x))
myCor==min(myCor) # 負の最大相関の位置を確認 (気温と気圧)
myCor==max(myCor-diag(diag(myCor))) # 対角を除く最大相関の位置を確認 (気温と湿度)
abs(myCor)==min(abs(myCor)) # 最小相関の位置を確認 (降雪量と風速)
## 散布図の描画
par(family="HiraginoSans-W4") 
pairs(~ 気温 + 気圧, data=myData, col="blue")
pairs(~ 気温 + 湿度, data=myData, col="red")
pairs(~ 降雪量 + 風速, data=myData, col="grey")
pairs(x, col="orange") # 数値データを全部表示してみる

### 練習4
### 分位点と最頻値の計算
## データの読み込み
myData <- read.csv("data/tokyo_weather.csv", fileEncoding="utf8")
## 気温の分位点
## 全データによる計算
(myTruth <- summary(subset(myData,select=気温,drop=TRUE)))
## 5の付く日のデータによる計算
summary(subset(myData,subset=日%in%c(5,15,25),select=気温,drop=TRUE))
## ランダムに選択した36日で推定した場合のばらつき
mc <- 5000 # 実験回数を指定
myFuncs <- c("min","1Q","median","mean","3Q","max")
myTrial <- function(){
    idx <- sample(1:nrow(myData),36) # 重複なしで36行選ぶ
    summary(subset(myData[idx,],select=気温,drop=TRUE))}
quants <- replicate(mc,myTrial())
## ヒストグラムの表示
for(fnum in 1:6) {
    par(family="HiraginoSans-W4") 
    hist(quants[fnum,], breaks=40, freq=FALSE,
         col="lightblue", border="blue",
         xlab="気温", main=paste("気温の",myFuncs[fnum],"の推定"))
    abline(v=myTruth[fnum],col="red",lwd=2)
}
## 最多風向の最頻値
(myTable <- table(subset(myData,select=最多風向))) # 頻度表
max(myTable) # 最大値 
which.max(myTable) # 最頻値の表の位置
names(which.max(myTable)) # 最頻値の項目名
