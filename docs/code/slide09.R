### 第9講 練習問題解答例

### 練習問題 平均・分散・標準偏差の計算
## データの読み込み
tw_data <- read.csv("data/tokyo_weather.csv")
## 全データによる計算
my_data <- subset(tw_data, 
                 select = c(temp,solar,wind))
(mu <- apply(my_data,2,FUN=mean))
(s2 <- apply(my_data,2,FUN=var))
(s  <- apply(my_data,2,FUN=sd))
## 毎月5日のデータによる計算
my_data <- subset(tw_data,
                 subset = day==5,
                 select = c(temp,solar,wind))
apply(my_data,2,FUN=mean)
apply(my_data,2,FUN=var)
apply(my_data,2,FUN=sd)
## 5の付く日のデータによる計算
my_data <- subset(tw_data,
                 subset = day%in%c(5,15,25),
                 select=c(temp,solar,wind))
apply(my_data,2,FUN=mean)
apply(my_data,2,FUN=var)
apply(my_data,2,FUN=sd)
## ランダムに選択した36日で推定した場合のばらつき
mc <- 5000 # 実験回数を指定
my_items <- c("temp","solar","wind") # 文字列のベクトルとして定義する場合は""が必要
my_funcs <- c("mean","var","sd")
my_truth <- list(mu,s2,s) # 各変数ごとの平均，分散，標準偏差ベクトルをまとめておく
inum <- 1; fnum <- 1 # 気温の標本平均の例
my_trial <- function(){ # return と明示していないが，最後の行が返される
    idx <- sample(1:nrow(tw_data),36) # 重複なしで36行選ぶ
    apply(subset(tw_data[idx,],select=my_items[inum]),2,FUN=my_funcs[fnum])}
xbars <- replicate(mc,my_trial())
if(Sys.info()["sysname"]=="Darwin"){par(family="HiraginoSans-W4")}
hist(xbars, breaks=40, freq=FALSE,
     col="lightblue", border="blue",
     xlab=my_items[inum], main=paste(my_items[inum],"の",my_funcs[fnum],"の推定"))
abline(v=my_truth[[fnum]][inum],col="red",lwd=2)
## 全ての組み合わせは for 文で実行可能
for(inum in 1:3){
    for(fnum in 1:3){
        my_trial <- function(){
            idx <- sample(1:nrow(tw_data),36) # 重複なしで36行選ぶ
            apply(subset(tw_data[idx,],select=my_items[inum]),2,FUN=my_funcs[fnum])}
        xbars <- replicate(mc,my_trial())
        if(Sys.info()["sysname"]=="Darwin"){par(family="HiraginoSans-W4")}
        hist(xbars, breaks=40, freq=FALSE,
             col="lightblue", border="blue",
             xlab=my_items[inum], main=paste(my_items[inum],"の",my_funcs[fnum],"の推定"))
        abline(v=mean(xbars),col="green",lwd=2) # 推定量の平均
        abline(v=my_truth[[fnum]][inum],col="red",lwd=2) # 真の値
    }
}

### 練習問題 歪度と超過尖度の計算
library("e1071")
## データの読み込み
tw_data <- read.csv("data/tokyo_weather.csv")
## 全データによる計算
my_data <- subset(tw_data,
                 select = c(temp,solar,wind))
(skew <- apply(my_data,2,FUN=skewness))
(kurt <- apply(my_data,2,FUN=kurtosis))
## 5の付く日のデータによる計算
my_data <- subset(tw_data,
                 subset = day%in%c(5,15,25),
                 select = c(temp,solar,wind))
apply(my_data,2,FUN=skewness)
apply(my_data,2,FUN=kurtosis)
## ヒストグラムの描画と
my_items <- c("temp","solar","wind")
if(Sys.info()["sysname"]=="Darwin"){par(family="HiraginoSans-W4")}
## 全ての組み合わせは for 文で実行可能
for(inum in 1:3){
  foo <- tw_data[,my_items[inum]] # ベクトルにする必要がある
  ## foo <- subset(tw_data, select=my_items[inum], drop=TRUE) # でもよい
  if(Sys.info()["sysname"]=="Darwin"){par(family="HiraginoSans-W4")}
  hist(foo, breaks=20, freq=FALSE,
       col="lightgreen", border="green",
       xlab=my_items[inum], main=my_items[inum])
  curve(dnorm(x,mean=mean(foo),sd=sd(foo)), # x は curve の変数
        add=TRUE, col="orange", lwd=2)
}

### 練習問題 共分散と相関の計算
## データの読み込み
tw_data <- read.csv("data/tokyo_weather.csv")
## 共分散・相関行列の計算
my_data <- subset(tw_data,
                 select = c(temp,rain,solar,snow,wind,press,humid)) # 数値データ
(my_cov <- cov(my_data))
(my_cor <- cor(my_data))
my_cor==min(my_cor) # 負の最大相関の位置を確認 (solar,rain)
my_cor==max(my_cor-diag(diag(my_cor))) # 対角を除く最大相関の位置を確認 (temp,humid)
abs(my_cor)==min(abs(my_cor)) # 最小相関の位置を確認 (snow,humid)
## 散布図の描画
if(Sys.info()["sysname"]=="Darwin"){par(family="HiraginoSans-W4")}
pairs(~ solar + humid, data=my_data, col="blue")
pairs(~ temp + humid, data=my_data, col="red")
pairs(~ snow + wind, data=my_data, col="grey")
pairs(my_data, col="orange") # 数値データを全部表示してみる

### 練習問題 分位点と最頻値の計算
## データの読み込み
tw_data <- read.csv("data/tokyo_weather.csv")
## 気温の分位点
## 全データによる計算
(my_truth <- summary(subset(tw_data,select=temp,drop=TRUE)))
## 5の付く日のデータによる計算
summary(subset(tw_data,subset=day%in%c(5,15,25),select=temp,drop=TRUE))
## ランダムに選択した36日で推定した場合のばらつき
mc <- 5000 # 実験回数を指定
my_funcs <- c("min","1Q","median","mean","3Q","max")
my_trial <- function(){
  idx <- sample(1:nrow(tw_data),36) # 重複なしで36行選ぶ
  summary(subset(tw_data[idx,],select=temp,drop=TRUE))}
quants <- replicate(mc,my_trial())
## ヒストグラムの表示
for(fnum in 1:6) {
  if(Sys.info()["sysname"]=="Darwin"){par(family="HiraginoSans-W4")}
  hist(quants[fnum,], breaks=40, freq=FALSE,
       col="lightblue", border="blue",
       xlab="気温", main=paste("気温の",my_funcs[fnum],"の推定"))
  abline(v=my_truth[fnum],col="red",lwd=2)
}
## ヒストグラムの表示 (定義域とビンを揃えて表示する)
for(fnum in 1:6) {
  if(Sys.info()["sysname"]=="Darwin"){par(family="HiraginoSans-W4")}
  hist(quants[fnum,],
       breaks=pretty(subset(tw_data,select=temp,drop=TRUE),n=50), # 固定
       freq=FALSE,
       col="lightblue", border="blue",
       xlab="気温", main=paste("気温の",my_funcs[fnum],"の推定"))
  abline(v=my_truth[fnum],col="red",lwd=2)
}
## 最多風向の最頻値
(my_table <- table(subset(tw_data,select=wdir))) # 頻度表
max(my_table) # 最大値 
which.max(my_table) # 最頻値の表の位置
names(which.max(my_table)) # 最頻値の項目名
