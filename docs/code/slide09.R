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
n <- 36 # ランダムに選択する日数を指定
## 気温の標本平均の例
my_trial <- function(){ # return と明示していないが，最後の行が返される
  idx <- sample(nrow(tw_data),n) # 重複なしでn=36行の番号を選ぶ
  sapply(subset(tw_data[idx,],select=temp),FUN=mean)}
xbars <- replicate(mc,my_trial())
if(Sys.info()["sysname"]=="Darwin"){par(family="HiraginoSans-W4")}
hist(xbars, breaks=40, freq=FALSE,
     col="lightblue", border="blue",
     xlab="temp", main="平均気温の推定")
abline(v=mu[1],col="red",lwd=2)
## 項目および統計量を総当たりで調べる
my_items <- c("temp","solar","wind") # 文字列のベクトルとして定義する場合は""が必要
my_funcs <- c("mean","var","sd") # 統計量を計算する関数名
my_truth <- cbind(mu,s2,s) # 各変数ごとの平均，分散，標準偏差ベクトルをまとめておく
## 実験を行う関数で項目と統計量を指定できるように定義する
my_trial <- function(item,stat){
  idx <- sample(nrow(tw_data),n) # 重複なしでn行の番号を選ぶ
  sapply(subset(tw_data[idx,],select=item),FUN=stat)}
## 図示する部分を関数として定義しておく
my_plot <- function(estimates,true_value,xlabel,title){
  if(Sys.info()["sysname"]=="Darwin"){par(family="HiraginoSans-W4")}
  hist(estimates, breaks=40, freq=FALSE,
       col="lightblue", border="blue",
       xlab=xlabel, main=title)
  abline(v=mean(estimates),col="green",lwd=2) # 推定量の平均
  abline(v=true_value,col="red",lwd=2) # 真の値
}
## 全ての組み合わせは for 文で実行可能
for(i in 1:3){ # 項目の番号を指定する
  for(j in 1:3){ # 関数の番号を指定する
    my_plot(estimates=replicate(mc,my_trial(my_items[i],my_funcs[j])),
            true_value=my_truth[i,j],
            xlabel=my_items[i],
            title=paste(my_items[i],"の",my_funcs[j],"の推定"))
  }
}
## 標準偏差の推定量は偏りがあることが確認できる
## 風速の分散の推定量の分布が他と異なり正規分布に近くないので，
## サンプル数を増やしてみる
i <- 3; j <- 2 # 風速の分散
n <- 72 # サンプル数を倍にしてみる
my_plot(estimates=replicate(mc,my_trial(my_items[i],my_funcs[j])),
        true_value=my_truth[i,j],
        xlabel=my_items[i],
        title=paste(my_items[i],"の",my_funcs[j],"の推定"))
## 形状が正規分布に近づいたことが確認できる

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
pairs(~ solar + rain, data=my_data, col="blue")
pairs(~ temp + humid, data=my_data, col="red")
pairs(~ snow + humid, data=my_data, col="grey")
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
