## plot(x) の場合 
x <- pi/6*(0:12) # 30度(pi/6)おきに1周期分 (0-2*pi)
plot(sin(x)) # x軸はベクトルの要素番号(Index)，y軸はsin(x)の値を描画

## plot(x,y) の場合
x <- pi/6*(0:12)
plot(x, sin(x)) # x の値に対する y=sin(x) の値を対応づけて描画

## オプションを追加
x <- pi/6*(0:12)
plot(x,sin(x),type="l",lwd=3,col="blue",ylab="y=sin(x)")

## ベクトルデータの重ね描き
x <- seq(0, 4*pi, by=0.5)
y <- sin(x)
z <- cos(x)
plot(x, y, type="b", pch="x", ylim=c(-2,2), col="red") # "b"="p+l"
points(x, z, col="blue", pch="C") # 点を追加. pchは文字も指定できる
lines(x, z, col="cyan", lwd=3) # 折れ線を追加

## 関数の描画
curve(sin, from=0, to=4*pi, 
      col="blue", lwd=2, # グラフの線の色と太さ
      xlab="time", ylab="sin/cos") # x/y軸のラベルの文字列を指定
curve(cos, 
      add=TRUE, # グラフを上書き
      col="red", lwd=2)

## 関数とベクトルデータの重ね描き
x <- seq(0, 4*pi, by=0.25)
y <- sin(x) + rep(c(-0.2, 0.1), len=length(x))
plot(x, y, type="p", pch="x", ylim=c(-2,2), col="red") 
lines(x, y, col="blue", lwd=2) # 折れ線を追加
curve(sin, add=TRUE, col="orange", lwd=3)

## データフレームを用いた散布図
plot(Ozone ~ Wind, data=airquality,
     pch="*", col="red", cex=2) # cexは点の大きさの倍率を指定

### 練習1.1
### 婚姻・離婚率の散布図
## データの読み込み
myData <- read.csv(file="data/jpdata1.csv",fileEncoding="utf8",row.names=1)
myArea <- read.csv(file="data/jpdata3.csv",fileEncoding="utf8")
## 散布図
par(family="HiraginoSans-W4") # 日本語表示
plot(離婚 ~ 婚姻, data=myData, col="cyan", pch=19)
with(myData, text(婚姻,離婚,labels=rownames(myData)))

### 練習1.2
### 地方別に異なる記号の散布図
plot(離婚 ~ 婚姻, data=myData, col="red", pch=myArea$コード)
with(myData, text(婚姻,離婚,col="gray",cex=0.5,
		  labels=rownames(myData)))

## 関数histによるヒストグラムの作図
myData <- read.csv("data/tokyo_weather.csv", fileEncoding="utf8")
par(family="HiraginoSans-W4") # 日本語表示
hist(myData$気温, 
     xlab="", ylab="頻度",
     breaks=25, # ビンの数を約25に設定
     labels=TRUE, # 各ビンの度数を表示
     col="green", main="気温のヒストグラム")

## 関数histによるヒストグラムの作図(密度での表示)
par(family="HiraginoSans-W4") # 日本語表示
hist(myData$風速, freq=FALSE, # 全体に対する割合で表示
     xlab="", ylab="密度", breaks=25, 
     col="lightblue", border="blue", # 長方形の境界の色
     main="風速の密度")

## 関数boxplotによる箱ひげ図の作図
myData <- read.csv("data/tokyo_weather.csv", fileEncoding="utf8")
## 基本的な箱ひげ図
par(family="HiraginoSans-W4") # 日本語表示
boxplot(subset(myData, select=気温:風速)) # 数値データの一部を抽出

## 関数boxplotによる箱ひげ図の作図
myData <- read.csv("data/tokyo_weather.csv", fileEncoding="utf8")
## 月ごとに気温を分類
par(family="HiraginoSans-W4") # 日本語表示
boxplot(気温 ~ 月, data=myData, col="orange", main="月ごとの気温")
## 図を回転する場合は horizontal を指定する
## boxplot(気温 ~ 月, data=myData,
## 	col="purple", main="月ごとの気温", horizontal=TRUE)

## 関数barplotによる棒グラフの作図
myData <- read.csv("data/tokyo_weather.csv", fileEncoding="utf8")
## 月ごとに各変数の平均を計算
par(family="HiraginoSans-W4") # 日本語表示
x <- aggregate(. ~ 月, FUN=mean,
	       data=subset(myData, select=c(月,気温:風速)))
## 基本的な棒グラフ
barplot(x[,2], # 棒の高さのベクトル
	col="slateblue", # 棒の色の指定
	names.arg=x[,1], # x軸のラベル
	main=names(x)[2]) # タイトル

## 関数barplotによる棒グラフの作図
## 複数の棒グラフ
par(family="HiraginoSans-W4") # 日本語表示
barplot(as.matrix(x[ ,-1]), # 第1引数のデータフレームは行列にする
	col=rainbow(12)[c(8:1,12:9)], # 12色に色分け
	beside=TRUE, # 棒グラフを横に並べる
	space=c(1.5, 3), # 棒グラフ間・変数間のスペースを指定
	legend.text=paste0(x[ ,1], "月"), # 凡例の指定
	args.legend=list(ncol=2)) # 凡例を2列にして表示

## 関数pieによる円グラフの作図
myData <- read.csv("data/tokyo_weather.csv", fileEncoding="utf8")
z <- hist(myData$日射量, breaks=5, plot=FALSE) # 5つ程度に分類
x <- z$count
y <- z$breaks
names(x) <- paste(y[-length(y)], y[-1], sep="-")
## 向きと色を調整
par(family="HiraginoSans-W4") # 日本語表示
pie(x, clockwise=TRUE, main="日射量別の日数の割合",
    col=heat.colors(length(x),rev=TRUE))

### 練習2.1
### 陽性患者数の推移 (折れ線グラフ)
## データの読み込み
myData <- read.csv(file="data/covid19_tokyo.csv",fileEncoding="utf8")
## 折れ線グラフ
par(family="HiraginoSans-W4") # 日本語表示
plot(myData$陽性患者数, type="l", col="red", ylab="陽性患者数") 
## 日付ラベルの作成例
days <- as.Date(with(myData,paste("2020",月,日, sep="-"))) # 2020-月-日
plot(days,myData$陽性患者数, type="l", col="red", ylab="陽性患者数")

### 練習2.2
### 検査実施人数の推移 (棒グラフ)
par(family="HiraginoSans-W4") # 日本語表示
barplot(myData$検査実施人数, col="lightblue", ylab="検査実施人数") # 棒グラフ
plot(myData$検査実施人数, type="h", # 棒が多い場合はこういう方法もある
     col="blue", ylab="検査実施人数") 
## 日付ラベルの付加
plot(days, myData$検査実施人数, type="h", col="blue", ylab="検査実施人数") 
grid(col="darkgray") # 格子線の追加

### 練習2.3
### 曜日ごとの検査実施件数 (箱ひげ図)
par(family="HiraginoSans-W4") # 日本語表示
boxplot(検査実施件数 ~ 曜日, data=myData, col=cm.colors(7))

## 関数pairsによる散布図の作図
myData <- read.csv("data/tokyo_weather.csv", fileEncoding="utf8")
## 表示する項目を指定
par(family = "HiraginoSans-W4") 
pairs(~ 気温 + 日射量 + 風速, data=myData,
      col=rainbow(12)[myData$月]) # 月毎に異なる色で表示

## 関数perspによる2変数関数の俯瞰図
f <- function(x,y) x^2 - y^2
x <- seq(-3, 3, length=51) # x座標の定義域の分割
y <- seq(-3, 3, length=51) # y座標の定義域の分割
z <- outer(x, y, f) # z座標の計算
## 基本的な俯瞰図
## persp(x, y, z, col="lightblue")
## 俯瞰する向きを指定
persp(x, y, z, theta=30, phi=30, expand=0.5, # 俯瞰する視線の設定
      col="royalblue", main=expression(z==x^2-y^2))

## 3次元散布図
## install.packages("scatterplot3d") # 初めて使う時に必要
library(scatterplot3d) # パッケージのロード
myData <- read.csv("data/tokyo_weather.csv", fileEncoding="utf8")
par(family = "HiraginoSans-W4") 
scatterplot3d(subset(myData, select=c(風速, 日射量, 気温)), 
	      pch=4, color="orchid")

## 凡例の追加
f <- function(x) exp(-x) * cos(x)
plot(f, 0, 2*pi, col="red", lwd=2, ylab="")
g <- function(x) exp(-x) * sin(x)
curve(g, lty=2, # グラフの線の形式 2は破線
      add=TRUE, col="blue", lwd=2)
legend(4, # 凡例の左上のx座標
       1, # 凡例の左上のy座標
       legend=c(expression(e^{-x}*cos(x)),expression(e^{-x}*sin(x))),
       lty=c(1,2), lwd=2, col=c("red","blue"), # 指定はグラフに準拠
       bty="n", # 凡例の枠線の形式(オプション) "n"は枠線なし
       y.intersp=2) # 行間の指定(オプション)

## 日本語フォントの指定
par(family="HiraginoSans-W4") 
## 東京の気候データから月ごとの気温,降水量,日射量の平均を計算し描画する
myData <- read.csv("data/tokyo_weather.csv", fileEncoding="utf8")
(x <- aggregate(. ~ 月, FUN=mean,
		data=subset(myData, select=c(月,気温,降水量,日射量))))
plot(x$気温, type ="b", lwd=3, col="green", ylim=c(0, max(x$気温)+1),
     xlab="月", ylab="", main="東京の気候データ", axes=FALSE) # 軸は無
axis(1, 1:12, 1:12); axis(2) # x(1),y(2)軸の作成
lines(x$降水量, type="h", lwd=3, col="blue") # 棒グラフ
lines(x$日射量, type="s", lwd=3, lty=2, col="red") # 階段グラフ
abline(0, 0, lwd=2, lty="dotted") #  y=0の線を引く
legend("topleft", inset=0.02, # 左上で全体の2%(0.02)内側に良せる
       legend=c("気温","降水量","日射量"),
       col=c("green","blue","red"), lwd=3, lty=c(1,1,2))

### 練習3.1
### 3次元の散布図 (jpdat1.csvを用いた例)
## データの読み込み
myData <- read.csv(file="data/jpdata1.csv",fileEncoding="utf8",row.names=1)
## 散布図
par(family="HiraginoSans-W4") # 日本語表示
scatterplot3d(subset(myData, select=c(婚姻,離婚,失業)), 
	      pch=19, color="blue")
pairs(subset(myData, select=c(婚姻,離婚,失業)), col="blue") # 三面図で見てみる

### 練習3.2
### 凡例の追加 (covid19_tokyo.csvを用いた例)
## データの読み込み
myData <- read.csv(file="data/covid19_tokyo.csv",fileEncoding="utf8")
days <- as.Date(with(myData,paste("2020",月,日, sep="-"))) # 日付ラベル
## 
par(family="HiraginoSans-W4") # 日本語表示
plot(days,myData$検査実施人数, type="h", col="blue", xlab="日付", ylab="人数")
abline(h=seq(0,500,by=100), lty=2, col="darkgray") # 補助線の追加
lines(days,myData$陽性患者数, col="red")
title(main="検査実績の推移") 
legend("topleft", inset=0.01, 
       legend=c("検査実施人数","陽性患者数"),
       col=c("blue","red"), lwd=3, lty=1)

### 凡例の追加 (covid19_tokyo_patients.csvを用いた例)
## データの読み込み
myData <- read.csv(file="data/covid19_tokyo_patients.csv")
## 簡単な集計には関数table()を使うとよい
table(subset(myData,select=c(患者_年代))) # 名前のついたベクトル
barplot(table(subset(myData,select=c(患者_年代))))
## 月別の年齢分布を調べる
x <-with(myData,data.frame(age=患者_年代,
		    month=months(as.Date(公表_年月日))))
(tab1 <- table(x)) # (年齢 x 月) の患者数の表(行列)
(tab2 <- apply(tab1,2,function(z){z/sum(z)})) # 月ごとの年齢分布
## 描画
par(family="HiraginoSans-W4") # 日本語表示
barplot(tab1, # 人数のグラフ
	col=rainbow(13), # 13色に色分け
	beside=TRUE, # 棒グラフを横に並べる
	space=c(1.5, 3), # 棒グラフ間・変数間のスペースを指定
	legend.text=rownames(tab1), # 凡例の指定, 2列，縮小, 左上に表示
	args.legend=list(ncol=2,cex=0.5,x="topleft",inset=0.01)) 
barplot(tab2, # 比率のグラフ
	col=rainbow(13), # 13色に色分け
	beside=TRUE, # 棒グラフを横に並べる
	space=c(1.5, 3), # 棒グラフ間・変数間のスペースを指定
	legend.text=rownames(tab1), # 凡例の指定，2列，縮小
	args.legend=list(ncol=2,cex=0.5))
