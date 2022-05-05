### 第05回 練習問題解答例

### 基本的な描画で用いた例

x <- pi/6*(0:12) # 30度(pi/6)おきに1周期分 (0-2*pi)
plot(sin(x)) # x軸はベクトルの要素番号(Index)，y軸はsin(x)の値を描画

x <- pi/6*(0:12)
plot(x, sin(x)) # x の値に対する y=sin(x) の値を対応づけて描画

x <- pi/6*(0:12)
plot(x,sin(x),type="l",lwd=3,col="blue",ylab="y=sin(x)")

x <- seq(0, 4*pi, by=0.5)
y <- sin(x)
z <- cos(x)
plot(x, y, type="b", pch="x", ylim=c(-2,2), col="red") # "b"="p+l"
points(x, z, col="blue", pch="C") # 点を追加. pchは文字も指定できる
lines(x, z, col="cyan", lwd=3) # 折れ線を追加

curve(sin, from=0, to=4*pi, 
      col="blue", lwd=2, # グラフの線の色と太さ
      xlab="time", ylab="sin/cos") # x/y軸のラベルの文字列を指定
curve(cos, # 上書きする場合は範囲の指定は不要
      add=TRUE, # グラフを上書き
      col="red", lwd=2)

x <- seq(0, 4*pi, by=0.25)
y <- sin(x) + rep(c(-0.2, 0.1), len=length(x))
plot(x, y, type="p", pch="x", ylim=c(-2,2), col="red") 
lines(x, y, col="blue", lwd=2) # 折れ線を追加
curve(sin, add=TRUE, col="orange", lwd=3)

plot(Ozone ~ Wind, data=airquality, # xy軸名は列の名前が使われる
     pch="*", # 点の形を文字で指定することもできる
     col="red", 
     cex=2) # cexは点の大きさの倍率を指定する

### 練習問題 関数 plot() による描画
## データの読み込み
JP.data <- read.csv(file="data/jpdata1.csv", fileEncoding="utf8", row.names=1)
JP.area <- read.csv(file="data/jpdata3.csv", fileEncoding="utf8")

## jpdata1 に jpdata3 を付加する
JP.data <- cbind(JP.data,JP.area)

## 婚姻・離婚率の散布図
if(Sys.info()["sysname"]=="Darwin"){ # macOSの場合の日本語表示
    par(family="HiraginoSans-W4")}  
plot(離婚 ~ 婚姻, data=JP.data, # データフレームを用いた散布図の指定
     col="green", # 点の色を指定
     pch=19) # 点の形を指定 (help("points")参照)
with(JP.data, text(婚姻, 離婚, labels=県名)) # X軸, Y軸 の順に注意
## 明示するには text(x=婚姻, y=離婚) とする
## 関数 text() には引数 data はないが，関数 with() を利用するとよい

## 地方別に異なる記号の散布図
plot(離婚 ~ 婚姻, data=JP.data,
     col="red", 
     pch=コード)
with(JP.data, text(婚姻, 離婚, labels=県名,
                   col="gray", # 文字の色を指定
                   cex=0.5)) # 文字の大きさを指定(既定値は1)

### 分布の視覚化で用いた例

if(Sys.info()["sysname"]=="Darwin"){par(family="HiraginoSans-W4")} # 日本語表示  
## 東京都の気温のヒストグラムを作成する
TW.data <- read.csv("data/tokyo_weather.csv") # 東京都の気象データの読み込み
hist(TW.data$temp, 
     xlab="気温(℃)", ylab="頻度",
     breaks=25, # ビンの数を約25に設定
     labels=TRUE, # 各ビンの度数を表示
     col="lightpink", main="気温のヒストグラム")

if(Sys.info()["sysname"]=="Darwin"){par(family="HiraginoSans-W4")} # 日本語表示  
hist(TW.data$wind, freq=FALSE, # 全体に対する割合で表示
     xlab="風速(m/s)", ylab="密度",
     breaks=25, 
     col="lightblue", border="blue", # 長方形の境界の色
     main="風速の密度")

if(Sys.info()["sysname"]=="Darwin"){par(family="HiraginoSans-W4")} # 日本語表示  
## 気温, 降雨, 日射, 降雪, 風速の箱ひげ図を作成する
boxplot(subset(TW.data, select=c(temp:snow,wind)), # 数値データの一部を抽出
        names=c("気温","降雨","日射","降雪","風速")) # 各箱ひげ図の名前を指定
## names を指定しなければ列名が使われる

if(Sys.info()["sysname"]=="Darwin"){par(family="HiraginoSans-W4")} # 日本語表示  
## 月ごとの気温の分布を箱ひげ図によって可視化する
boxplot(temp ~ month, data=TW.data,
        col="orange",
        xlab="月",ylab="気温",main="月ごとの気温")
## 図を回転する場合は horizontal を指定する
## boxplot(気温 ~ 月, data=myData,
##         col="purple", main="月ごとの気温", horizontal=TRUE)

### 比率の視覚化で用いた例

if(Sys.info()["sysname"]=="Darwin"){par(family="HiraginoSans-W4")} # 日本語表示  
## 月ごとに各変数の平均を計算
(foo <- aggregate(. ~ month, FUN=mean,
                  data=subset(TW.data, select=c(month,temp:snow,wind))))
## 月ごとの気温の平均値の棒グラフを作成する
barplot(foo$temp, # 棒の高さのベクトル
        col="slateblue", # 棒の色の指定
        names.arg=foo$month, # x軸のラベル
        xlab="月",main="平均気温") # タイトル

if(Sys.info()["sysname"]=="Darwin"){par(family="HiraginoSans-W4")} # 日本語表示  
## 気温, 降雨, 日射, 降雪, 風速の月ごとの棒グラフを作成する
barplot(as.matrix(foo[ ,-1]), # 第1引数のデータフレームは行列にする
        col=rainbow(12)[c(8:1,12:9)], # 12色に色分け．季節に合うように色を並べ変えている
        beside=TRUE, # 各列ごとの棒グラフを横に並べる
        space=c(1.5, 3), # 棒グラフ間・変数間のスペースを指定
        names.arg=c("気温","降雨","日射","降雪","風速"), # 各列の名前を指定．指定しなければ列名が使われる
        legend.text=paste0(foo$month,"月"), # 凡例の指定
        args.legend=list(ncol=2)) # 凡例を2列にして表示

if(Sys.info()["sysname"]=="Darwin"){par(family="HiraginoSans-W4")} # 日本語表示  
## ヒストグラムの機能を用いてデータの集計を行う
foo <- hist(TW.data$solar, breaks=5, plot=FALSE) # 5つ程度に分類を指定．実際には6つに分類
bar <- foo$count # 各ビン内のデータ数
baz <- foo$breaks # ビンの境界
names(bar) <- paste(baz[-length(baz)],baz[-1],sep="-") # ビンの範囲の文字列を作成
## 6つに分類した日射量ごとの日数の割合を示す円グラフを作成する
pie(bar, clockwise=TRUE, main="日射量別の日数の割合",
    col=heat.colors(length(bar),rev=TRUE)) # 日射量が高いほど赤を濃く指定

### 練習問題 東京都の感染動向データによる例
## 陽性患者数の推移 (折れ線グラフ)
## データの読み込み
TC.data <- read.csv(file="data/tokyo_covid19_2021.csv",fileEncoding="utf8")
names(TC.data)[1] <- "年月日" # CSVファイルの1列目の名前が空白なので定義しておく
TC.data <- transform(TC.data,年月日=as.Date(年月日)) # 日付の属性を変えておく

## 折れ線グラフ
if(Sys.info()["sysname"]=="Darwin"){par(family="HiraginoSans-W4")} # 日本語表示  
plot(TC.data$陽性者数, type="l", col="red", ylab="陽性者数") 
## 日付ラベルを用いた作図の例
with(TC.data,
     plot(年月日,陽性者数,
          type="l", col="red", ylab="陽性者数"))
## 日付は月日から文字列操作で作ることもできる
## days <- with(TC.data,as.Date(paste("2021",月,日,sep="-"))) # 2021-月-日

## x軸のラベルのフォーマットを指定する例
plot(陽性者数 ~ 年月日, data=TC.data, xaxt="n",
     type="l", col="red", ylab="陽性者数")
axis.Date(1, TC.data$年月日, format="%m/%d", labels=TRUE) #x軸ラベルを書く

## 検査実施人数の推移 (棒グラフ)
barplot(TC.data$総検査実施件数, col="lightblue", ylab="検査実施件数") # 棒グラフ
plot(TC.data$総検査実施件数, type="h", # 棒が多い場合はこういう方法もある
     col="blue", ylab="検査実施件数") 
## 日付ラベルの付加
with(TC.data,
     plot(年月日, 総検査実施件数, type="h", col="blue", ylab="検査実施人数")) 
grid(col="darkgray") # 格子線の追加

## 曜日ごとの検査実施件数 (箱ひげ図)
boxplot(総検査実施件数 ~ 曜日, data=TC.data, col=cm.colors(7))
## 曜日の並び順を修正
TC.data <- transform(TC.data,
                     曜日=factor(曜日,
                                 levels=c("日曜日","月曜日","火曜日","水曜日","木曜日","金曜日","土曜日"), # 順序を指定
                                 labels=c("日","月","火","水","木","金","土"))) # 名称を変更
boxplot(総検査実施件数 ~ 曜日, data=TC.data, col=cm.colors(7))

### 多次元データの視覚化で用いた例

if(Sys.info()["sysname"]=="Darwin"){par(family="HiraginoSans-W4")} # 日本語表示  
## 気温, 日射, 風速に関する散布図を作成する
pairs(~ temp + solar + wind, data=TW.data,
      labels=c("気温","日射","風速"), # 指定しなければ列名が使われる
      col=rainbow(12)[TW.data$month]) # 月毎に異なる色で表示

f <- function(x,y) x^2 - y^2
x <- seq(-3, 3, length=51) # x座標の定義域の分割
y <- seq(-3, 3, length=51) # y座標の定義域の分割
z <- outer(x, y, f) # z座標の計算
## 基本的な俯瞰図
## persp(x, y, z, col="lightblue")
## 俯瞰する向きを指定
persp(x, y, z, theta=30, phi=30, expand=0.5, # 俯瞰する視線の設定
      col="royalblue", main=expression(z==x^2-y^2))

## install.packages("scatterplot3d") # 初めて使う時に必要
library(scatterplot3d) # パッケージのロード
if(Sys.info()["sysname"]=="Darwin"){par(family="HiraginoSans-W4")} # 日本語表示  
## 風速, 日射, 気温の3次元散布図を作成する
scatterplot3d(subset(TW.data, select=c(wind, solar, temp)),
              xlab="風速",ylab="日射",zlab="気温", # 指定しなければ列名が使われる
              pch=4, color="orchid")

### 凡例の追加で用いた例

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

if(Sys.info()["sysname"]=="Darwin"){par(family="HiraginoSans-W4")} # 日本語表示  
## 東京の気象データから月ごとの気温,降水量,日射量の平均を計算し描画する
(foo <- aggregate(. ~ month, FUN=mean,
                  data=subset(TW.data, select=c(month,temp,rain,solar))))
plot(foo$temp, type ="b", lwd=3, col="green", ylim=c(0, max(foo$temp)+1),
     xlab="月", ylab="", main="東京の気候データ", axes=FALSE) # 軸は無
axis(1, 1:12, 1:12); axis(2) # x(1),y(2)軸の作成
lines(foo$rain, type="h", lwd=3, col="blue") # 棒グラフ
lines(foo$solar, type="s", lwd=3, lty=2, col="red") # 階段グラフ
abline(0, 0, lwd=2, lty="dotted") #  y=0の線を引く
legend("topleft", inset=0.02, # 左上で全体の2%(0.02)内側に良せる
       legend=c("気温","降水量","日射量"),
       col=c("green","blue","red"), lwd=3, lty=c(1,1,2))

### 練習問題
## 3次元の散布図 (jpdat1/3.csvを用いた例)
if(Sys.info()["sysname"]=="Darwin"){par(family="HiraginoSans-W4")} # 日本語表示  
scatterplot3d(subset(JP.data, select=c(婚姻,離婚,失業)), 
              pch=19, color="blue")
pairs(subset(JP.data, select=c(婚姻,離婚,失業)), col="blue") # 三面図で見てみる

### 凡例の追加 (tokyo_covid19_2021.csvを用いた例)
## データの読み込み
if(Sys.info()["sysname"]=="Darwin"){par(family="HiraginoSans-W4")} # 日本語表示  
plot(総検査実施件数/10 ~ 年月日, data=TC.data,
     type="h", col="blue", xlab="日付", ylab="人数")
abline(h=seq(0,2000,by=100), lty=2, col="darkgray") # 補助線の追加
lines(陽性者数 ~ 年月日, data=TC.data, col="red") 
title(main="検査実績の推移") 
legend("topright", inset=0.01, 
       legend=c("検査実施件数/10","陽性者数"),
       col=c("blue","red"), lwd=3, lty=1)

### 凡例の追加 (tokyo_covid19_patients_2021.csvを用いた例)
## データの読み込み
TCP.data <- read.csv(file="data/covid19_tokyo_patients.csv")
## 簡単な集計には関数table()を使うとよい
table(subset(TCP.data, select=c(患者_年代))) # 名前のついたベクトル
barplot(table(subset(TCP.data, select=c(患者_年代))))
## 月別の年齢分布を調べる
foo <-with(TCP.data,
           data.frame(age=患者_年代,
                      month=months(as.Date(公表_年月日))))
(bar <- table(foo)) # (年齢 x 月) の患者数の表(行列)
(baz <- apply(bar, 2, function(z){z/sum(z)})) # 月ごとの年齢分布
## 描画
if(Sys.info()["sysname"]=="Darwin"){par(family="HiraginoSans-W4")} # 日本語表示  
barplot(bar, # 人数のグラフ
        col=rainbow(13), # 13色に色分け
        beside=TRUE, # 棒グラフを横に並べる
        space=c(1.5, 3), # 棒グラフ間・変数間のスペースを指定
        legend.text=rownames(bar), # 凡例の指定, 2列，縮小, 左上に表示
        args.legend=list(ncol=2,cex=0.5,x="topleft",inset=0.01)) 
barplot(baz, # 比率のグラフ
        col=rainbow(13), # 13色に色分け
        beside=TRUE, # 棒グラフを横に並べる
        space=c(1.5, 3), # 棒グラフ間・変数間のスペースを指定
        legend.text=rownames(baz), # 凡例の指定，2列，縮小
        args.legend=list(ncol=2,cex=0.5))
