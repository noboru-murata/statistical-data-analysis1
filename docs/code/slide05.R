if(Sys.info()["sysname"] == "Darwin") { # MacOSか調べて日本語フォントを指定
  theme_update(text = element_text(family = "HiraginoSans-W4"))}
pcr_data |> 
  ggplot(aes(x = niid, y = mi)) + # x軸を niid，y軸を mi に設定
  geom_point(colour = "blue", shape = 19) + # 色と形を指定(点の形は '?points' を参照)
  labs(x = pcr_colnames["niid"], y = pcr_colnames["mi"]) # 軸の名前を指定

pcr_data |> 
  ggplot(aes(x = niid, y = mi)) + 
  geom_point(colour = "blue", shape = 19) + 
  scale_x_log10() + scale_y_log10() + # 各軸を対数で表示
  labs(x = pcr_colnames["niid"], y = pcr_colnames["mi"])

### 練習問題 東京都の感染動向データによる例
## 陽性患者数の推移 (折れ線グラフ)
## データの読み込み
tc_data <- read.csv(file="data/tokyo_covid19_2021.csv",fileEncoding="utf8")
names(tc_data)[1] <- "年月日" # CSVファイルの1列目の名前が空白なので定義しておく
tc_data <- transform(tc_data,年月日=as.Date(年月日)) # 日付の属性を変えておく
## 日本語の扱いでうまくいかない場合は以下で対応して下さい．
## library(readr)
## tc_data <- read_csv(file="data/tokyo_covid19_2021.csv")

## 折れ線グラフ
if(Sys.info()["sysname"]=="Darwin"){par(family="HiraginoSans-W4")} # 日本語表示  
plot(tc_data$陽性者数, type="l", col="red", ylab="陽性者数") 
## 日付ラベルを用いた作図の例
with(tc_data,
     plot(年月日,陽性者数,
          type="l", col="red", ylab="陽性者数"))
## 日付は月日から文字列操作で作ることもできる
## days <- with(tc_data,as.Date(paste("2021",月,日,sep="-"))) # 2021-月-日

## x軸のラベルのフォーマットを指定する例
plot(陽性者数 ~ 年月日, data=tc_data, xaxt="n",
     type="l", col="red", ylab="陽性者数")
axis.Date(1, tc_data$年月日, format="%m/%d", labels=TRUE) #x軸ラベルを書く

## 検査実施人数の推移 (棒グラフ)
barplot(tc_data$総検査実施件数, col="lightblue", ylab="検査実施件数") # 棒グラフ
plot(tc_data$総検査実施件数, type="h", # 棒が多い場合はこういう方法もある
     col="blue", ylab="検査実施件数") 
## 日付ラベルの付加
with(tc_data,
     plot(年月日, 総検査実施件数, type="h", col="blue", ylab="検査実施人数")) 
grid(col="darkgray") # 格子線の追加

## 曜日ごとの検査実施件数 (箱ひげ図)
boxplot(総検査実施件数 ~ 曜日, data=tc_data, col=cm.colors(7))
## 曜日の並び順を修正
tc_data <- transform(tc_data,
                     曜日=factor(曜日,
                                 levels=c("日曜日","月曜日","火曜日","水曜日","木曜日","金曜日","土曜日"), # 順序を指定
                                 labels=c("日","月","火","水","木","金","土"))) # 名称を変更
boxplot(総検査実施件数 ~ 曜日, data=tc_data, col=cm.colors(7))

### 練習問題
## 3次元の散布図 (jpdat1/3.csvを用いた例)
if(Sys.info()["sysname"]=="Darwin"){par(family="HiraginoSans-W4")} # 日本語表示  
scatterplot3d(subset(jp_data, select=c(婚姻,離婚,失業)), 
              pch=19, color="blue")
pairs(subset(jp_data, select=c(婚姻,離婚,失業)), col="blue") # 三面図で見てみる

### 凡例の追加 (tokyo_covid19_2021.csvを用いた例)
## データの読み込み
if(Sys.info()["sysname"]=="Darwin"){par(family="HiraginoSans-W4")} # 日本語表示  
plot(総検査実施件数/10 ~ 年月日, data=tc_data,
     type="h", col="blue", xlab="日付", ylab="人数")
abline(h=seq(0,2000,by=100), lty=2, col="darkgray") # 補助線の追加
lines(陽性者数 ~ 年月日, data=tc_data, col="red") 
title(main="検査実績の推移") 
legend("topright", inset=0.01, 
       legend=c("検査実施件数/10","陽性者数"),
       col=c("blue","red"), lwd=3, lty=1)

### 凡例の追加 (tokyo_covid19_patients_2021.csvを用いた例)
## データの読み込み
tcp_data <- read.csv(file="data/tokyo_covid19_patients_2021.csv")
## 簡単な集計には関数table()を使うとよい
table(subset(tcp_data, select=c(患者_年代))) # 名前のついたベクトル
barplot(table(subset(tcp_data, select=c(患者_年代))))
## 月別の年齢分布を調べる
library(lubridate) # 年月日の文字列を操作するパッケージ
foo <-with(tcp_data,
           data.frame(age=患者_年代,
                      month=month(公表_年月日,label=TRUE,abbr=FALSE)))
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
