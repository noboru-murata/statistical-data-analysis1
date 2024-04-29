### 第5講 サンプルコード
library(tidyverse)

#' 最初に一度だけ以下のいずれかを実行しておく
#'  - Package タブから tidyverse をインストール
#'  - コンソール上で次のコマンドを実行 'install.packages("tidyverse")'
#' tidyverse パッケージの読み込み
library(tidyverse)

#' @exercise 折れ線グラフの描画
#' 東京の5月の気温と日射量の推移

tw_data |> filter(month == 5) |> # 5月を抽出
  ggplot(aes(x = day)) + # day をx軸に指定
  geom_line(aes(y = temp), colour = "blue") + # 気温を青
  geom_line(aes(y = solar), colour = "red") + # 日射量を赤
  labs(y = "temp.(blue) / solar rad.(red)") # y軸のラベルを変更

#' 前例の別の書き方

tw_data |> filter(month == 5) |> select(c(day, temp, solar)) |>
  pivot_longer(!day, names_to = "index") |> 
  ggplot(aes(x = day, y = value, colour = index)) +
  geom_line() + # index ごとに異なる色で線が引かれる
  labs(title = "Weather in May")

#' @notes
#' 色はカラーパレットに従って自動的に選択される

tw_data |> filter(month == 5) |> select(c(day, temp, solar)) |>
  pivot_longer(!day, names_to = "index") |> 
  ggplot(aes(x = day, y = value, colour = index)) +
  geom_line(show.legend  =  FALSE) + # 凡例は不要なので消す
  labs(title = "Weather in May") +
  facet_grid(rows = vars(index)) # index ごとに行に並べる．(rowsは省略できる)

#' @notes
#' 属性ごとに描いた異なるグラフを並べる場合には
#' 関数 ggplot2::facet_grid() (属性ごとに行・列を構成)や
#' 関数 ggplot2::facet_wrap() (行数・列数を指定)を用いる
foo <-
  tw_data |> filter(month %in% c(5,6,7,8)) |>
  select(c(month, day, temp, solar, wind)) |>
  pivot_longer(!c(month, day), names_to = "index") |> 
  ggplot(aes(x = day, y = value, colour = index)) +
  geom_line(show.legend  =  FALSE) 
foo + facet_grid(rows = vars(index), cols = vars(month)) 
foo + facet_grid(index ~ month) # 同上
foo + facet_wrap(vars(index, month), nrow = 4, ncol = 3) # 4x3 に並べる
foo + facet_wrap(index ~ month, nrow = 4, ncol = 3) # 同上

#' @exercise 散布図の描画
#' 夏季の日射量と気温の関係

tw_data |> filter(month %in% 7:9) |> # 7月-9月を抽出
  ggplot(aes(x = solar, y = temp)) + # x軸を日射量，y軸を気温に設定
  geom_point(colour = "blue", shape = 19) + # 色と形を指定(点の形は '?points' を参照)
  labs(x = "solar radiation", y = "temperature") # 軸の名前を指定

#' 湿度の情報を点のサイズとして追加

tw_data |> filter(month %in% 7:9) |> 
  ggplot(aes(x = solar, y = temp, size = humid)) + # 湿度を点の大きさで表示
  geom_point(colour = "blue", shape = 19) + 
  labs(x = "solar radiation", y = "temperature")

#' 各軸を対数表示に変更

tw_data |> filter(month %in% 7:9) |> 
  ggplot(aes(x = solar, y = temp, size = humid)) + 
  geom_point(colour = "blue", shape = 19) + 
  labs(x = "solar radiation", y = "temperature") +
  scale_x_log10() + scale_y_log10() # x軸，y軸を対数表示

#' @exercise 散布図行列の描画
#' 夏季の気温と日射量と湿度の関係
library(GGally)

tw_data |> filter(month %in% 7:9) |> 
  select(c(temp, solar, humid)) |> # 必要な列を選択
  ggpairs() # 標準の散布図行列 (上三角は相関，対角は密度，下三角は散布図)

#' 月ごとに色分けして表示する

tw_data |> filter(month %in% 7:9) |> select(c(month, temp, solar, humid)) |>
  mutate(month = as_factor(month)) |> # 月を因子化(ラベルとして扱う)
  ggpairs(columns = 2:4, legend = c(1,1), # 表示する列．凡例の雛型
          aes(colour = month), # 月ごとに色づける
          diag = list(continuous = "barDiag")) + # 対角をヒストグラム
  theme(legend.position = "top") # 凡例(上で指定した1行1列の凡例)の位置

#' @notes
#' 事例ベースの使い方は以下のコマンドで見ることができる
vig_ggally("ggpairs")

if(Sys.info()["sysname"] == "Darwin") { # macOS か調べる
  #' OS標準のヒラギノフォントを指定する場合
  theme_update(text = element_text(family = "HiraginoSans-W4"))
  #' gome_text/geom_label内で用いられる日本語フォントの指定
  update_geom_defaults("text", list(family = theme_get()$text$family))
  update_geom_defaults("label", list(family = theme_get()$text$family))}

#' ---------------------------------------------------------------------------
#' @practice 基本的なグラフの描画
#' 
#' データの読み込み
jp_data <- read_csv(file = "data/jpdata1.csv")
jp_area <- read_csv(file = "data/jpdata3.csv")
#'
#' @notes
#' 日本語を用いる場合は macOS ではフォントの指定が必要
if(Sys.info()["sysname"] == "Darwin") { # macOS か調べて日本語フォントを指定
  theme_update(text = element_text(family = "HiraginoSans-W4"))
  update_geom_defaults("text", list(family = theme_get()$text$family))
  update_geom_defaults("label", list(family = theme_get()$text$family))}
#'
#' 単純な散布図
jp_data |> # データフレームを指定
  ggplot(aes(x = 婚姻, y = 離婚)) + # xy軸を設定
  geom_point(colour = "blue", # 表示する色
             shape = 19)      # 表示する形．'?graphics::points' を参照
#' 軸やタイトルを変更
jp_data |> 
  ggplot(aes(x = 婚姻, y = 離婚)) + 
  geom_point(colour = "blue", 
             shape = 19) +    
  labs(x = "1000人あたりの婚姻数",      # x軸のラベル
       y = "1000人あたりの離婚数",      # y軸のラベル
       title = "婚姻数と離婚数の散布図") # タイトル
#' 県名を追加
jp_data |> 
  ggplot(aes(x = 婚姻, y = 離婚)) + 
  geom_point(colour = "blue",
             shape = 19) + 
  geom_text(aes(label = 県名), # ラベルとして県名を指定
            size = 3,         # サイズは適宜調整
            vjust = -1) +     # ラベルに位置(縦)の調整  
  labs(x = "1000人あたりの婚姻数",
       y = "1000人あたりの離婚数",
       title = "婚姻数と離婚数の散布図")
#' 地方ごとに色と点の形を変える
jp_data |> 
  mutate(地方 = as_factor(jp_area[["地方"]])) |> # 地方区分を追加
  ggplot(aes(x = 婚姻, y = 離婚)) + 
  geom_point(aes(colour = 地方,   # 地方ごとに色を変える
                 shape = 地方)) + # 地方ごとに形を変える
  geom_text(aes(label = 県名), 
            size = 2,
            vjust = -1) +
  labs(x = "1000人あたりの婚姻数",
       y = "1000人あたりの離婚数",
       title = "婚姻数と離婚数の散布図")
#'
#' @notes
#' 図中に入れる文字を自動的に調整するパッケージもある
#' 重なりが多いところはラベルを削除するので注意は必要
library(ggrepel)
if(Sys.info()["sysname"] == "Darwin") { # maxOS のための日本語フォントの設定
  update_geom_defaults("text_repel", list(family = theme_get()$text$family))
  update_geom_defaults("label_repel", list(family = theme_get()$text$family))}
jp_data |> 
  mutate(地方 = as_factor(jp_area[["地方"]])) |>
  ggplot(aes(x = 婚姻, y = 離婚)) + 
  geom_point(aes(colour = 地方,
                 shape = 地方)) +
  geom_text_repel(aes(label = 県名), # geom_text の拡張
                  size = 2) +       # サイズは適宜調整 
  labs(x = "1000人あたりの婚姻数",
       y = "1000人あたりの離婚数",
       title = "婚姻数と離婚数の散布図")
#' ---------------------------------------------------------------------------

#' @exercise ヒストグラムの描画

#' 行政検査(ai)での検査件数の分布
pcr_data |>
  ggplot(aes(x = ai)) + # 分布を描画する列を指定
  geom_histogram(bins = 30, fill = "lightblue", colour = "blue") +
  labs(x = pcr_colnames["ai"], y = "頻度", title = "検査件数のヒストグラム")

#' @notes
#' 各ビンの頻度を表示するためには例えば以下のようにすればよい
pcr_data |>
  ggplot(aes(x = ai)) + # 分布を描画する列を指定
  geom_histogram(bins = 30, fill = "lightblue", colour = "blue") +
  geom_text(stat="bin", bins = 30, colour = "darkblue", size = 3, 
            aes(label = after_stat(count), y = after_stat(count) + 2)) +
  labs(x = pcr_colnames["ai"], y = "頻度", title = "検査件数のヒストグラム")

#' @exercise 箱ひげ図の描画

#' 大学等(univ)での検査件数の分布(2021年分)
pcr_data |>
  filter(year(date) == 2021) |> # 2021年を抽出
  mutate(date = as_factor(month(date))) |> # 月を因子化する
  ggplot(aes(x = date, y = univ)) + # 月毎に集計する
  geom_boxplot(fill = "orange") + # 塗り潰しの色を指定
  labs(title = "月ごとの検査件数 (2021年)", x = "月", y = pcr_colnames["univ"])

#' @exercise 棒グラフの描画

#' 機関ごとの月の検査件数の推移 (2021年分)
pcr_data |>
  filter(year(date) == 2021) |>
  mutate(month = as_factor(month(date))) |> # 月を作成
  select(!c(date,sub,total)) |> # 機関に限定
  group_by(month) |> # 月でグループ化
  summarize(across(everything(), sum)) |> # 全て(月以外)を集計
  pivot_longer(!month, names_to = "organ", values_to = "nums",
               names_transform = list(organ = as_factor)) |>
  ## 最後のオプションは organ 列のラベルを出てきた順で因子化して元の列の並びにしている
  ggplot(aes(x = organ, y = nums, fill = month)) +
  geom_bar(stat = "identity", position = "dodge", na.rm = TRUE) +
  theme(legend.position = "top") + guides(fill = guide_legend(nrow = 1))

## CSVファイルは作業ディレクトリの下の data サブディレクトリにあるとする
tc_data <- read.csv(file="data/tokyo_covid19_2021.csv",fileEncoding="utf8")

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

### 多次元データの視覚化で用いた例

persp(x, y, z, theta = 0, phi = 15, expand = 1, ...) # ... は関数 plot() と同様に指定可能
## x,y,z: x,y,z 座標
##        z は点(x[i],y[j])に対応する値を(i,j) 成分とする行列で与える必要がある
## theta,phi: 俯瞰の方向を指定する極座標
## expand: z軸の拡大度

f <- function(x,y) x^2 - y^2
x <- seq(-3, 3, length=51) # x座標の定義域の分割
y <- seq(-3, 3, length=51) # y座標の定義域の分割
z <- outer(x, y, f) # z座標の計算
## 基本的な俯瞰図
## persp(x, y, z, col="lightblue")
## 俯瞰する向きを指定
persp(x, y, z, theta=30, phi=30, expand=0.5, # 俯瞰する視線の設定
      col="royalblue", main=expression(z==x^2-y^2))

library("scatterplot3d") # パッケージの読み込み
scatterplot3d(x, color, angle = 40, ...) # ... は関数 plot() とは若干異なる
## x: x,y,z座標を指定するデータフレーム
##    関数 persp() のようにx,y,zを個別に指定することも可能
## color: 色を指定(colではないので注意). 既定値は黒
## angle: x軸とy軸の間の角度

## install.packages("scatterplot3d") # 初めて使う時に必要
library("scatterplot3d") # パッケージのロード
if(Sys.info()["sysname"]=="Darwin"){par(family="HiraginoSans-W4")} # 日本語表示  
## 風速, 日射, 気温の3次元散布図を作成する
scatterplot3d(subset(tw_data, select=c(wind, solar, temp)),
              xlab="風速",ylab="日射",zlab="気温", # 指定しなければ列名が使われる
              pch=4, color="orchid")

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
