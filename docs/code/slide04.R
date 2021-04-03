### 例題1
### データフレームの操作

## 抽出する行番号のベクトルで指定
airquality[1:10,] # 1-10行を抽出

## 条件に合致する行はTRUE (NAは欠損値)
airquality[1:16,]$Ozone>100 # 条件の指定
airquality[1:16,]$Ozone>100 & airquality[1:16,]$Wind<=5 # 条件のAND
with(airquality[1:16,], Ozone>100 & Wind<=5) # 上と同じ(短い書き方)
with(airquality[1:24,], Ozone>100 | Wind<=5) # 条件のOR

## 関数whichでTRUEの番号を抽出
which(with(airquality, Ozone>100 & Wind<=5)) # 全データから抽出

## 行の抽出
airquality[which(with(airquality, Ozone>100 & Wind<=5)), ]

## 列番号のベクトルで指定
airquality[which(with(airquality, Ozone>100 & Wind<=5)), c(1,5,6)]

## 複数の列の場合
airquality[which(with(airquality, Ozone>100 & Wind<=5)), 
           c("Month","Day")]

## 1つの列の場合は以下でも良い (ただしベクトルになる)
airquality[which(with(airquality, Ozone>100 & Wind<=5)),]$Month

### 例題2
### 関数subsetの使い方

subset(airquality,
       subset = Ozone>100 & Wind<=5,
       select = c(1,5,6))
subset(airquality,
       Ozone>100 & Wind<=5, # 順序通りなら引数の名前は省略可
       c(Month,Day)) # 名前は$の後と同じ扱い

## Ozoneに欠測(NA)がなく, かつDayが5か10のWindからDayまでの列を抽出
subset(airquality, 
       subset = !is.na(Ozone) & Day %in% c(5,10),
       select = Wind:Day)

## Ozoneが120以上か，またはWindが3以下のTemp以外の列を抽出
subset(airquality,
       subset = Ozone>120 | Wind <= 3,
       select = -Temp)

### 練習1.1
### 7月のオゾン濃度
subset(airquality,
       subset = Month == 7,
       select = Ozone)

### 練習1.2
### 風速時速10マイル以上かつ気温が華氏80度以上
subset(airquality,
       subset = Wind >= 10 & Temp >=80)

### 練習1.3
### 日射量が欠測でないデータの月と日
subset(airquality,
       subset = !is.na(Ozone) & !is.na(Solar.R),
       select = c(Month,Day)) # 
subset(airquality, # 書き方はいろいろある
       subset = !(is.na(Ozone) | is.na(Solar.R)),
       select = Month:Day) # c(Month,Day)と同じ

### 例題3
### CSVファイルの操作

## 関数write.csvの使い方
(myData <- subset(airquality, 
                  subset = Ozone>120,
                  select = -Temp)) # データフレームの作成
dim(myData) # データフレームの大きさを確認
write.csv(myData,file="data/mydata.csv") # csvファイルとして書き出し

## 関数read.csvの使い方
(newdata <- read.csv(file="data/mydata.csv",
                     row.names=1)) # 1列目を行名に指定
dim(newdata) # 正しく読み込めたか大きさを確認

### 例題4
### RDataファイルの操作

## 関数saveの使い方
(myDat1 <- subset(airquality, Temp>95, select=-Ozone)) 
(myDat2 <- subset(airquality, Temp<57, select=-Ozone)) 
dim(myDat1); dim(myDat2) # 大きさを確認
save(myDat1,myDat2,file="data/mydata.rdata") # RData形式で書き出し

## 関数loadの使い方
(myDat1 <- subset(airquality, Ozone > 160)) # 新たに作成
load(file="data/mydata.rdata") # RData形式の読み込み
myDat1 # saveしたときの名前で読み込まれ上書きされる
myDat2

### 練習2
### サンプルデータの読み込み

myData <- read.csv(file="data/jpdata1.csv", fileEncoding="utf8", row.names=1)
myItem <- read.csv(file="data/jpdata2.csv", fileEncoding="utf8")
myArea <- read.csv(file="data/jpdata3.csv", fileEncoding="utf8")

### データ操作の例
## 項目名の内容を確認
myItem
## 最初の6県を表示
head(myData)
## 最後の6県を表示
tail(myData)
## 地方名を確認
myArea
## 人口の最大値を見る
with(myData,max(人口))
## どの都道府県か調べる
with(myData,which.max(人口)) # 行番号が返る
myData[with(myData,which.max(人口)),] 
## 人口の多い都道府県を調べる
with(myData,order(人口,decreasing=TRUE)) # 行番号を取得
rownames(myData)[with(myData,order(人口,decreasing=TRUE))] # 県名を表示

### 例題5
### データを集約する関数

myData <- read.csv(file="data/jpdata1.csv",
                   row.names=1, fileEncoding="utf8")
## 一度読み込んでいれば上の行は不要
sum(myData$人口) # 全国の総人口 (列名で選択)
mean(myData[,4]) # 面積の平均値 (行列として列を選択)
median(myData[[4]]) # 面積の中央値 (リストとして列を選択)
min(myData["若年"])  # 若年人口の最小値 (列名で選択)
with(myData,max(老人))  # 老年人口の最大値 (関数withを利用)

### 例題6
### 関数applyの使い方

x <- subset(myData, select=婚姻:勤女) # 抽出
colMeans(x) # 各列の平均
apply(x, 2, max) # 列ごとの最大値
sapply(x, max)   # 上と同じ (help(sapply)を参照)
## 自作関数の適用 (関数に名前を付けずに利用できる)
apply(x, 2, function(z){sum(z>mean(z))}) # 平均より大きいデータ数

### 例題7
### 関数aggregateの使い方

## 人口から面積まで地方ごとの平均値を計算
x <- subset(myData,select=人口:面積)
aggregate(x, by=list(地方=myArea$地方), FUN=mean)

aggregate(subset(myData,select=人口:面積),
          by=list(地方=myArea$地方),
          FUN=mean)

y <- data.frame(x,地方=myArea$地方) 
aggregate( . ~ 地方, data=y, FUN=mean)

aggregate( . ~ 地方, # 右辺で条件付けて左辺(右辺以外)を計算
          data=data.frame(subset(myData,select=人口:面積),
                          地方=myArea$地方), 
          FUN=mean)

## 地方と，人口が中央値以下か否かでグループ分けして平均値を計算
aggregate(x, by=list(地方=myArea$地方,
                     過疎=with(myData, 人口<=median(人口))),
          FUN=mean)

aggregate( . ~ 地方 + 過疎, FUN=mean, # + で条件を追加
          data=data.frame(subset(myData,select=人口:面積),
                          地方=myArea$地方,
                          過疎=with(myData, 人口<=median(人口))))

### 練習3.1
### 県別の人口密度 (人口/面積)
with(myData,人口/面積) # 値のみ返す
## 人口密度に関係するデータをまとめてデータフレームを作成
(myDat1 <- data.frame(subset(myData,select=c(人口,面積)),
                      人口密度=with(myData,人口/面積)))

### 練習3.2
### 地方別の人口密度 (地方の総人口/地方の総面積)
## 地方ごとに人口と面積の合計を計算
(myDat2 <- aggregate(subset(myData,select=c("人口","面積")),
                     by=list(地方=myArea$地方), FUN=sum))
## 人口密度を計算して追加
(myDat2 <- data.frame(myDat2, 
                      人口密度=with(myDat2,人口/面積)))

### 練習3.3
### 地方別の婚姻率・離婚率
## 婚姻可能な人口を推計
(tmp <- data.frame(subset(myData, select=婚姻:離婚),
                   婚姻可能=with(myData,人口-若年)))
## 婚姻率と離婚率から人数を推計
(tmp <- data.frame(tmp,
                  婚姻数=with(tmp, 婚姻可能*婚姻/1000),
                  離婚数=with(tmp, 婚姻可能*離婚/1000),
                  地方=myArea$地方))
## 地方別の婚姻・離婚数を集計
(myDat3 <- aggregate(. ~ 地方, FUN=sum,
                     data=subset(tmp,select=-c(婚姻,離婚))))
## 婚姻率・離婚率を計算して追加
(myDat3 <- data.frame(myDat3,
                      婚姻率=with(myDat3,婚姻数/婚姻可能*1000),
                      離婚率=with(myDat3,離婚数/婚姻可能*1000)))
