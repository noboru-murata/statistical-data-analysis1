### 第04回 練習問題解答例

### データフレームの操作で用いた例

## 行番号のベクトルで指定して抽出
airquality[1:10,] # 1-10行を抽出

## 条件の指定の仕方
airquality[1:15,]$Ozone>100 # 条件に合致する行はTRUE (NAは欠損値)
airquality[1:15,]$Ozone>100 & airquality[1:15,]$Wind<=5 # 条件のAND
with(airquality[1:15,], Ozone>100 & Wind<=5) # 上と同じ(短い書き方)
with(airquality[1:60,], Ozone>100 | Wind<=5) # 条件のOR

## 関数 which() でTRUEの番号を抽出
which(with(airquality, Ozone>100 & Wind<=5)) # 全データからTRUEを抽出

## 条件を指定して行を抽出
airquality[which(with(airquality, Ozone>100 & Wind<=5)), ]

## 列番号のベクトルで指定して抽出
airquality[which(with(airquality, Ozone>100 & Wind<=5)), c(1,5,6)]

## 複数の列の場合
airquality[which(with(airquality, Ozone>100 & Wind<=5)), 
           c("Month","Day")]

## 1つの列の場合は以下でも良い (ただしデータフレームではなくベクトルになる)
airquality[which(with(airquality, Ozone>100 & Wind<=5)),]$Month
airquality[which(with(airquality, Ozone>100 & Wind<=5)),"Month"] # 上と同じ
## データフレームとして抽出したい場合は drop=FALSE を指定する
airquality[which(with(airquality, Ozone>100 & Wind<=5)),"Month",drop=FALSE]

### 関数 subset() の使い方
subset(airquality,
       subset = Ozone>100 & Wind<=5,
       select = c(1,5,6))
subset(airquality,
       Ozone>100 & Wind<=5, # 順序通りなら引数の名前は省略可
       c(Month,Day)) # 名前は$の後と同じ扱いで "" は不要

## Ozoneに欠測(NA)がなく, かつDayが5か10のデータのWindからDayまでの列を抽出
subset(airquality, 
       subset = !is.na(Ozone) & Day %in% c(5,10),
       select = Wind:Day)

## Ozoneが120以上か，またはWindが3以下のTemp以外の列を抽出
subset(airquality,
       subset = Ozone>120 | Wind <= 3,
       select = -Temp)

### 練習問題 関数 subset() によるデータフレームの操作
## 7月のオゾン濃度
subset(airquality,
       subset = Month == 7,
       select = Ozone)

## 風速時速10マイル以上かつ気温が華氏80度以上
subset(airquality,
       subset = Wind >= 10 & Temp >=80)

## 日射量が欠測でないデータの月と日
subset(airquality,
       subset = !is.na(Ozone) & !is.na(Solar.R),
       select = c(Month,Day)) # 書いた順に並ぶ
subset(airquality, # 書き方はいろいろある
       subset = !(is.na(Ozone) | is.na(Solar.R)),
       select = Month:Day) # もともと並んでいるので c(Month,Day) と同じ

### ファイルの取り扱いで用いた例

## 関数 write.csv() の使い方 (CSVファイルの操作)
(myData <- subset(airquality, 
                  subset = Ozone>120,
                  select = -Temp)) # データフレームの作成
dim(myData) # データフレームの大きさを確認
## 作業ディレクトリの中に data というフォルダを用意しておく
write.csv(myData,file="data/mydata.csv") # csvファイルとして書き出し

## 関数 read.csv() の使い方 (CSVファイルの操作)
(newdata <- read.csv(file="data/mydata.csv", # 前の例のファイル
                     row.names=1)) # 1列目を行名に指定
dim(newdata) # 正しく読み込めたか大きさを確認

### 関数 save() の使い方 (RDataファイルの操作)
(myDat1 <- subset(airquality, Temp>95, select=-Ozone)) 
(myDat2 <- subset(airquality, Temp<57, select=-Ozone)) 
dim(myDat1); dim(myDat2) # 大きさを確認
save(myDat1,myDat2,file="data/mydata.rdata") # RData形式で書き出し

## 関数 load() の使い方 (RDataファイルの操作)
(myDat1 <- subset(airquality, Ozone > 160)) # 新たに作成
load(file="data/mydata.rdata") # RData形式の読み込み
myDat1 # saveしたときの名前で読み込まれ上書きされる
myDat2

### 練習問題 サンプルデータの読み込み
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
## どの都道府県の人口多いか調べる
with(myData,which.max(人口)) # 行番号が返る
myData[with(myData,which.max(人口)),] 
## 人口の多い順に都道府県を並べる
with(myData,order(人口,decreasing=TRUE)) # 行番号を取得
rownames(myData)[with(myData,order(人口,decreasing=TRUE))] # 県名を表示

## 上記と同じ内容を英語版で実行する場合
myDataEn <- read.csv(file="data/jpdata1-en.csv", row.names=1)
myAreaEn <- read.csv(file="data/jpdata3-en.csv")
## データを確認する
head(myDataEn) 
tail(myDataEn)
myAreaEn
## 人口の最大値を見る
with(myDataEn,max(population))
## どの都道府県の人口多いか調べる
myDataEn[with(myDataEn,which.max(population)),] 
## 人口の多い順に都道府県を並べる
rownames(myDataEn)[with(myDataEn,order(population,decreasing=TRUE))]

### データの集計で用いた例

myData <- read.csv(file="data/jpdata1.csv",
                   row.names=1, fileEncoding="utf8")
## 一度読み込んでいれば上の行は不要
sum(myData$人口) # 全国の総人口 (列名で選択)
mean(myData[,4]) # 面積の平均値 (行列として列を選択)
median(myData[[4]]) # 面積の中央値 (リストとして列を選択)
min(myData["若年"])  # 若年人口の最小値 (列名で選択)
with(myData,max(老人))  # 老年人口の最大値 (関数 with() を利用)

### 日本語に不具合がある場合
myDataEn <- read.csv(file="data/jpdata1-en.csv", row.names=1)
## 一度読み込んでいれば上の行は不要
sum(myDataEn$population) # 全国の総人口 (列名で選択)
mean(myDataEn[,4]) # 面積の平均値 (行列として列を選択)
median(myDataEn[[4]]) # 面積の中央値 (リストとして列を選択)
min(myDataEn["young_population"])  # 若年人口の最小値 (列名で選択)
with(myDataEn,max(old_population))  # 老年人口の最大値 (関数 with() を利用)

### 関数applyの使い方
x <- subset(myData, select=婚姻:失業) # 抽出
colMeans(x) # 各列の平均
apply(x, 2, max) # 列ごとの最大値
sapply(x, max)   # 上と同じ (help(sapply)を参照)
## 自作関数の適用 (関数に名前を付けずに利用できる)
apply(x, 2, function(z){sum(z>mean(z))}) # 平均より大きいデータ数

### 日本語に不具合がある場合
x <- subset(myDataEn, select=marriage:unemployed) # 抽出
colMeans(x) # 各列の平均
apply(x, 2, max) # 列ごとの最大値
sapply(x, max)   # 上と同じ (help(sapply)を参照)
apply(x, 2, function(z){sum(z>mean(z))}) # 平均より大きいデータ数

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

### 日本語に不具合がある場合
## 人口から面積まで地方ごとの平均値を計算
x <- subset(myDataEn,select=population:area)
aggregate(x, by=list(region=myAreaEn$region), FUN=mean) 

aggregate(subset(myDataEn,select=population:area),
          by=list(region=myAreaEn$region),
          FUN=mean) 

y <- data.frame(x,region=myAreaEn$region) 
aggregate( . ~ region, data=y, FUN=mean)

aggregate( . ~ region, # 右辺で条件付けて左辺(右辺以外)を計算
          data=data.frame(subset(myDataEn,select=population:area),
                          region=myAreaEn$region), 
          FUN=mean)

## 地方と，人口が中央値以下か否かでグループ分けして平均値を計算
aggregate(x, by=list(region=myAreaEn$region,
                     depop=with(myDataEn, population<=median(population))),
          FUN=mean)

aggregate( . ~ region + depop, FUN=mean, 
          data=data.frame(subset(myDataEn,select=population:area),
                          region=myAreaEn$region,
                          depop=with(myDataEn, population<=median(population))))

### 練習問題 データの集計
### 県別の人口密度 (人口/面積)
with(myData,人口/面積) # 値のみ返す
## 人口密度に関係するデータをまとめてデータフレームを作成
(myDat1 <- data.frame(subset(myData,select=c(人口,面積)),
		      人口密度=with(myData,人口/面積)))

### 地方別の人口密度 (地方の総人口/地方の総面積)
## 地方ごとに人口と面積の合計を計算
(myDat2 <- aggregate(subset(myData,select=c(人口,面積)),
		     by=list(地方=myArea$地方), FUN=sum))
## 人口密度を計算して追加
(myDat2 <- data.frame(myDat2, 
		      人口密度=with(myDat2,人口/面積)))

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

### 日本語に不具合がある場合

### 県別の人口密度 (人口/面積)
with(myDataEn,population/area) # 値のみ返す
## 人口密度に関係するデータをまとめてデータフレームを作成
(myDat1En <- data.frame(subset(myDataEn,select=c(population,area)),
			density=with(myDataEn,population/area)))

### 地方別の人口密度 (地方の総人口/地方の総面積)
## 地方ごとに人口と面積の合計を計算
(myDat2En <- aggregate(subset(myDataEn,select=c(population,area)),
		       by=list(region=myAreaEn$region), FUN=sum))
## 人口密度を計算して追加
(myDat2En <- data.frame(myDat2En, 
			density=with(myDat2En,population/area)))

### 地方別の婚姻率・離婚率
## 婚姻可能な人口を推計
(tmp <- data.frame(subset(myDataEn, select=marriage:divorce),
		   marriageable=with(myDataEn,population-young_population)))
## 婚姻率と離婚率から人数を推計
(tmp <- data.frame(tmp,
		   nmarriage=with(tmp, marriageable*marriage/1000),
		   ndivorce=with(tmp, marriageable*divorce/1000),
		   region=myAreaEn$region))
## 地方別の婚姻・離婚数を集計
(myDat3En <- aggregate(. ~ region, FUN=sum,
		       data=subset(tmp,select=-c(marriage,divorce))))
## 婚姻率・離婚率を計算して追加
(myDat3En <- data.frame(myDat3En,
			ratio_marriage=with(myDat3En,nmarriage/marriageable*1000),
			ratio_divorce=with(myDat3En,ndivorce/marriageable*1000)))
