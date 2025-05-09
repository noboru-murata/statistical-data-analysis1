### 第4講 資料の例題

#' @exercise データフレームの操作

#' 最初に一度だけ以下のいずれかを実行しておく
#'  - Package タブから tidyverse をインストール
#'  - コンソール上で次のコマンドを実行 'install.packages("tidyverse")'
#' tidyverse パッケージの読み込み
library(tidyverse)

#' @exercise tibble 形式への変換

aq_tbl <- as_tibble(airquality) # tibble 形式へ変換
aq_df <- airquality             # 比較のため data.frame を別名で定義 (コピー)
aq_tbl                          # 最初の10行のみ表示される
aq_df                           # 全てのデータが表示される
print(aq_tbl, n = 15)           # 最初の15行が表示される．'?tibble::print.tbl' を参照
print(aq_tbl, n = Inf)          # 全て表示
head(aq_df, n = 10)             # 最初の10行が表示される．tibbleでも同様
tail(aq_df, n = 10)             # 最後の10行が表示される．tibbleでも同様

aq_tbl[1,2]                                 # 1行2列の要素を選択
aq_tbl[-seq(2, nrow(aq_tbl), by = 2),]      # 偶数行を除外
aq_tbl[,c(TRUE,FALSE,TRUE,TRUE,FALSE,TRUE)] # 1,3,4,6列を選択
aq_tbl["Ozone"]                             # Ozone列を選択．aq_tbl[,"Ozone"] でも同様
aq_tbl[c("Ozone","Wind")]                   # 複数列の選択も同様
aq_tbl[["Ozone"]]                           # リストとして扱い1列を選択するとベクトルになる
aq_tbl$Ozone                                # 同上

#' 行番号のベクトルで指定して抽出
aq_tbl[20:30,] # 20-30行を抽出

#' 条件の指定の仕方
aq_tbl[1:15,]$Ozone > 100 # 条件に合致する行はTRUE (NAは欠損値)
aq_tbl[1:15,]$Ozone > 100 & aq_tbl[1:15,]$Wind <= 5 # 条件のAND
with(aq_tbl[1:15,], Ozone > 100 & Wind <= 5)        # 上と同じ(短い書き方)
with(aq_tbl[1:60,], Ozone > 100 | Wind <= 5)        # 条件のOR

#' 関数 which() でTRUEとなる行番号を抽出して指定
which(with(aq_tbl, Ozone>100 & Wind<=5)) # FALSEおよびNAは除去される
aq_tbl[which(with(aq_tbl, Ozone>100 & Wind<=5)), ]

#' 列番号のベクトルで指定して抽出
aq_tbl[which(with(aq_tbl, Ozone>100 & Wind <= 5)), c(1,5,6)]

#' 複数の列の場合
aq_tbl[which(with(aq_tbl, Ozone > 100 & Wind <= 5)), c("Month","Day")]

#' 1つの列の場合はリストの操作として取り出せるが，ベクトルとなることに注意
aq_tbl[which(with(aq_tbl, Ozone > 100 & Wind <= 5)),]$Month      # ベクトル
aq_tbl[which(with(aq_tbl, Ozone > 100 & Wind <= 5)),][["Month"]] # 同上
aq_tbl[which(with(aq_tbl, Ozone > 100 & Wind <= 5)),"Month"]     # データフレーム

#' 素朴な方法の例は以下のように書ける
aq_tbl |> filter(Ozone > 100 & Wind <= 5) |> select(1,5,6)
aq_tbl |> filter(Ozone > 100 & Wind <= 5) |> select(Month,Day)
#' Ozoneに欠測(NA)がなく, かつDayが5か10のデータのWindからDayまでの列を抽出
aq_tbl |> filter(!is.na(Ozone) & Day %in% c(5,10)) |> select(Wind:Day)
#' Ozoneが120以上か，またはWindが3以下のTemp以外の列を抽出
aq_tbl |> filter(Ozone > 120 | Wind <= 3) |> select(!Temp)

#' @exercise ファイルの取り扱い

#' 関数 write_csv() の使い方 (CSVファイルの操作)
my_data <-              # データフレームの整理
  aq_tbl |> 
  filter(Ozone > 80) |> # Ozone が80を越える日を抽出
  select(!Temp)         # 温度は除く
dim(my_data) # データフレームの大きさを確認
#' 作業ディレクトリの中に data というフォルダを用意しておく
write_csv(my_data,      # 保存するデータフレーム
          file = "data/my_data.csv") # (場所と)ファイル名

#' 関数 read_csv() の使い方 (CSVファイルの操作)
new_data <- read_csv(file = "data/my_data.csv") # 前の例のファイル
dim(new_data) # 正しく読み込めたか大きさを確認

#' 関数 save() の使い方 (RDataファイルの操作)
my_data_1 <- aq_tbl |> filter(Temp > 90) |> select(!Ozone)
my_data_2 <- aq_tbl |> filter(Temp < 60) |> select(!Ozone)
dim(my_data_1); dim(my_data_2)    # 大きさを確認
save(my_data_1, my_data_2,        # 保存するオブジェクトを列挙
     file = "data/my_data.rdata") # ファイル名

#' 関数 load() の使い方 (RDataファイルの操作)
my_data_1 <- aq_tbl |> filter(Ozone > 160) # 新たに作成
load(file = "data/my_data.rdata")          # ファイル名
my_data_1 # saveしたときの名前で読み込まれ上書きされる
my_data_2

#' @exercise データの集計

#' 練習問題のデータを用いる
sum(jp_data$人口)                  # 全国の総人口 (列名でベクトルを選択)
sum(jp_data[,"人口"])              # 1列のデータフレームとして計算
jp_data |> select(人口) |> sum()   # 同上
mean(jp_data[,5])                  # 1列のデータフレームではエラーになる
mean(jp_data[[5]])                 # ベクトルとして抜き出す必要がある
median(jp_data[[5]])               # 面積の中央値 (リストとして列を選択)
jp_data |> select(5) |> median()   # データフレームなので動かない
min(jp_data["若年"])               # 若年人口の最小値 (列名で選択)
jp_data |> select("若年") |> min() # 同上
with(jp_data, max(老人))           # 老年人口の最大値 (関数 with() を利用)
jp_data |> select("老人") |> max() # 同上

#' @exercise 列ごとの集計

#' 練習問題のデータを用いた例
jp_data |> summarise(平均失業率 = mean(失業), 件数 = n()) # 失業の列の平均
jp_data |> summarise(across(婚姻:失業, median))           # 婚姻から失業の列の中央値
jp_data |> summarise(across(!県名, max))                  # 県名の列以外の最大値
jp_data |> summarise(across(where(is.double), min))       # 数値列の最小値

#' @notes
#' 関数 dplyr::across() の列の指定には上記以外にも様々な関数が使える
#' 詳細は '?dplyr::across' を参照

#' 練習問題のデータを用いた例
#' 地方ごとに人口から面積の列の合計を計算する
jp_data |>
  mutate(地方 = as_factor(jp_area[["地方"]])) |> # 地方の情報を付加
  group_by(地方) |>                              # 地方ごとにグループ化
  summarize((across(人口:面積, sum)))            # グループごとに集計

#' @notes
#' 関数 dplyr::mutate() で新たな列を加えることができる
#' 関数 forcats::as_factor() で文字列を出現順に順序付因子にすると
#' 集計結果は因子の順番で表示される
#' 関数 dplyr::group_by() は複数の条件で条件付けることもできる
jp_data |>
  mutate(地方 = as_factor(jp_area[["地方"]]),    # 地方の情報を付加
         人口密度 = 人口/面積,                   # 人口密度を計算
         過密 = 人口密度 >= median(人口密度)) |> # 人口密度が中央値以上
  group_by(地方, 過密) |>                      # 地方ごとにグループ化
  summarize((across(人口:面積, sum)))          # グループごとに集計
