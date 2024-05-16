### 第4講 サンプルコード

#' @exercise データフレームの操作

#' 最初に一度だけ以下のいずれかを実行しておく
#'  - Package タブから tidyverse をインストール
#'  - コンソール上で次のコマンドを実行 'install.packages("tidyverse")'
#' tidyverse パッケージの読み込み
library(tidyverse)

#' @exercise tibble 形式への変換

aq_tbl <- as_tibble(airquality) # tibble 形式へ変換
aq_df <- airquality # 比較のため data.frame を別名で定義 (コピー)
aq_tbl # 最初の10行のみ表示される
aq_df  # 全てのデータが表示される
print(aq_tbl, n = 15)  # 最初の15行が表示される．'?tibble::print.tbl' を参照
print(aq_tbl, n = Inf) # 全て表示
head(aq_df, n = 10)    # 最初の10行が表示される．tibbleでも同様
tail(aq_df, n = 10)    # 最後の10行が表示される．tibbleでも同様

aq_tbl[1,2] # 1行2列の要素を選択
aq_tbl[-seq(2, nrow(aq_tbl), by = 2),] # 偶数行を除外
aq_tbl[,c(TRUE,FALSE,TRUE,TRUE,FALSE,TRUE)] # 1,3,4,6列を選択
aq_tbl["Ozone"] # Ozone列を選択．aq_tbl[,"Ozone"] でも同様
aq_tbl[c("Ozone","Wind")] # 複数列の選択も同様
aq_tbl[["Ozone"]] # リストとして扱い1列を選択するとベクトルになる
aq_tbl$Ozone      # 同上

#' 行番号のベクトルで指定して抽出
aq_tbl[20:30,] # 20-30行を抽出

#' 条件の指定の仕方
aq_tbl[1:15,]$Ozone > 100 # 条件に合致する行はTRUE (NAは欠損値)
aq_tbl[1:15,]$Ozone > 100 & aq_tbl[1:15,]$Wind <= 5 # 条件のAND
with(aq_tbl[1:15,], Ozone > 100 & Wind <= 5) # 上と同じ(短い書き方)
with(aq_tbl[1:60,], Ozone > 100 | Wind <= 5) # 条件のOR

#' 関数 which() でTRUEとなる行番号を抽出して指定
which(with(aq_tbl, Ozone>100 & Wind<=5)) 
aq_tbl[which(with(aq_tbl, Ozone>100 & Wind<=5)), ]

#' 列番号のベクトルで指定して抽出
aq_tbl[which(with(aq_tbl, Ozone>100 & Wind <= 5)), c(1,5,6)]

#' 複数の列の場合
aq_tbl[which(with(aq_tbl, Ozone > 100 & Wind <= 5)), c("Month","Day")]

#' 1つの列の場合はリストの操作として取り出せるが，ベクトルとなることに注意
aq_tbl[which(with(aq_tbl, Ozone > 100 & Wind <= 5)),]$Month  # ベクトル
aq_tbl[which(with(aq_tbl, Ozone > 100 & Wind <= 5)),][["Month"]] # 同上
aq_tbl[which(with(aq_tbl, Ozone > 100 & Wind <= 5)),"Month"] # データフレーム

#' 素朴の方法の例は以下のように書ける
aq_tbl |> filter(Ozone > 100 & Wind <= 5) |> select(1,5,6)
aq_tbl |> filter(Ozone > 100 & Wind <= 5) |> select(Month,Day)
#' Ozoneに欠測(NA)がなく, かつDayが5か10のデータのWindからDayまでの列を抽出
aq_tbl |> filter(!is.na(Ozone) & Day %in% c(5,10)) |> select(Wind:Day)
#' Ozoneが120以上か，またはWindが3以下のTemp以外の列を抽出
aq_tbl |> filter(Ozone > 120 | Wind <= 3) |> select(!Temp)

#' ---------------------------------------------------------------------------
#' @practice データフレームの操作
#'
#' 7月のオゾン濃度
aq_tbl |>
  filter(Month == 7) |> # Month %in% 7 などとしても良い
  select(Ozone)
#'
#' 風速時速10マイル以上かつ気温が華氏80度以上
aq_tbl |>
  filter(Wind >= 10 & Temp >= 80)
#' 
## 日射量が欠測でないデータの月と日
aq_tbl |>
  filter(!is.na(Ozone) & !is.na(Solar.R)) |>
  select(Month,Day) # 書いた順に並ぶ
aq_tbl |>
  filter(!is.na(Ozone) & !is.na(Solar.R)) |>
  select(Month:Day) # もともと並んでいるので c(Month,Day) と同じ
#' ---------------------------------------------------------------------------

#' @exercise ファイルの取り扱い

#' 関数 write_csv() の使い方 (CSVファイルの操作)
my_data <- # データフレームの整理
  aq_tbl |> 
  filter(Ozone > 80) |> # Ozone が80を越える日を抽出
  select(!Temp)          # 温度は除く
dim(my_data) # データフレームの大きさを確認
#' 作業ディレクトリの中に data というフォルダを用意しておく
write_csv(my_data, # 保存するデータフレーム
          file = "data/my_data.csv") # (場所と)ファイル名

#' 関数 read_csv() の使い方 (CSVファイルの操作)
new_data <- read_csv(file = "data/my_data.csv") # 前の例のファイル
dim(new_data) # 正しく読み込めたか大きさを確認

#' 関数 save() の使い方 (RDataファイルの操作)
my_data_1 <- aq_tbl |> filter(Temp > 90) |> select(!Ozone)
my_data_2 <- aq_tbl |> filter(Temp < 60) |> select(!Ozone)
dim(my_data_1); dim(my_data_2) # 大きさを確認
save(my_data_1, my_data_2, # 保存するオブジェクトを列挙
     file = "data/my_data.rdata") # ファイル名

#' 関数 load() の使い方 (RDataファイルの操作)
my_data_1 <- aq_tbl |> filter(Ozone > 160) # 新たに作成
load(file = "data/my_data.rdata") # ファイル名
my_data_1 # saveしたときの名前で読み込まれ上書きされる
my_data_2

#' ---------------------------------------------------------------------------
#' @practice データの読み込みと操作
#' 
jp_data <- read_csv(file = "data/jpdata1.csv")
jp_item <- read_csv(file = "data/jpdata2.csv")
jp_area <- read_csv(file = "data/jpdata3.csv")
#'
#' @notes
#' 文字コードは関数 readr::guess_encoding() により推測される
#' guess_encoding("data/jpdata1.csv")
#'
#' データ操作の例
#' 項目名の内容を確認
jp_item
#' 最初の10県を表示
head(jp_data, n = 10) 
#' 最後の10県を表示
tail(jp_data, n = 10)
#' 地方名を確認
View(jp_area) # 左上ペインに表として表示される
#' 人口の最大値を見る
with(jp_data, max(人口))
#' どの都道府県の人口多いか調べる ('?which.max' を参照)
with(jp_data, which.max(人口)) # 行番号が返る
jp_data[with(jp_data, which.max(人口)),]
#' 行を抽出するには関数 dplyr::filter() を使うのが簡単
jp_data |> filter(人口 == max(人口))
#' 人口の多い順に都道府県を並べる ('?order' を参照)
with(jp_data, order(人口, decreasing = TRUE)) # 行番号を取得
jp_data[with(jp_data, order(人口, decreasing = TRUE)),] # 並べ替え
with(jp_data, 県名[order(人口, decreasing = TRUE)]) # 県名のみを表示
#' 行を並べ替えるには関数 dplyr::arrange() を使うのが簡単 
jp_data |> arrange(desc(人口)) |> print(n = Inf) # ('?dplyr::desc' を参照)
#'
#' 上記と同じ内容を英語版で実行する場合
jp_data_en <- read_csv(file="data/jpdata1-en.csv")
jp_area_en <- read_csv(file="data/jpdata3-en.csv")
#' データを確認する
head(jp_data_en, n = 10) 
tail(jp_data_en, n = 10)
View(jp_area_en)
#' 人口の最大値を見る
with(jp_data_en, max(population))
#' 人口の最大値を与える県の番号を見る
with(jp_data_en, which.max(population))
#' どの都道府県の人口多いか調べる
jp_data_en |> filter(population == max(population))
#' 人口の多い順に都道府県を並べる
jp_data_en |> arrange(desc(population)) |> View()
#' ---------------------------------------------------------------------------

#' @exercise データの集計

#' 練習問題のデータを用いる
sum(jp_data$人口)        # 全国の総人口 (列名でベクトルを選択)
sum(jp_data[,"人口"])     # 1列のデータフレームとして計算
jp_data |> select(人口) |> sum() # 同上
mean(jp_data[,5])       # 1列のデータフレームではエラーになる
mean(jp_data[[5]])      # ベクトルとして抜き出す必要がある
median(jp_data[[5]])    # 面積の中央値 (リストとして列を選択)
jp_data |> select(5) |> median() # データフレームなので動かない
min(jp_data["若年"])     # 若年人口の最小値 (列名で選択)
jp_data |> select("若年") |> min() # 同上
with(jp_data, max(老人)) # 老年人口の最大値 (関数 with() を利用)
jp_data |> select("老人") |> max() # 同上

#' @exercise 列ごとの集計

#' 練習問題のデータを用いた例
jp_data |> summarise(平均失業率 = mean(失業), 件数 = n()) # 失業の列の平均
jp_data |> summarise(across(婚姻:失業, median)) # 婚姻から失業の列の中央値
jp_data |> summarise(across(!県名, max)) # 県名の列以外の最大値
jp_data |> summarise(across(where(is.double), min)) # 数値列の最小値

#' @notes
#' 関数 dplyr::across() の列の指定には上記以外にも様々な関数が使える
#' 詳細は '?dplyr::across' を参照

#' 練習問題のデータを用いた例
#' 地方ごとに人口から面積の列の合計を計算する
jp_data |>
  mutate(地方 = as_factor(jp_area[["地方"]])) |> # 地方の情報を付加
  group_by(地方) |> # 地方ごとにグループ化
  summarize((across(人口:面積, sum))) # グループごとに集計

#' @notes
#' 関数 dplyr::mutate() で新たな列を加えることができる
#' 関数 forcats::as_factor() で文字列を出現順に順序付因子にすると
#' 集計結果は因子の順番で表示される
#' 関数 dplyr::group_by() は複数の条件で条件付けることもできる
jp_data |>
  mutate(地方 = as_factor(jp_area[["地方"]]), # 地方の情報を付加
         人口密度 = 人口/面積,                  # 人口密度を計算
         過密 = 人口密度 >= median(人口密度)) |> # 人口密度が中央値以上
      group_by(地方, 過密) |> # 地方ごとにグループ化
      summarize((across(人口:面積, sum))) # グループごとに集計

#' ---------------------------------------------------------------------------
#' @practice データの集計
#' 
#' 県別の人口密度 (人口/面積)
with(jp_data, 人口/面積) # 値のみ返す
#'
#' 都道府県別の人口密度
jp_data |>
  select(県名, 人口, 面積) |> # 県名，人口，面積を選択
  mutate(人口密度 = 人口/面積) # 人口密度を計算
#'
#' 地方別の人口密度 (地方の総人口/地方の総面積)
jp_data |>
  mutate(地方 = as_factor(jp_area[["地方"]])) |> # 地方の情報を付加
  group_by(地方) |> # 地方ごとにグループ化
  summarize((across(c(人口, 面積), sum))) |> # 地方ごとに人口と面積を集計
  mutate(人口密度 = 人口/面積) # 地方別の人口密度を計算
#'
#' 地方別の婚姻・離婚数/1000人
jp_data |>
  mutate(婚姻可能 = 人口-若年, # 婚姻可能な人口を推計 
         婚姻数 = 婚姻可能*婚姻/1000, # 婚姻数/1000人から人数を推計
         離婚数 = 婚姻可能*離婚/1000, # 離婚数/1000人から人数を推計
         地方 = as_factor(jp_area[["地方"]])) |> # 地方の情報を付加
  group_by(地方) |> # 地方ごとにグループ化
  summarize((across(婚姻可能:離婚数, sum))) |> # 地方別の合計を計算
  mutate(婚姻 = 婚姻数/婚姻可能*1000, # 1000人あたりの婚姻数を計算
         離婚 = 離婚数/婚姻可能*1000) # 1000人あたりの離婚数を計算
#'
#' @notes
#' 日本語に不具合がある場合は
#'   jp_data -> jp_data_en
#'   人口 -> population
#'   若年 -> young_population
#'   婚姻 -> marriage 
#'   離婚 -> divorce
#' など書き換えて試してみて下さい．
#' ---------------------------------------------------------------------------
