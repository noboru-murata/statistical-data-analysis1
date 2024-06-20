### 第12講 サンプルコード
library(tidyverse)
#' 以下，日本語を用いるため macOS では以下の設定を行う
if(Sys.info()["sysname"] == "Darwin") { # macOS か調べて日本語フォントを指定
  theme_update(text = element_text(family = "HiraginoSans-W4"))
  update_geom_defaults("text", list(family = theme_get()$text$family))
  update_geom_defaults("label", list(family = theme_get()$text$family))}

#' ---------------------------------------------------------------------------
#' @practice 分散分析(一元配置)の Monte-Carlo 実験例
#'
#' A,B,Cの3つの水準をもつデータを考える
fact <- as.factor(rep(LETTERS[1:3], c(10,8,12))) # 因子(水準A,B,Cが10,8,12個)
toy_data0 <- # 平均が全て等しい場合 (帰無仮説が正しい例)
  tibble(factor = fact,                                 # 水準
         value = rnorm(length(fact), mean = 3, sd = 2)) # 観測値
#' データの状況を図示する
toy_data0 |>
  ggplot(aes(x = factor, y = value)) +
  geom_boxplot(fill = "pink") +
  geom_jitter(colour = "red", width = 0.2) +
  labs(title = "帰無仮説が正しい場合")
#' 
alt <- rep(c(1,-1,1), c(10,8,12)) # 水準ごとの平均のずれ(対立仮説の例)
toy_data1 <- # 平均が水準ごとに異なる場合(対立仮説が正しい例)
  tibble(factor = fact,                                       # 水準
         value = rnorm(length(fact), mean = 3, sd = 2) + alt) # 観測値
#' 同様に図示する
toy_data1 |>
  ggplot(aes(x = factor, y = value)) +
  geom_boxplot(fill = "lightblue") +
  geom_jitter(colour = "blue", width = 0.2) +
  labs(title = "帰無仮説が誤りの場合")
#'
#' 分散分析を行う
#' 帰無仮説が正しい場合
aov(value ~ factor, data = toy_data0) 
aov(value ~ factor, data = toy_data0) |>
  broom::tidy() # 分散分析表
#' 帰無仮説が誤りの場合(対立仮説が正しい場合)
aov(value ~ factor, data = toy_data1) 
aov(value ~ factor, data = toy_data1) |>
  broom::tidy() # 分散分析表
#'
#' @notes
#' 関数 aov() のようにデータが第1引数でない場合にパイプ演算子を使うには
#' "_" (placeholder) を使って位置を明示すればよい
toy_data0 |> aov(value ~ factor, data = _) 
toy_data0 |>
  aov(value ~ factor, data = _) |>
  broom::tidy()
#'
#' Monte-Carlo 実験
mc_trial <- function(h0 = TRUE){
  if(h0) { # 帰無仮説が正しい場合
    tmp <-
      tibble(factor = fact,
             value = rnorm(length(fact), mean = 3, sd = 2)) |>
      aov(value ~ factor, data = _) |> broom::tidy()
  } else { # 対立仮説が正しい場合
    tmp <- 
      tibble(factor = fact, # 水準ごとに平均が異なる
             value = rnorm(length(fact), mean = 3, sd = 2) + alt) |>
      aov(value ~ factor, data = _) |> broom::tidy()
  }
  #' 分散分析表における factor 行の統計量とp値を返す
  return(c(statistic = tmp[1,5, drop = TRUE],
           p.value   = tmp[1,6, drop = TRUE]))
  #' 以下のように書いても良い
  #' return(tmp |> select(5:6) |> t() |> _[,1])
}
#'
#' 帰無仮説が正しい場合のF統計量/p値の分布 (p値は一様分布になる)
replicate(2000, mc_trial()) |>
  t() |> as_tibble() |> pivot_longer(everything()) |> # 描画用に変形
  ggplot(aes(x = value, fill = name)) +
  geom_histogram(show.legend = FALSE) +
  facet_wrap(vars(name), ncol = 2, scales = "free") + # それぞれ個別の座標で描画
  labs(title = "帰無仮説が正しい場合")
#'
#' 対立仮説が正しい場合のF統計量/p値の分布 (p値は小さな値に偏る)
replicate(2000, mc_trial(h0 = FALSE)) |>
  t() |> as_tibble() |> pivot_longer(everything()) |> 
  ggplot(aes(x = value, fill = name)) +
  geom_histogram(show.legend = FALSE) +
  facet_wrap(vars(name), ncol = 2, scales = "free") + 
  labs(title = "帰無仮説が誤りの場合")
#' ---------------------------------------------------------------------------

#' ---------------------------------------------------------------------------
#' @practice 一元配置分散分析
#'
#' 気候データによる例
#' 曜日ごとの気温に差があるか否かを分散分析
tw_data <- read_csv("data/tokyo_weather.csv") |>
  mutate(day_of_week = ordered(day_of_week, # 曜日を順序付き因子にする
                               levels = wday(1:7, label = TRUE))) 
#'
#' 曜日ごとの分布を見るとともに平均・分散(標準偏差)を確認する
tw_data |>
  ggplot(aes(x = day_of_week, y = temp)) +
  geom_boxplot(fill = "lavender") +
  labs(x = "曜日", y = "気温", title = "曜日ごとの気温の分布") 
tw_data |>
  group_by(day_of_week) |>
  summarize(across(temp, list(mean = mean,
                              var = var,
                              sd = sd)))
#'
#' 曜日ごとの気温差に関する分散分析
aov(temp ~ day_of_week, data = tw_data) # aovによる分析
aov(temp ~ day_of_week, data = tw_data) |>
  summary() # 分散分析表の表示(棄却されない)
aov(temp ~ day_of_week, data = tw_data) |>
  broom::tidy() # 分散分析表のデータフレーム (行・列の番号で必要な情報が取得できる)
aov(temp ~ day_of_week, data = tw_data) |>
  model.tables(type = "means")   # 水準(曜日)ごとの平均値
aov(temp ~ day_of_week, data = tw_data) |>
  model.tables(type = "effects") # 水準(曜日)ごとの効果
#'
#' 検定のみ実行する場合
#' 級内の分散が等しいことを仮定
oneway.test(temp ~ day_of_week, data = tw_data, var.equal = TRUE) 
#' 級内の分散が等しいことが仮定できない場合はWelchの近似法による検定が行われる
oneway.test(temp ~ day_of_week, data = tw_data) # 
#'
#' @notes
#' 月ごとの気温に差があるか否かを分散分析 (棄却されるはず)
#' 
tw_data <- tw_data |> mutate(month = as_factor(month)) # 月を因子化
tw_data |>
  ggplot(aes(x = month, y = temp)) +
  geom_boxplot(fill = "lavender") +
  labs(x = "月", y = "気温", title = "月ごとの気温の分布") 
aov(temp ~ month, data = tw_data) |>
  summary() # 分散分析表の表示(棄却される)
#'
#' 曜日の因子化などは package::lubridate に含まれる関数が利用できる
#' 曜日の文字列を得る方法
wday(1:7, label = TRUE) # 省略名
wday(1:7, label = TRUE, abbr = FALSE) # 正式名
wday(2:8, label = TRUE) # 月曜から始まる文字列を得る場合
wday(1:7, label = TRUE, week_start = 1) # 順序を月曜から始める場合
#'
#' 日付から曜日を得る方法
tw_data |>
  mutate(date = as_date(paste(year,month,day, sep = "-")), # 日付に変換
         weekday = wday(date, label = TRUE), # 日付から曜日(順序付因子)を追加
         .before = year) # year列の前に付加
#' ---------------------------------------------------------------------------

#' ---------------------------------------------------------------------------
#' @practice 二元配置分散分析
#' 
#' datarium::jobsatisfaction による例
#' 性別(gender)と学歴(education_level)による仕事の満足度
#' 満足度にそれぞれの因子の効果があるかを検証
#' 
#' install.packages("datarium") # 検定用のデータが集められている
library("datarium") # packageの読み込み
#'
#' データの内容を確認
head(jobsatisfaction) # データの一部を見る
View(jobsatisfaction) # 左上ペインに表を表示する
#'
#' データの視覚化
jobsatisfaction |> # 学歴ごとに性別を比較
  ggplot(aes(x = education_level, y = score, fill = gender)) +
  geom_boxplot()
jobsatisfaction |> # 性別ごとに学歴を比較
  ggplot(aes(x = gender, y = score, fill = education_level)) +
  geom_boxplot()
#' 二元配置分散分析
aov(score ~ gender + education_level, data=jobsatisfaction) |>
  summary()
aov(score ~ gender + education_level, data=jobsatisfaction) |>
  model.tables(type = "means")  
aov(score ~ gender + education_level, data=jobsatisfaction) |>
  model.tables(type = "effects")
#' ---------------------------------------------------------------------------
