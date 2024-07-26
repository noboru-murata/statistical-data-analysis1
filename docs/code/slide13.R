### 第13講 サンプルコード
library(tidyverse)
#' 日本語を用いる場合 macOS では以下の設定を行うと良い
if(Sys.info()["sysname"] == "Darwin") { # macOS か調べて日本語フォントを指定
  theme_update(text = element_text(family = "HiraginoSans-W4"))
  update_geom_defaults("text", list(family = theme_get()$text$family))
  update_geom_defaults("label", list(family = theme_get()$text$family))}

#' ---------------------------------------------------------------------------
#' @practice 回帰分析の Monte-Carlo 実験の例
#'
#' 人工データによる回帰モデルの推定
#' 以下のモデルのパラメタは適当に変更せよ
alpha <- 2    # 切片
beta  <- 3    # 回帰係数
n <- 20       # データ数
sigma <- 0.5  # 誤差の標準偏差
#' データの生成
x <- runif(n, min = -1, max = 1) # 説明変数 (区間[-1,1]を想定)
epsilon <- rnorm(n, sd = sigma)  # 誤差 (正規分布)
y <- alpha + beta * x + epsilon
#' データの視覚化
toy_data <- tibble(x = x, y = y)
gg <-
  toy_data |>
  ggplot(aes(x = x, y = y)) +
  geom_point(colour = "forestgreen")
print(gg)
#' 回帰式の推定
toy_lm <- lm(y ~ x, data = toy_data)
coef(toy_lm) # 推定された係数の取得
#'
#' 以下は回帰係数の有意性検定で詳述
#' base R での情報の表示 
toy_lm |> summary() # さまざまな情報がlist形式
#' tidyverse での情報の表示 (tibble形式)
toy_lm |> broom::tidy()    # 推定された係数の情報
toy_lm |> broom::glance()  # 推定に関するさまざまな情報
toy_lm |> broom::augment() # 推定に用いられたデータの情報
#' 推定結果の視覚化
gg +
  geom_abline(intercept = alpha,
              slope = beta,
              colour = "red") + # 真の回帰式
  geom_abline(intercept = coef(toy_lm)[1],
              slope = coef(toy_lm)[2],
              colour = "blue")  # 推定された回帰式
#'
#' Monte-Carlo 実験
mc_trial <- function(){
  epsilon <- rnorm(n, sd = sigma)
  y <- alpha + beta * x + epsilon # 説明変数は固定しておく
  est <- lm(y ~ x) # データフレームにせずに直接 x,y を渡す
  #' データフレームにする場合は以下のようにすればよい
  #' lm(y ~ x, data = tibble(x = x, y = y))
  return(coef(est))
}
mc_data <- # 実験結果をデータフレームに変換
  replicate(2000, mc_trial()) |> t() |> as_tibble()
#'
#' 推定値の分布を視覚化
mc_data |> # 切片
  ggplot(aes(x = `(Intercept)`)) +
  geom_density(fill = "pink") +
  geom_vline(xintercept = alpha, colour = "orange") +
  labs(x = expression(hat(alpha)), title = "切片の分布")
mc_data |> # 傾き
  ggplot(aes(x = x)) +
  geom_density(fill = "palegreen") +
  geom_vline(xintercept = beta, colour = "darkgreen") +
  labs(x = expression(hat(beta)), title = "傾きの分布")
#'
#' 推定された回帰式のばらつきの表示
mc_data |>
  slice_sample(n = 40) |> # Monte-Carlo 実験から40個ランダムに選択
  rowid_to_column() |>    # 番号列(rowid)を作成
  ggplot() +
  geom_abline(aes(intercept = `(Intercept)`,
                  slope = x,
                  colour = as_factor(rowid)), # 色を変える
              alpha = 0.4, # alpha値を低くして色を薄くする
              show.legend = FALSE) + # 凡例は表示しない
  geom_abline(intercept = alpha,
              slope = beta,
              colour = "red",
              linewidth = 1.1) + # 真の回帰式(太め)
  xlim(-1,1) + ylim(alpha-beta*1.1, alpha+beta*1.1) + # 描画範囲の指定
  labs(title = "推定された回帰式のばらつき")
#' 同じ生成モデルでもデータによって推定結果がばらつくことがわかる
#' ---------------------------------------------------------------------------

#' ---------------------------------------------------------------------------
#' @practice 回帰モデルの点推定
#'
#' 気候データによる例
#' データの読み込み
tw_data <- read_csv("data/tokyo_weather.csv")
#'
#' データの散布図 (1年分)
tw_data |>
  ggplot(aes(x = solar, y = temp)) +
  geom_point(colour = "olivedrab") +
  labs(x = "日射量", y = "気温")
#'
#' 回帰式の推定
tw_lm <- lm(temp ~ solar,    # 目的変数 ~ 説明変数
            data = tw_data) # 気温を日射量で説明
tw_lm |> summary()       # 結果の要約
tw_lm |> broom::tidy()   # 係数とその統計量
tw_lm |> broom::glance() # その他の統計量
#'
#' 回帰直線の図示 (前の図に重ね描き)
last_plot() +
  geom_abline(intercept = coef(tw_lm)[1], # 切片
              slope = coef(tw_lm)[2],     # 回帰係数
              colour = "slateblue",
              linewidth = 1.2) 
#'
#' 関数 geom_smooth を用いることもできる
tw_data |>
  ggplot(aes(x = solar, y = temp)) +
  geom_point(colour = "olivedrab") +
  geom_smooth(method = "lm", # 関数 lm を用いて信頼区間(平滑化方法)を計算
              se = FALSE,    # 信頼区間を付けない
              colour = "slateblue") +
  labs(x = "日射量", y = "気温")
#'
#' 期間を限って分析する
#' データの散布図 (夏のモデル)
tw_data |>
  filter(month %in% 7:9) |> # 7月-9月を抽出
  ggplot(aes(x = solar, y = temp)) +
  geom_point(colour = "olivedrab") +
  labs(x = "日射量", y = "気温")
#'
#' 回帰式の推定
tw_lm2 <- lm(formula(tw_lm), # 前の式を利用
             data = tw_data,
             subset = month %in% 7:9) # 7月-9月を抽出
tw_lm2 |> summary()
#'
#' @notes
#' 期間を限った分析は以下のように書くこともできる
#' 
tw_data |>
  filter(month %in% 7:9) |>    # 7月-9月を抽出
  lm(formula(tw_lm), data = _) # パイプ演算の内容を data に渡す
#'
#' 推定結果の可視化
last_plot() +
  geom_abline(intercept = coef(tw_lm2)[1],
              slope = coef(tw_lm2)[2],
              colour = "tomato",
              linewidth = 1.2) 
#'
#' 関数 geom_smooth を用いてもよい
tw_data |>
  filter(month %in% 7:9) |> 
  ggplot(aes(x = solar, y = temp)) +
  geom_point(colour = "olivedrab") +
  geom_smooth(method = "lm", 
              se = FALSE,    
              colour = "tomato") +
  labs(x = "日射量", y = "気温")
#'
#' 全データのモデルと夏のモデルを比較する
tw_data |>
  mutate(summer = ifelse(month %in% 7:9,
                         "Jul-Sep", # 7月-9月のラベル
                         "others")) |> 
  ggplot(aes(x = solar, y = temp, colour = summer)) +
  geom_point() +
  geom_abline(intercept = coef(tw_lm)[1],
              slope = coef(tw_lm)[2],
              colour = "slateblue",
              linewidth = 1.2) +
  geom_abline(intercept = coef(tw_lm2)[1],
              slope = coef(tw_lm2)[2],
              colour = "tomato",
              linewidth = 1.2) +
  labs(x = "日射量", y = "気温", colour = NULL)
#'
#' @notes
#' 関数 geom_smooth を用いる場合は以下のようにすればよい
#' 
tw_data |>
  mutate(summer = ifelse(month %in% 7:9,
                         "7-9月", # 7月-9月のラベル
                         "その他")) |> 
  ggplot(aes(x = solar, y = temp, colour = summer)) +
  geom_point() +
  geom_smooth(method = "lm", 
              se = FALSE,    
              colour = "slateblue") +
  geom_smooth(data = \(x)filter(x, summer == "7-9月"), # 7-9月に限定
              method = "lm", 
              se = FALSE,    
              colour = "tomato") +
  labs(x = "日射量", y = "気温")
#' data に一変数関数を渡すと geom_smooth 内での処理を限定することができる
#' 上記は base R での無名関数の記述を用いているが
#' tidyverse では
#'   ~ filter(., summer == "7-9月")
#'   . %>% filter(summer == "7-9月")
#' のように書くこともできる
#' ---------------------------------------------------------------------------

#' ---------------------------------------------------------------------------
#' @practice 回帰モデルの区間推定
#'
#' 気候データによる例 (前問で構成したモデルを用いる)
#' 
#' tw_lm: 1年分のモデル 
confint(tw_lm)
#'
#' 区間推定を視覚化
#' 関数 geom_smooth を用いると簡潔に記述できる
#' 
tw_data |>
  ggplot(aes(x = solar, y = temp)) +
  geom_point(aes(colour = factor(month))) + # 月ごとに色を変える
  geom_smooth(method = "lm", # 関数 lm を用いて信頼区間(平滑化方法)を計算
              level = 0.95,  # 既定値なので無くても良い
              colour = "royalblue",  # 線の色
              fill = "steelblue") +  # 塗り潰しの色
  labs(x = "日射量", y = "気温", colour = NULL)
#'
#' @notes
#' 上記の描画を愚直に行うには以下のような手続きを考えればよい
#' 
#' 視覚化したい信頼区間に合わせて適切な説明変数と信頼区間を作成する
tw_conf <-
  tw_lm |> 
  broom::augment(newdata = tibble(solar = tw_data |> pull(solar) |>
                                    range() |> # 日射量の範囲を取得
                                    pretty(n = 50)), # 50個程度の区間を作成
                 interval = "confidence",
                 conf.level = 0.95) # 既定値なので無くてもよい
#' データ点と信頼区間を描画する
tw_data |>
  ggplot(aes(x = solar, y = temp)) +
  geom_point(aes(colour = factor(month))) +
  geom_abline(intercept = coef(tw_lm)[1], 
              slope = coef(tw_lm)[2],
              colour = "royalblue",
              linewidth = 1.2) +
  geom_ribbon(data = tw_conf, # temp 列の代わりに .fitted列がある
              aes(x = solar, y = .fitted,
                  ymin = .lower, ymax = .upper),
              fill = alpha("steelblue", 0.3)) +
  labs(x = "日射量", y = "気温", colour = NULL)
#'
#' データ数がある程度多い場合はデータの説明変数をそのまま用いてもよい
tw_lm |>
  broom::augment(interval = "confidence") |>
  ggplot(aes(x = solar, y = temp)) +
  geom_point(colour = "olivedrab") + # augmentの出力にmonthは含まれていないため
  geom_line(aes(y = .fitted),
            colour = "royalblue") +
  geom_ribbon(aes(ymin = .lower, ymax = .upper),
              fill = alpha("steelblue", 0.3)) +
  labs(x = "日射量", y = "気温")
#'
#' データに信頼区間を追加する方法もある
tw_data |>
  mutate(broom::augment(tw_lm, interval = "confidence")) |>
  ggplot(aes(x = solar, y = temp)) +
  geom_point(aes(colour = factor(month))) + 
  geom_line(aes(y = .fitted), 
            colour = "royalblue") +
  geom_ribbon(aes(ymin = .lower, ymax = .upper), 
              fill = alpha("steelblue", 0.3)) +
  labs(x = "日射量", y = "気温", colour = NULL)
tw_data |>
  mutate(predict(tw_lm, # 推定したモデル
                 interval = "confidence", # 信頼区間の推定を指定
                 level = 0.95) |>
         as_tibble()) |> # tibble形式に変換して追加する (列名に注意)
  ggplot(aes(x = solar, y = temp)) +
  geom_point(aes(colour = factor(month))) + # augmentの出力にmonthは含まれていないため
  geom_line(aes(y = fit), # predict の出力は .fitted でない
            colour = "royalblue") +
  geom_ribbon(aes(ymin = lwr, ymax = upr), # predict の出力は .lower/.upper でない
              fill = alpha("steelblue", 0.3)) +
  labs(x = "日射量", y = "気温", colour = NULL)
#'
#' tw_lm2: 夏のモデル 
confint(tw_lm2)
#'
#' 視覚化
model.frame(tw_lm2) |> # モデルの作成に用いたデータ
  ggplot(aes(x = solar, y = temp)) +
  geom_point(colour = "orange") +
  geom_smooth(method = "lm", level = 0.95,
              colour = "red", fill = "pink") +
  labs(x = "日射量", y = "気温", colour = NULL)
#'
#' 1年のデータも重ねて表示
tw_lm2 |> broom::augment() |> # この関数でも取り出せる
  ggplot(aes(x = solar, y = temp)) +
  geom_point(data = tw_data, # 1年分のデータを使用
             colour = alpha("grey", 0.5)) + # 薄い灰色で表示
  geom_point(colour = "orange") +
  geom_smooth(method = "lm", level = 0.95,
              colour = "red", fill = "pink") +
  labs(x = "日射量", y = "気温", colour = NULL)
#'
#' 1年のデータから限定して信頼区間を描いてもよい
tw_data |> 
  ggplot(aes(x = solar, y = temp)) +
  geom_point(aes(colour = factor(month))) +
  geom_smooth(data = \(x)filter(x, month %in% 7:9),
              method = "lm", level = 0.95,
              colour = "red", fill = "pink") +
  labs(x = "日射量", y = "気温", colour = NULL)
#' ---------------------------------------------------------------------------

#' ---------------------------------------------------------------------------
#' @practice 回帰モデルの係数の検定
#'
#' 気候データによる例
#' 前問で構成したモデルを用いる
#'
#' tw_lm: 1年分のモデル (base R での操作)
summary(tw_lm)
#' 情報が多いので，整理してみる
summary(tw_lm)$coef # 名前は識別できれば途中まででも可
summary(tw_lm)$coef["solar",c("t value","Pr(>|t|)")]
summary(tw_lm)$coef[2,3:4] # 上と同じ
summary(tw_lm)$fstat # F統計量 (モデルの有意性の評価)
anova(tw_lm)$'Pr(>F)'[1] # F統計量のp値 (summaryからは取り出し難い)
#'
#' パイプを使う場合は以下のようにすればよい
tw_lm |> summary() |> _$coef 
tw_lm |> summary() |> _$coef["solar",c("t value","Pr(>|t|)")]
tw_lm |> summary() |> _$coef[2,3:4]
#'
#' tw_lm: 1年分のモデル (tidyverse での操作)
tw_lm |> broom::tidy() # 係数の情報をまとめたデータフレーム
tw_lm |> broom::tidy() |> # solarの係数のt統計量とp値を抽出
  filter(term == "solar") |> select(statistic, p.value)
tw_lm |> broom::tidy() |> _[2, c("statistic","p.value")]
tw_lm |> broom::tidy() |> _[2,4:5]
tw_lm |> broom::glance() # モデルの情報をまとめたデータフレーム
tw_lm |> broom::glance() |> # F統計量に関する情報
  select(statistic, p.value, df, df.residual)
#' 
#' tw_lm2: 夏のモデル (base R での操作)
summary(tw_lm2)
coef(summary(tw_lm2)) # 関数coefでも可
coef(summary(tw_lm2))["solar",c("t value","Pr(>|t|)")]
coef(tw_lm2) # 推定された係数のみ取り出す場合
coef(summary(tw_lm2))[,"Estimate"] # 上と同じ
#' tw_lm2: 夏のモデル (tidyverse での操作)
tw_lm2 |> broom::tidy()   # 係数の情報
tw_lm2 |> broom::glance() # モデルの情報
#' ---------------------------------------------------------------------------

#' ---------------------------------------------------------------------------
#' @practice 決定係数による回帰モデルの検討
#'
#' 気候データによる例
#' 前問で構成したモデルを用いる
#' tw_lm: 1年分のモデル (気温 ~ 日射量)
summary(tw_lm) # 全情報の表示
summary(tw_lm)$r.squared
summary(tw_lm)$adj.r.squared
tw_lm |> broom::glance() 
tw_lm |> broom::glance() |> # 1:2列が決定係数
  select(r.squared, adj.r.squared)
#'
#' tw_lm2: 夏のモデル (気温 ~ 日射量)
summary(tw_lm2) # 全情報の表示
summary(tw_lm2)$r.squared
summary(tw_lm2)$adj.r.squared
tw_lm2 |> broom::glance() 
tw_lm2 |> broom::glance() |> 
  select(r.squared, adj.r.squared)
#'
#' 降水量と気温の関係を調べる
tw_lm3 <- lm(temp ~ rain, data = tw_data)
tw_lm4 <- lm(formula(tw_lm3), # 上の式を用いる
             data = tw_data,
             subset = month %in% 7:9) # 夏(7-9月)のモデル
#'
#' tw_lm3: 1年分のモデル (気温 ~ 降水量)
tw_lm3 |> summary()
model.frame(tw_lm3) |> # 推定に用いたデータを利用
  ggplot(aes(x = rain, y = temp)) +
  geom_point(colour = "blue") +
  geom_smooth(method = "lm",
              colour = "red", fill = "pink") +
  labs(x = "降水量", y = "気温")
#' tw_lm3 に有意性はないことがわかる
#'
#' tw_lm4: 夏のモデル (気温 ~ 降水量)
tw_lm4 |> summary()
model.frame(tw_lm4) |> # 推定に用いたデータを利用
  ggplot(aes(x = rain, y = temp)) +
  geom_point(colour = "orange") +
  geom_smooth(method = "lm",
              colour = "red", fill = "pink") +
  labs(x = "降水量", y = "気温")
#' 夏場は雨が降ると気温が下がる傾向が有意にあることが読み取れる
#' 決定係数が低いのはそもそも気温のばらつきが大きいことに起因すると考えられる
#' ---------------------------------------------------------------------------

#' ---------------------------------------------------------------------------
#' @notes
#' ggplot2 を用いた線形回帰分析(単回帰)の例
#' - Brain and Body Weights for 28 Species
#'
#' この項で用いるオブジェクトは 'bb_' とする
#' 
#' パッケージの読み込み
#' 以下のほかに 'ggrepel' 'broom' 'magrittr' 'MASS' を利用
library(tidyverse) 
library(ggfortify) 
#'
#' データの読み込み ("MASS::Animals"を用いる)
data(Animals, package = "MASS")
#' 以下 "Animals" で参照可能
#' 
#' データの内容を確認
help(Animals, package = "MASS")  # 内容の詳細を表示 
print(Animals) # データの表示
#'
#' データのプロット (normal plot)
Animals |>
  ggplot(aes(body, brain)) + # x軸，y軸に用いる列の指定
  geom_point(colour = "royalblue") + # 点の追加
  labs(title = "Brain and Body Weights (normal plot)",
       x = "body [kg]", y = "brain [g]") # タイトルと軸名の追加
#'
#' データのプロット (log plot)
Animals |>
  ggplot(aes(body, brain)) +
  geom_point(colour = "royalblue") +
  scale_x_log10() + scale_y_log10() + # log-log plot を指定
  labs(title = "Brain and Body Weights (log-log plot)",
       x = "body [kg]", y="brain [g]")
#' データの分布から両対数変換が分析においては適切であることがわかる
#' 
#' 回帰分析 (単回帰)
bb_lm <- lm(log(brain) ~ log(body), # 対数変換した変数で線形回帰
            data = Animals)
bb_lm |> summary() # 分析結果のまとめを表示
#'
#' 回帰式の表示 (信頼区間付き)
Animals |>
  rownames_to_column() |>
  ggplot(aes(body, brain)) + 
  geom_point(colour = alpha("royalblue", 0.75)) +
  ggrepel::geom_text_repel(aes(label = rowname), # 各点の名前を追加
                  size = 3) + 
  geom_smooth(method = "lm", # 回帰式
              colour = "dodgerblue",
              fill = "dodgerblue") + 
  scale_x_log10() + scale_y_log10() + # log-log plot
  labs(title = "Brain and Body Weights",
       x = "body [kg]", y = "brain [g]")
#' 回帰式の表示 (信頼区間・予測区間付き)
Animals |>
  rownames_to_column() |>
  mutate( # 予測区間の情報を追加する
    broom::augment(bb_lm,
                   interval = "prediction", # 予測区間を指定
                   conf.level = 0.95) |>    # 既定値なので省略可能
    select(.fitted, .lower, .upper) |> exp() # 必要な列を選択して対数変換を戻す
  ) |>   
  ggplot(aes(body, brain)) + 
  geom_point(colour = "royalblue") +
  ggrepel::geom_text_repel(aes(label = rowname), # 各点の名前を追加
                  size = 3) + # 文字の大きさを調整
  geom_smooth(method = "lm", # 回帰式
              colour = "dodgerblue",
              fill = "dodgerblue") +
  geom_ribbon(aes(ymin = .lower, ymax = .upper), # 予測区間の描画
              fill = alpha("dodgerblue", 0.2)) +
  scale_x_log10() + scale_y_log10() + # log-log plot
  labs(title = "Brain and Body Weights",
       x = "body [kg]", y = "brain [g]")
#'
#' 予測区間の計算は関数 predict() を用いてもよい
#' 
#' mutate( # 予測区間の情報を追加する
#'   predict(bb_lm,
#'           newdata = Animals, # 対数変換のため指定しないと警告が出る
#'           interval = "prediction") |>
#'   exp() |>    # 対数変換を戻す
#'   as_tibble() # tibble形式でfit/lwr/upr列を追加
#' ) 
#'
#' 診断プロット
autoplot(bb_lm,
         colour = "royalblue",
         smooth.colour = "gray50",
         smooth.linetype = "dashed",
         ad.colour = "blue",
         label.size = 3,      # 以下，外れ値ラベルに関する設定
         label.n = 5, 
         label.colour = "red")
#'
#' 外れ値を除いた回帰分析
idx <- c(6,16,26) # 外れ値のindex
bb_lm <- lm(log(brain) ~ log(body),
                 data = Animals,
                 subset = -idx) # 外れ値を除く
bb_lm |> summary()
#'
#' 回帰式の表示 (信頼区間・予測区間付き)
Animals |>
  rownames_to_column() |>
  mutate( 
    broom::augment(bb_lm,
                   newdata = Animals, # 外れ値を除いたので全データで計算
                   interval = "prediction") |>
    select(.fitted, .lower, .upper) |> exp()) |>   
  ggplot(aes(body, brain)) + 
  geom_point(colour = "royalblue") +
  ggrepel::geom_text_repel(aes(label = rowname), # 各点の名前を追加
                  size = 3) + # 文字の大きさを調整
  geom_smooth(data = \(x)slice(x, -idx), # 外れ値を除く
              method = "lm", # 回帰式
              colour = "dodgerblue",
              fill = "dodgerblue") +
  geom_ribbon(data = \(x)slice(x, -idx), # 外れ値を除く
              aes(ymin = .lower, ymax = .upper), # 予測区間の描画
              fill = alpha("dodgerblue", 0.2)) +
  scale_x_log10() + scale_y_log10() + # log-log plot
  labs(title = "Brain and Body Weights",
       x = "body [kg]", y = "brain [g]")
#'
#' 恐竜の領域でも信頼・予測区間を描画する
bb_new_body <- # 計算用のbody列を作成
  tibble(body = Animals |> pull(body) |>
           log() |> pretty(n=32) |> exp())
bb_new_data <- # 作成したbody列に対する区間の計算
  tibble(bb_new_body,
         broom::augment(bb_lm,
                        newdata = bb_new_body,
                        interval = "confidence") |> # 信頼区間
         select(.fitted, .lower, .upper) |> exp() |>
         magrittr::set_colnames(c("brain","c.lower","c.upper")), # 列名の指定
         broom::augment(bb_lm,
                        newdata = bb_new_body,
                        interval = "prediction") |> # 予測区間
         select(.lower, .upper) |> exp() |>
         magrittr::set_colnames(c("p.lower","p.upper"))) # 列名の指定
Animals |>
  rownames_to_column() |>
  ggplot(aes(body, brain)) + 
  geom_point(colour = "royalblue") +
  ggrepel::geom_text_repel(aes(label = rowname),
                  size = 3) +
  geom_line(data = bb_new_data,                    # 回帰式の描画
            colour = "dodgerblue") +
  geom_ribbon(data = bb_new_data,
              aes(ymin = c.lower, ymax = c.upper), # 信頼区間の描画
              fill = alpha("dodgerblue", 0.4)) +
  geom_ribbon(data = bb_new_data,
              aes(ymin = p.lower, ymax = p.upper), # 予測区間の描画
              fill = alpha("dodgerblue", 0.2)) +
  scale_x_log10() + scale_y_log10() +
  labs(title = "Brain and Body Weights",
       x = "body [kg]", y = "brain [g]")
#' 
#' 診断プロット
autoplot(bb_lm,
         colour = "royalblue",
         smooth.colour = "gray50",
         smooth.linetype = "dashed",
         ad.colour = "blue",
         label.size = 3,      # 以下，外れ値ラベルに関する設定
         label.n = 5, 
         label.colour = "red")
#' ---------------------------------------------------------------------------
