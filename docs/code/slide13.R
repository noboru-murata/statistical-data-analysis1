### 第13講 サンプルコード
library(tidyverse)
#' 日本語を用いる場合 macOS では以下の設定を行うと良い
if(Sys.info()["sysname"] == "Darwin") { # macOS か調べて日本語フォントを指定
  theme_update(text = element_text(family = "HiraginoSans-W4"))
  update_geom_defaults("text", list(family = theme_get()$text$family))
  update_geom_defaults("label", list(family = theme_get()$text$family))}

lm(formula, data, subset, na.action, ...)
## formula: 式 (目的変数 ~ 説明変数)
## data: データフレーム
## subset: 対象とする部分データ
## na.action: 欠損値の扱い
## ...: 他のオプション．詳細は help(lm) を参照

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
#' base R での情報の表示 
toy_lm |> summary() # さまざまな情報がlist形式
coef(toy_lm)        # 推定された係数
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
  ## データフレームにする場合は以下のようにすればよい
  ## lm(y ~ x, data = tibble(x = x, y = y))
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
  slice_sample(n = 20) |> # Monte-Carlo 実験から20個ランダムに選択
  rowid_to_column() |>    # 番号列(rowid)を作成
  ggplot() +
  geom_abline(aes(intercept = `(Intercept)`,
                  slope = x,
                  colour = as_factor(rowid)), # 色を変える
              show.legend = FALSE) + # 凡例は表示しない
  geom_abline(intercept = alpha,
              slope = beta,
              linewidth = 1.2) + # 真の回帰式(太め)
  xlim(-1,1) + ylim(alpha-beta*1.1, alpha+beta*1.1) + # 描画範囲の指定
  labs(title = "推定された回帰式のばらつき")
#' 同じ生成モデルでも，生成されたデータによって推定結果がばらつくことがわかる
#' ---------------------------------------------------------------------------

#' ---------------------------------------------------------------------------
#' @practice 
### 練習問題 回帰モデルの点推定
## 気候データによる例
tw_data <- read.csv("data/tokyo_weather.csv")
## データの散布図 (1年分)
if(Sys.info()["sysname"]=="Darwin"){par(family="HiraginoSans-W4")}
plot(temp ~ solar, # Y軸 ~ X軸
     data=tw_data,
     pch=20, col="blue")
## 
(my_model <- lm(temp ~ solar, # 目的変数 ~ 説明変数
               data=tw_data)) # 気温を日射量で説明
## 回帰直線の図示 (重ね描き)
abline(reg=my_model, # 得られた回帰式を用いて描画
       col="royalblue", lwd=2)
## 期間を限って分析する
## データの散布図 (夏のモデル)
plot(formula(my_model), # my_model で用いた式を再利用する
     data=tw_data,
     subset=month%in%7:9, # 7月-9月を指定
     pch=20, col="orange")
(my_model2 <- lm(formula(my_model),
               data=tw_data,
               subset=month%in%7:9)) 
abline(reg=my_model2,
       col="red", lwd=2)
## 全データのモデルと夏のモデルを比較する
plot(formula(my_model),
     data=model.frame(my_model), # my_model で整理されたデータを利用する
     pch=20, col="gray")
abline(reg=my_model, 
       col="royalblue", lwd=2)
points(formula(my_model2),
     data=model.frame(my_model2), # my_model2 で整理されたデータを利用する
     pch=20, col="orange")
abline(reg=my_model2,
       col="red", lwd=2)
#' ---------------------------------------------------------------------------

confint(object, parm, level = 0.95, ...)
## object: 関数 lm で推定したモデル
## parm: 区間推定をするパラメタ．指定しなければ全て
## level: 信頼係数
## ...: 他のオプション．詳細は help(confint) を参照

predict(object, newdata, interval="confidence", level=0.95,..)
## object: 関数 lm で推定したモデル
## newdata: 予測値を計算する説明変数
## interval: 信頼区間 "confidence" (既定値は "none")
## level: 信頼係数 (既定値は0.95)
## ...: 他のオプション．詳細は help(predict.lm) を参照

#' ---------------------------------------------------------------------------
#' @practice 
### 練習問題 回帰モデルの区間推定
## 気候データによる例
## 前問で構成したモデルを用いる
## my_model: 1年分のモデル 
confint(my_model)
## 区間推定を視覚化
plot(formula(my_model),
     data=model.frame(my_model), # 用いたデータを取り出すことができる
     pch=20, col="blue")
xrange <- with(tw_data,range(solar)) # 日射量の範囲を取得
x <- seq(xrange[1], xrange[2], by=0.5) # 適当な刻み幅で説明変数を用意
y <- predict(my_model,
             newdata=data.frame(solar=x), # 予測値を計算
             interval="confidence", level=0.95) # 信頼区間を付与
matlines(x, y, lwd=2,
         lty=c(1,4,4), col=c("royalblue","steelblue","steelblue"))
## my_model2: 夏のモデル 
confint(my_model2)
plot(formula(my_model2),
     data=model.frame(my_model2),
     pch=20, col="orange")
y <- predict(my_model2,
             newdata=data.frame(solar=x),
             interval="confidence", level=0.95)
matlines(x, y, lwd=2,
         lty=c(1,4,4), col=c("red","pink","pink"))
#' ---------------------------------------------------------------------------

summary(object)
## object: 関数 lm で推定したモデル

#' ---------------------------------------------------------------------------
#' @practice 
### 練習問題 回帰モデルの係数の検定
## 気候データによる例
## 前問で構成したモデルを用いる
## my_model: 1年分のモデル
summary(my_model)
## 情報が多いので，整理してみる
summary(my_model)$coef # 名前は識別できれば途中まででも可
summary(my_model)$coef["solar",c("t value","Pr(>|t|)")]
summary(my_model)$coef[2,3:4] # 上と同じ
summary(my_model)$fstat # モデルの有意性の評価
## my_model2: 夏のモデル
summary(my_model2)
coef(summary(my_model2)) # 関数coefでも可
coef(summary(my_model2))["solar",c("t value","Pr(>|t|)")]
coef(my_model2) # 推定された係数のみ取り出す場合
coef(summary(my_model2))[,"Estimate"] # 上と同じ
#' ---------------------------------------------------------------------------

summary(object)
## object: 関数 lm で推定したモデル

#' ---------------------------------------------------------------------------
#' @practice 
### 練習問題 決定係数による回帰モデルの検討
## 気候データによる例
## 前問で構成したモデルを用いる
## my_model: 1年分のモデル (気温~日射量)
summary(my_model) # 全情報の表示
summary(my_model)$r.squared
summary(my_model)$adj.r.squared

## my_model2: 夏のモデル (気温~日射量)
summary(my_model2) # 全情報の表示
summary(my_model2)$r.squared
summary(my_model2)$adj.r.squared

## 降水量と気温の関係を調べる
(my_model3 <- lm(temp ~ rain, # モデル式
               data=tw_data)) 
(my_model4 <- lm(formula(my_model3), # 上の式を用いる
               data=tw_data, subset=month%in%7:9)) 

## my_model3: 1年分のモデル (気温~降水量)
plot(formula(my_model3),
     data=model.frame(my_model3),
     pch=20, col="blue")
abline(reg=my_model3, col="red", lwd=2)
summary(my_model3)
## my_model3 に有意性はないことがわかる

## my_model4: 夏のモデル (気温~降水量)
plot(formula(my_model4),
     data=model.frame(my_model4),
     pch=20, col="orange")
abline(reg=my_model4, col="red", lwd=2)
summary(my_model4)
## 夏場は雨が降ると気温が下がる傾向が有意にあることが読み取れる
## 決定係数が低いのはそもそも気温のばらつきが大きいことに起因すると考えられる
#' ---------------------------------------------------------------------------

#' ---------------------------------------------------------------------------
### 補遺
### ggplot2 を用いた線形回帰分析(単回帰)の例
### - Brain and Body Weights for 28 Species

## パッケージの読み込み
require(MASS) 
require(tidyverse) 
require(ggfortify)

## データの読み込み ("MASS::Animals"を用いる)
data(Animals)
## 以下 "Animals" で参照可能

## データの内容を確認
help(Animals)  # 内容の詳細を表示 
print(Animals) # データの表示

## データのプロット (normal plot)
ggplot(Animals, # 用いるdata の指定
       aes(body, brain)) + # x軸，y軸に用いる列の指定
    geom_point(colour="royalblue") + # 点の追加
    labs(title="Brain and Body Weights (normal plot)",
         x="body [kg]", y="brain [g]") # タイトルと軸名の追加

## データのプロット (log plot)
ggplot(Animals,
       aes(body, brain)) +
    scale_x_log10() + scale_y_log10() + # log-log plot を指定
    geom_point(colour="royalblue") +
    labs(title="Brain and Body Weights (log-log plot)",
         x="body [kg]", y="brain [g]")

## データの分布から両対数変換が分析においては適切であることがわかる

## 回帰分析 (単回帰)
model <- lm(log(brain) ~ log(body), # 対数変換した変数で線形回帰
            data=Animals)
summary(model) # 分析結果のまとめを表示

## あてはめ値を用いた回帰式の表示
ggplot(Animals,
       aes(body, brain)) + 
    scale_x_log10() + scale_y_log10() + # log-log plot
    geom_line(aes(y=exp(predict(model,newdata=Animals))), 
              color="dodgerblue", lwd=1.2) +
    geom_text(size=3, label=rownames(Animals)) + # 各点の名前を追加
    labs(title="Brain and Body Weights",
         x="body [kg]", y="brain [g]")

## ggplot の機能を用いた信頼区間付きでの回帰式の表示
ggplot(Animals,
       aes(body, brain)) + 
    scale_x_log10() + scale_y_log10() + # log-log plot
    geom_smooth(method='lm', formula=y ~ x, # 95%信頼区間付 (不要ならse=FALSE)
                color="dodgerblue", lwd=1.2) + 
    geom_text(size=3, label=rownames(Animals)) + # 各点の名前を追加
    labs(title="Brain and Body Weights",
         x="body [kg]", y="brain [g]")

## 信頼区間・予測区間の推定
yc <- exp(predict(model, 
                  interval="confidence", # 信頼区間を指定
                  newdata=Animals))
## predict は newdata で指定されたデータの fit, lwr, upr を返す
colnames(yc) <- paste("c",colnames(yc),sep=".")
## 信頼区間であることを明示するために列名に "c." を追加
yp <- exp(predict(model, 
                  interval="prediction", # 予測区間
                  newdata=Animals))
colnames(yp) <- paste("p",colnames(yp),sep=".")
## 予測区間についても同様に "p.c" を追加

## 区間推定の結果を含めた data.frame を作成
my_data <- cbind(Animals, yc, yp)

## 回帰式および信頼区間・予測区間の表示
ggplot(my_data,
       aes(body, brain)) + 
    scale_x_log10() + scale_y_log10() + # log-log plot
    geom_line(aes(y=c.fit), # あてはめ値を用いて回帰直線
              color="dodgerblue", lwd=1.2) +
    geom_ribbon(aes(ymin=c.lwr,ymax=c.upr), # 信頼区間
                fill="blue", alpha=0.2)+
    geom_ribbon(aes(ymin=p.lwr,ymax=p.upr), # 予測区間
                fill="blue", alpha=0.1)+
    geom_text(size=3, label=rownames(my_data)) + # 各点の名前を追加
    labs(title="Brain and Body Weights",
         x="body [kg]", y="brain [g]")

## 診断プロット
autoplot(model, colour="royalblue",
         smooth.colour="gray50", smooth.linetype="dashed",
         ad.colour="blue",
         label.size=3, label.n=5, label.colour="red")

## 外れ値を除いた回帰分析
idx <- c(6,16,26) # 外れ値のindex
model <- lm(log(brain) ~ log(body), data=Animals, subset=-idx)
summary(model)

## 区間推定
yc <- exp(predict(model, newdata=Animals, interval="confidence"))
colnames(yc) <- paste("c",colnames(yc),sep=".")
yp <- exp(predict(model, newdata=Animals, interval="prediction"))
colnames(yp) <- paste("p",colnames(yp),sep=".")
my_data <- cbind(Animals, yc, yp)

## 回帰式および信頼区間・予測区間の表示
ggplot(my_data, aes(body, brain)) +
    scale_x_log10() + scale_y_log10() +
    geom_line(aes(y=c.fit), color="royalblue", lwd=1.2) +
    geom_ribbon(aes(ymin=c.lwr,ymax=c.upr), fill="blue", alpha=0.2)+
    geom_ribbon(aes(ymin=p.lwr,ymax=p.upr), fill="blue", alpha=0.1)+
    geom_text(size=3, label=rownames(my_data)) + 
    labs(title="Brain and Body Weights", x="body [kg]", y="brain [g]")

## 診断プロット
autoplot(model, colour="royalblue",
         smooth.colour="gray50", smooth.linetype="dashed",
         ad.colour="blue",
         label.size=3, label.n=5, label.colour="red")
#' ---------------------------------------------------------------------------
