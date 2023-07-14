### 第13講 練習問題解答例

### 回帰分析の Monte-Carlo 実験
## 人工データによる回帰モデルの推定
alpha <- 2
beta <- 3
n <- 20
sigma <- 0.3
x <- runif(n)
epsilon <- rnorm(n,sd=sigma)
y <- alpha + beta * x + epsilon
plot(x,y, type="p")
my_data <- data.frame(x=x,y=y)
est <- lm(y ~ x, data=my_data)
summary(est)
abline(coef=c(alpha,beta),col="red")
abline(reg=est,col="blue")

## 実験
my_trial <- function(){
  epsilon <- rnorm(n,sd=sigma)
  y <- alpha + beta * x + epsilon # 説明変数は固定しておく
  est <- lm(y ~ x) 
  return(coef(est))
}
foo <- replicate(2000,my_trial())
## 推定値の分布
if(Sys.info()["sysname"]=="Darwin"){par(family="HiraginoSans-W4")}
hist(foo[1,],
     breaks=30, freq=FALSE,
     col="pink",
     xlab=expression(hat(alpha)), main="切片の分布")
abline(v=alpha,col="orange",lwd=2)
hist(foo[2,],
     breaks=30, freq=FALSE,
     col="palegreen",
     xlab=expression(hat(beta)), main="傾きの分布")
abline(v=beta,col="darkgreen",lwd=2)

## 推定された回帰式のばらつきの表示
plot(x,y, type="n",
     main="推定された回帰式のばらつき") # 表示するための適当な領域を指定
for(i in seq(1,ncol(foo),by=10)) { # 推定された回帰式を間引いて表示
  abline(coef=foo[,i],col="gray90")
}
abline(coef=c(alpha,beta),col="red")
## 同じ生成モデルでも，生成されたデータによって推定結果がばらつくことがわかる

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
