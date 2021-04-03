### 練習1
### 回帰モデルの点推定
## 気候データによる例
myData <- read.csv("data/tokyo_weather.csv",
                   fileEncoding="utf8")
## データの散布図 (1年分)
par(family="HiraginoSans-W4") 
plot(気温 ~ 日射量, data=myData, pch=20, col="blue")
## 
(myModel <- lm(気温 ~ 日射量, data=myData)) # 気温を日射量で説明
## 回帰直線の図示 (重ね描き)
abline(reg=myModel, col="red", lwd=2)
## 期間を限って分析する
## データの散布図 (夏のモデル)
plot(気温 ~ 日射量, data=myData, subset=月%in%7:9,
     pch=20, col="orange")
(myMode2 <- lm(気温 ~ 日射量, data=myData, subset=月%in%7:9)) 
abline(reg=myMode2, col="red", lwd=2)

### 練習2
### 回帰モデルの区間推定
## 気候データによる例
## 前問で構成したモデルを用いる
## myModel: 1年分のモデル 
confint(myModel)
## 区間推定を視覚化
plot(気温 ~ 日射量, data=model.frame(myModel), pch=20, col="blue")
xrange <- with(myData,range(日射量)) # 日射量の範囲を取得
x <- seq(xrange[1], xrange[2], by=0.5) # 適当な刻み幅で説明変数を用意
y <- predict(myModel, newdata=data.frame(日射量=x), # 予測値を計算
             interval="confidence", level=0.95) # 信頼区間を付与
matlines(x, y, lwd=2,
         lty=c(1,2,2), col=c("red","pink","pink"))
## myMode2: 夏のモデル 
confint(myMode2)
plot(気温 ~ 日射量, data=model.frame(myMode2), pch=20, col="orange")
y <- predict(myMode2, newdata=data.frame(日射量=x),
             interval="confidence", level=0.95)
matlines(x, y, lwd=2,
         lty=c(1,2,2), col=c("red","pink","pink"))

### 練習3
### 回帰モデルの係数の検定
## 気候データによる例
## 前問で構成したモデルを用いる
## myModel: 1年分のモデル
summary(myModel)
## 情報が多いので，整理してみる
summary(myModel)$coef # 名前は識別できれば途中まででも可
summary(myModel)$fstat
## myMode2: 夏のモデル
summary(myMode2)
coef(summary(myMode2)) # 関数coefでも可

### 練習3
### 回帰モデルの係数の検定
## 気候データによる例
## 前問で構成したモデルを用いる
## myModel: 1年分のモデル (気温~日射量)
summary(myModel)$r.squared
summary(myModel)$adj.r.squared

## myMode2: 夏のモデル (気温~日射量)
summary(myMode2)$r.squared
summary(myMode2)$adj.r.squared

## 降水量と気温の関係を調べる
(myMode3 <- lm(気温 ~ 降水量, data=myData)) 
(myMode4 <- lm(気温 ~ 降水量, data=myData, subset=月%in%7:9)) 

## myModel3: 1年分のモデル (気温~降水量)
plot(気温 ~ 降水量, data=model.frame(myMode3), pch=20, col="blue")
abline(reg=myMode3, col="red", lwd=2)
summary(myMode3)
## myMode3 に有意性はないことがわかる

## myMode4: 夏のモデル (気温~降水量)
plot(気温 ~ 降水量, data=model.frame(myMode4), pch=20, col="orange")
abline(reg=myMode4, col="red", lwd=2)
summary(myMode4)
## 夏場は雨が降ると気温が下がる傾向が有意にあることが読み取れる
## 決定係数が低いのはそもそも気温のばらつきが大きいことに起因すると考えられる
