### 第13回 練習問題解答例

### 練習問題 回帰モデルの点推定
## 気候データによる例
TW.data <- read.csv("data/tokyo_weather.csv")
## データの散布図 (1年分)
if(Sys.info()["sysname"]=="Darwin"){par(family="HiraginoSans-W4")}
plot(temp ~ solar, # Y軸 ~ X軸
     data=TW.data,
     pch=20, col="blue")
## 
(myModel <- lm(temp ~ solar, # 目的変数 ~ 説明変数
               data=TW.data)) # 気温を日射量で説明
## 回帰直線の図示 (重ね描き)
abline(reg=myModel, # 得られた回帰式を用いて描画
       col="royalblue", lwd=2)
## 期間を限って分析する
## データの散布図 (夏のモデル)
plot(formula(myModel), # myModel で用いた式を再利用する
     data=TW.data,
     subset=month%in%7:9, # 7月-9月を指定
     pch=20, col="orange")
(myMode2 <- lm(formula(myModel),
               data=TW.data,
               subset=month%in%7:9)) 
abline(reg=myMode2,
       col="red", lwd=2)
## 全データのモデルと夏のモデルを比較する
plot(formula(myModel),
     data=model.frame(myModel), # myModel で整理されたデータを利用する
     pch=20, col="gray")
abline(reg=myModel, 
       col="royalblue", lwd=2)
points(formula(myMode2),
     data=model.frame(myMode2), # myMode2 で整理されたデータを利用する
     pch=20, col="orange")
abline(reg=myMode2,
       col="red", lwd=2)

### 練習問題 回帰モデルの区間推定
## 気候データによる例
## 前問で構成したモデルを用いる
## myModel: 1年分のモデル 
confint(myModel)
## 区間推定を視覚化
plot(formula(myModel),
     data=model.frame(myModel), # 用いたデータを取り出すことができる
     pch=20, col="blue")
xrange <- with(TW.data,range(solar)) # 日射量の範囲を取得
x <- seq(xrange[1], xrange[2], by=0.5) # 適当な刻み幅で説明変数を用意
y <- predict(myModel,
             newdata=data.frame(solar=x), # 予測値を計算
             interval="confidence", level=0.95) # 信頼区間を付与
matlines(x, y, lwd=2,
         lty=c(1,4,4), col=c("royalblue","steelblue","steelblue"))
## myMode2: 夏のモデル 
confint(myMode2)
plot(formula(myMode2),
     data=model.frame(myMode2),
     pch=20, col="orange")
y <- predict(myMode2,
             newdata=data.frame(solar=x),
             interval="confidence", level=0.95)
matlines(x, y, lwd=2,
         lty=c(1,4,4), col=c("red","pink","pink"))

### 練習問題 回帰モデルの係数の検定
## 気候データによる例
## 前問で構成したモデルを用いる
## myModel: 1年分のモデル
summary(myModel)
## 情報が多いので，整理してみる
summary(myModel)$coef # 名前は識別できれば途中まででも可
summary(myModel)$coef["solar",c("t value","Pr(>|t|)")]
summary(myModel)$coef[2,3:4] # 上と同じ
summary(myModel)$fstat # モデルの有意性の評価
## myMode2: 夏のモデル
summary(myMode2)
coef(summary(myMode2)) # 関数coefでも可
coef(summary(myMode2))["solar",c("t value","Pr(>|t|)")]
coef(myMode2) # 推定された係数のみ取り出す場合
coef(summary(myMode2))[,"Estimate"] # 上と同じ

### 練習問題 決定係数による回帰モデルの検討
## 気候データによる例
## 前問で構成したモデルを用いる
## myModel: 1年分のモデル (気温~日射量)
summary(myModel) # 全情報の表示
summary(myModel)$r.squared
summary(myModel)$adj.r.squared

## myMode2: 夏のモデル (気温~日射量)
summary(myMode2) # 全情報の表示
summary(myMode2)$r.squared
summary(myMode2)$adj.r.squared

## 降水量と気温の関係を調べる
(myMode3 <- lm(temp ~ rain, # モデル式
               data=TW.data)) 
(myMode4 <- lm(formula(myMode3), # 上の式を用いる
               data=TW.data, subset=month%in%7:9)) 

## myModel3: 1年分のモデル (気温~降水量)
plot(formula(myMode3),
     data=model.frame(myMode3),
     pch=20, col="blue")
abline(reg=myMode3, col="red", lwd=2)
summary(myMode3)
## myMode3 に有意性はないことがわかる

## myMode4: 夏のモデル (気温~降水量)
plot(formula(myMode4),
     data=model.frame(myMode4),
     pch=20, col="orange")
abline(reg=myMode4, col="red", lwd=2)
summary(myMode4)
## 夏場は雨が降ると気温が下がる傾向が有意にあることが読み取れる
## 決定係数が低いのはそもそも気温のばらつきが大きいことに起因すると考えられる
