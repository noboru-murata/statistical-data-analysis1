### 第13講 練習問題解答例

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
