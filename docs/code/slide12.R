### 第12回 練習問題解答例

### 練習問題 一元配置分散分析
## 気候データによる例
## 曜日ごとの気温に差があるか否かを分散分析
TW.data <- read.csv("data/tokyo_weather.csv")
## 曜日の情報を付加
days <- with(TW.data,
             as.Date(paste(year,month,day,sep="-"))) # 日付を作成
wdays <- weekdays(days) # 各日付の曜日を計算
TW.data <- cbind(TW.data, weekday=as.factor(wdays)) # 曜日因子を追加
## 箱ひげ図で可視化
par(family="HiraginoSans-W4") # 日本語フォントの指定
boxplot(temp ~ weekday, data=TW.data, 
        col="lavender", main="曜日ごとの気温") 
aggregate(temp ~ weekday, data=TW.data, FUN=mean)
aggregate(temp ~ weekday, data=TW.data, FUN=var)
aggregate(temp ~ weekday, data=TW.data, FUN=sd)
## 曜日ごとの気温差に関する分散分析
(myAov <- aov(temp ~ weekday, data=TW.data)) # aovによる分析
summary(myAov) # 分散分析表の表示(棄却されない)
model.tables(myAov, type="means")   # 水準(曜日)ごとの平均値
model.tables(myAov, type="effects") # 水準(曜日)ごとの効果
## 検定のみ実行する場合
oneway.test(temp ~ weekday, data=TW.data, var.equal=TRUE) # 等分散での検定
oneway.test(temp ~ weekday, data=TW.data) # Welchの近似法による検定

## 参考: 月ごとの気温に差があるか否かを分散分析 (棄却されるはず)
TW.data$month <- as.factor(TW.data$month) # 月を因子化
boxplot(temp ~ month, data=TW.data, 
        col="lavender", main="月ごとの気温") 
(myAov <- aov(temp ~ month, data=TW.data)) # aovによる分析
summary(myAov) # 分散分析表の表示(棄却される)

### 練習問題 二元配置分散分析
## datarium::jobsatisfaction による例
## 性別(gender)と学歴(education_level)による仕事の満足度
## 満足度にそれぞれの因子の効果があるかを検証
## install.packages("datarium") # 検定用のデータが集められている
library(datarium) # packageの読み込み
head(jobsatisfaction) # データの一部を見る
## boxplotによる視覚化
boxplot(score ~ education_level + gender,
        data=jobsatisfaction,
        col=rep(c("lightblue","pink"),each=3))
## 比較するために表示を工夫してみる
boxplot(score ~ education_level + gender,
        data=jobsatisfaction,
        col=rep(c("yellow","palegreen","royalblue"),2),
        names=c("","male","","","female",""))
legend("topleft",inset=0.05,
       col=c("yellow","palegreen","royalblue"),
       lwd=3, cex=0.5,
       legend=c("school","college","university"))
## 二元配置分散分析
(myAov <- aov(score ~ gender + education_level,
              data=jobsatisfaction))
summary(myAov)
model.tables(myAov, type="means")  
model.tables(myAov, type="effects")
