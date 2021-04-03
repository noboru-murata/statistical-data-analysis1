### 練習1
### 一元配置分散分析
## 気候データによる例
## 曜日ごとの気温に差があるか否かを分散分析
myData <- read.csv("data/tokyo_weather.csv",
                   fileEncoding="utf8")
## 曜日の情報を付加
days <- with(myData,
             as.Date(paste(年,月,日, sep="-"))) # 日付を作成
wdays <- weekdays(days) # 各日付の曜日を計算
myData <- cbind(myData, 曜日=as.factor(wdays)) # 曜日因子を追加
## 箱ひげ図で可視化
par(family="HiraginoSans-W4") # 日本語フォントの指定
boxplot(気温 ~ 曜日, data=myData, 
        col="lavender", main="曜日ごとの気温") 
aggregate(気温 ~ 曜日, data=myData, FUN=mean)
aggregate(気温 ~ 曜日, data=myData, FUN=var)
aggregate(気温 ~ 曜日, data=myData, FUN=sd)
## 曜日ごとの気温差に関する分散分析
(myAov <- aov(気温 ~ 曜日, data=myData)) # aovによる分析
summary(myAov) # 分散分析表の表示(棄却されない)
model.tables(myAov, type="means")   # 水準(曜日)ごとの平均値
model.tables(myAov, type="effects") # 水準(曜日)ごとの効果
## 検定のみ実行する場合
oneway.test(気温 ~ 曜日, data=myData, var.equal=TRUE) # 等分散での検定
oneway.test(気温 ~ 曜日, data=myData) # Welchの近似法による検定

## 参考: 月ごとの気温に差があるか否かを分散分析 (棄却されるはず)
myData$月 <- as.factor(myData$月) # 月を因子化
boxplot(気温 ~ 月, data=myData, 
        col="lavender", main="月ごとの気温") 
(myAov <- aov(気温 ~ 月, data=myData)) # aovによる分析
summary(myAov) # 分散分析表の表示(棄却される)

### 練習2
### 二元配置分散分析
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
