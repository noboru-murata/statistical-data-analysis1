### 第12回 練習問題解答例

### 分散分析(一元配置)の Monte-Carlo 実験
fact <- as.factor(rep(LETTERS[1:3],c(10,8,12))) # 因子
alt <- rep(c(1,-1,1),c(10,8,12)) # 対立仮説の例
foo <- data.frame( # 帰無仮説が正しい場合
    obs=rnorm(length(fact),mean=3,sd=2), # 観測値
    fact=fact) # 因子
bar <- data.frame( # 対立仮説が正しい場合
    obs=rnorm(length(fact),mean=3,sd=2)+alt, # 水準ごとに平均が異なる
    fact=fact) # 因子
library(beeswarm) # boxplot上に散布図を表示するため
boxplot(obs ~ fact, data=foo, 
        col="pink", main="H0 = TRUE")
beeswarm(obs ~ fact, data=foo,
         add=TRUE, col="red")
anova(aov(obs ~ fact, data=foo))
boxplot(obs ~ fact, data=bar, 
        col="lightblue", main="H0 = FALSE")
beeswarm(obs ~ fact, data=bar,
         add=TRUE, col="blue")
anova(aov(obs ~ fact, data=bar))

## 実験
myTrial <- function(h0=TRUE){
    if(h0) {
        tmp <- data.frame( # 帰無仮説が正しい場合
            obs=rnorm(length(fact),mean=3,sd=2), # 観測値
            fact=fact # 因子
        )
    } else {
        tmp <- data.frame( # 対立仮説が正しい場合
            obs=rnorm(length(fact),mean=3,sd=2)+alt, # 水準ごとに平均が異なる
            fact=fact # 因子
        )
    }
    return(anova(aov(obs ~ fact, data=tmp))["fact",c("F value","Pr(>F)"),drop=TRUE])
    ## p-値を返す
}
## 帰無仮説が正しい場合のF-値/p-値の分布 (一様分布になる)
foo <- replicate(2000,myTrial())
hist(unlist(foo["F value",]),
     xlab="F-value", main="H0 = TRUE")
hist(unlist(foo["Pr(>F)",]),
     xlab="p-value", main="H0 = TRUE")
## 対立仮説が正しい場合のp-値の分布 (小さな値に偏る)
bar <- replicate(2000,myTrial(h0=FALSE))
hist(unlist(bar["F value",]),
     xlab="F-value", main="H0 = FALSE")
hist(unlist(bar["Pr(>F)",]),
     xlab="p-value", main="H0 = FALSE")
## foo <- data.frame(t(replicate....)) としてもよいが
## data.frame の名前に空白や>が許されず，適宜書き変わるので注意

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
anova(myAov)["weekday","Pr(>F)"] # p値の取得 (行・列の番号で指定してもよい)
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
anova(myAov)["month","Pr(>F)"] # p値の取得 (行・列の番号で指定してもよい)

### 練習問題 二元配置分散分析
## datarium::jobsatisfaction による例
## 性別(gender)と学歴(education_level)による仕事の満足度
## 満足度にそれぞれの因子の効果があるかを検証
## install.packages("datarium") # 検定用のデータが集められている
library(datarium) # packageの読み込み
head(jobsatisfaction) # データの一部を見る
View(jobsatisfaction) # 左上ペインに表を表示する
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
