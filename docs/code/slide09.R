### 第9講 サンプルコード
library(tidyverse)
#' 以下，日本語を用いるため macOS では以下の設定を行う
if(Sys.info()["sysname"] == "Darwin") { # macOS か調べて日本語フォントを指定
  theme_update(text = element_text(family = "HiraginoSans-W4"))
  update_geom_defaults("text", list(family = theme_get()$text$family))
  update_geom_defaults("label", list(family = theme_get()$text$family))}

#' ---------------------------------------------------------------------------
#' @practice 平均・分散・標準偏差の計算
#' データの読み込み
tw_data <- read_csv("data/tokyo_weather.csv")
#'
#' 全データによる計算
tw_data |>
  summarise(across(c(temp,solar,wind), mean))
tw_data |>
  summarise(across(c(temp,solar,wind), var))
tw_data |>
  summarise(across(c(temp,solar,wind), sd))
(tw_summary <- # まとめて計算して保存しておく
   tw_data |> 
   summarise(across(c(temp,solar,wind),
                    list(mean = mean, var = var, sd =sd))))
#'
#' 毎月5日のデータによる計算
tw_data |>
  filter(day == 5) |>
  summarise(across(c(temp,solar,wind),
                   list(mean = mean, var = var, sd =sd)))
#'
#' 5の付く日のデータによる計算
tw_data |>
  filter(day %in% c(5,15,25)) |>
  summarise(across(c(temp,solar,wind),
                   list(mean = mean, var = var, sd =sd)))
#'
#' ランダムに選択した36日で推定した場合のばらつきを調べる
mc <- 5000 # 実験回数を指定
n <- 36 # ランダムに選択する日数を指定
#' 気温の標本平均による例
my_trial <- function(){ 
  tw_data |>
    slice(sample(nrow(tw_data), n)) |>
    summarise(mean = mean(temp)) |>
    as.numeric() # tibble形式ではなく単なる数値として返す
}
xbars <- replicate(mc, my_trial())
tibble(平均気温の推定値 = xbars) |>
  ggplot(aes(x = 平均気温の推定値)) +
  geom_histogram(aes(y = after_stat(density)), 
                 bins = 30,                    
                 fill ="lightblue",          
                 colour = "blue") + 
  geom_vline(xintercept = tw_summary[["temp_mean"]], # 全体の平均
             colour = "red") 
#' @notes
#' 上記の例では保存しておいた平均の値を参照しているが，
#' geom_vline の中で計算し直すこともできる
tibble(平均気温の推定値 = xbars) |>
  ggplot(aes(x = 平均気温の推定値)) +
  geom_histogram(aes(y = after_stat(density)), 
                 bins = 30,                    
                 fill ="lightblue",          
                 colour = "blue") + 
  geom_vline(xintercept = tw_data |> 
               summarise(mean(temp)) |>
               as.numeric(), # tibble形式を数値に変換
             colour = "red") 
#'
#' 項目および統計量を総当たりで調べる
#' 
my_items <- c("temp","solar","wind") # 文字列のベクトルとして定義する場合は""が必要
my_funcs <- c("mean","var","sd") # 統計量を計算する関数名
my_truth <- cbind(mu,s2,s) # 各変数ごとの平均，分散，標準偏差ベクトルをまとめておく
#' 実験を行う関数で項目と統計量を指定できるように定義する
my_trial <- function(){ # まとめて計算するように変更する
  tmp <- # 計算結果を一時保存
    tw_data |>
    slice(sample(nrow(tw_data), n)) |>
    summarise(across(c(temp,solar,wind),
                     list(mean = mean, var = var, sd =sd)))
  return(t(tmp)[,1,drop = TRUE]) # データフレームではなくベクトルとして返す
}
my_data <- replicate(mc, my_trial())
#' 図示する部分を関数として定義しておく
my_plot <- function(estimates,true_value,xlabel,title){
  if(Sys.info()["sysname"]=="Darwin"){par(family="HiraginoSans-W4")}
  hist(estimates, breaks=40, freq=FALSE,
       col="lightblue", border="blue",
       xlab=xlabel, main=title)
  abline(v=mean(estimates),col="green",lwd=2) # 推定量の平均
  abline(v=true_value,col="red",lwd=2) # 真の値
}
#' 全ての組み合わせは for 文で実行可能
for(i in 1:3){ # 項目の番号を指定する
  for(j in 1:3){ # 関数の番号を指定する
    my_plot(estimates=replicate(mc,my_trial(my_items[i],my_funcs[j])),
            true_value=my_truth[i,j],
            xlabel=my_items[i],
            title=paste(my_items[i],"の",my_funcs[j],"の推定"))
  }
}
#' 標準偏差の推定量は偏りがあることが確認できる
#' 風速の分散の推定量の分布が他と異なり正規分布に近くないので，
#' サンプル数を増やしてみる
i <- 3; j <- 2 # 風速の分散
n <- 72 # サンプル数を倍にしてみる
my_plot(estimates=replicate(mc,my_trial(my_items[i],my_funcs[j])),
        true_value=my_truth[i,j],
        xlabel=my_items[i],
        title=paste(my_items[i],"の",my_funcs[j],"の推定"))
#' 形状が正規分布に近づいたことが確認できる
#' ---------------------------------------------------------------------------

if(Sys.info()["sysname"]=="Darwin"){par(family="HiraginoSans-W4")}
curve(dt(x,df=10), from=-3, to=3,
      col="blue", lwd=3,
      ylab="密度", main="t-分布 (自由度 10; 歪度0, 超過尖度1)")

if(Sys.info()["sysname"]=="Darwin"){par(family="HiraginoSans-W4")}
curve(dgamma(x,shape=4,rate=1), from=0, to=10,
      col="blue", lwd=3,
      ylab="密度", main="Gamma分布 (形状4，レート1; 歪度1, 超過尖度1.5)")

library("e1071") # package::e1071 の読み込み．必要なら install する
skewness(x, na.rm = FALSE, type = 3) # 標本歪度
kurtosis(x, na.rm = FALSE, type = 3) # 標本超過尖度 (尖度ではない)
#' x: ベクトル，データフレームなど
#' na.rm: 欠損値を取り除くか否か
#' type: 計算法の指定(通常は既定値でよい)

#' ---------------------------------------------------------------------------
#' @practice 歪度と超過尖度の計算
library("e1071")
#' データの読み込み
tw_data <- read.csv("data/tokyo_weather.csv")
#' 全データによる計算
my_data <- subset(tw_data,
                 select = c(temp,solar,wind))
(skew <- apply(my_data,2,FUN=skewness))
(kurt <- apply(my_data,2,FUN=kurtosis))
#' 5の付く日のデータによる計算
my_data <- subset(tw_data,
                 subset = day%in%c(5,15,25),
                 select = c(temp,solar,wind))
apply(my_data,2,FUN=skewness)
apply(my_data,2,FUN=kurtosis)
#' ヒストグラムの描画と
my_items <- c("temp","solar","wind")
if(Sys.info()["sysname"]=="Darwin"){par(family="HiraginoSans-W4")}
#' 全ての組み合わせは for 文で実行可能
for(inum in 1:3){
  foo <- tw_data[,my_items[inum]] # ベクトルにする必要がある
  #' foo <- subset(tw_data, select=my_items[inum], drop=TRUE) # でもよい
  if(Sys.info()["sysname"]=="Darwin"){par(family="HiraginoSans-W4")}
  hist(foo, breaks=20, freq=FALSE,
       col="lightgreen", border="green",
       xlab=my_items[inum], main=my_items[inum])
  curve(dnorm(x,mean=mean(foo),sd=sd(foo)), # x は curve の変数
        add=TRUE, col="orange", lwd=2)
}
#' ---------------------------------------------------------------------------

cov(x, y = NULL, use = "everything", 
    method = c("pearson", "kendall", "spearman")) # 共分散
cor(x, y = NULL, use = "everything", 
    method = c("pearson", "kendall", "spearman")) # 相関
#' x,y: ベクトル，データフレームなど (データフレームの時は列間の関係を計算)
#' use: 欠損値などの扱いに関する指定
#' method: 計算法の指定(通常は既定値 pearson でよい)

#' ---------------------------------------------------------------------------
#' @practice 共分散と相関の計算
#' データの読み込み
tw_data <- read.csv("data/tokyo_weather.csv")
#' 共分散・相関行列の計算
my_data <- subset(tw_data,
                 select = c(temp,rain,solar,snow,wind,press,humid)) # 数値データ
(my_cov <- cov(my_data))
(my_cor <- cor(my_data))
my_cor==min(my_cor) # 負の最大相関の位置を確認 (solar,rain)
my_cor==max(my_cor-diag(diag(my_cor))) # 対角を除く最大相関の位置を確認 (temp,humid)
abs(my_cor)==min(abs(my_cor)) # 最小相関の位置を確認 (snow,humid)
#' 散布図の描画
if(Sys.info()["sysname"]=="Darwin"){par(family="HiraginoSans-W4")}
pairs(~ solar + rain, data=my_data, col="blue")
pairs(~ temp + humid, data=my_data, col="red")
pairs(~ snow + humid, data=my_data, col="grey")
pairs(my_data, col="orange") # 数値データを全部表示してみる
#' ---------------------------------------------------------------------------

median(x, na.rm = FALSE) # 中央値
quantile(x, probs = seq(0, 1, 0.25), na.rm = FALSE,
         names = TRUE, type = 7) # 分位点
summary(x) # 最大，最小，四分位点，平均を計算する
#' x: ベクトル
#' na.rm: 欠損値を取り除くか否か
#' probs: 計算する分位点の値
#' names: 出力に関する指定，多数の分位点を計算する場合は FALSE とした方がよい
#' type: 計算法の指定(help(quantile) を参照)

#' 以下は正規分布での計算例
qnorm(p, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
#' p: 分位点 (100p%)
#' mean, sd: 正規分布の特性を決めるoption
#' lower.tail: TRUE なら P(X<x) を計算
#'             FALSE なら逆
#' log.p: 出力を対数とするか否か (値が小さい場合に利用)

#' xxx分布の場合は以下の形式
qxxx(p, "分布の特性を決める option の指定")

#' ---------------------------------------------------------------------------
#' @practice 分位点と最頻値の計算
#' データの読み込み
tw_data <- read.csv("data/tokyo_weather.csv")
#' 気温の分位点
#' 全データによる計算
(my_truth <- summary(subset(tw_data,select=temp,drop=TRUE)))
#' 5の付く日のデータによる計算
summary(subset(tw_data,subset=day%in%c(5,15,25),select=temp,drop=TRUE))
#' ランダムに選択した36日で推定した場合のばらつき
mc <- 5000 # 実験回数を指定
my_funcs <- c("min","1Q","median","mean","3Q","max")
my_trial <- function(){
  idx <- sample(1:nrow(tw_data),36) # 重複なしで36行選ぶ
  summary(subset(tw_data[idx,],select=temp,drop=TRUE))}
quants <- replicate(mc,my_trial())
#' ヒストグラムの表示
for(fnum in 1:6) {
  if(Sys.info()["sysname"]=="Darwin"){par(family="HiraginoSans-W4")}
  hist(quants[fnum,], breaks=40, freq=FALSE,
       col="lightblue", border="blue",
       xlab="気温", main=paste("気温の",my_funcs[fnum],"の推定"))
  abline(v=my_truth[fnum],col="red",lwd=2)
}
#' ヒストグラムの表示 (定義域とビンを揃えて表示する)
for(fnum in 1:6) {
  if(Sys.info()["sysname"]=="Darwin"){par(family="HiraginoSans-W4")}
  hist(quants[fnum,],
       breaks=pretty(subset(tw_data,select=temp,drop=TRUE),n=50), # 固定
       freq=FALSE,
       col="lightblue", border="blue",
       xlab="気温", main=paste("気温の",my_funcs[fnum],"の推定"))
  abline(v=my_truth[fnum],col="red",lwd=2)
}
#' 最多風向の最頻値
(my_table <- table(subset(tw_data,select=wdir))) # 頻度表
max(my_table) # 最大値 
which.max(my_table) # 最頻値の表の位置
names(which.max(my_table)) # 最頻値の項目名
#' ---------------------------------------------------------------------------
