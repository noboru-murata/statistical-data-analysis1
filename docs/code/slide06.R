### 第6講 サンプルコード
library(tidyverse)

#' @exercise 乱数で用いた例

#' 関数 sample() の使い方
x <- 1:10 # サンプリング対象の集合をベクトルとして定義
set.seed(123) # 乱数のシード値(任意に決めてよい)を指定
sample(x, 5)  # xから5つの要素を重複なしでランダムに抽出
sample(x, length(x)) # xの要素のランダムな並べ替えとなる
sample(x, 5, replace=TRUE) # xから5つの要素を重複ありでランダムに抽出
sample(1:6, 10, replace=TRUE) # サイコロを10回振る実験の再現
sample(1:6, 10, prob=6:1, replace=TRUE) # 出る目の確率に偏りがある場合

#' 関数 rbinom() の使い方
rbinom(10, size=4, prob=0.5) # 確率0.5に対する次数4の二項乱数を10個発生
rbinom(20, size=4, prob=0.2) # 個数を20, 確率を0.2に変更
#' 関数 runif() の使い方
runif(5, min=-1, max=2) # 区間(-1,2)上の一様乱数を5個発生
runif(5) # 指定しない場合は区間(0,1)が既定値

#' 関数 set.seed() の使い方
set.seed(1) # 乱数のシード値をseed=1で指定
runif(5) 
set.seed(2) # 乱数のシード値をseed=2で指定
runif(5)    # seed=1の場合と異なる系列が観測される
set.seed(1) # 乱数のシード値をseed=1で指定
runif(5)    # 初めのseed=1の場合と同じ系列が再現される

#' 2つのサイコロを振る試行の数値実験
#' 試行(1回の実験)を行う関数を用意する
my_trial <- function(){ # この問題では引数は不要
  dice <- sample(1:6, 2, replace=TRUE) # 2個のサイコロを振る
  return(dice)
}
#' 乱数のシード値を指定
set.seed(20240524)
#' 確率シミュレーションを実行
my_data <- replicate(10, # 10回実験
                     my_trial()) # 実行する関数を指定
print(my_data) # 実験結果は行列(配列)として保存されている
