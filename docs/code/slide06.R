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

#' ---------------------------------------------------------------------------
#' @practice コイン投げの賭け
#'
#' 試行を行う関数
my_trial <- function(){ 
  while(TRUE){ # 永久に回るループ
    if(sample(c("h","t"),1) == "h"){ # h:head，t:tail
      return("Alice") # Aliceが表を出して終了
    }
    if(sample(c("h","t"),1) == "h"){
      return("Bob") # Bobが表を出して終了
    }
    #' どちらも裏ならもう一度ループ
  }
}
#' @notes
#' この関数は rbinom を用いても実装できるので試みてみよう
#' 
#' 試行を行ってみる (勝った方の名前が表示される)
my_trial()
my_trial()
#'
#' 確率シミュレーション
#' set.seed(8888) # 実験を再現する場合はシードを指定
mc <- 10000 # 回数を設定 
my_data <- replicate(mc, my_trial())
str(my_data) # 結果が mc 次元のベクトルになっていることが確認できる
#'
#' 簡単な集計 (この賭けは先手が有利なはず)
table(my_data) # 頻度を表示する
table(my_data)/mc # 勝率の計算 (全実験回数で除算)
#' ---------------------------------------------------------------------------

#' ---------------------------------------------------------------------------
#' @practice Monty Hole 問題
#' 
#' クイズに答える試行 (賞品の位置は固定して解答者をランダムに配置する場合)
monty_trial <- function(verbose = FALSE){
  #' 賞品は1に置いてあるものとする
  choice <- sample(1:3, size = 1) # 解答者の最初の選択
  if(choice == 1) { # 変えないのが正解の場合
    win <- "stay"
    door <- sample(2:3, size = 1) # Monty が開ける扉 (2か3をランダムに選択)
  } else { # 変えるのが正解の場合
    win <- "change"
    if(choice == 2) { # 2を選択したら3を開ける
      door <- 3
    } else { # そうでなければ(3を選択)2を開ける
      door <-2 
    }
  }
  if(verbose == TRUE){ # 賞品，解答者の選択，Monty が開ける扉を返す
    return(c(prize = 1, choice = choice, door = door))
  } else { # 勝ち負けの条件を返す
    return(win)
  }
}
#'
#' クイズに答える試行 (賞品と解答者をランダムに配置する場合)
#' if 文で素朴に書くこともできるが
#' 集合を操作する関数 setdiff(), union() を利用することができる
#' 
monty_trial <- function(verbose = FALSE){
  prize <- sample(1:3, size = 1)  # 賞品の置かれた扉
  choice <- sample(1:3, size = 1) # 解答者の最初の選択
  if(prize == choice) { # 変えないのが正解の場合
    win <- "stay"
    door <- sample(setdiff(1:3, prize), size = 1) # Monty が開ける扉 (ランダムに選択)
  } else { # 変えるのが正解の場合
    win <- "change"
    door <- setdiff(1:3, union(prize, choice)) # Monty が開ける扉
  }
  if(verbose == TRUE){ # 賞品，解答者の選択，Monty が開ける扉を返す
    return(c(prize = prize, choice = choice, door = door))
  } else { # 勝ち負けの条件を返す
    return(win)
  }
}
#'
#' 試行を行ってみる
monty_trial()
monty_trial()
#'
#' 詳細な情報を表示する場合 (絵を描くときに用いる)
monty_trial(verbose = TRUE)
monty_trial(verbose = TRUE)
#'
#' 実験の状況を絵にしてみる
mc <- 15
replicate(mc, monty_trial(verbose = TRUE)) |>
  t() |> # 配列を転置
  as_tibble() |> # データフレームに変更
  rowid_to_column(var = "index") |> # 試行の番号を追加
  pivot_longer(!index) |> # index 以外を long format 化
  ggplot(aes(x = index, y = value,
             colour = name, shape = name)) +
  geom_point(size = 4) + # サイズを大きめに表示
  scale_y_continuous(breaks = 1:3)
#'
#' 実験とともに勝率がどのように変化するか図示してみる
mc <- 400
replicate(mc, monty_trial()) |>
  as_tibble_col(column_name = "win") |>
  rowid_to_column(var = "index") |>
  mutate(stay_win = win == "stay", # 留まって勝つ場合
         ratio = cumsum(stay_win)/index) |> # 勝率の推移を計算
  ggplot(aes(x = index, y = ratio)) +
  geom_line(colour = "blue") + 
  geom_hline(yintercept = 1/3, colour = "orange") + # 理論値
  ylim(c(0,1)) # y軸の描画範囲を指定
#'
#' 大規模な確率シミュレーション
mc <- 50000 # 回数を設定 
monty_data <- replicate(mc, monty_trial()) 
#'
#' 簡単な集計
table(monty_data)    # 頻度
table(monty_data)/mc # 確率(推定値)
#' ---------------------------------------------------------------------------

#' ---------------------------------------------------------------------------
#' @practice Buffonの針
#'
#' 針を投げる試行を定義する
#' 試行の周期性から1本の線の回りで問題を考えれば良い
#' 針の中心位置 x (-d/2<x<d/2) と向き theta (0<theta<2π) をランダムに生成
#' 針の先と尾の座標の符号が異なれば位置0の線と交わっていると判断できる
#' 
buffon_trial <- function(d, l, verbose = FALSE){ # dとlを指定
  x <- runif(1, min = -d/2, max = d/2) # 位置
  theta <- runif(1, min = 0, max = 2*pi) # 角度
  cross <- FALSE # 交わったかどうかを示す変数
  if((x+l*cos(theta)/2)*(x-l*cos(theta)/2) < 0){
    cross <- TRUE # 交わった場合に書き換え
  }
  if(verbose == TRUE){ # 位置と角度も返す
    return(c(x = x, theta = theta,       # ベクトルで返すので
             cross = as.numeric(cross))) # データ型を揃える
  } else { # 交わったかどうかだけ返す
    return(cross) # 論理値のまま返す
  }
}
#'
#' 試行を行ってみる
d <- 10
l <- 7
buffon_trial(d, l)
buffon_trial(d, l)
#'
#' 詳細な情報を表示する場合 (絵を描くときに用いる)
buffon_trial(d, l, verbose = TRUE)
buffon_trial(d, l, verbose = TRUE)
#'
#' 絵にしてみる
mc <- 10
replicate(mc, buffon_trial(d, l, verbose = TRUE)) |>
  t() |> # 配列を転置
  as_tibble() |> # tibble に変換
  mutate( # 図に必要な情報を加える
    y = runif(mc, min = -(d+l)/2, max = (d+l)/2), # 中心のy座標をランダムに生成
    x1 = x-l/2*cos(theta), 
    x2 = x+l/2*cos(theta),
    y1 = y-l/2*sin(theta),
    y2 = y+l/2*sin(theta),
    cross = as.logical(cross)) |> # 交わり表すラベル(論理値)
  ggplot() + 
  geom_vline(xintercept = c(-d,0,d)) + # 縦棒を描く
  geom_segment(aes(x = x1, y = y1,       # 始点
                   xend = x2, yend = y2, # 終点
                   colour = cross)) +    # 交わりを表示
  labs(x = "x", y = "y") +
  coord_fixed() # 座標軸の比を1に固定
#'
#' xとthetaの関係を図にしてみる
mc <- 3000
replicate(mc, buffon_trial(d, l, verbose = TRUE)) |>
  t() |> 
  as_tibble() |> 
  mutate( # 図に必要な情報を加える
    cross = as.logical(cross)) |> # 交わり表すラベル(論理値)
  ggplot(aes(x = x, y = theta, colour = cross)) + 
  geom_vline(xintercept = c(-d/2,d/2)) + # 境界を描く
  geom_hline(yintercept = c(0,2*pi)) +   # 横線を描く
  geom_point() + labs(y = expression(theta)) 
#' 
#' 大規模な確率シミュレーション
mc <- 100000 # 回数を設定 
buffon_data <- replicate(mc, buffon_trial(d, l)) 
#'
#' 簡単な集計
table(buffon_data)      # 頻度 (TRUEが針の交わった回数)
table(buffon_data)/mc   # 確率(推定値)
print((2*l)/(pi*d)) # 針の交わる確率 (理論値)
2*l/d/(table(buffon_data)/mc)["TRUE"] # π の推定値
#' ---------------------------------------------------------------------------

#' ---------------------------------------------------------------------------
#' @practice 秘書問題
#' 
#' 秘書の採用の試行
secretary_trial <- function(n, r, verbose = FALSE){ # nとrを指定
  applicants <- sample(1:n, size = n) # n人の順位をランダムに並べ替える
  ref <- applicants[1:(r-1)] # r-1人目までは参照(最高順位が基準点)
  test <- applicants[r:n]    # r人目以降が採用候補
  idx <- which(test < min(ref)) # 採用候補の中で基準点より良い人を選出
  if(length(idx) == 0) { # 1人も規準良い人がいない場合
    employed <- applicants[n] # 最後の人を採用
  } else {
    employed <- test[idx[1]]  # 最初に基準点を越えた人を採用
  }
  if(verbose == TRUE){ # 全順位も返す
    return(list(applicants = applicants, 
                employed = employed)) # 2つの異なるベクトルをリストで束ねる
  } else { # 採用した者の順位のみ返す
    return(employed)
  }
}
#'
#' 試行を行ってみる
n <- 10 # 候補者は10名
secretary_trial(n, 2, verbose = TRUE) # 2人目から採用を考える
secretary_trial(n, 3, verbose = TRUE) # 3人目から採用を考える
secretary_trial(n, 4, verbose = TRUE) # 4人目から採用を考える
secretary_trial(n, 5, verbose = TRUE) # 5人目から採用を考える
secretary_trial(n, 6, verbose = TRUE) # 6人目から採用を考える
#'
#' set.seed(8888) # 実験を再現したい場合はシードを指定
mc <- 3000
n <- 30 # 候補者数を変えて実験
secretary_data <- tibble(r = NULL,
                  employed = NULL)
for (r in 2:(n-1)) {
  foo <- replicate(mc, secretary_trial(n, r))
  if(r %in% c(2,6,10,14,18,22)) { # いくつか表示
    cat("採用開始: ", r, "\n")
    print(table(foo))
  }
  secretary_data <- bind_rows(secretary_data, # 前の実験結果に追加
                       tibble(r = rep(r,mc),
                              employed = foo))
}
#'
#' 各rでどのような順位が採用されたか分布を図示してみる
secretary_data |>
  ggplot(aes(x = r, y = employed)) +
  geom_boxplot(aes(group = r), # rごとに集計
               fill = "white", colour ="royalblue") +
  labs(title = paste("n =", n)) # nをタイトルに表示
#'
#' 理論的に良いとされるrの値 (nが十分大きい場合)
n/exp(1) 
#'
#' 各rで1位を採用できる確率を図示してみる
secretary_data |>
  group_by(r) |> 
  summarize(ratio = mean(employed == 1)) |> # 1位ならTRUE(1)
  ggplot(aes(x = r, y = ratio)) +
  geom_step(colour = "royalblue") + # 階段関数で描画
  geom_vline(xintercept = n/exp(1), colour = "red") + # 理論値
  labs(title = paste("n =", n)) # nをタイトルに表示
#' ---------------------------------------------------------------------------
