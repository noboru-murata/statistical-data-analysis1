### 第3講 サンプルコード

#' @exercise R言語における関数

sin(x = pi/2) # "引数名 = 値" で指定 
sin(pi/2) # 上と同値 (引数と値の関係が明かなら引数名は省略可能)

help(log) # ヘルプを表示して使い方を確認する
x <- 16; b <- 2 # xやbに適当な数値を代入する．1行で複数の処理を行う場合は ; を用いて並べる
log(x = x, base = b) # 底をbとする対数
log(x, b) # 上と同値
log(base = b, x = x) # 上と同値
log(b, x) # 上と異なる (=log(x = b, base = x))
log(x) # 自然対数 (既定値による計算 =log(x, base = exp(1)))

#' 正規乱数を生成する関数
help(rnorm) # Help タブから指定しても良い
#' ヒストグラムを表示する関数
?hist # help(hist) と同値

rnorm(5) # 平均0 分散1 の正規乱数を5個生成
rnorm(5, mean = 10) # 平均10 分散1 の正規乱数を5個生成
rnorm(sd = 0.1, n = 5) # 平均0 分散0.01 の正規乱数を5個生成
rnorm(n = 5, mean = 2, sd = 2) # 平均2 分散4 の正規乱数を5個生成

foo <- rnorm(n = 10000, mean = 50, sd = 10) # 平均50 標準偏差10 の正規乱数
hist(foo) # データ以外全て既定値で表示
hist(foo, # 既定値のいくつかを変更する
     breaks = 30, # ビンを30程度に調整する
     col = "lightgreen", # 色の指定
     main = "mathematics", # タイトルの指定
     xlab = "score") # x軸ラベルの指定
#' Plots タブで描画結果を確認

#' ---------------------------------------------------------------------------
#' @practice 関数の使い方
#'
#' sample を調べる
help(sample)
#' サイコロを1回振る試行
sample(x = 1:6, size = 1)
#' サイコロを10回振る試行
sample(x = 1:6, size = 10) # エラーになる
sample(x = 1:6, size = 10, replace = TRUE)
#' 1が他の目の3倍出易いサイコロ
sample(1:6, 10, replace = TRUE, prob = c(3, rep(1,5))) # 引数の一部を省略
#' 1から6をランダムに並べ替える
sample(1:6, 6) # 既定値ではreplaceしない(復元抽出しない)ので並べ替えになる
#' ---------------------------------------------------------------------------

#' @exercise 関数の定義

foo <- function(r){
  volume <- (4/3) * pi * r^3 # 球の体積
  surface <- 4 * pi * r^2     # 球の表面積
  out <- c(volume, surface) # 返り値のベクトルを作る
  names(out) <- c("volume", "surface_area") # 返り値の要素に名前を付ける
  return(out) # 値を返す
}
foo(r = 2) # 実行
foo(3)

bar <- function(a, r, n = 5){
  out <- a*r^(1:n-1) # 1:n-1 と 1:(n-1) は異なるので注意
  return(out) # 値を返す
}
bar(1, 2) # 初項1 公比2 の最初の5項
bar(1, 2, 10) # 初項1 公比2 の最初の10項
bar(n = 10, 1, 2) # 変数名を指定すると引数の位置を変えることができる
bar(r = 0.5, n = 10, a = 512) # 同上

#' @exercise 三角形の面積を計算する関数

my_heron <- function(x, y, z){
  #' 関数名は上書きされるので独特の名前にするのがお薦め
  s <- (x+y+z)/2 # 補助変数 s の計算
  S <- sqrt(s*(s-x)*(s-y)*(s-z)) # ヘロンの公式による面積の計算
  return(S) # 面積を返す
}
my_heron(3, 4, 5) # よく知られた直角三角形を使って計算結果を確認する
my_heron(12, 13, 5)

#' ---------------------------------------------------------------------------
#' @practice 自作関数の定義
#'
#' 和 (summation) を計算する関数
my_sum <- function(n){
  out <- sum(1:n) # 1からnまでの整数を生成して和を求める
  return(out)
}
my_sum(10) # 1から10までの和は?
#' 別解
my_sum2 <- function(n){
  out <- n*(n+1)/2 # 等差数列の和を利用した場合
  return(out)
}
my_sum2(10)
#' 階乗 (factorial) を計算する関数
my_fact <- function(n){
  out <- prod(1:n) # 1からnまでの整数を生成して積を求める
  return(out)
}
my_fact(5) # 5! は?
#' ---------------------------------------------------------------------------

#' @exercise 制御構造

today <- 20240426 # 今日の日付
if(today %% 19 == 0) {# %% は余りを計算
  print("割り切れます．商は以下の値です．") 
  print(today %/% 19) # 商を表示
} else { # {}で囲まれたブロックが1つのまとまった処理に対応する
  print("割り切れません．余りは以下の値です．")
  print(today %% 19) # 余りを表示
}

print(LETTERS) # LETTERS ベクトルの内容を表示
for(i in c(20,15,11,25,15)) {
  print(LETTERS[i]) # 順番に表示
}

(n <- 2*11*17*31) # 分解の対象．今日の日付や my_fact(5) なども試してみよ
p <- 2 # 最初に調べる数
while(n != 1){ # 商(for文の中で計算している)が1になるまで計算する
  if(n%%p == 0) { # 余りが0か確認
    print(p) # 割り切った数を表示
    n <- n/p # 商を計算して分解の対象を更新
  } else {
    p <- p+1 # 割り切れない場合は次の数を調べる
  } # 更新される p は素数とは限らないのに上手く動く理由を考えてみよう
}

#' @exercise nの階乗を求める関数 (for文を用いた例)

my_fact1 <- function(n){
  val <- 1 # 初期値の代入
  for(i in 1:n){ # 1からnまで順に掛ける
    val <- val*i
  }
  return(val) # 計算結果を返す
}
my_fact1(4) # 正しい
my_fact1(3) # 正しい
my_fact1(2) # 正しい
my_fact1(1) # 正しい
my_fact1(0) # 間違い (0!=1)

#' @exercise nの階乗を求める関数 (if文を用いた修正版)

my_fact2 <- function(n){
  if(n==0){ # n=0 か確認して分岐する
    return(1)
  } else {
    val <- 1
    for(i in 1:n){ val <- val*i }
    return(val)
  }
}
my_fact2(4) # 正しい
my_fact2(3) # 正しい
my_fact2(2) # 正しい
my_fact2(1) # 正しい
my_fact2(0) # 正しい

#' @exercise nの階乗を求める関数 (while文を用いた例)

my_fact3 <- function(n){
  val <- 1 # 初期値の代入
  while(n>0){ # nから1まで順に掛ける．nが0なら計算しない
    val <- val*n
    n <- n-1
  }
  return(val)
}
my_fact3(4) # 正しい
my_fact3(3) # 正しい
my_fact3(2) # 正しい
my_fact3(1) # 正しい
my_fact3(0) # 正しい

#' @notes
#' 再帰を用いて書くこともできる
my_fact4 <- function(n){
  if(n<0) return(NA)
  if(n == 0 | n == 1) return(1)
  return(n * my_fact4(n-1))
}
sapply(-2:5, my_fact4) # -2から5を my_fact4 で計算した結果を返す

#' ---------------------------------------------------------------------------
#' @practice 制御構造
#'
#' Fibonacci 数を返す関数
my_fibo <- function(n){
  f0 <- 0 # 第0項の設定
  f1 <- 1 # 第1項の設定
  if(n<0) {
    print("計算できません")
    return(NA) # 欠損値を返す
  }
  if(n==0) { # n=0の場合
    return(f0)
  }
  if(n==1) { # n=1の場合
    return(f1)
  }
  for(i in 2:n) { # n>=2の場合
    fn <- f1 + f0 # fn = fn-1 + fn-2 の計算
    f0 <- f1 # fn-2 の値の更新 (f0が覚えておく)
    f1 <- fn # fn-1 の値の更新 (f1が覚えておく)
  }
  return(fn) # 計算結果を返す
}
#' Fibonacci 数を6項目まで計算
c(my_fibo(1),my_fibo(2),my_fibo(3),my_fibo(4),my_fibo(5),my_fibo(6))
#' 同じ関数に別の値を入れて計算する方法はいくつか用意されている
sapply(1:30, my_fibo)
#'
#' 行列の列の平均を計算する関数
my_col_ave <- function(X) {
  ave <- rep(0, length = ncol(X)) # 平均を記録するベクトルを用意
  for(i in 1:ncol(X)){ # 列ごとに計算
    ave[i] <- sum(X[,i])/nrow(X) # 平均の定義に従って計算
    ## ave[i] <- mean(X[,i]) # 平均を計算する関数を用いても良い
  }
  return(ave)
}
(A <- matrix(1:12,3,4,byrow=TRUE))
my_col_ave(A) # 正しい答えを返す
(x <- 1:12)
my_col_ave(x) # うまく動かない
#'
#' ベクトルと行列を扱えるように修正
my_col_ave <- function(X){ 
  if(is.vector(X)){ # ベクトルとそれ以外の場合分け
    ave <- mean(X)
  } else { # ベクトル以外は行列と想定して計算
    ave <- rep(0,length=ncol(X))
    for(i in 1:ncol(X)){
      ave[i] <- mean(X[,i])
    }
  }
  return(ave)
}
my_col_ave(A)
my_col_ave(x) # 正しい答えを返す
#' ---------------------------------------------------------------------------
