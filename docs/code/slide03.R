### 第03回 練習問題解答例

### R言語における関数で用いた例

sin(x = pi/2) # "引数名 = 値" で指定 
sin(pi/2) # 上と同値 (引数と値の関係が明かなら引数名は省略可能)

help(log) # ヘルプを表示して使い方を確認する
x <- 16; b <- 2 # xやbに適当な数値を代入する．複数コマンドは ; で区切る
log(x=x, base=b) # 底をbとする対数
log(x, b) # 上と同値
log(base=b, x=x) # 上と同値
log(b,x) # 上と異なる (=log(x=b,base=x))
log(x) # 自然対数 (既定値による計算 =log(x,base=exp(1)))

## 正規乱数を生成する関数
help(rnorm) # Help Pane から指定しても良い
## ヒストグラムを表示する関数
?hist

rnorm(7) # 平均0 分散1 の正規乱数を7個生成
rnorm(7, mean=10) # 平均10 分散1 の正規乱数を7個生成
rnorm(sd=0.1, n=7) # 平均0 分散0.01 の正規乱数を7個生成
rnorm(n=7, mean=2, sd=2) # 平均2 分散4 の正規乱数を7個生成

foo <- rnorm(n=10000, mean=50, sd=10) # 平均50 標準偏差10 の正規乱数
hist(foo) # データ以外全て既定値で表示
hist(foo, # 既定値のいくつかを変更する
     breaks=30, # ビンを30程度に調整する
     col="lightgreen", # 色の指定
     main="mathematics", # タイトルの指定
     xlab="score") # x軸ラベルの指定
## Plots Pane に着目

### 練習問題 関数の使い方
## sample を調べる
help(sample)
## サイコロを1回振る試行
sample(x=1:6, size=1)
## サイコロを10回振る試行
sample(x=1:6, size=10) # エラーになる
sample(x=1:6, size=10, replace=TRUE)
## 1が他の目の3倍出易いサイコロ
sample(1:6, 10, replace=TRUE, prob=c(3,rep(1,5))) # 引数の一部を省略
## 1から6をランダムに並べ替える
sample(1:6, 6) # 既定値ではreplaceしない(復元抽出)ので並べ替えになる

### 関数の定義で用いた例

foo <- function(r){
  V <- (4/3) * pi * r^3 # 球の体積
  S <- 4 * pi * r^2     # 球の表面積
  out <- c(V,S) # 返り値のベクトルを作る
  names(out) <- c("volume", "area") # 返り値の要素に名前を付ける
  return(out) # 値を返す
}
foo(r=2) # 実行
foo(3)

bar <- function(a, r, n=5){
  out <- a*r^(1:n-1) # 1:n-1 と 1:(n-1) は異なるので注意
  return(out) # 値を返す
}
bar(1,2) # 初項1 公比2 の最初の5項
bar(1,2,10) # 初項1 公比2 の最初の10項
bar(n=10,1,2) # 変数名を指定すると引数の位置を変えることができる
bar(r=0.5,n=10,a=512) # 同上

### 例題 三角形の面積を計算する関数

myHeron <- function(x,y,z){
  ## 関数名は上書きされるので独特の名前にするのがお薦め
  s <- (x+y+z)/2 # 補助変数 s の計算
  S <- sqrt(s*(s-x)*(s-y)*(s-z)) # ヘロンの公式による面積の計算
  return(S) # 面積を返す
}
myHeron(3,4,5) # よく知られた直角三角形を使って計算結果を確認する
myHeron(12,13,5)

### 練習問題 自作関数の定義
## 和 (summation) を計算する関数
mySum <- function(n){
  out <- sum(1:n) # 1からnまでの整数を生成して和を求める
  return(out)
}
mySum(10) # 1から10までの和は?
## 別解
mySum2 <- function(n){
  out <- n*(n+1)/2 # 等差数列の和を利用した場合
  return(out)
}
mySum2(10)
## 階乗 (factorial) を計算する関数
myFact <- function(n){
  out <- prod(1:n) # 1からnまでの整数を生成して積を求める
  return(out)
}
myFact(5) # 5! は?

### 制御構造で用いた例

if(20220422 %% 19 == 0) {# %% は余りを計算
  print("割り切れます") 
  print(20220422 %/% 19) # 商を表示
} else { # {}で囲まれたブロックが1つのプログラム
  print("割り切れません")
  print(20220422 %% 19) # 余りを表示
}

print(LETTERS) # LETTERS ベクトルの内容を表示
for(i in c(20,15,11,25,15)) {
  print(LETTERS[i]) # 順番に表示
}

n <- 20220422 # 分解の対象．2*2*3*19/myFact(5) なども試してみよ
p <- 2 # 最初に調べる数
while(n != 1){ # 商(for文の中で計算している)が1になるまで計算する
  if(n%%p == 0) { # 余りが0か確認
    print(p) # 割り切った数を表示
    n <- n/p # 商を計算して分解の対象を更新
  } else {
    p <- p+1 # 割り切れない場合は次の数を調べる
  } # 更新される p は素数とは限らないが，上手く動く理由は考えてみよ
}

### 例題 nの階乗を求める関数 (for文)

myFact1 <- function(n){
  val <- 1 # 初期値の代入
  for(i in 1:n){ # 1からnまで順に掛ける
    val <- val*i
  }
  return(val) # 計算結果を返す
}
myFact1(4) # 正しい
myFact1(3) # 正しい
myFact1(2) # 正しい
myFact1(1) # 正しい
myFact1(0) # 間違い (0!=1)

myFact2 <- function(n){
  if(n==0){ # n=0 か確認して分岐する
    return(1)
  } else {
    val <- 1
    for(i in 1:n){ 
      val <- val*i
    }
    return(val)
  }
}
myFact2(4) # 正しい
myFact2(3) # 正しい
myFact2(2) # 正しい
myFact2(1) # 正しい
myFact2(0) # 正しい

myFact3 <- function(n){
  val <- 1 # 初期値の代入
  while(n>0){ # nから1まで順に掛ける．nが0なら計算しない
    val <- val*n
    n <- n-1
  }
  return(val)
}
myFact3(4) # 正しい
myFact3(3) # 正しい
myFact3(2) # 正しい
myFact3(1) # 正しい
myFact3(0) # 正しい

### 練習問題 制御構造
## Fibonacci 数を返す関数
myFibo <- function(n){
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
    f0 <- f1 # fn-2 の値の更新
    f1 <- fn # fn-1 の値の更新
  }
  return(fn) # 計算結果を返す
}
## Fibonacci 数を6項目まで計算
c(myFibo(1),myFibo(2),myFibo(3),myFibo(4),myFibo(5),myFibo(6))
## 同じ関数に別の値を入れて計算する方法はいくつか用意されている
sapply(1:30, myFibo)

## 行列の列の平均を計算する関数
myColAve <- function(X) {
  ave <- rep(0,length=ncol(X)) # 平均を記録するベクトルを用意
  for(i in 1:ncol(X)){ # 列ごとに計算
    ave[i] <- sum(X[,i])/nrow(X) # 平均の定義に従って計算
    ## ave[i] <- mean(X[,i]) # 平均を計算する関数を用いても良い
  }
  return(ave)
}
(A <- matrix(1:12,3,4,byrow=TRUE))
myColAve(A) # 正しい答えを返す
(x <- 1:12)
myColAve(x) # うまく動かない

## ベクトルと行列を扱えるように修正
myColAve <- function(X){ 
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
myColAve(A)
myColAve(x) # 正しい答えを返す
##
