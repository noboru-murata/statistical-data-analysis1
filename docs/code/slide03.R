### 例題1
### Hamilton-Cayleyの定理の確認

## 行列を作成 (好きに設定してよい)
(A <- matrix(1:4,2,2)-diag(rep(3,2)))

## 左辺を計算
A%*%A - sum(diag(A)) * A + det(A) * diag(rep(1,2))

### 練習1.1
### 1から10までの2乗値からなるベクトル
1:10 # 1から10までのベクトル
1:10 * 1:10 # 2乗値のベクトル

### 練習1.2
### 1から10までの和
1:10 %*% rep(1,10) # (1,2,...,10)と(1,1,...,1)の内積

### 練習1.3
### 九九の表
matrix(rep(1:9,9),9,9) # 行ごとに1から9を並べる
matrix(rep(1:9,9),9,9,byrow=TRUE) # 列ごとに1から9を並べる
matrix(rep(1:9,9),9,9) * matrix(rep(1:9,9),9,9,byrow=TRUE)

### 練習1.4
### 30度の回転行列の2乗は60度の回転行列
theta <- pi/6 # 30度のラジアン値
R30 <- matrix(c(cos(theta),sin(theta),
                -sin(theta),cos(theta)),2,2)
R60 <- matrix(c(cos(2*theta),sin(2*theta),
                -sin(2*theta),cos(2*theta)),2,2)
R30 # 30度の回転行列
R30 %*% R30 # 30度の回転行列の2乗
R60 # 60度の回転行列

### 例題2
### 3元連立1次方程式の解法

## 行列とベクトルを作成 (好きに設定してよい)
## rnorm(9) は正規乱数を9つ作成する(第5回で詳しく説明)
(A <- matrix(rnorm(9),3,3)+diag(rep(1,3)))
(b <- 1:3)

## 解を計算
(x <- solve(A,b))
A%*%x # 結果の確認(b になるはず)

### 練習2.1
### 1から10までの2乗値からなるベクトル
(1:10)^2 # ^2も関数として成分ごとに計算される

### 練習2.2
### 回転してもベクトルの長さが変わらないことを確認
## 回転行列とベクトルを作成 (好きに設定してよい)
theta <- 2*pi/3 # 120度のラジアン値
(R <- matrix(c(cos(theta),sin(theta),
              -sin(theta),cos(theta)),2,2))
(x <- 1:2)
(y <- R %*% x) # xを回転してyを作成
## 長さの2乗はベクトルの内積で計算できる
x %*% x # xの長さの2乗
as.vector(y) %*% as.vector(y) # yの長さの2乗

### 練習2.3
### エラーになる理由を考察
A %*% b # 列ベクトル (3x1型行列)
b %*% A # 行ベクトル (1x3型行列)
A %*% b + b %*% A # 大きさが異なるので計算できない

### 例題3
### if文の例

if(20200724 %% 19 == 0) {# %% は余りを計算
    print("割り切れます")
    print(20200724 %/% 19) # 商を表示
} else { # {}で囲まれたブロックが1つのプログラム
    print("割り切れません")
    print(20200724 %% 19) # 余りを表示
}

### 例題4
### for文の例

print(LETTERS) # LETTERS ベクトルの内容を表示
for(i in c(20,15,11,25,15)) {
    print(LETTERS[i]) # 順番に表示
}

### 例題5
### while文の例

n <- 20200809 # 分解の対象
p <- 2 # 最初に調べる数
while(n != 1){ # 商が1になるまで計算する
    for(i in p:n){ # pからnまで順に調べる
        if(n%%i == 0) { # 余りが0か確認
            print(i) # 割り切った数を表示
            n <- n/i # 商を計算して分解の対象を更新
            p <- i # 最初に調べる数を更新
            break # for文を途中で終了
        }  
    }
}

### 例題6
### 三角形の面積を計算する関数

area <- function(x,y,z){
    s <- (x+y+z)/2
    S <- (s*(s-x)*(s-y)*(s-z))^(1/2)
    ## S <- sqrt(s*(s-x)*(s-y)*(s-z)) # 平方根を求める関数を用いても良い
  return(S)
}
area(3,4,5) # 直角三角形で検算
area(12,13,5)

### 練習3.1
### 階乗を計算する関数
## for文を用いた関数
fact1 <- function(n){
    val <- 1
    for(i in 1:n){
        val <- val*i
    }
    return(val)
}
fact1(0) # 間違い
fact1(1)
fact1(2)
fact1(3)
fact1(4)
## if文を用いた修正版
fact2 <- function(n){
    if(n==0){
        return(1)
    } else {
        val <- 1
        for(i in 1:n){
            val <- val*i
        }
        return(val)
    }
}
fact2(0) # 正しい
fact2(1)
fact2(2)
fact2(3)
fact2(4)
## while文を用いた関数
fact3 <- function(n){
    val <- 1
    while(n>0){
        val <- val*n
        n <- n-1
    }
    return(val)
}
fact3(0)
fact3(1)
fact3(2)
fact3(3)
fact3(4)

### 練習3.2
### Fibonacci数を返す関数
fibo <- function(n){
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

### 練習3.3
### 行列の列の平均を計算する関数
colave <- function(X) {
    ave <- rep(0,length=ncol(X)) # 平均を記録するベクトルを用意
    for(i in 1:ncol(X)){ # 列ごとに計算
        ave[i] <- sum(X[,i])/nrow(X) # 平均の定義に従って計算
        ## ave[i] <- mean(X[,i]) # 平均を計算する関数を用いても良い
    }
    return(ave)
}
(A <- matrix(1:12,3,4,byrow=TRUE))
colave(A)

### 練習3.4
### ベクトルと行列を扱えるように修正
colave <- function(X){ 
    if(is.vector(X)){
        ave <- mean(X)
    } else {
        ave <- rep(0,length=ncol(X))
        for(i in 1:ncol(X)){
            ave[i] <- mean(X[,i])
        }
    }
    return(ave)
}
(A <- matrix(1:12,3,4,byrow=TRUE))
colave(A)
(x <- 1:12)
colave(x)
