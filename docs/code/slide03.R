### 第03回 練習問題解答例

### 例題 三角形の面積を計算する関数

myHeron <- function(x,y,z){
    s <- (x+y+z)/2 # 補助変数 s の計算
    S <- sqrt(s*(s-x)*(s-y)*(s-z)) # ヘロンの公式による面積の計算
    return(S)
}
myHeron(3,4,5) # よく知られた直角三角形を使って計算結果を確認する
myHeron(12,13,5)

### 練習問題
## 和を計算する関数
mySum <- function(n){
    out <- sum(1:n) # 1からnまでの整数を生成して和を求める
    return(out)
}
mySum(10) # 1から10までの和は?
## 別解
mySum <- function(n){
    out <- n*(n+1)/2 # 等差数列の和を利用した場合
    return(out)
}

## 階乗を計算する関数
myFact <- function(n){
    out <- prod(1:n) # 1からnまでの整数を生成して積を求める
    return(out)
}
myFact(5) # 5! は?

### 例題 if文の例

if(20210423 %% 19 == 0) {# %% は余りを計算
    print("割り切れます") 
    print(20200724 %/% 19) # 商を表示
} else { # {}で囲まれたブロックが1つのプログラム
    print("割り切れません")
    print(20210423 %% 19) # 余りを表示
}

### 例題 for文の例

print(LETTERS) # LETTERS ベクトルの内容を表示
for(i in c(20,15,11,25,15)) {
    print(LETTERS[i]) # 順番に表示
}

### 例題 while文の例

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

### 例題 nの階乗を求める関数 (for文)

myFact1 <- function(n){
    val <- 1 # 初期値の代入
    for(i in 1:n){ # 1からnまで順に掛ける
        val <- val*i
    }
    return(val) # 計算結果を返す
}
myFact1(0) # 間違い (0!=1)
myFact1(1) # 正しい
myFact1(2) # 正しい
myFact1(3) # 正しい
myFact1(4) # 正しい

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
myFact2(0) # 正しい
myFact2(1) # 正しい
myFact2(2) # 正しい
myFact2(3) # 正しい
myFact2(4) # 正しい

myFact3 <- function(n){
    val <- 1 # 初期値の代入
    while(n>0){ # nから1まで順に掛ける．nが0なら計算しない
        val <- val*n
        n <- n-1
    }
    return(val)
}
myFact3(0) # 正しい
myFact3(1) # 正しい
myFact3(2) # 正しい
myFact3(3) # 正しい
myFact3(4) # 正しい

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
