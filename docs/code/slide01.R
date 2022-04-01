### 第1講 練習問題解答例

### 練習問題 基本的な使い方
123 * 456 - 789
(2^2^5 + 1) / 641 # 羃乗の優先順位に注意
sin(pi/3)^2 + cos(pi/3)^2
log(exp(2)+1) # 適当な数学関数の例

### 練習問題 データフレームの取り扱い
## データフレームの作成
df <- data.frame(
  name=c("Alice","Bob","Cathy","David"),
  literature=c(90,80,70,60),
  math=c(25,50,75,100),
  english=c(65,100,70,40))
## 日本語文字を使うことも可能だが，設定により文字化けが起こるので注意
df # または Environment タグから click する

## データフレームの操作
df[,"math"]
df[3,]

## name列の代わりに行に名前を付けることもできる
df2 <- data.frame(
  literature=c(90,80,70,60),
  math=c(25,50,75,100),
  english=c(65,100,70,40))
row.names(df2) <- c("Alice","Bob","Cathy","David")
df2
df2[,"math"]
df2["Cathy",] # 行の名前で参照可能
