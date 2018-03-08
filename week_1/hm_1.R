### hw_1_question


########################################################### Task 1

# 查看內建資料集: 鳶尾花(iris)資料集
require(datasets)
data(iris)

# 使用dim(), 回傳iris的列數與欄數
dim(iris)

# 使用head() 回傳iris的前六列
head(iris, n = 6)

# 使用tail() 回傳iris的後六列
tail(iris, n=6)

# 使用str() 
str(iris)

# 使用summary() 查看iris敘述性統計、類別型資料概述。
summary(iris)

########################################################### Task 2

# 使用for loop 印出九九乘法表
# Ex: (1x1=1 1x2=2...1x9=9 ~ 9x1=9 9x2=18... 9x9=81)


for(a in c(1:9)){
  for(b in c(1:9)){
    c<-paste( a,"x",b,"=",a*b)
    print(c)}
  }

for(a in c(1:9)){
  b<-paste("偶數且大於50:",2*a+50))
  print(b)}

########################################################### Task 3

# 使用sample(), 產出10個介於10~100的整數，並存在變數 nums
x<-10:100
nums<-sample(x,10)

# 查看nums
nums

# 1.使用for loop 以及 if-else，印出大於50的偶數，並提示("偶數且大於50": 數字value)
# 2.特別規則：若數字為66，則提示("太66666666666了")並中止迴圈。
for( i in c(1:100)){
 if (50+2*i != 66){
  m<-paste("偶數且大於50:",50+2*i)
  print(m)
  }
  
 else if(50+2*i>= 66){
  print("太66666666666了")
  break}
}  

x=800
if (x%%4 !== 0){
  a<-paste(X,"不是閏年")
  print(a)
}
else if(x%%100== 0 & x%%400 !=0){
  b<-paste(x,"不是閏年")
  print(b)
}
else if(x%%4 ==0 & x%%100 !=0){
  c<-paste(x,"是閏年")
  print(c)
}
else if (x%%400 == 0){
  d<-paste(x,"是閏年")
  print(d)
}
########################################################### Task 4

# 請寫一段程式碼，能判斷輸入之西元年分 year 是否為閏年
year<-function(x){
  if (x%%4 !== 0){
    a<-paste(x,"不是閏年")
    print(a)
  }
  else if(x%%100 == 0 & x%%400 !=0){
    b<-paste(x,"不是閏年")
    print(b)
  }
  else if(x%%4 == 0 & x%%100 !=0){
    c<-paste(x,"是閏年")
    print(c)
  }
  else if (x%%400 == 0){
    d<-paste(x,"是閏年")
    print(d)
  }
}

########################################################### Task 5

# 猜數字遊戲
# 1. 請寫一個由電腦隨機產生不同數字的四位數(1A2B遊戲)
# 2. 玩家可重覆猜電腦所產生的數字，並提示猜測的結果(EX:1A2B)
# 3. 一旦猜對，系統可自動計算玩家猜測的次數
GuessNum<-function(h)
  