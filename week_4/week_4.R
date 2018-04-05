#1.取得fb上的post
library(Rfacebook)
id<-"184899118190398"
token<-"EAACEdEose0cBAKOqO9zOMPReUgcYhWwIwA1dVhERCs7NxIDYfkeK35p8PDYMHNlZCJNKUdc09OWY8waqswaJBwhmhguZB9nTbx2MP8evF8kDSNwZAiv9LFSZArmwRGzXkbWRv3eNVO10gqBjVEi2zXr2KdRWXaCcdExc02CSNqbzWNICp8T0Y3uupMzvDs4ZD"
post<-getPage(id,token,n=500)

#2.最受歡迎的前300篇裡的文字
library(dplyr)
library(magrittr)
post1<-arrange(post,desc(likes_count))
doc<-post1[1:300,]%$%message

#3.文字整理
#3.1斷詞
library(jiebaR)
cut=worker()
newword<-c("一個人","被愛","對的人")
new_user_word(cut,newword)
word<-jiebaR::segment(doc,cut)
#3.2清洗文字
library(tm)
word1<-VCorpus(VectorSource(word))
##非tm自帶函數用content_transformer()“包???”起???才能用
space<-content_transformer(function (x, pattern){gsub(pattern," ",x)})
word2<-tm_map(word1,space,"[a-zA-Z]")
word2<-tm_map(word2,removeNumbers)
word2<-tm_map(word2,removeWords,stopwordsCN())

 #4.計算詞頻
wordnum<-length(word2)
allword<-list()
allfreq<-list()
##此作法沒有把字先append起來會不見，因為老師的doc已經是很多字在裡面，我的是一個一個
for(i in 1:wordnum){
  word<-as.data.frame(table(word2[[i]][1]))
  allword<-append(allword,word)
}

qq<-as.data.frame(table(allword))
View(head(qq))
