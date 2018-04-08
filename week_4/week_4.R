#1.取得fb上的post
library(Rfacebook)
id<-"184899118190398"
token<-"EAACEdEose0cBAO8kU8MBlZCFymdZBuddkKpd0UOBjdGgOZB3pzO1COCujgKwAx8byMQE6WnfSk6cJZCZCwyfHoR5QAtUZAYeyI2m4eD1VHrd4uBekanNU0AVldJxKiRruFc6GEJsBeZAnGPXsXpM5UbiTm88cvHW3OjmjnQ5ZA23wuu8hMO3EAxMY6hAnGgIU2AZD"
post<-getPage(id,token,n=500)

#2.最受歡迎的前300篇裡的文字
library(dplyr)
library(magrittr)
post1<-arrange(post,desc(likes_count))
doc<-post1[1:300,]%$%message

#3.文字整理
#3.1清洗文字
library(tm)
docs1<-VCorpus(VectorSource(doc))
##非tm自帶函數用content_transformer()包起才能用
space<-content_transformer(function (x, pattern){gsub(pattern," ",x)})
docs2<-tm_map(docs1,space,"[a-zA-Z]")
docs2<-tm_map(docs2,removeNumbers)
docs2<-tm_map(docs2, removePunctuation)
docs2<-tm_map(docs2, stripWhitespace)

#3.2斷詞&去除停用字
library(jiebaR)
setwd("C:/Users/b0520/Desktop")
cut=worker(,stop_word ="stop.txt")
newword<-c("一個人","被愛","對的人")
new_user_word(cut,newword)
allword<-list()
docsnum<-length(docs2)
for(i in 1:docsnum){
   word<-list(jiebaR::segment(unlist(docs2[[i]][1]),cut))
   allword<-append(allword,word)
}

#4.計算詞頻
allfreq<-list()
for(i in 1:docsnum){
  freq<-list(as.data.frame(table(allword[i])))
  colnames(freq[[1]])<-c("word","freq")
  allfreq<-append(allfreq,freq)
}
#5.文字雲
jieba_tokenizer=function(d){
  unlist(segment(d[[1]],cut))
}
seg = lapply(docs2, jieba_tokenizer)
freqFrame <- as.data.frame(table(unlist(seg)))
freqFrame <-freqFrame[-c(1:27),]
wordcloud(freqFrame$Var1,freqFrame$Freq)

#為什麼文字雲畫不出來嗚嗚嗚
library(wordcloud2)
letterCloud(freqFrame, word = "F",color = "random-light",backgroundColor = "black",size = 0.3)

#5.300篇文章的字詞詞頻合併
alltable = allfreq[[1]]
for( m in 1:299 )
{
  alltable = merge(alltable, allfreq[[m+1]], by="word",all = T)
}


