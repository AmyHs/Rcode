#1.???b銝?ost
library(Rfacebook)
id<-"184899118190398"
token<-"EAACEdEose0cBAO8kU8MBlZCFymdZBuddkKpd0UOBjdGgOZB3pzO1COCujgKwAx8byMQE6WnfSk6cJZCZCwyfHoR5QAtUZAYeyI2m4eD1VHrd4uBekanNU0AVldJxKiRruFc6GEJsBeZAnGPXsXpM5UbiTm88cvHW3OjmjnQ5ZA23wuu8hMO3EAxMY6hAnGgIU2AZD"
post<-getPage(id,token,n=500)

#2.????迭餈???300蝭ㄐ?????
library(dplyr)
library(magrittr)
post1<-arrange(post,desc(likes_count))
doc<-post1[1:300,]%$%message

#3.??????
#3.1皜????
library(tm)
docs1<-VCorpus(VectorSource(doc))
##??m?撣嗅??content_transformer()??絲???
space<-content_transformer(function (x, pattern){gsub(pattern," ",x)})
docs2<-tm_map(docs1,space,"[a-zA-Z]")
docs2<-tm_map(docs2,removeNumbers)
docs2<-tm_map(docs2, removePunctuation)
docs2<-tm_map(docs2, stripWhitespace)

#3.2?閰?&????摮?
library(jiebaR)
setwd("C:/Users/b0520/Desktop")
cut=worker(,stop_word ="stop.txt")
newword<-c("銝?犖","鋡急??","撠?犖")
new_user_word(cut,newword)
allword<-list()
docsnum<-length(docs2)
for(i in 1:docsnum){
   word<-list(jiebaR::segment(unlist(docs2[[i]][1]),cut))
   allword<-append(allword,word)
}

#4.閮??
allfreq<-list()
for(i in 1:docsnum){
  freq<-list(as.data.frame(table(allword[i])))
  colnames(freq[[1]])<-c("word","freq")
  allfreq<-append(allfreq,freq)
}
#5.???
jieba_tokenizer=function(d){
  unlist(segment(d[[1]],cut))
}
seg = lapply(docs2, jieba_tokenizer)
freqFrame <- as.data.frame(table(unlist(seg)))
freqFrame <-freqFrame[-c(1:27),]
wordcloud(freqFrame$Var1,freqFrame$Freq)

#?隞暻潭???銝靘????
library(wordcloud2)
letterCloud(freqFrame, word = "F",color = "random-light",backgroundColor = "black",size = 0.3)

#5.300蝭????????蔥
alltable = allfreq[[1]]
for( m in 1:299 )
{
  alltable = merge(alltable, allfreq[[m+1]], by="word",all = T)
}


