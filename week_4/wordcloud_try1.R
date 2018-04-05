install.packages("wordcloud2")

getwd
install.packages("rJava")#Rwordseg依???于rJava
#無法裝Rwordseg
install.packages("Rwordseg", repos="http://R-Forge.R-project.org")
install.packages("tmcn", repos="http://R-Forge.R-project.org")
install.packages("tm")
install.packages("jiebaR")
library(rJava)
library(magrittr)
library(dplyr)
library(Rfacebook)
library(tm)
library(tmcn)
library(Rwordseg)#無法
library(jiebaR)




#直接用graph API
prefex="184899118190398"
token ="EAACEdEose0cBAF9HMuGHulkhMKCmw3NbHoeOtuyFxjccgmfTD9ixICZAhZAONemZBzai7QEhWaEtaIodez0ZCZAW6fZAqKl4fiy3WUYND6xlzPsT8j5ZC3jNZB8Wv85OglSHignBSYQIHBQgChxCzFjgDDe7WnsZCXpQQrW30I9FAQuVkhfk0g5xRWb0CrhCPhHEZD"
page<-getPage(prefex,token,n=300)
docs<-Corpus(VectorSource(page$message))


#斷詞
cut<-worker()
docs2<-segment(as.character(docs),cut)
docs3<-Corpus(VectorSource(docs2))

#清洗
toSpace <- content_transformer(function(x, pattern) {
  return (gsub(pattern, " ", x))}
)
inspect(docs3)
docs3<-tm_map(docs3, toSpace, "[A-Za-z0-9]")
docs3<-tm_map(docs3, removePunctuation)
docs3<-tm_map(docs3, removeNumbers)
docs3<-tm_map(docs3, stripWhitespace)
#覺得停止詞包不好用所以不用?
docs3<-tm_map(docs3,removeWords,stopwordsCN())

#不用
docs3 <- tm_map(docs3,toSpace, "為")
docs3 <- tm_map(docs3,toSpace, "都")
docs3 <- tm_map(docs3,toSpace,"不")
docs3 <- tm_map(docs3,toSpace,"與")
docs3 <- tm_map(docs3,toSpace, "個")
docs3 <- tm_map(docs3,toSpace, "讓")
docs3 <- tm_map(docs3,toSpace, "什麼")
docs3 <- tm_map(docs3,toSpace, "對")
docs3 <- tm_map(docs3,toSpace, "很")
docs3 <- tm_map(docs3,toSpace, "是")
docs3 <- tm_map(docs3,toSpace, "上")
docs3 <- tm_map(docs3,toSpace, "中")
docs3 <- tm_map(docs3,toSpace, "這")
docs3 <- tm_map(docs3,toSpace, "嗎")
docs3 <- tm_map(docs3,toSpace, "這樣")
docs3 <- tm_map(docs3,toSpace, "卻")
docs3 <- tm_map(docs3,toSpace, "最")
docs3 <- tm_map(docs3,toSpace, "著")
inspect(docs2)

#wordcloud
library(wordcloud2)
#Encoding(docs1[1])  <- "UTF-8"
temp <- Corpus(VectorSource(docs1[1]))
docmat<- TermDocumentMatrix(temp, control=list(wordLengths = c(1, Inf)))
#docmat<- TermDocumentMatrix(temp)
docmat<-as.matrix(docmat)
fre <- sort(rowSums(docmat), decreasing = TRUE)
df<- data.frame(word = names(fre), freq = fre)
wordcloud2(df)

#拿來做w4分析

page1<-select(page,-starts_with("from"),-type,-story)%>%
  filter()

group<-matrix(unlist(page))







###無法解決這個問題 放棄
fb_oauth<-fbOAuth("212369089345583","887971b2d1dfcdd536da260a4c0799bb")
###嘗試使用tmcn，但並未成功
stopwords<-c(stopwordsCN(),"但是","有","還是","了","的")
###改用jiebaR 還是失敗 
setwd("C:/Users/b0520/Desktop/csx_R_course")
stopwords<-c("但","是","的","有","沒","還","了")
write.table(stopwords,"stopwords.txt",sep = " ",row.names = FALSE)
tb<-read.table("stopwords.txt")
iconv(tb,"UTF-8")
cutter = worker(stop_word = "stopwords.txt")
###想要用Rwordseg斷詞但是載入失敗
docs3<- tm_map(docs2, segmentCN, nature = TRUE)
inspect(docs2)
docs7<-Corpus(VectorSource(docs2))
docs7<-tm_map(docs7, toSpace, "[A-Za-z0-9]")
docs7<-tm_map(docs7, removePunctuation)
docs7<-tm_map(docs7, removeNumbers)
docs7<-tm_map(docs7, stripWhitespace)
docs7<-tm_map(docs7,removeWords,stopwordsCN())
docmat2<- TermDocumentMatrix(docs7, control = list(wordLengths = c(2, Inf)))%>%as.matrix()
fre2 <- sort(rowSums(docmat2), decreasing = TRUE)
df2<- data.frame(word = names(fre2), freq = fre2)

wordcloud2(df)