library(rvest)

#read_html 會回傳一個 xml_document 物件。
#使用這些函數時要注意函數要吃的input、output是什麼。
#這樣才知道怎麼樣把不同的輸入輸出接在一起。
bbcread<-read_html("http://www.bbc.com/news/entertainment_and_arts")

#不能用html欸 差在哪??
bbc<-html("http://www.bbc.com/news/entertainment_and_arts")

#取得href屬性(網址)失敗??
x_1<-bbcread %>% html_nodes(".title-link__title-text")%>% html_attr("href")
?html_attr()

#只取得文字
x_2<-bbcread %>% html_nodes(".title-link__title-text") %>% html_text() 

bbcTitle<-data.frame(bbc_news_title=x_2)
