library(rvest)
library(magrittr)

#網址
url<-read_html("http://www.imdb.com/chart/top?pf_rd_m=A2FGELUUNOQJNL&pf_rd_p=4da9d9a5-d299-43f2-9c53-f0efa18182cd&pf_rd_r=17BXDA2B9WWHC96K70QD&pf_rd_s=right-4&pf_rd_t=15506&pf_rd_i=toptv&ref_=chttvtp_ql_3")
#取得電影top250
movieimg<-html_nodes(url,".posterColumn")%>%html_nodes("img")%>%html_attr("src")
title<-html_nodes(url,".titleColumn")%>%html_text(.,trim = T)
movieUrl<-html_nodes(url,".titleColumn")%>%html_nodes("a")%>%html_attr("href")%>%paste("http://www.imdb.com",.)
rating<-html_nodes(url,".imdbRating")%>%html_text()
#做成表格
df<-data.frame(image = movieimg, movie = title, rating, url = movieUrl, stringsAsFactors=FALSE)
#不需要網址、圖片只需要電影和評分的表格
df_for_reading<-df[,c(-1,-4)]

#存成csv
setwd("C:/Users/b0520/Desktop/csx_R_course")
write.table(df_for_reading,file = "TopMovies.csv",sep = ",",row.names = F)

