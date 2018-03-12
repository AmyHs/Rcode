library(httr)
url <- "http://www.bbc.com/news/world"
getUrl<-GET(url)
conUrl<-content(getUrl)
?do.call
