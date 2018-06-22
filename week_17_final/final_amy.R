#install.packages("jiebaR")
#install.packages("tm")
#install.packages("tmcn")
library(NLP)
library(tm)
library(tmcn)
library(stringr)
library(jiebaRD)
library(jiebaR)

setwd("C:/Users/b0520/Desktop/r project/")
#編碼(讀文檔有亂碼)
#Sys.getlocale()
#Sys.setlocale("LC_ALL", 'en.us.UTF-8')
##但是沒用QQQ

#文本
#天啊編碼搞死我
Ti <- read.table(file = "太宰治-人間失格.txt", stringsAsFactors = F, fill = T, fileEncoding = "UTF-8-BOM")
Ti <- unlist(Ti)
cc = worker()
test <- cc[Ti]
test
View(test)

Nan <- read.table(file = "南康-我等你到三十五歲.txt", stringsAsFactors = F, fill = T, fileEncoding = "UTF-8-BOM")
Nan <- unlist(Nan)
testn <- cc[Nan]
View(testn)

Hi <- read.table(file = "海子-短詩.txt", stringsAsFactors = F , fill = T, fileEncoding = "UTF-8-BOM")
Hi <- unlist(Hi)
testh <- cc[Hi]
View(testh)

Mon <- read.table(file = "邱妙津-蒙馬特遺書.txt", stringsAsFactors = F, fill = T, fileEncoding = "UTF-8-BOM")
Mon <- unlist(mon)
testm <- cc[Mon]
View(testm)
