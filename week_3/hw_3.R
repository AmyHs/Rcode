#分析實驗資料
setwd("C:/Users/b0520/Desktop")
data<-read.csv("20171027_01_1.csv")

#繪製player1&player2資產變化
library(tidyverse)
total<-ggplot(data,mapping = aes(data$trials, data$player1TotalAsset))+geom_point(color ="pink")+geom_smooth(color ="pink",se = F)+
  geom_point(aes(data$trials,data$player2TotalAsset),color ="yellow")+geom_smooth(aes(data$trials,data$player2TotalAsset),color ="yellow",se = F)

#player1&2股票與現金數量和總資產變化的關係
player1<-ggplot(data)+geom_point(aes(data$trial,data$player1Cash, shape = player1Decision),color ="red")+geom_smooth(aes(data$trials,data$player1TotalAsset),color ="lightpink")
player2<-ggplot(data)+geom_point(aes(data$trial,data$player2Cash, shape = player2Decision),color ="#006666")+geom_smooth(aes(data$trials,data$player2TotalAsset),color ="#33FFFF")                                                       
playerdata<-ggplot(data)+geom_point(aes(data$trial,data$player1Cash, shape = player1Decision),color ="red")+geom_smooth(aes(data$trials,data$player1TotalAsset),color ="lightpink")+geom_point(aes(data$trial,data$player2Cash, shape = player2Decision),color ="#006666")+geom_smooth(aes(data$trials,data$player2TotalAsset),color ="#33FFFF")

#股價與雙方買賣影響
library(reshape2)
##先把player1,2的資料轉成長資料
###本來要直接用reshape裡的melt進行，但因為要寫很多保留欄然後分別進行很多次所以放棄
data1<-select(data,starts_with("player"),-ends_with("event"),-ends_with("Stock"),stockPrice,marketCondition)
data2<-select(data,trials,player1Stock,player2Stock)%>%melt(id.var="trials",variable.name="player")%>%cbind(.,data1)
###改用各自合併再併起來的方法
data2<-select(data,trials,player1Stock,player2Stock)%>%melt(id.var="trials",variable.name="playerStock",value.name = "Stock_value")
data3<-select(data,trials,player1Cash,player2Cash)%>%melt(id.var="trials",variable.name="playerCash",value.name = "Cash_value")
data4<-select(data,trials,player1TotalAsset,player2TotalAsset)%>%melt(id.var="trials",variable.name="playerTotalAsset",value.name = "Total_value")
data5<-select(data,trials,player1Decision,player2Decision)%>%melt(id.var="trials",variable.name="playerDecision",value.name = "Decision")
data6<-select(data,trials,marketCondition,stockPrice)
###[未解決]本來要用merge合併但是併起來的方式卻導致trial數越來越多，儘管用了交集而不是聯集也沒改善
data1<-merge.data.frame(data2,data3,by = "trials",all =F)%>%merge.data.frame(.,data4,by = "trials")%>%merge.data.frame(.,data5,by = "trials")%>%merge.data.frame(.,data6,by = "trials")
###改增加ID來合併而不是用trial來合併
data2<-select(data,trials,player1Stock,player2Stock)%>%melt(id.var="trials",variable.name="playerStock",value.name = "Stock_value")%>%mutate(.,ID = 1:202)
data3<-select(data,trials,player1Cash,player2Cash)%>%melt(id.var="trials",variable.name="playerCash",value.name = "Cash_value")%>%mutate(.,ID = 1:202)
data4<-select(data,trials,player1TotalAsset,player2TotalAsset)%>%melt(id.var="trials",variable.name="playerTotalAsset",value.name = "Total_value")%>%mutate(.,ID = 1:202)
data5<-select(data,trials,player1Decision,player2Decision)%>%melt(id.var="trials",variable.name="playerDecision",value.name = "Decision")%>%mutate(.,ID = 1:202)
data6<-select(data,trials,marketCondition,stockPrice)%>%mutate(.,ID = 1:101)
data7<-select(data,trials,marketCondition,stockPrice)%>%mutate(.,ID = 101:202)
###[未解決]為什麼都會跑出trials.x trials.y 
###[未解決]data 6 行數不一樣 把data6、data7用不同ID命名,查了很多交集聯集還是無法讓資料根據ID併起來 而不會stockPrice maketCondition出現四欄 裡面有NA
data1<-right_join(data2,data3,by = "ID")%>%right_join(.,data4,by = "ID")%>%right_join(.,data5,by = "ID")%>%right_join(.,data6,by = "ID",all =T)%>%right_join(.,data7,by = "ID",all =T)
stock<-ggplot(data1,aes(data1$trials))+geom_bar(aes(y=data1$Stock_value,fill = data1$playerStock),stat="identity",position = "dodge")+geom_smooth(aes(y=data1$stockPrice),se =F)#facet_grid(.~data1$marketCondition)
