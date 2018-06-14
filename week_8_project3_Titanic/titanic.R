#資料
library(tidyverse)
setwd("C:/Users/b0520/Desktop/csx_R_course/106-2RSampleCode/week_8/task_8")
train<-read.csv("titanicTrain.csv")
train<-train[-c(1001:1310),]
quest<-read.csv("titanicQuestion.csv")
all<-rbind(train,train)
#資料特徵
str(train)

#資料特徵缺失值
is_na<-function(x){
  sum(is.na(x))
}
sapply(train,is_na)
sapply(quest,is_na)
sapply(all,is_na)#怎麼fare變成0缺失
quest[is.na(quest$fare)==1,]
all[all$name=="Storey, Mr. Thomas",]

#資料探索
##0.1資料轉換
train$survived<-as.factor(train$survived)
train$pclass<-as.ordered(train$pclass)
##0.2死亡存活數
die_survived<-ggplot(train[!is.na(train$survived),],aes(x=survived))+geom_bar(stat="count")+labs(x="How many people die")+geom_label(stat = "count",aes (label= ..count..))
##0.3性別與死亡
sex_die<-ggplot(train[!is.na(train$survived),], aes(x = sex, fill = survived)) +
  geom_bar(stat='count', position='dodge') + theme_grey() +
  labs(x = 'Training data only') +
  geom_label(stat='count', aes(label=..count..))
##0.4死亡與pclass
###0.4.1 class與死亡
pclass_die<-ggplot(train[!is.na(train$survived),], aes(x = pclass, fill = survived)) +
  geom_bar(stat='count', position='dodge') + labs(x = 'Training data only') +
  theme(legend.position="none") 
###0.4.2 class& sex &死亡
pclass_sex_die1<-ggplot(train[!is.na(train$survived),], aes(x = pclass, fill = survived)) +
  geom_bar(stat='count', position='fill') +
  labs(x = 'Training data only', y= "Percent") + facet_grid(.~sex) 
pclass_sex_die2<-ggplot(train[!is.na(train$survived),], aes(x = pclass, fill = survived)) +
  geom_bar(stat='count', position='stack') +
  labs(x = 'Training data only', y= "Count") + facet_grid(.~sex)
###0.4.3把pclass & sex做組合
p1f<-which((train$pclass == "1") & (train$sex == "female"))
p2f<-which((train$pclass == "2") & (train$sex == "female"))
p3f<-which((train$pclass == "3") & (train$sex == "female"))
p1m<-which((train$pclass == "1") & (train$sex == "male"))
p2m<-which((train$pclass == "2") & (train$sex == "male"))
p3m<-which((train$pclass == "3") & (train$sex == "male"))
train$pclass_sex[p1f]="p1f"
train$pclass_sex[p2f]="p2f"
train$pclass_sex[p3f]="p3f"
train$pclass_sex[p1m]="p1m"
train$pclass_sex[p2m]="p2m"
train$pclass_sex[p3m]="p3m"
as.factor(train$pclass_sex)


##0.5 fare&死亡
train[which(train$fare==0),]#有class1&大人是fare=0
pclass_fare<-ggplot(train[!is.na(train$survived),], aes(x = fare, fill = survived)) +
  geom_histogram( bins= 30) +
  labs(x = 'Training data only', y= "Count") + facet_grid(.~pclass)
###看不出什麼結果

##0.6 年紀與死亡
###0.6.1先解決boat的缺失值
train$boat = as.numeric(train$boat != "")
###0.6.2用現有資料預測年紀的缺失值
agelm <- lm(age ~ pclass+boat+sex+parch+sibsp, data=train[!is.na(train$age),])
summary(agelm)
train$agelm<-predict(agelm,train)
###0.6.3預測結果
hist(train$age[!is.na(train$age)], main='Original data, non-missing', xlab='age')
hist(train$agelm, main= 'LM predictions', xlab='age', xlim=range(0:80))
###0.6.4把現有資料跟預測結果作合併
indexMissingAge <- which(is.na(train$age))
train$age[indexMissingAge] <- train$agelm[indexMissingAge]

survivedlm<-lm(survived~pclass+sex+boat+parch+sibsp,data=train)


##0.7
all$survived<-as.factor(all$survived)
all$pclass<-as.ordered(all$pclass)

p1f<-which((all$pclass == "1") & (all$sex == "female"))
p2f<-which((all$pclass == "2") & (all$sex == "female"))
p3f<-which((all$pclass == "3") & (all$sex == "female"))
p1m<-which((all$pclass == "1") & (all$sex == "male"))
p2m<-which((all$pclass == "2") & (all$sex == "male"))
p3m<-which((all$pclass == "3") & (all$sex == "male"))
all$pclass_sex[p1f]="p1f"
all$pclass_sex[p2f]="p2f"
all$pclass_sex[p3f]="p3f"
all$pclass_sex[p1m]="p1m"
all$pclass_sex[p2m]="p2m"
all$pclass_sex[p3m]="p3m"
as.factor(all$pclass_sex)

all$boat = as.numeric(all$boat != "")
agelm <- lm(age ~ pclass+boat+sex+parch+sibsp, data=all[!is.na(train$age),])
summary(agelm)
all$agelm<-predict(agelm,all)

hist(all$age[!is.na(all$age)], main='Original data, non-missing', xlab='age')
hist(all$agelm, main= 'LM predictions', xlab='age', xlim=range(0:80))

indexMissingAge <- which(is.na(all$age))
train$age[indexMissingAge] <- all$agelm[indexMissingAge]

survivedLM<-lm(survived~pclass+boat+sex,parch)



###0.8 PCA 
library(factoextra)
titanic_train<-all[]
res.pca <- prcomp(train)
