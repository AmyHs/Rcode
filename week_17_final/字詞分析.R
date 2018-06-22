library(tidyverse)
#data
data <- read.csv("C:/Users/b0520/Desktop/csx_R_course/Rcode/week17_final/詞頻統計.csv")
data <- data[-(10:22),]
suitotal <- data[c(1,3,4,5),]
natotal <- data[c(2,6,7,8,9,10),]
#計算詞頻
data <- mutate(data, friendrate = 朋友詞/總字數)
data <- mutate(data, postrate = 正面情緒詞/總字數)
data <- mutate(data, negrate = 負面情緒詞/總字數)
data <- mutate(data, pronrate = 人稱代名詞/總字數)
data <- mutate(data, cograte = 認知歷程詞/總字數)
data <- mutate(data, Ppostrate = 人稱前後正面/總字數)
data <- mutate(data, Pnegrate = 人稱前後負面/總字數)
#獨立樣本比率差異檢定
friendtest <- prop.test(x = c(sum(suitotal$朋友詞), sum(natotal$朋友詞)), n = c(sum(data$總字數), sum(data$總字數)), alternative = "greater")
postest <- prop.test(x = c(sum(suitotal$正面情緒詞), sum(natotal$正面情緒詞)), n = c(sum(data$總字數), sum(data$總字數)), alternative = "greater")
negtest <- prop.test(x = c(sum(suitotal$負面情緒詞), sum(natotal$負面情緒詞)), n = c(sum(data$總字數), sum(data$總字數)), alternative = "greater")
cogtest <- prop.test(x = c(sum(suitotal$認知歷程詞), sum(natotal$認知歷程詞)), n = c(sum(data$總字數), sum(data$總字數)), alternative = "greater")
Ppostest <- prop.test(x = c(sum(suitotal$人稱前後正面), sum(natotal$人稱前後正面)), n = c(sum(data$總字數), sum(data$總字數)), alternative = "greater")
Pnegtest <- prop.test(x = c(sum(suitotal$人稱前後負面), sum(natotal$人稱前後負面)), n = c(sum(data$總字數), sum(data$總字數)), alternative = "greater")
pronteat <- prop.test(x = c(sum(suitotal$人稱代名詞), sum(natotal$人稱代名詞)), n = c(sum(data$總字數), sum(data$總字數)), alternative = "greater")
#你我他詞頻
data1 <- read.csv("C:/Users/b0520/Desktop/csx_R_course/Rcode/week17_final/詞頻.csv")
data1 <- data1[-(10:22),]
suitotal1 <- data1[c(1,3,4,5),]
natotal1 <- data1[c(2,6,7,8,9,10),]
prontest <- prop.test(x = c(sum(suitotal1$我), sum(natotal1$我)), n = c(sum(data1$總字數), sum(data1$總字數)), alternative = "greater")
prontest <- prop.test(x = c(sum(suitotal1$你), sum(natotal1$你)), n = c(sum(data1$總字數), sum(data1$總字數)), alternative = "greater")
prontest <- prop.test(x = c(sum(suitotal1$她, suitotal1$他), sum(natotal1$她, suitotal1$他)), n = c(sum(data1$總字數), sum(data1$總字數)), alternative = "greater")
#作圖
freq <- read.csv("C:/Users/b0520/Desktop/csx_R_course/Rcode/week17_final/freq.csv")
freq <- gather(freq, key = key, value= value, "自殺作家","非自殺作家")
#freq <- mutate(freq, logsui = log(自殺作家))
#freq <- mutate(freq, logna = log(非自殺作家))
freq <- mutate(freq, logval = log(value) ) 
library(ggplot2)
ggplot(freq, aes(x = X , y = logval, color = key)) + geom_point() + geom_line(aes(group = key))

