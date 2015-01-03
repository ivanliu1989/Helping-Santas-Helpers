setwd('C:/Users/Ivan.Liuyanfeng/Desktop/Data_Mining_Work_Space/FICO/Helping-Santas-Helpers')
# setwd('H:/Machine_Learning/FICO/Helping-Santas-Helpers')
gc(); rm(list=ls()); source('R code/Functions.R');
load('data/toys_regulated.RData')
# load('data/toys.RData')

### break down toys dataset ###
toys <- data.matrix(toys)

toy_ex1 <- 5760; toy_ex2 <- 15480;

toy_remain <- 3080 # 2848

toy_retrain1 <- c(120:153); toy_retrain2 <- c(174:210); toy_retrain3 <- c(211:256);
toy_retrain4 <- c(257:312); toy_retrain5 <- c(313:381); toy_retrain6 <- c(382:464); toy_retrain7 <- c(465:566,30:34);
# toy_retrain1 <- c(150); toy_retrain2 <- c(183); toy_retrain3 <- c(223);
# toy_retrain4 <- c(272); toy_retrain5 <- c(331); toy_retrain6 <- c(404); toy_retrain7 <- c(492);

toy_train1 <- c(567:690,35:39); toy_train2 <- c(691:841,40:49); toy_train3 <- c(842:1025,50:59);
toy_train4 <- c(1026:1250,60:69); toy_train5 <- c(1251:1523,70:79); toy_train6 <- c(1524:1857,80:89); toy_train7 <- c(1858:2264,100:105);

toys <- data.matrix(transform(toys, Size = 0)) # trival
toys[which(toys[,'Duration']<=toy_remain),'Size'] <- 1 # remain
toys[which(toys[,'Duration']<1858),'Size'] <- 0 # trival
toys[which(toys[,'Duration']>toy_remain),'Size'] <- 2 # ex1
toys[which(toys[,'Duration']>=toy_ex2),'Size'] <- 3 # ex2
toys[which(toys[,'Duration']%in%toy_retrain1),'Size'] <- 4 # re1
toys[which(toys[,'Duration']%in%toy_retrain2),'Size'] <- 5 # re2
toys[which(toys[,'Duration']%in%toy_retrain3),'Size'] <- 6 # re3
toys[which(toys[,'Duration']%in%toy_retrain4),'Size'] <- 7 # re4
toys[which(toys[,'Duration']%in%toy_retrain5),'Size'] <- 8 # re5
toys[which(toys[,'Duration']%in%toy_retrain6),'Size'] <- 9 # re6
toys[which(toys[,'Duration']%in%toy_retrain7),'Size'] <- 10 # re7
toys[which(toys[,'Duration']%in%toy_train1),'Size'] <- 11 # tr1
toys[which(toys[,'Duration']%in%toy_train2),'Size'] <- 12 # tr2
toys[which(toys[,'Duration']%in%toy_train3),'Size'] <- 13 # tr3
toys[which(toys[,'Duration']%in%toy_train4),'Size'] <- 14 # tr4
toys[which(toys[,'Duration']%in%toy_train5),'Size'] <- 15 # tr5
toys[which(toys[,'Duration']%in%toy_train6),'Size'] <- 16 # tr6
toys[which(toys[,'Duration']%in%toy_train7),'Size'] <- 17 # tr7

#table(toys[,'Size'])

toys_0 <- toys[which(toys[,'Size']==0),]  
toys_1 <- toys[which(toys[,'Size']==1),] 
toys_2 <- toys[which(toys[,'Size']==2),] 
toys_3 <- toys[which(toys[,'Size']==3),] 
toys_4 <- toys[which(toys[,'Size']==4),] 
toys_5 <- toys[which(toys[,'Size']==5),] 
toys_6 <- toys[which(toys[,'Size']==6),] 
toys_7 <- toys[which(toys[,'Size']==7),]
toys_8 <- toys[which(toys[,'Size']==8),]
toys_9 <- toys[which(toys[,'Size']==9),]
toys_10 <- toys[which(toys[,'Size']==10),]
toys_11 <- toys[which(toys[,'Size']==11),]
toys_12 <- toys[which(toys[,'Size']==12),]
toys_13 <- toys[which(toys[,'Size']==13),]
toys_14 <- toys[which(toys[,'Size']==14),]
toys_15 <- toys[which(toys[,'Size']==15),]
toys_16 <- toys[which(toys[,'Size']==16),]
toys_17 <- toys[which(toys[,'Size']==17),]

toys_0 <- toys_0[order(toys_0[,'Arrival_time']),]
toys_1 <- toys_1[order(-toys_1[,'Duration'],toys_1[,'Arrival_time']),]
toys_2 <- toys_2[order(toys_2[,'Arrival_time'],toys_2[,'Duration']),]
toys_3 <- toys_3[order(-toys_3[,'Duration'],toys_3[,'Arrival_time']),]
toys_4 <- toys_4[order(toys_4[,'Arrival_time'],-toys_4[,'Duration']),]
toys_5 <- toys_5[order(toys_5[,'Arrival_time'],-toys_5[,'Duration']),]
toys_6 <- toys_6[order(toys_6[,'Arrival_time'],-toys_6[,'Duration']),]
toys_7 <- toys_7[order(toys_7[,'Arrival_time'],-toys_7[,'Duration']),]
toys_8 <- toys_8[order(toys_8[,'Arrival_time'],-toys_8[,'Duration']),]
toys_9 <- toys_9[order(toys_9[,'Arrival_time'],-toys_9[,'Duration']),]
toys_10 <- toys_10[order(toys_10[,'Arrival_time'],-toys_10[,'Duration']),]
toys_11 <- toys_11[order(toys_11[,'Arrival_time'],-toys_11[,'Duration']),]
toys_12 <- toys_12[order(toys_12[,'Arrival_time'],-toys_12[,'Duration']),]
toys_13 <- toys_13[order(toys_13[,'Arrival_time'],-toys_13[,'Duration']),]
toys_14 <- toys_14[order(toys_14[,'Arrival_time'],-toys_14[,'Duration']),]
toys_15 <- toys_15[order(toys_15[,'Arrival_time'],-toys_15[,'Duration']),]
toys_16 <- toys_16[order(toys_16[,'Arrival_time'],-toys_16[,'Duration']),]
toys_17 <- toys_17[order(toys_17[,'Arrival_time'],-toys_17[,'Duration']),]

# 7days seperate
