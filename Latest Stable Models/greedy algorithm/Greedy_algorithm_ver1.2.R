setwd('H:/Machine_Learning/FICO/Helping-Santas-Helpers')
gc(); rm(list=ls()); source('R code/Functions.R');
load('data/toys_regulated.RData')
require(Rcpp)
sourceCpp('Latest Stable Models/greedy algorithm/main_greedy_2.cpp')

### break down toys dataset ###
toys <- data.matrix(toys)

toy_ex1 <- 5760; toy_ex2 <- 12480;

toy_remain <- 2848

toy_retrain1 <- c(140:160); toy_retrain2 <- c(173:193); toy_retrain3 <- c(213:233);
toy_retrain4 <- c(262:282); toy_retrain5 <- c(321:341); toy_retrain6 <- c(394:414); toy_retrain7 <- c(482:502);

toy_train1 <- c(590:610); toy_train2 <- c(721:741); toy_train3 <- c(881:901);
toy_train4 <- c(1077:1097); toy_train5 <- c(1315:1335); toy_train6 <- c(1605:1625); toy_train7 <- c(1958:1978);

toys <- data.matrix(transform(toys, Size = 0)) # trival
toys[which(toys[,'Duration']>=toy_remain),'Size'] <- 1 # remain
toys[which(toys[,'Duration']>=toy_ex1),'Size'] <- 2 # ex1
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

table(toys[,'Size'])

toys_0 <- toys[which(toys[,'Size']==0),];  
toys_0 <- toys_0[order(toys_0[,'Arrival_time'],toys_0[,'Duration']),]
toys_1 <- toys[which(toys[,'Size']==1),] 
toys_1 <- toys_1[order(toys_1[,'Arrival_time'],toys_1[,'Duration']),]
toys_2 <- toys[which(toys[,'Size']==2),] 
toys_2 <- toys_2[order(toys_2[,'Arrival_time'],toys_2[,'Duration'])]
toys_3 <- toys[which(toys[,'Size']==3),] 
toys_3 <- toys_3[order(toys_3[,'Arrival_time'],toys_3[,'Duration'])]
toys_4 <- toys[which(toys[,'Size']==4),] 
toys_4 <- toys_4[order(toys_4[,'Arrival_time'],toys_4[,'Duration'])]
toys_5 <- toys[which(toys[,'Size']==5),] 
toys_5 <- toys_5[order(toys_5[,'Arrival_time'],toys_5[,'Duration'])]
toys_6 <- toys[which(toys[,'Size']==6),] 
toys_6 <- toys_6[order(toys_6[,'Arrival_time'],toys_6[,'Duration'])]
# 7days seperate

### start from 9.00 am, start time => proper toys dataset ###

### maintain skill at 4 set(time)

### decide circle => 2848(penalty free) (first arrive first work)

### end circle in an exhausion work

### set aside 14 days tasks to retrain
 
### assign remaining tasks before the exhausion tasks

### only fully train 1/2 longest exhausion tasks | train first 1/2 exhausion tasks

### 1. circle aside <= different chunks of tasks

