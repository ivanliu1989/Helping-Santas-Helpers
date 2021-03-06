# setwd('/Users/ivan/Work_directory/FICO/Helping-Santas-Helpers/')
# setwd('C:/Users/Ivan.Liuyanfeng/Desktop/Data_Mining_Work_Space/FICO/Helping-Santas-Helpers')
setwd('H:/Machine_Learning/FICO/Helping-Santas-Helpers')
gc(); rm(list=ls()); source('R code/Functions.R');
# load('data/toys.RData')
load('data/toys_regulated.RData')
require(Rcpp)
sourceCpp('Latest Stable Models/greedy algorithm/main_greedy.cpp')

### Segmentation Toys ###
toys <- data.matrix(toys)
toy_break1 <- 2.5*60*1.2; toy_break2 <- 10*60; toy_break3 <- 24*60; toy_break4 <- 52*60; 
toys <- data.matrix(transform(toys, Size = 0)) # <toy_break1 | 0.6408928 | Arrival, Increase
toys[which(toys[,'Duration']>=toy_break1),'Size'] <- 1 # >=150 mins | 0.1202851 | Arrival, Increase
toys[which(toys[,'Duration']>=toy_break2),'Size'] <- 2 # >=600 mins | 0.0366753 | Least decrease
toys[which(toys[,'Duration']>=toy_break3),'Size'] <- 3 # >=2400 mins | 0.2021468 | Least recover / finish
# table(toys[,'Size'])/10000000 # 
toys_0 <- toys[which(toys[,'Size']==0),];  
toys_0 <- toys_0[order(toys_0[,'Arrival_time'],toys_0[,'Duration']),]
toys_1 <- toys[which(toys[,'Size']==1),] 
toys_1 <- toys_1[order(toys_1[,'Arrival_time'],toys_1[,'Duration']),]
toys_2 <- toys[which(toys[,'Size']==2),] 
toys_2 <- toys_2[order(toys_2[,'Duration']),]
toys_3 <- toys[which(toys[,'Size']==3),] 
toys_3 <- toys_3[order(toys_3[,'Duration']),]

### Segmentation Elf ###
NUM_ELVES <- 900
myelves <- create_elves(NUM_ELVES)
myelves_rate <- myelves[,'current_rating']

### main loop ###
submissions <- solution_Elf(toys_0,toys_1,toys_2,toys_3,myelves,myelves_rate)
(submissions[which.max(submissions[,3]),3]+submissions[which.max(submissions[,3]), 4])*log(901)

submissions_output <- data.frame(ToyId = as.integer(submissions[,1]), 
                                 ElfId = as.integer(submissions[,2]), 
                                 StartTime = convert_to_chardate(submissions[,3]), 
                                 Duration = as.integer(submissions[,4]), stringsAsFactors = FALSE)
write.csv(submissions_output, 'toys_submission_classification_sort.csv', row.names = FALSE)
