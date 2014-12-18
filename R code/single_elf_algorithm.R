setwd('/Users/ivan/Work_directory/FICO/Helping-Santas-Helpers/')
setwd('C:/Users/Ivan.Liuyanfeng/Desktop/Data_Mining_Work_Space/FICO/Helping-Santas-Helpers')
setwd('H:/Machine_Learning/FICO/Helping-Santas-Helpers')
gc(); rm(list=ls())
source('R code/Functions.R')
source('R code/c_Functions.r')
load('data/toys.RData')
load('R_results/baseSchedule.RData')
load('data/900_Folds.RData')

toys_dat <- data.frame(toys)
myToys <- data.matrix(toys_dat[index[[index_num]],])
myToys <- myToys[order(myToys[,2]+myToys[,3], myToys[,2]),]

NUM_ELVES <- 1
myelves <- create_elves(NUM_ELVES)

# toys label
# 2.5 | 10 | 40 | 48 | 96 | 208
myToys[,'Size'] <- 6 # 4.0
myToys[which(myToys[,'Duration']<96*60),'Size'] <- 5 # 4.0
myToys[which(myToys[,'Duration']<48*60),'Size'] <- 4 # 4.0 
myToys[which(myToys[,'Duration']<40*60),'Size'] <- 3 # 3.0 - 3.9
myToys[which(myToys[,'Duration']<10*60),'Size'] <- 2 # 1.0 - 3.0
myToys[which(myToys[,'Duration']<2.5*60),'Size'] <- 1 # 0.25

myelves[,'Size'] <- 4 # 4.0
myelves[which(myelves[,'Rate']<4.0),'Size'] <- 3 # 4.0
myelves[which(myelves[,'Rate']<3.0),'Size'] <- 2 # 4.0
myelves[which(myelves[,'Rate']<1.0),'Size'] <- 1 # 4.0

### Submit ###
solution_Elf_submit <- function(myToys, myelves){
    outcomes <- matrix(0, nrow = nrow(myToys), ncol = 5, 
                       dimnames = list(NULL, c('ToyId', 'ElfId', 'StartTime', 'Duration', 'current_rating')))
    while(current_toy < nrow(myToys)){
        
        c_toy_id <- myToys[current_toy,'ToyId']
        c_toy_arrival <- myToys[current_toy, 'Arrival_time'] 
        c_toy_duration <- myToys[current_toy,'Duration']
        
        c_elf_id <- myelves[, 'elf_id']
        c_elf_start_time <- myelves[, 'next_available_time']
        c_elf_rating <- myelves[, 'current_rating']
        
        if(c_elf_start_time < c_toy_arrival) c_elf_start_time <- c_toy_arrival  
        work_duration <- as.integer(ceiling(c_toy_duration/c_elf_rating))
        myelves[c_elf_id, 'next_available_time'] <- updateNextAvailableMinute(c_elf_start_time, work_duration)
        myelves[c_elf_id, 'current_rating'] <- updateProductivity(c_elf_start_time, work_duration, c_elf_rating)
        
        outcomes[current_toy,] <- c(c_toy_id, c_elf_id, c_elf_start_time, work_duration, c_elf_rating)
        if(current_toy %% 100000 == 0) cat('\nCompleted', current_toy/1000000, 'mil toys, makespan', c_elf_start_time, 'minutes',
                                           format(Sys.time(),format = '%Y-%m-%d %H:%M:%S')) 
    }
    return(outcomes)
}
