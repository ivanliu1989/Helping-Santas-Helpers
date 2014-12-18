setwd('/Users/ivan/Work_directory/FICO/Helping-Santas-Helpers/')
setwd('C:/Users/Ivan.Liuyanfeng/Desktop/Data_Mining_Work_Space/FICO/Helping-Santas-Helpers')
setwd('H:/Machine_Learning/FICO/Helping-Santas-Helpers')
gc(); rm(list=ls())
source('R code/Functions.R')
source('R code/c_Functions.r')
load('data/toys.RData')
load('data/900_Folds.RData')

toys_dat <- data.frame(toys)
myToys <- data.matrix(toys_dat[index[[1]],])

NUM_ELVES <- 1
myelves <- create_elves(NUM_ELVES)

# toys label
# 2.5 | 10 | 40 | 48 | 96 | 208
# 64% | 12% | 4% | 0.4% | 3% | 17% (partition)
# 64% | 12% | 4% | 0.5% | 3% | 17% (total)
myToys <- transform(myToys, 'Size'= 6) # 4.0
myToys[which(myToys[,'Duration']<96*60),'Size'] <- 5 # 4.0
myToys[which(myToys[,'Duration']<48*60),'Size'] <- 4 # 4.0 
myToys[which(myToys[,'Duration']<40*60),'Size'] <- 3 # 3.0 - 3.9
myToys[which(myToys[,'Duration']<10*60),'Size'] <- 2 # 1.0 - 3.0
myToys[which(myToys[,'Duration']<2.5*60),'Size'] <- 1 # 0.25

### Submit ###
solution_Elf_submit <- function(myToys, myelves){
    outcomes <- matrix(0, nrow = nrow(myToys), ncol = 5, 
                       dimnames = list(NULL, c('ToyId', 'ElfId', 'StartTime', 'Duration', 'current_rating')))
    current_toy <- 1
    c_elf_id <- myelves[, 'elf_id']
    for(current_toy in 1:nrow(myToys)){
        c_elf_start_time <- myelves[, 'next_available_time']
        c_elf_rating <- myelves[, 'current_rating']
        i <- ifelse(c_elf_rating==4,4,ifelse(c_elf_rating>=3,3,ifelse(c_elf_rating>=1,2,1)))
        toys <- myToys[which(myToys[,'Size']>=i),]
        if(nrow(toys)<1) toys <- myToys
        toys <- toys[order(toys[,4], toys[,2]),]
        
        c_toy_id <- toys[1,'ToyId']
        c_toy_arrival <- toys[1, 'Arrival_time'] 
        c_toy_duration <- toys[1,'Duration']
        
        if(c_elf_start_time < c_toy_arrival) c_elf_start_time <- c_toy_arrival  
        work_duration <- as.integer(ceiling(c_toy_duration/c_elf_rating))
        myelves[, 'next_available_time'] <- updateNextAvailableMinute(c_elf_start_time, work_duration)
        myelves[, 'current_rating'] <- updateProductivity(c_elf_start_time, work_duration, c_elf_rating)
        
        outcomes[current_toy,] <- c(c_toy_id, c_elf_id, c_elf_start_time, work_duration, c_elf_rating)
        if(current_toy %% 10000 == 0) cat('\nCompleted', current_toy/1000000, 'mil toys, makespan', c_elf_start_time, 'minutes',
                                           format(Sys.time(),format = '%Y-%m-%d %H:%M:%S'))
        myToys <- myToys[-which(myToys[,'ToyId']==c_toy_id),]
    }
    return(outcomes)
}

submissions <- solution_Elf_submit(myToys, myelves)
submissions_output <- data.frame(ToyId = as.integer(submissions[,1]), 
                                 ElfId = as.integer(submissions[,2]), 
                                 StartTime = convert_to_chardate(submissions[,3]), 
                                 Duration = as.integer(submissions[,4]), stringsAsFactors = FALSE)
(submissions[which.max(submissions),3]+submissions[which.max(submissions[,3]), 4])*log(901)
write.csv(submissions_output, 'toys_submission_classification_sort.csv', row.names = FALSE)
