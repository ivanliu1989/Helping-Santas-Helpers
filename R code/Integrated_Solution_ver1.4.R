#########################
### Schedule Strategy ###
#########################
# a: current_rating
# dt: required training time to restore
# di: duration
assign_elf <- function(myelves, di, c_toy_size) {
    if(sum(myelves[,'score']==c_toy_size)<1) c_toy_size <- 2
    if(sum(myelves[,'score']==c_toy_size)<1) c_toy_size <- 1
    if(sum(myelves[,'score']==c_toy_size)<1) c_toy_size <- 4
    if(sum(myelves[,'score']==c_toy_size)<1) c_toy_size <- 5
    myelves <- myelves[myelves[,'score']==c_toy_size,]
    
    if(length(myelves)==4){
        return(as.integer(myelves['elf_id']))
    }else{
        a <- myelves[,'current_rating']
        myelves[,'score'] <- 60 * (4*a -1) / (4 * log(1.02)) # >=da
        myelves[di < -1440*log(4*a)/(log(1.02)+log(0.9)),'score'] <- a[di < -1440*log(4*a)/(log(1.02)+log(0.9))] * (1.02^(10 * di / 24 * a[di < -1440*log(4*a)/(log(1.02)+log(0.9))])) * (0.9^(14 * di / 24 * a[di < -1440*log(4*a)/(log(1.02)+log(0.9))])) * 60 * exp(-di*(10*log(1.02) + 14*log(0.9))/(a[di < -1440*log(4*a)/(log(1.02)+log(0.9))] * 1440)-1) # [a * 600, da)
        myelves[di < a * 600 ,'score'] <- 0 # < 600 * a
        assigned_elf <-as.integer(myelves[order(myelves[,'score'], myelves[,'next_available_time']) ,'elf_id'][1])
        return(assigned_elf)
    }
}

#################
### Main Loop ###
#################
solution_sortedElf <- function(myToys, myelves){
    cat(format(Sys.time(),format = '%Y-%m-%d %H:%M:%S'))
    outcomes <- matrix(0, nrow = nrow(myToys), ncol = 5, 
                       dimnames = list(NULL, c('ToyId', 'ElfId', 'StartTime', 'Duration', 'current_rating')))
    
    for(current_toy in 1:nrow(myToys)){
        
        # retrain_elf <- 0.5; train_elf <- 2.5; overwork_elf <- 3.9
        myelves[,'score'] <- 5
        myelves[myelves[,'current_rating']<overwork_elf,'score'] <- 4
        myelves[myelves[,'current_rating']<train_elf,'score'] <- 2
        myelves[myelves[,'current_rating']<retrain_elf,'score'] <- 1
        
        c_toy_id <- myToys[current_toy,'ToyId']
        c_toy_arrival <- myToys[current_toy, 'Arrival_time'] 
        c_toy_duration <- myToys[current_toy,'Duration']
        c_toy_size <- myToys[current_toy,'Size']
            
        next_elf <- assign_elf(myelves, c_toy_duration, c_toy_size)
        
        c_elf_id <- myelves[next_elf, 'elf_id']
        c_elf_start_time <- myelves[next_elf, 'next_available_time']
        c_elf_rating <- myelves[next_elf, 'current_rating']
        
        if(c_elf_start_time < c_toy_arrival) c_elf_start_time <- c_toy_arrival    
        
        work_duration <- as.integer(ceiling(c_toy_duration/c_elf_rating))
        
        myelves[next_elf, 'next_available_time'] <- update_next_available_minute(c_elf_start_time, work_duration)
        
        myelves[next_elf, 'current_rating'] <- update_productivity(c_elf_start_time, work_duration, c_elf_rating)
        
        outcomes[current_toy,] <- c(c_toy_id, c_elf_id, c_elf_start_time, work_duration, c_elf_rating)
        
        if(current_toy %% 100000 == 0) cat('\nCompleted', current_toy/1000000, 'mil toys, makespan', c_elf_start_time, 'minutes',
                                   format(Sys.time(),format = '%Y-%m-%d %H:%M:%S'))    
    }
    cat('\nCompleted 10 mil toys at', convert_to_chardate(c_elf_start_time)) 
    return(outcomes)
}

############
### MAIN ###
############
# elf: Retrain-1 (0.25, 0.5) | Train-2 (0.5, 4.0) | Overwork-3 (4.0)
# toy: Small-1 | Median-2 | Large-3
# toy: 57% (0,100) mins | 20% (100,720) mins | 23% (720, inf) mins

setwd('/Users/ivan/Work_directory/FICO/Helping-Santas-Helpers/')
setwd('C:/Users/Ivan.Liuyanfeng/Desktop/Data_Mining_Work_Space/FICO/Helping-Santas-Helpers')
setwd('H:/Machine_Learning/FICO/Helping-Santas-Helpers')
gc(); rm(list=ls())
source('R code/Functions.R')

NUM_ELVES <- 900
myelves <- create_elves(NUM_ELVES)

load('data/toys_classified.RData')
toy_break1 <- 2.5*60; toy_break2 <- 10*60; toy_break3 <- 40*60; toy_break4<- 48*60; exhaustion <- 208*60
toys <- data.matrix(transform(toys, Exhaustion = 0))
toys[which(toys[,'Duration']>=toy_break4),'Size'] <- 5 # 1969432 20% - overwork
toys[which(toys[,'Duration']<toy_break4),'Size'] <- 4 # 52036 0.5% - 48 hour
# toys[which(toys[,'Duration']<toy_break3),'Size'] <- 3 # 366753 3.7% - 40 hour
toys[which(toys[,'Duration']<toy_break2),'Size'] <- 2 # 1202851 12% - 10 hour
toys[which(toys[,'Duration']<toy_break1),'Size'] <- 1 # 6408928 64% - 2.5 hour
toys[which(toys[,'Duration']>=exhaustion),'Exhaustion'] <- 1 # 995590 10% larger than 208 hours
toys[which(toys[,'Duration']<exhaustion),'Exhaustion'] <- 0 # 9004410 90% 208 hour
toys <- toys[order(toys[,'Exhaustion'], toys[,'Arrival_time']),]

retrain_elf <- 1; train_elf <- 2.5; overwork_elf <- 3.8

submissions <- solution_sortedElf(toys, myelves)
submissions_output <- data.frame(ToyId = as.integer(submissions[,1]), 
                                 ElfId = as.integer(submissions[,2]), 
                                 StartTime = convert_to_chardate(submissions[,3]), 
                                 Duration = as.integer(submissions[,4]), stringsAsFactors = FALSE)

convert_to_minute(submissions_output[nrow(submissions_output),3]) * log(1+NUM_ELVES)
write.csv(submissions_output, 'toys_submission_classification_sort.csv', row.names = FALSE)
