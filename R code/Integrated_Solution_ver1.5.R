#########################
### Schedule Strategy ###
#########################
assign_toy <- function(myelves, myToys) {
    if(sum(myelves[,'score'] == 3) > 0){
        assigned_toy <-as.integer(myToys[myToys[,'Size']==3,'ToyId'][1])
        return(assigned_toy)
    }else if(sum(myelves[,'score'] == 2) > 0){
        assigned_toy <-as.integer(myToys[myToys[,'Size']==2,'ToyId'][1])
        return(assigned_toy)
    }else{
        assigned_toy <-as.integer(myToys[myToys[,'Size']==1,'ToyId'][1])
        return(assigned_toy)
    }
}

assign_elf <- function(myelves, di, c_toy_size) {
    myelves <- myelves[myelves[,'score']==c_toy_size,]
    assigned_elf <-as.integer(myelves[order(myelves[,'next_available_time']) ,'elf_id'][1])
    return(assigned_elf)
}

#################
### Main Loop ###
#################
solution_sortedElf <- function(myToys, myelves){
    cat(format(Sys.time(),format = '%Y-%m-%d %H:%M:%S'))
    outcomes <- matrix(0, nrow = nrow(myToys), ncol = 5, 
                       dimnames = list(NULL, c('ToyId', 'ElfId', 'StartTime', 'Duration', 'current_rating')))
    
    for(current_toy in 1:nrow(myToys)){
        
        # train_elf <- 1.0; overwork_elf <- 3.9
        myelves[myelves[,'current_rating']>overwork_elf,'score'] <- 3
        myelves[myelves[,'current_rating']<=overwork_elf,'score'] <- 2
        myelves[myelves[,'current_rating']<=train_elf,'score'] <- 1
        
        c_toy_id <- assign_toy(myelves, myToys)
        c_toy_arrival <- myToys[c_toy_id, 'Arrival_time'] 
        c_toy_duration <- myToys[c_toy_id,'Duration']
        c_toy_size <- myToys[c_toy_id,'Size']
        
        next_elf <- assign_elf(myelves, c_toy_duration, c_toy_size)
        
        c_elf_id <- myelves[next_elf, 'elf_id']
        c_elf_start_time <- myelves[next_elf, 'next_available_time']
        c_elf_rating <- myelves[next_elf, 'current_rating']
        
        if(c_elf_start_time < c_toy_arrival) c_elf_start_time <- c_toy_arrival    
        
        work_duration <- as.integer(ceiling(c_toy_duration/c_elf_rating))
        
        myelves[next_elf, 'next_available_time'] <- update_next_available_minute(c_elf_start_time, work_duration)
        
        myelves[next_elf, 'current_rating'] <- update_productivity(c_elf_start_time, work_duration, c_elf_rating)
        
        outcomes[current_toy,] <- c(c_toy_id, c_elf_id, c_elf_start_time, work_duration, c_elf_rating)
        
        myToys <- myToys[-c_toy_id, ]
        
        if(current_toy %% 100000 == 0) cat('\nCompleted', current_toy/1000000, 'mil toys, makespan', c_elf_start_time, 'minutes',
                                           format(Sys.time(),format = '%Y-%m-%d %H:%M:%S'))    
    }
    cat('\nCompleted 10 mil toys at', convert_to_chardate(c_elf_start_time)) 
    return(outcomes)
}

############
### MAIN ###
############
# elf: Retrain-1 (0.25, 1) | Train-2 (1, 4.0) | Overwork-3 (4.0)
# toy: Small-1 | Median-2 | Large-3
# toy: 57% (0,600) mins | 20% (600,2880) mins | 23% (2880, inf) mins

setwd('/Users/ivan/Work_directory/FICO/Helping-Santas-Helpers/')
setwd('C:/Users/Ivan.Liuyanfeng/Desktop/Data_Mining_Work_Space/FICO/Helping-Santas-Helpers')
setwd('H:/Machine_Learning/FICO/Helping-Santas-Helpers')
gc(); rm(list=ls())
source('R code/Functions.R')

NUM_ELVES <- 900
myelves <- create_elves(NUM_ELVES)

load('data/toys_regulated.RData')
toy_break1 <- 600; toy_break2 <- 2880
toys <- data.matrix(transform(toys, Size = 0))
toys[which(toys[,'Duration']>toy_break2),'Size'] <- 3 # overwork
toys[which(toys[,'Duration']<=toy_break2),'Size'] <- 2 # 48 hour
toys[which(toys[,'Duration']<=toy_break1),'Size'] <- 1 # 10 hour

# toys <- toys[order(toys[,'Exhaustion'], toys[,'Arrival_time']),]

train_elf <- 1.0; overwork_elf <- 3.9

submissions <- solution_sortedElf(toys, myelves)
submissions_output <- data.frame(ToyId = as.integer(submissions[,1]), 
                                 ElfId = as.integer(submissions[,2]), 
                                 StartTime = convert_to_chardate(submissions[,3]), 
                                 Duration = as.integer(submissions[,4]), stringsAsFactors = FALSE)

convert_to_minute(submissions_output[nrow(submissions_output),3]) * log(1+NUM_ELVES)
write.csv(submissions_output, 'toys_submission_classification_sort.csv', row.names = FALSE)
