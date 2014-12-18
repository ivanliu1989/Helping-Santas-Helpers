#########################
### Schedule Strategy ###
#########################
source('R code/c_Assign.r');

#################
### Main Loop ###
#################
solution_sortedElf <- function(myToys, myelves){
    cat(format(Sys.time(),format = '%Y-%m-%d %H:%M:%S'))
    outcomes <- matrix(0, nrow = nrow(myToys), ncol = 5, 
                       dimnames = list(NULL, c('ToyId', 'ElfId', 'StartTime', 'Duration', 'current_rating')))
    
    for(current_toy in 1:nrow(myToys)){
        
        c_toy_id <- assign_toy(myelves, myToys)
        c_toy_arrival <- myToys[c_toy_id, 'Arrival_time'] 
        c_toy_duration <- myToys[c_toy_id,'Duration']
        c_toy_size <- myToys[c_toy_id,'size']
        
        next_elf <- assign_elf(myelves, c_toy_duration, c_toy_size)
        
        c_elf_id <- myelves[next_elf, 'elf_id']
        c_elf_start_time <- myelves[next_elf, 'next_available_time']
        c_elf_rating <- myelves[next_elf, 'current_rating']
        
        if(c_elf_start_time < c_toy_arrival) c_elf_start_time <- c_toy_arrival    
        work_duration <- as.integer(ceiling(c_toy_duration/c_elf_rating))
        myelves[next_elf, 'next_available_time'] <- updateNextAvailableMinute(c_elf_start_time, work_duration)
        myelves[next_elf, 'current_rating'] <- updateProductivity(c_elf_start_time, work_duration, c_elf_rating)
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
setwd('/Users/ivan/Work_directory/FICO/Helping-Santas-Helpers/')
setwd('C:/Users/Ivan.Liuyanfeng/Desktop/Data_Mining_Work_Space/FICO/Helping-Santas-Helpers')
setwd('H:/Machine_Learning/FICO/Helping-Santas-Helpers')
gc(); rm(list=ls()); source('R code/Functions.R'); source('R code/c_Functions.r')
load('data/toys.RData')

NUM_ELVES <- 900
myelves <- create_elves(NUM_ELVES)
toys <- toys[order(toys[,'Exhaustion'], toys[,'Arrival_time']),]

submissions <- solution_sortedElf(toys, myelves)
submissions_output <- data.frame(ToyId = as.integer(submissions[,1]), 
                                 ElfId = as.integer(submissions[,2]), 
                                 StartTime = convert_to_chardate(submissions[,3]), 
                                 Duration = as.integer(submissions[,4]), stringsAsFactors = FALSE)
(submissions[which.max(submissions),3]+submissions[which.max(submissions[,3]), 4])*log(901)
write.csv(submissions_output, 'toys_submission_classification_sort.csv', row.names = FALSE)
