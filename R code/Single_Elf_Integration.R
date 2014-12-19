gc(); rm(list=ls())
load('data/900_Folds.RData')
load('data/toys.RData')
source('R code/Functions.R');source('R code/c_Functions.r')
library(Rcpp);sourceCpp("R code/c_Functions.cpp")
toys_dat <- data.frame(toys)
outcome_all <- matrix(0, nrow = nrow(myToys), ncol = 5, 
                   dimnames = list(NULL, c('ToyId', 'ElfId', 'StartTime', 'Duration', 'current_rating')))

load('optimization_results/simulated_annealing_1_300.RData')
load('simulated_annealing_301_600.RData')
load('optimization_results/simulated_annealing_601_900.RData')

NUM_ELVES <- 1
myelves <- create_elves(NUM_ELVES)
solution_Elf_submit <- function(myToys, myelves, schedule){
    outcomes <- matrix(0, nrow = nrow(myToys), ncol = 5, 
                       dimnames = list(NULL, c('ToyId', 'ElfId', 'StartTime', 'Duration', 'current_rating')))
    myToys <- myToys[schedule,]
    for(current_toy in 1:nrow(myToys)){
        
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

for (index_num in 601:900){
    myToys <- data.matrix(toys_dat[index[[index_num]],])
    schedule <- x_all[[index_num]] 
    outcome <- solution_Elf_submit(myToys, myelves, schedule)
    outcome_all <- rbind(outcome_all, outcome)
    cat('\nsuccess! no:', index_num, 'score:', solution_Elf_c(myToys, myelves, schedule),'fbest:',f_all[index_num])
}

submissions_output <- data.frame(ToyId = as.integer(outcome_all[,1]), 
                                 ElfId = as.integer(outcome_all[,2]), 
                                 StartTime = convert_to_chardate(outcome_all[,3]), 
                                 Duration = as.integer(outcome_all[,4]), stringsAsFactors = FALSE)
# schedule <- data.matrix(submissions[,1:2])
# save(schedule, file='baseSchedule.RData')
write.csv(submissions_output, 'toys_submission_18_Dec.csv', row.names = FALSE)

(outcome_all[which.max(outcome_all[,3]),3]+outcome_all[which.max(outcome_all[,3]), 4])*log(901)

x_all <- x_1_300
f_all <- f_1_300
save(x_all,f_all, file='optimization_results/simulated_annealing_1_300.RData')
