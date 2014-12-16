# 1. Creating the initial population
# 2. Cycle: the next generation
# 3. Evaluation of the population of individual fitness
# 4. Define the selection of fitness function
# 5. Changes in the population (mating and mutation)
# 6. Return to Step
# 7. End termination condition is satisfied

#############
### Setup ###
#############
setwd('/Users/ivan/Work_directory/FICO/Helping-Santas-Helpers/')
setwd('C:/Users/Ivan.Liuyanfeng/Desktop/Data_Mining_Work_Space/FICO/Helping-Santas-Helpers')
setwd('H:/Machine_Learning/FICO/Helping-Santas-Helpers')
gc(); rm(list=ls())
source('R code/Functions.R')
load('data/toys.RData')
load('R code/benchmark_schedule.RData')

### Submit ###
solution_Elf_submit <- function(myToys, myelves, schedule){
    outcomes <- matrix(0, nrow = nrow(myToys), ncol = 5, 
                       dimnames = list(NULL, c('ToyId', 'ElfId', 'StartTime', 'Duration', 'current_rating')))
    myToys <- myToys[schedule[,'ToyId'],]
    for(current_toy in 1:nrow(myToys)){
        
        c_toy_id <- myToys[current_toy,'ToyId']
        c_toy_arrival <- myToys[current_toy, 'Arrival_time'] 
        c_toy_duration <- myToys[current_toy,'Duration']
        
        c_elf_id <- schedule[current_toy, 'ElfId']
        c_elf_start_time <- myelves[c_elf_id, 'next_available_time']
        c_elf_rating <- myelves[c_elf_id, 'current_rating']
        
        if(c_elf_start_time < c_toy_arrival) c_elf_start_time <- c_toy_arrival  
        work_duration <- as.integer(ceiling(c_toy_duration/c_elf_rating))
        myelves[, 'next_available_time'] <- update_next_available_minute(c_elf_start_time, work_duration)
        myelves[, 'current_rating'] <- update_productivity(c_elf_start_time, work_duration, c_elf_rating)
        
        outcomes[current_toy,] <- c(c_toy_id, c_elf_id, c_elf_start_time, work_duration, c_elf_rating)
    }
    return(outcomes)
}

### Population ###
elf_population <- diag(900)



