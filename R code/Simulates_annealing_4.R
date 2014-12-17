library(compiler)
enableJIT(3)
#############
### Setup ###
#############
setwd('/Users/ivan/Work_directory/FICO/Helping-Santas-Helpers/')
setwd('C:/Users/Ivan.Liuyanfeng/Desktop/Data_Mining_Work_Space/FICO/Helping-Santas-Helpers')
setwd('H:/Machine_Learning/FICO/Helping-Santas-Helpers')
gc(); rm(list=ls())
source('R code/Functions.R')
source('R code/c_Functions.r')
load('data/toys.RData')
load('R_results/baseSchedule.RData')

#################
### Functions ###
#################
### f(x) ###
solution_Elf <- function(myToys, myelves, schedule){
    outcomes <- matrix(0, nrow = nrow(myToys), ncol = 4, 
                       dimnames = list(NULL, c('ToyId', 'ElfId', 'StartTime', 'Duration')))
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
        myelves[c_elf_id, 'next_available_time'] <- updateNextAvailableMinute(c_elf_start_time, work_duration)
        myelves[c_elf_id, 'current_rating'] <- updateProductivity(c_elf_start_time, work_duration, c_elf_rating)
        
        outcomes[current_toy,] <- c(c_toy_id, c_elf_id, c_elf_start_time, work_duration)
        if(current_toy %% 2000000 == 0) cat('\nCompleted', current_toy/1000000, 'mil toys, makespan', c_elf_start_time, 'minutes',
                                            format(Sys.time(),format = '%Y-%m-%d %H:%M:%S')) 
    }
    return((outcomes[which.max(outcomes[,3]),3]+outcomes[which.max(outcomes[,3]), 4])*log(901))
}

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
        myelves[c_elf_id, 'next_available_time'] <- update_next_available_minute(c_elf_start_time, work_duration)
        myelves[c_elf_id, 'current_rating'] <- update_productivity(c_elf_start_time, work_duration, c_elf_rating)
        
        outcomes[current_toy,] <- c(c_toy_id, c_elf_id, c_elf_start_time, work_duration, c_elf_rating)
        if(current_toy %% 100000 == 0) cat('\nCompleted', current_toy/1000000, 'mil toys, makespan', c_elf_start_time, 'minutes',
                                           format(Sys.time(),format = '%Y-%m-%d %H:%M:%S')) 
    }
    return(outcomes)
}

#########################
### Optimization Body ###
#########################
### Toys establishment ###
set.seed(999)
myToys <- toys; rm(toys)
schedule <- xbest ## last optimal solution xbest(toyID, elfID)
NUM_ELVES <- 900
myelves <- create_elves(NUM_ELVES)

### parameters ###
C <- 5 # multiple cooling chain
N0 <- runif(C)*nrow(myToys) # initial point
h <- 50 # used to modulate the step length.
S <- 1 # current value times, step width
x0 <- schedule; fx0 <- solution_Elf(myToys, myelves, x0)
xbest <- x0; fbest <- fx0

### main loop ###
for (c in 1:C){ 
    toy_row <- nrow(myToys)
    Ns <- xbest[N0[c]]
    cat(paste('\nChain:',c, '; Initial point:', Ns, '; Current best score:', fbest))
    bk_s <- 0
    for (s in 1:S){ 
        cat(paste('\n - Step:',s))
        Np <- (1+h+s/10) 
        num <- length(max((Ns-Np),1):min((Ns+Np),toy_row))
        bk <-0
        for (np in 1:num){ 
            partition_1 <- max(((np-1)/num)*toy_row + 1, 1) 
            partition_2 <- min((np/num)*toy_row, toy_row) 
            x1 <- xbest
            x1[partition_1:partition_2, 'ToyId'] <- sample(x1[partition_1:partition_2, 'ToyId']) ## reallocate Toys to a random chosen group of Elves
            x1[partition_1:partition_2, 'ElfId'] <- sample(x1[partition_1:partition_2, 'ElfId'])
            fx1 <- solution_Elf(myToys, myelves, x1)
            delta <- fx1-fbest
            if(delta<0){
                xbest <- x1; fbest <- fx1
                cat(paste('\n -- Find Improvement:',delta, '!!!'))
                cat(paste('\n -- Find Global Improvement!!! Current Score:',fbest))
            }else{
                cat(paste('\n -- Failed~:',fx1, '(', delta,')'))
#                 bk <- bk + 1
#                 if (bk > 3){
#                     bk_s <- bk_s + 1
#                     break
                } 
            }   
        }
#         if (bk_s > 3) break
    }
}

### simple loop ###
for (c in 1:C){ 
    x1 <- xbest
    x1[,'ToyId'] <- sample(x1[,'ToyId']) ## reallocate Toys to a random chosen group of Elves
    
    fx1 <- solution_Elf(myToys, myelves, x1)
    delta <- fx1-fbest
    if(delta<0){
        xbest <- x1; fbest <- fx1
        cat(paste('\n -- Find Improvement:',delta, '!!!'))
        cat(paste('\n -- Find Global Improvement!!! Current Score:',fbest))
    }
}


##################
### Submission ###
##################
save(xbest, file='R_results/submit_1866324812.RData')
submit_best <- solution_Elf_submit(myToys, myelves, xbest)
submissions_output <- data.frame(ToyId = as.integer(submit_best[,1]), 
                                 ElfId = as.integer(submit_best[,2]), 
                                 StartTime = convert_to_chardate(submit_best[,3]), 
                                 Duration = as.integer(submit_best[,4]), stringsAsFactors = FALSE)
(submit_best[which.max(submit_best[,3]),3]+submit_best[which.max(submit_best[,3]), 4])*log(901)
write.csv(submissions_output, 'toys_submission_1865922691.csv', row.names = FALSE)
