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
        myelves[c_elf_id, 'next_available_time'] <- update_next_available_minute(c_elf_start_time, work_duration)
        myelves[c_elf_id, 'current_rating'] <- update_productivity(c_elf_start_time, work_duration, c_elf_rating)
        
        outcomes[current_toy,] <- c(c_toy_id, c_elf_id, c_elf_start_time, work_duration)
        if(current_toy %% 2000000 == 0) cat('\nCompleted', current_toy/1000000, 'mil toys, makespan', c_elf_start_time, 'minutes',
                                           format(Sys.time(),format = '%Y-%m-%d %H:%M:%S')) 
    }
    return((outcomes[nrow(outcomes), 3]+outcomes[nrow(outcomes), 4])*log(901))
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
myToys <- toys; rm(toys)
schedule <- benchmark_schedule ## last optimal solution xbest(toyID, elfID)
NUM_ELVES <- 900
myelves <- create_elves(NUM_ELVES)

### parameters ###
C <- 3 # multiple cooling chain
N0 <- runif(C)*nrow(myToys) # initial point
h <- 5 # used to modulate the step length.
S <- 5 # current value times, step width
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
            
            fx1 <- solution_Elf(myToys, myelves, x1)
            delta <- fx1-fbest
            if(delta<0){
                xbest <- x1; fbest <- fx1
                cat(paste('\n -- Find Improvement:',delta, '!!!'))
                cat(paste('\n -- Find Global Improvement!!! Current Score:',fbest))
            }else{
                bk <- bk + 1
                if (bk > 3){
                    bk_s <- bk_s + 1
                    break
                } 
            }   
        }
        if (bk_s > 3) break
    }
}

### Speed up ###
library(compiler)
enableJIT(1)
c_soultion_Elf <- cmpfun(solution_Elf)

Rprof("out.out")
for (i in 1:1000) pos = rw2s1(1000)
Rprof(NULL)
summaryRprof("out.out")
# Extra parentheses
