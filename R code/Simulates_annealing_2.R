#############
### Setup ###
#############
setwd('/Users/ivan/Work_directory/FICO/Helping-Santas-Helpers/')
setwd('C:/Users/Ivan.Liuyanfeng/Desktop/Data_Mining_Work_Space/FICO/Helping-Santas-Helpers')
setwd('H:/Machine_Learning/FICO/Helping-Santas-Helpers')
gc(); rm(list=ls())
source('R code/Functions.R')
load('data/toys.RData')
load('data/900_Folds.RData')

###################
### Calculation ###
###################
set.seed(8888)
toys_dat <- data.frame(toys)

### f(x) ###
solution_Elf <- function(myToys, myelves, schedule){
    # cat(format(Sys.time(),format = '%Y-%m-%d %H:%M:%S'))
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
        
        myelves[, 'next_available_time'] <- update_next_available_minute(c_elf_start_time, work_duration)
        
        myelves[, 'current_rating'] <- update_productivity(c_elf_start_time, work_duration, c_elf_rating)
        
        outcomes[current_toy,] <- c(c_toy_id, c_elf_id, c_elf_start_time, work_duration, c_elf_rating)
        
        #         if(current_toy %% 10000 == 0) cat('\nCompleted', current_toy/1000000, 'mil toys, makespan', c_elf_start_time, 'minutes',
        #                                            format(Sys.time(),format = '%Y-%m-%d %H:%M:%S'))    
    }
    # cat('\nCompleted 10 mil toys at', convert_to_chardate(c_elf_start_time)) 
    return(outcomes[nrow(outcomes), 3]+outcomes[nrow(outcomes), 4])
}

solution_Elf_outcome <- function(myToys, myelves, schedule){
    # cat(format(Sys.time(),format = '%Y-%m-%d %H:%M:%S'))
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
        
        myelves[, 'next_available_time'] <- update_next_available_minute(c_elf_start_time, work_duration)
        
        myelves[, 'current_rating'] <- update_productivity(c_elf_start_time, work_duration, c_elf_rating)
        
        outcomes[current_toy,] <- c(c_toy_id, c_elf_id, c_elf_start_time, work_duration, c_elf_rating)
        
        #         if(current_toy %% 10000 == 0) cat('\nCompleted', current_toy/1000000, 'mil toys, makespan', c_elf_start_time, 'minutes',
        #                                            format(Sys.time(),format = '%Y-%m-%d %H:%M:%S'))    
    }
    # cat('\nCompleted 10 mil toys at', convert_to_chardate(c_elf_start_time)) 
    return(outcomes)
}

#########################
### Optimization Body ###
#########################
index_range <- 1:50
x_all <- list()
f_all <- matrix()
outcome_all <- list()
for (index_num in index_range){
    
    ### Toys establishment ###
    myToys <- data.matrix(toys_dat[index[[index_num]],])
    myToys <- myToys[order(myToys[,2]+myToys[,3], myToys[,2]),]
    if(nrow(myToys)==length(xbest)){
        schedule <- xbest
    }else{
        schedule <- c(1:nrow(myToys))
    }
    NUM_ELVES <- 1
    myelves <- create_elves(NUM_ELVES)
    
    ### parameters ###
    C <- 10 # multiple cooling chain
    N0 <- runif(C)*nrow(myToys) # initial point
    h <- 10 # used to modulate the step length.
    S <- 10 # current value times, step width
    x0 <- schedule; fx0 <- solution_Elf(myToys, myelves, x0)
    xbest <- x0; fbest <- fx0
    
    ### main loop ###
    for (c in 1:C){ 
        toy_row <- nrow(myToys)
        Ns <- xbest[N0[c]]
        cat(paste('\nChain:',c, '; Initial point:', Ns, '; Current best score:', fbest))
        for (s in 1:S){ 
            cat(paste('\n - Step:',s))
            Np <- (1+h+s/10) 
            num <- length(max((Ns-Np),1):min((Ns+Np),toy_row))
            bk <-0
            for (np in 1:num){ 
                partition_1 <- max(((np-1)/num)*toy_row + 1, 1) 
                partition_2 <- min((np/num)*toy_row, toy_row)
                x1 <- xbest
                x1[partition_1:partition_2] <- sample(x1[partition_1:partition_2])
                
                fx1 <- solution_Elf(myToys, myelves, x1)
                delta <- fx1-fbest
                if(delta<0){
                    xbest <- x1; fbest <- fx1
                    cat(paste('\n -- Find Improvement:',delta, '!!!'))
                    cat(paste('\n -- Find Global Improvement!!! Current Score:',fbest))
                }else{
                    bk <- bk + 1
                    if (bk > 3) break
                }
                
            }
        }
    }
    
    ### Record ### 263161276 248065071636
    x_all[[index_num]] <- xbest
    f_all[index_num] <- fbest
    outcome_all[[index_num]] <- solution_Elf_outcome(myToys, myelves, xbest)
}
save(x_all,f_all,outcome_all, file='R_SA.RData')

### Test Validation ###
for (i in 1:900){
    outcome_all <- list()
    myToys <- data.matrix(toys_dat[index[[i]],])
    sch <- c(1:nrow(myToys))
    NUM_ELVES <- 1
    myelves <- create_elves(NUM_ELVES)
    outcome_all[[i]] <- solution_Elf_outcome(myToys, myelves, sch)
}
outcome_all[[601]] <- solution_Elf_outcome(myToys, myelves, xbest)
myToys <- data.matrix(toys_dat[index[[601]],])
outcome_all_in_one <- outcome_all[[1]]
for(i in 2:900){
    outcome_all_in_one <- rbind(outcome_all_in_one, outcome_all[[i]])
}
dim(outcome_all_in_one)
outcome_all[[4]]
