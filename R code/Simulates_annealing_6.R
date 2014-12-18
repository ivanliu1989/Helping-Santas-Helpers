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
load('data/900_Folds.RData')
load('simulated_annealing_1_100.RData')
toys_dat <- data.frame(toys)

#################
### Functions ###
#################
### f(x) ###
solution_Elf <- function(myToys, myelves, schedule){
    outcomes <- matrix(0, nrow = nrow(myToys), ncol = 4, 
                       dimnames = list(NULL, c('ToyId', 'ElfId', 'StartTime', 'Duration')))
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
        myelves[, 'next_available_time'] <- updateNextAvailableMinute(c_elf_start_time, work_duration)
        myelves[, 'current_rating'] <- updateProductivity(c_elf_start_time, work_duration, c_elf_rating)
        outcomes[current_toy,] <- c(c_toy_id, c_elf_id, c_elf_start_time, work_duration)
        if(current_toy %% 20000 == 0) cat('\nCompleted', current_toy/1000000, 'mil toys, makespan', c_elf_start_time, 'minutes',
                                          format(Sys.time(),format = '%Y-%m-%d %H:%M:%S')) 
    }
    return((outcomes[which.max(outcomes[,3]),3]+outcomes[which.max(outcomes[,3]), 4])*log(901))
}

### Submit ###
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

#########################
### Optimization Body ###
#########################
### main loop ###
index_range <- 1:10
for (index_num in index_range){
    n <- match(max(f_all),f_all)
    now <- Sys.time()
    cat(paste('\n\nRound :',index_num))
    cat(paste('\n Elf:',n))
    
    ### Toys establishment ###
    myToys <- data.matrix(toys_dat[index[[n]],])
    myToys <- myToys[order(myToys[,2]+myToys[,3], myToys[,2]),]
    schedule <- x_all[[n]]
    NUM_ELVES <- 1
    myelves <- create_elves(NUM_ELVES)
    
    ### parameters ###
    C <- 20 # multiple cooling chain
    N0 <- runif(C)*nrow(myToys) # initial point
    h <- 10 # used to modulate the step length.
    S <- 5 # current value times, step width
    x0 <- schedule; fx0 <- solution_Elf(myToys, myelves, x0)
    xbest <- x0; fbest <- fx0
    
    for (c in 1:C){ 
        toy_row <- nrow(myToys)
        Ns <- xbest[N0[c]]
        cat(paste('\nChain:',c, '; Initial point:', Ns, '; Current best score:', round(fbest)))
        bk <-0
        #         while(fbest > 1800000000){
        for (s in 1:S){   
            cat(paste('\n - Step:',s, 'bk:', bk))
            Np <- (1+h+s/10) 
            num <- length(max((Ns-Np),1):min((Ns+Np),toy_row))
            for (np in 1:num){ 
                partition_1 <- max(((np-1)/num)*toy_row + 1, 1) 
                partition_2 <- min((np/num)*toy_row, toy_row) 
                x1 <- xbest
                x1[partition_1:partition_2] <- sample(x1[partition_1:partition_2])
                fx1 <- solution_Elf(myToys, myelves, x1)
                delta <- fx1-fbest
                if(delta<0){
                    xbest <- x1; fbest <- fx1
                    cat(paste('\n -- Find Improvement:',round(delta), '!!!'))
                    cat(paste('\n -- Find Global Improvement!!! Current Score:',round(fbest), 'bk:', bk))
                    bk <- 0
                }else{
                    #                         cat(paste('\n -- Failed~:',fx1, '(', delta,')'))
                    bk <- bk + 1
                    #                         if (bk > 3) break
                }
                if (bk > 10) break
            }
        }
        #         }
    }
    ### Record ###
    x_all[[n]] <- xbest
    f_all[n] <- fbest
    #     outcome_all[[index_num]] <- solution_Elf_outcome(myToys, myelves, xbest)
    cat(paste('\n Time used:',Sys.time() - now, '!!!\n'))
}

save(fbest, xbest, file='elf_900.RData')
save(x_all,f_all, file='simulated_annealing_1_100.RData')
