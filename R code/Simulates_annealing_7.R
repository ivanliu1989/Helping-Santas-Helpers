#############
### Setup ###
#############
setwd('/Users/ivan/Work_directory/FICO/Helping-Santas-Helpers/')
setwd('C:/Users/Ivan.Liuyanfeng/Desktop/Data_Mining_Work_Space/FICO/Helping-Santas-Helpers')
setwd('H:/Machine_Learning/FICO/Helping-Santas-Helpers')
gc(); rm(list=ls())
source('R code/Functions.R')
load('data/toys.RData'); load('data/900_Folds.RData'); load('simulated_annealing_601_900.RData')

#################
### Functions ###
#################
### f(x) ###
require(Rcpp)
sourceCpp('R code/c_Functions.cpp')

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
index_range <- 1:500 # 5pm-8am | 1.8 min | 33/Hour | 215
toys_dat <- data.frame(toys)
C <- 8 # multiple cooling chain
h <- 0 # used to modulate the step length.
S <- c(1,10,30,100,300,1000,3000,6000,9000) #c(1,3,9,30,90,300,1000,3000,9000) # current value times, step width
NUM_ELVES <- 1

for (index_num in index_range){
    n <- match(max(f_all[601:900]),f_all)
    set.seed(n)
    now <- Sys.time()
    cat(paste('\n\nRound :',index_num))
    cat(paste('\n Elf:',n))
    
    ### Toys establishment ###
    myToys <- data.matrix(toys_dat[index[[n]],])
    myToys <- myToys[order(myToys[,2]+myToys[,3], myToys[,2]),] # ??
    schedule <- x_all[[n]]
    myelves <- create_elves(NUM_ELVES)
    
    ### parameters ###
    N0 <- runif(C)*nrow(myToys) # initial point
    x0 <- schedule; fx0 <- solution_Elf_c(myToys, myelves, x0)
    xbest <- x0; fbest <- fx0
    
    for (c in 1:C){ 
        toy_row <- nrow(myToys)
        if(c==1){
            Ns <- 6588
        }else{
            Ns <- xbest[N0[c]]
        }
        Nd <- xbest[N0[min(c+1, C)]]
        cat(paste('\nChain:',c, '; Initial point:', Ns, '; Current best score:', round(fbest)))
        bk <-0
        for (s in S){   
            cat(paste('\n - Step:',s, 'bk:', bk))
            Np <- (1+h+s/10) 
            num <- length(max((Ns-Np),1):min((Ns+Np),toy_row))
            for (np in 1:num){ 
                p <- runif(1)
                #                 if(p<=0.5){
                partition_1 <- max(((np-1)/num)*toy_row + 1, 1) 
                partition_2 <- min((np/num)*toy_row, toy_row) 
                rep_range <- as.integer(partition_1:partition_2)
                x1 <- xbest
                x1[rep_range] <- sample(x1[rep_range])
                #                 }else{
                #                     partition_1 <- max((Ns-Np),1):min((Ns+Np),toy_row) ## New
                #                     partition_2 <- max((Nd-Np),1):min((Nd+Np),toy_row)
                #                     x1 <- xbest
                #                     ori_partition <- sample(x1[partition_1]) ## New
                #                     des_partition <- sample(x1[partition_2])
                #                     x1[partition_1] <- des_partition
                #                     x1[partition_2] <- ori_partition
                #                 }   
                fx1 <- solution_Elf_c(myToys, myelves, x1)
                delta <- fx1-fbest
                if(delta<0){
                    xbest <- x1; fbest <- fx1
                    cat(paste('\n -- Find Improvement:',round(delta), '!!!'))
                    cat(paste('\n -- Find Global Improvement!!! Current Score:',round(fbest), 'bk:', bk))
                    bk <- 0
                }else{
                    bk <- bk + 1
                }
                if (bk > 10) break
            }
        }
    }
    x_all[[n]] <- xbest # Record
    f_all[n] <- fbest
    #     outcome_all[[index_num]] <- solution_Elf_outcome(myToys, myelves, xbest)
    cat(paste('\n Time used:',round(Sys.time() - now, digits = 2), '!!!\n'))
}

save(fbest, xbest, file='elf_900.RData') #1784549033 1894363228
save(x_all,f_all, file='optimization_results/simulated_annealing_601_900.RData')

x_all <- x_1_300
f_all <- f_1_300




