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
load('data/toys.RData')
load('R_results/baseSchedule.RData')
load('data/900_Folds.RData')

toys_dat <- data.frame(toys)

#################
### Functions ###
#################
### f(x) ###
require(Rcpp)
sourceCpp('R code/c_Functions.cpp')

#########################
### Optimization Body ###
#########################
load('elf_1.RData')
### main loop ###
index_range <- 1:900
x_all <- list()
f_all <- matrix()
outcome_all <- list()
for (index_num in index_range){
    now <- Sys.time()
    cat(paste('\n\n Elf:',index_num))
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
    C <- 1 # multiple cooling chain
    N0 <- runif(C)*nrow(myToys) # initial point
    h <- 10 # used to modulate the step length.
    S <- 5 # current value times, step width
    x0 <- schedule; fx0 <- solution_Elf_c(myToys, myelves, x0)
    xbest <- x0; fbest <- fx0
    
    for (c in 1:C){ 
        toy_row <- nrow(myToys)
        Ns <- xbest[N0[c]]
        cat(paste('\nChain:',c, '; Initial point:', Ns, '; Current best score:', round(fbest)))
            for (s in 1:S){   
                cat(paste('\n - Step:',s, 'bk:', bk))
                Np <- (1+h+s/10) 
                num <- length(max((Ns-Np),1):min((Ns+Np),toy_row))
                for (np in 1:num){ 
                    partition_1 <- max(((np-1)/num)*toy_row + 1, 1) 
                    partition_2 <- min((np/num)*toy_row, toy_row) 
                    x1 <- xbest
                    x1[partition_1:partition_2] <- sample(x1[partition_1:partition_2])
                    fx1 <- solution_Elf_c(myToys, myelves, x1)
                    delta <- fx1-fbest
                    if(delta<0){
                        xbest <- x1; fbest <- fx1
                        cat(paste('\n -- Find Improvement:',round(delta), '!!!'))
                        cat(paste('\n -- Find Global Improvement!!! Current Score:',round(fbest))
                    }
                }
            }
    }
    ### Record ###
    x_all[[index_num]] <- xbest
    f_all[index_num] <- fbest
#     outcome_all[[index_num]] <- solution_Elf_outcome(myToys, myelves, xbest)
    cat(paste('\n Time used:',Sys.time() - now, '!!!\n'))
}

save(x_all,f_all, file='simulated_annealing_1_900.RData')
