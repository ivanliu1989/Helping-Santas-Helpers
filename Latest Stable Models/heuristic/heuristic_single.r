#############
### Setup ###
#############
setwd('/Users/ivan/Work_directory/FICO/Helping-Santas-Helpers/')
setwd('C:/Users/Ivan.Liuyanfeng/Desktop/Data_Mining_Work_Space/FICO/Helping-Santas-Helpers')
setwd('H:/Machine_Learning/FICO/Helping-Santas-Helpers')
gc(); rm(list=ls())
source('R code/Functions.R')
load('data/toys.RData'); load('Latest Stable Models/heuristic/900_Folds.RData'); 
load('Latest Stable Models/heuristic/single_optimization.RData') 

#################
### Functions ###
#################
### f(x) ###
require(Rcpp)
sourceCpp('Latest Stable Models/heuristic/c_Functions.cpp')

#########################
### Optimization Body ###
#########################
#x_all <- list(); f_all <- c()
index_range <- 2:41
toys_dat <- data.frame(toys)
C <- 4 
S <- c(1,10,30,100,300,1000,3000,6000,9000) 
NUM_ELVES <- 1

for (index_num in index_range){
    Tolerance <- 2500 #runif(1,min = 10,max = 10000)
    n<-candidate[index_num]
    set.seed(index_num)
    cat(paste('\n\nRound :',index_num, Tolerance))
    cat(paste('\n Elf:',n))
    
    ### Toys establishment ###
    myToys <- data.matrix(toys_dat[index[[n]],])
    schedule <- c(1:11113)#x_all[[n]]
    myelves <- create_elves(NUM_ELVES)
    
    ### parameters ###
    N0 <- runif(C)*nrow(myToys) # initial point
    x0 <- schedule; fx0 <- solution_Elf_c(myToys, myelves, x0)
    xbest <- x0; fbest <- fx0
    
    for (c in 1:C){ 
        toy_row <- nrow(myToys)
        Ns <- xbest[N0[c]]
        Nd <- xbest[N0[min(c+1, C)]]
        cat(paste('\nChain:',c, '; Initial point:', Ns, '; Current best score:', round(fbest)))
        bk <-0
        for (s in S){   
            Np <- (1+s)
            num <- length(max((Ns-Np),1):min((Ns+Np),toy_row))
            for (np in 1:num){  
                x1 <- xbest
                partition_1 <- max(((np-1)/num)*toy_row + 1, 1) 
                partition_2 <- min((np/num)*toy_row, toy_row) 
                rep_range <- as.integer(partition_1:partition_2)
                x1[rep_range] <- sample(x1[rep_range])
                fx1 <- solution_Elf_c(myToys, myelves, x1)
                delta <- fx1-fbest
                if(delta<0){
                    a <- length(x1); b <- length(table(x1))
                    if(a==b){
                        xbest <- x1; fbest <- fx1
                        cat(paste('\n +++++ Find Improvement:',round(delta), '!!! Current Score:',round(fbest)))
                        bk <- 0
                    }else{
                        cat(paste('\n ----- Error happened during scheduling!!! Toy Number:',a, 'Unique Tasks:',b))
                        break
                    }
                }else{
                    bk <- bk + 1
                }
                if (bk > Tolerance) break
            }
        }
        
    }
    x_all[[n]] <- xbest
    f_all[n] <- fbest
    cat(paste('\n * Time used:',round(Sys.time() - now, digits = 2), '!!!\n'))
    if(fbest < 1700000000) save(x_all,f_all, file='/Users/ivan/Google Drive/Simulated_Annealing_676_900_temp.RData')
    #if(index_num %% 50 == 0) save(x_all,f_all, file='/Users/ivan/Google Drive/Simulated_Annealing_676_900_temp.RData') #==========>> 1:225 | 226:450 | 451:675 | 676:900
}

save(x_all,f_all, file='optimization_results/single_optimization.RData')
