#############
### Setup ###
#############
setwd('/Users/ivan/Work_directory/FICO/Helping-Santas-Helpers/')
setwd('C:/Users/Ivan.Liuyanfeng/Desktop/Data_Mining_Work_Space/FICO/Helping-Santas-Helpers')
setwd('H:/Machine_Learning/FICO/Helping-Santas-Helpers')
gc(); rm(list=ls())
source('R code/Functions.R')
load('data/toys.RData'); load('data/900_Folds.RData'); 
load('optimization_results/Simulated_Annealing_676_900.RData') #==========>> load('optimization_results/simulated_annealing_1_900.RData')
# x_all <- x_comb; f_all <- f_comb
# rm(x_comb);rm(f_comb)

#################
### Functions ###
#################
### f(x) ###
require(Rcpp)
sourceCpp('R code/c_Functions.cpp')

#########################
### Optimization Body ###
#########################
### main loop ###
index_range <- 1:8888 # 5pm-8am | 1.8 min | 33/Hour | 215
toys_dat <- data.frame(toys)
C <- 8 # multiple cooling chain
h <- 0 # used to modulate the step length.
S <- c(1,10,30,100,300,1000,3000,6000,9000) # current value times, step width
Tolerance <- 2000
NUM_ELVES <- 1

for (index_num in index_range){
    Tolerance <- runif(1,min = 10,max = 10000)
    #n <- match(min(f_all[676:900]),f_all) #==========>> 1:225 | 226:450 | 451:675 | 676:900
    n<-676
    set.seed(index_num)
    now <- Sys.time()
    cat(paste('\n\nRound :',index_num, Tolerance))
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
        Ns <- xbest[N0[c]]
        Nd <- xbest[N0[min(c+1, C)]]
        cat(paste('\nChain:',c, '; Initial point:', Ns, '; Current best score:', round(fbest)))
        bk <-0
        for (s in S){   
            Np <- (1+s)# (1+h+s/10) 
            num <- length(max((Ns-Np),1):min((Ns+Np),toy_row))
            for (np in 1:num){  
#                 p <- runif(1)
                x1 <- xbest
#                 if(p<=0.5){ # c++
                    partition_1 <- max(((np-1)/num)*toy_row + 1, 1) 
                    partition_2 <- min((np/num)*toy_row, toy_row) 
                    rep_range <- as.integer(partition_1:partition_2)
                    x1[rep_range] <- sample(x1[rep_range])
#                 }else{
#                     partition_1 <- max((Ns-Np),1):min((Ns+Np),toy_row) ## New
#                     partition_2 <- max((Nd-Np),1):min((Nd+Np),toy_row)
#                     regulate_rng <- min(length(partition_1),length(partition_2))
#                     partition_1 <- partition_1[1:regulate_rng]
#                     partition_2 <- partition_2[1:regulate_rng]
#                     ori_partition <- sample(x1[partition_1]) ## New
#                     des_partition <- sample(x1[partition_2])
#                     x1[partition_1] <- des_partition
#                     x1[partition_2] <- ori_partition
#                 } # c++  
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
    x_all[[n]] <- xbest # Record
    f_all[n] <- fbest
    cat(paste('\n * Time used:',round(Sys.time() - now, digits = 2), '!!!\n'))
    if(fbest < 1700000000) save(x_all,f_all, file='/Users/ivan/Google Drive/Simulated_Annealing_676_900_temp.RData')
    #if(index_num %% 50 == 0) save(x_all,f_all, file='/Users/ivan/Google Drive/Simulated_Annealing_676_900_temp.RData') #==========>> 1:225 | 226:450 | 451:675 | 676:900
}

for(n in 1:900){
    cat(paste('\n',length(table(x_all[[n]]))==length(x_all[[n]])))
}

save(x_all,f_all, file='optimization_results/Simulated_Annealing_676_900.RData') #==========>> 1:225 | 226:450 | 451:675 | 676:900
#1923241950.9752 | 1882883347(1882883346.8784)

