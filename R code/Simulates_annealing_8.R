#############
### Setup ###
#############
setwd('/Users/ivan/Work_directory/FICO/Helping-Santas-Helpers/')
setwd('C:/Users/Ivan.Liuyanfeng/Desktop/Data_Mining_Work_Space/FICO/Helping-Santas-Helpers')
setwd('H:/Machine_Learning/FICO/Helping-Santas-Helpers')
gc(); rm(list=ls())
source('R code/Functions.R')
load('data/toys.RData'); load('data/900_Folds.RData'); 
load('optimization_results/Simulated_Annealing_1_300.RData') #==========>> load('simulated_annealing_1_900.RData')

#################
### Functions ###
#################
### f(x) ###
require(Rcpp)
sourceCpp('C++ code/c_Loop.cpp')

#########################
### Optimization Body ###
#########################
### main loop ###
index_range <- 1:888 # 5pm-8am | 1.8 min | 33/Hour | 215
toys_dat <- data.frame(toys)
C <- 5 # multiple cooling chain
h <- 0 # used to modulate the step length.
S <- c(1,10,30,100,300,1000,3000,6000,9000) # current value times, step width
Tolerance <- 2000
NUM_ELVES <- 1

for (index_num in index_range){
    Tolerance <- runif(1,min = 300,max = 2500)
    n <- match(max(f_all[1:300]),f_all) #==========>> 1:300 | 301:600 | 601:900 | 1:900
    set.seed(n)
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
        cat(paste('\nChain:',c, '; Initial point:', Ns, '; Current best score:', round(fbest), 'Toys:', toy_row))
        bk <-0
        for (s in S){   
            Np <- (1+s)# (1+h+s/10) 
            num <- length(max((Ns-Np),1):min((Ns+Np),toy_row))
            for (np in 1:num){
                x1 <- xbest
                p1 <- as.integer(max(((np-1)/num)*toy_row + 1, 1))
                p2 <- as.integer(min((np/num)*toy_row, toy_row))
                
                xbest <- assignX1(x1, np, toy_row, num, p1,p2, myToys, myelves, fbest, xbest)
                fbest <- solution_Elf_c(myToys, myelves, xbest) # 1836299515 \ 1832587189
                cat(paste('\nNew score:',fbest))
            }
        } 
    }
    x_all[[n]] <- xbest # Record
    f_all[n] <- fbest
    cat(paste('\n * Time used:',round(Sys.time() - now, digits = 2), '!!!\n'))
    if(index_num %% 50 == 0) save(x_all,f_all, file='optimization_results/Simulated_Annealing_1_300_temp.RData') #==========>> 1:300 | 301:600 | 600:900 | 1:900
}

for(n in 1:900){
    cat(paste('\n',length(table(x_all[[n]]))==length(x_all[[n]])))
}

save(x_all,f_all, file='optimization_results/Simulated_Annealing_1_300.RData') #==========>> 1:300 | 301:600 | 601:900 | 1:900
#1923241950.9752 | 1882883347(1882883346.8784)
