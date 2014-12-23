setwd('C:/Users/Ivan.Liuyanfeng/Desktop/Data_Mining_Work_Space/FICO/Helping-Santas-Helpers')
setwd('H:/Machine_Learning/FICO/Helping-Santas-Helpers')

gc(); rm(list=ls())
source('R code/Functions.R')
load('data/toys.RData'); load('Latest Stable Models/heuristic/900_Folds.RData'); 
load('optimization_results/overall_optimization.RData')

require(Rcpp)
sourceCpp('Latest Stable Models/heuristic/main_elf.cpp')

# n <- 12
toys_dat <- data.frame(toys)
# schedule <- c(1:11113)#x_all[[n]]
# myelves <- create_elves(1)
S <- c(1,3,10,30,90,300,900,1500,3000,6000,9000,15000,30000,60000,90000) 

#x_all <- list(); f_all <- c()
myToys <- data.matrix(toys_dat[index[[606]],])
schedule <- x_all[[606]]
myelves <- create_elves(1)

for(n in 1:900){
    cat('\n round:', n)
    #schedule <- c(1:nrow(myToys))#x_all[[n]]
    x_all[[606]] <- solution_Elf_submit_c(myToys,myelves,x_all[[606]], S)   
    if(nrow(myToys)!=length(table(x_all[[606]]))) break
}
solution_Elf_c(myToys, myelves, x_all[[606]])

for(n in 1:900){
    myToys <- data.matrix(toys_dat[index[[n]],])
    cat('\n',nrow(myToys)==length(table(x_all[[n]])))
}

for(n in 1:900){
    myToys <- data.matrix(toys_dat[index[[n]],])
    myelves <- create_elves(1)
    f_all[n] <- solution_Elf_c(myToys, myelves, x_all[[n]])
}

save(x_all,f_all, file='optimization_results/overall_optimization.RData')
range(f_all)

for(n in 1:900){
    myToys <- data.matrix(toys_dat[index[[n]],])
    if(nrow(myToys)==11109){
        x0 <- x_all[[398]]
        x0 <- x0[-which(x0==11113)]
        x0 <- x0[-which(x0==11112)]
        x0 <- x0[-which(x0==11111)]
        x0 <- x0[-which(x0==11110)]
        f_all[n] <- solution_Elf_c(myToys, myelves, x0)
    }else if(nrow(myToys)==11110){
        x0 <- x_all[[398]]
        x0 <- x0[-which(x0==11113)]
        x0 <- x0[-which(x0==11112)]
        x0 <- x0[-which(x0==11111)]
        f_all[n] <- solution_Elf_c(myToys, myelves, x0)
    }else if(nrow(myToys)==11111){
        x0 <- x_all[[398]]
        x0 <- x0[-which(x0==11113)]
        x0 <- x0[-which(x0==11112)]
        f_all[n] <- solution_Elf_c(myToys, myelves, x0)        
    }else if(nrow(myToys)==11112){
        x0 <- x_all[[398]]
        x0 <- x0[-which(x0==11113)]
        f_all[n] <- solution_Elf_c(myToys, myelves, x0)
    }else{
        f_all[n] <- solution_Elf_c(myToys, myelves, x_all[[398]])
    }
    cat('\n',n, ' ', f_all[n]);
}
range(f_all)
