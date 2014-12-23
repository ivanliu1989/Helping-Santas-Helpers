setwd('C:/Users/Ivan.Liuyanfeng/Desktop/Data_Mining_Work_Space/FICO/Helping-Santas-Helpers')

gc(); rm(list=ls())
source('R code/Functions.R')
load('data/toys.RData'); load('Latest Stable Models/heuristic/900_Folds.RData'); 

require(Rcpp)
sourceCpp('Latest Stable Models/heuristic/main_elf.cpp')

n <- 12
toys_dat <- data.frame(toys)
myToys <- data.matrix(toys_dat[index[[n]],])
schedule <- c(1:11113)#x_all[[n]]
myelves <- create_elves(1)
S <- c(1:13)
solution_Elf_submit_c(myToys,myelves,schedule, S)
