setwd('C:/Users/Ivan.Liuyanfeng/Desktop/Data_Mining_Work_Space/FICO/Helping-Santas-Helpers')

gc(); rm(list=ls())
source('R code/Functions.R')
load('data/toys.RData'); load('Latest Stable Models/heuristic/900_Folds.RData'); 

sourceCpp('Latest Stable Models/heuristic/main_elf.cpp')
require(Rcpp)

n <- 12
myToys <- data.matrix(toys_dat[index[[n]],])
schedule <- c(1:11113)#x_all[[n]]
myelves <- create_elves(1)

solution_Elf_submit_c(myToys,myelves,schedule)
