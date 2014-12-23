setwd('/Users/ivan/Work_directory/FICO/Helping-Santas-Helpers/')
#setwd('C:/Users/Ivan.Liuyanfeng/Desktop/Data_Mining_Work_Space/FICO/Helping-Santas-Helpers')
#setwd('H:/Machine_Learning/FICO/Helping-Santas-Helpers')
gc(); rm(list=ls()); source('R code/Functions.R');
load('data/toys.RData')
require(Rcpp)
sourceCpp('Latest Stable Models/simulated_annealing/main_elf.cpp')
#############
### Setup ###
#############
load('data/toys.RData'); load('data/900_Folds.RData'); 
load('optimization_results/Simulated_Annealing_676_900.RData') #==========>> load('optimization_results/simulated_annealing_1_900.RData')
# x_all <- x_comb; f_all <- f_comb
# rm(x_comb);rm(f_comb)
schedule_c <- x_all[[1]]
toys <- toys[index[[1]],]
myelves <- create_elves(1)

sa_result <- solution_Elf_submit_c(toys, myelves, schedule_c)

rnd(1,1231231)
