gc(); rm(list=ls())
load('data/900_Folds.RData')
load('data/toys.RData')
source('R code/Functions.R');source('R code/c_Functions.r')
library(Rcpp);sourceCpp("R code/c_Functions.cpp")
toys_dat <- data.frame(toys)
outcome_all <- matrix(0, nrow = 0, ncol = 4, 
                      dimnames = list(NULL, c('ToyId', 'ElfId', 'StartTime', 'Duration')))

load('simulated_annealing_1_900.RData')

NUM_ELVES <- 1
myelves <- create_elves(NUM_ELVES)

for (index_num in 1:900){
    myelves[,'elf_id'] <- index_num
    myToys <- data.matrix(toys_dat[index[[index_num]],])
    myToys <- myToys[order(myToys[,2]+myToys[,3], myToys[,2]),] # ??
    schedule <- x_comb[[index_num]] 
    outcome <- solution_Elf_submit_c(myToys, myelves, schedule)
    outcome_all <- rbind(outcome_all, outcome)
    cat('\nsuccess! no:', index_num, 'score:', solution_Elf_c(myToys, myelves, schedule),'fbest:',f_comb[index_num])
}

dim(outcome_all); head(outcome_all); 
length(table(outcome_all[,1])); length(table(outcome_all[,2]))

submissions_output <- data.frame(ToyId = as.integer(outcome_all[,1]), 
                                 ElfId = as.integer(outcome_all[,2]), 
                                 StartTime = convert_to_chardate(outcome_all[,3]), 
                                 Duration = as.integer(outcome_all[,4]), stringsAsFactors = FALSE)
dim(submissions_output)

write.csv(submissions_output, 'toys_submission_SA_test.csv', row.names = FALSE)
(outcome_all[which.max(outcome_all[,3]),3]+outcome_all[which.max(outcome_all[,3]), 4])*log(901)

submit <- read.csv('toys_submission_SA_test.csv', stringsAsFactors=F)

###############
x_comb <- list()
f_comb <- c()
load('optimization_results/simulated_annealing_1_300.RData')
for (i in 1:300){
    f_comb[i] <- f_all[i]
    x_comb[[i]] <- x_all[[i]]
}
load('optimization_results/simulated_annealing_301_600.RData')
for (i in 301:600){
    f_comb[i] <- f_all[i]
    x_comb[[i]] <- x_all[[i]]
}
load('optimization_results/simulated_annealing_601_900.RData')
for (i in 601:900){
    f_comb[i] <- f_all[i]
    x_comb[[i]] <- x_all[[i]]
}
save(x_comb,f_comb, file='optimization_results/Simulated_Annealing_1_900.RData') 
# 1849295401 