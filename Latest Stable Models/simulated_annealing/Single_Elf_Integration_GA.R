gc(); rm(list=ls())
load('data/toys_regulated.RData'); # load('Latest Stable Models/simulated_annealing/greedy_algorithm_solution.RData'); 
source('R code/Functions.R');source('R code/c_Functions.r')
library(Rcpp);sourceCpp("Latest Stable Models/simulated_annealing/c_Functions_GA.cpp")
load('comparison_GA.RData') # 'toy_id','elf_id','start_time','work_duration'
toys_dat <- data.frame(toys)
outcome_all <- matrix(0, nrow = 0, ncol = 4, 
                      dimnames = list(NULL, c('ToyId', 'ElfId', 'StartTime', 'Duration')))

#load('simulated_annealing_1_900.RData')

NUM_ELVES <- 1
myelves <- create_elves(NUM_ELVES)

for (index_num in 1:900){
    myelves[,'elf_id'] <- index_num
    myToys <- data.matrix(submissions_output[which(submissions_output[,2]==index_num),])
    myToys <- myToys[order(myToys[,3]),]
    outcome <- solution_Elf_submit_c(myToys, myelves)
    outcome_score <- (outcome[which.max(outcome[,3]),3]+outcome[which.max(outcome[,3]), 4])*log(901)
    outcome_all <- rbind(outcome_all, outcome)
    cat('\nsuccess! no:', index_num, 'score:', outcome_score)#,'fbest:',f_all[index_num])
}

dim(outcome_all); head(outcome_all); 
(outcome_all[which.max(outcome_all[,3]),3]+outcome_all[which.max(outcome_all[,3]), 4])*log(901)
(submissions_output[which.max(submissions_output[,3]),3]+submissions_output[which.max(submissions_output[,3]), 4])*log(901)

###
length(table(outcome_all[,1])); length(table(outcome_all[,2]))

submissions_output <- data.frame(ToyId = as.integer(outcome_all[,1]), 
                                 ElfId = as.integer(outcome_all[,2]), 
                                 StartTime = convert_to_chardate(outcome_all[,3]), 
                                 Duration = as.integer(outcome_all[,4]), stringsAsFactors = FALSE)
dim(submissions_output)

write.csv(submissions_output, 'toys_submission_SA_test.csv', row.names = FALSE)
(outcome_all[which.max(outcome_all[,3]),3]+outcome_all[which.max(outcome_all[,3]), 4])*log(901)

submit <- read.csv('toys_submission_SA_test.csv', stringsAsFactors=F)

a <- outcome_all[which(outcome_all[,'ElfId'] == 1),]
dim(a)
length(x_all[[1]])
identical(x_all[[1]], a[,1])

test <- cbind(x_all[[1]], a[,1])
head(test)
