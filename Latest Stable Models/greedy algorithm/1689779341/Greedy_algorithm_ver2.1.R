#setwd('C:/Users/Ivan.Liuyanfeng/Desktop/Data_Mining_Work_Space/FICO/Helping-Santas-Helpers')
#gc(); rm(list=ls()); source('R code/Functions.R');
require(Rcpp)
sourceCpp('Latest Stable Models/greedy algorithm/main_greedy_ver2.1.cpp')

### Segmentation Elf ###
NUM_ELVES <- 900
myelves <- create_elves(NUM_ELVES)
myelves_rate <- myelves[,'current_rating']

### main loop ###
submissions <- solution_Elf(toys_0,toys_1,toys_2,toys_3,toys_4,toys_5,toys_6,toys_7,toys_8,
                            toys_9,toys_10,toys_11,toys_12,toys_13,toys_14,toys_15,toys_16,
                            toys_17,myelves,myelves_rate)
(submissions[which.max(submissions[,3]),3]+submissions[which.max(submissions[,3]), 4])*log(1+NUM_ELVES)

length(table(submissions[,1]));length(table(submissions[,2]))

submissions_output <- data.frame(ToyId = as.integer(submissions[,1]), 
                                 ElfId = as.integer(submissions[,2]), 
                                 StartTime = convert_to_chardate(submissions[,3]), 
                                 Duration = as.integer(submissions[,4]), stringsAsFactors = FALSE)
write.csv(submissions_output, 'toys_submission_greedy_algorithm_1_2.csv', row.names = FALSE)

# 1690679003
# 1690354959
# 1689779341

x_all <- list()
for (i in 1:900){
    x_all[[i]] <- submissions[which(submissions[,2]==i), 1]
}
save(x_all, file='Latest Stable Models/simulated_annealing/greedy_algorithm_solution.RData')

## search toys_0 | chunk of toys_0 = train1-14
## delay = 51274116 | 3% | 1644365377
