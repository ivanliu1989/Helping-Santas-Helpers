setwd('C:/Users/Ivan.Liuyanfeng/Desktop/Data_Mining_Work_Space/FICO/Helping-Santas-Helpers')
#gc(); rm(list=ls()); source('R code/Functions.R');
require(Rcpp)
sourceCpp('Latest Stable Models/greedy algorithm/main_greedy_3_2.cpp')

### Segmentation Elf ###
NUM_ELVES <- 900
myelves <- create_elves(NUM_ELVES)
myelves_rate <- myelves[,'current_rating']

### main loop ###
submissions <- solution_Elf(toys_0,toys_1,toys_2,toys_3,toys_4,toys_5,toys_6,toys_7,toys_8,
                            toys_9,toys_10,toys_11,toys_12,toys_13,toys_14,toys_15,toys_16,
                            toys_17,myelves,myelves_rate)
(submissions[which.max(submissions[,3]),3]+submissions[which.max(submissions[,3]), 4])*log(901)

submissions_output <- data.frame(ToyId = as.integer(submissions[,1]), 
                                 ElfId = as.integer(submissions[,2]), 
                                 StartTime = convert_to_chardate(submissions[,3]), 
                                 Duration = as.integer(submissions[,4]), stringsAsFactors = FALSE)
length(table(submissions[,1]));length(table(submissions[,2]))
write.csv(submissions_output, 'toys_submission_greedy_algorithm_1_2.csv', row.names = FALSE)


### start from 9.00 am, start time => proper toys dataset ###

### maintain skill at 4 set(time)

### decide circle => 2848(penalty free) (first arrive first work)

### end circle in an exhausion work

### set aside 14 days tasks to retrain

### assign remaining tasks before the exhausion tasks

### only fully train 1/2 longest exhausion tasks | train first 1/2 exhausion tasks

### 1. circle aside <= different chunks of tasks

# 1864597879
# 1843141195
# 1842881195
# 1842399667