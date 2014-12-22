############
### MAIN ###
############
#setwd('/Users/ivan/Work_directory/FICO/Helping-Santas-Helpers/')
setwd('C:/Users/Ivan.Liuyanfeng/Desktop/Data_Mining_Work_Space/FICO/Helping-Santas-Helpers')
#setwd('H:/Machine_Learning/FICO/Helping-Santas-Helpers')
gc(); rm(list=ls()); source('R code/Functions.R');
load('data/myToys.RData')
require(Rcpp)
sourceCpp('Latest Stable Models/modelling/main_elf.cpp')

NUM_ELVES <- 900
myelves <- create_elves(NUM_ELVES)
myelves_rate <- myelves[,'current_rating']

### Change ###
toys <- data.matrix(toys)
toys <- toys[order(toys[,4], toys[,3]),]

### Model ###
submissions <- solution_Elf(toys, myelves,myelves_rate)

(submissions[which.max(submissions[,3]),3]+submissions[which.max(submissions[,3]), 4])*log(901)

submissions_output <- data.frame(ToyId = as.integer(submissions[,1]), 
                                 ElfId = as.integer(submissions[,2]), 
                                 StartTime = convert_to_chardate(submissions[,3]), 
                                 Duration = as.integer(submissions[,4]), stringsAsFactors = FALSE)
write.csv(submissions_output, 'toys_submission_classification_sort.csv', row.names = FALSE)

# current best => 1842066118

# toys[,2&3] => 1855907408
# toys[,2] => 1875750845 1875613019(regulated)
# toys[,3<-2] => 1847256281 1847256281
# toys[,3-2] => 1873544890
# toys[,3<-2] => 14610970599 latest 14610970599
# toys[,3<-2] => 1846884694 regulated
# toys[,4(size)] => 1844916549
