sum_index <- c()
for(i in 1:900){
    sum_index <- c(sum_index,length(index[[i]]))
}

table(sum_index)

load('elf_900.RData')
load('data/900_Folds.RData')
load('data/toys.RData')
source('R code/Functions.R')
source('R code/c_Functions.r')
library(Rcpp)
sourceCpp("R code/c_Functions.cpp")
toys_dat <- data.frame(toys)

NUM_ELVES <- 1
myelves <- create_elves(NUM_ELVES)
outcome_all <- matrix(0, nrow = 0, ncol = 5, 
                   dimnames = list(NULL, c('ToyId', 'ElfId', 'StartTime', 'Duration', 'current_rating')))

for (index_num in 1:900){
    myToys <- data.matrix(toys_dat[index[[index_num]],])
    if (nrow(myToys)==11109){
        schedule <- xbest[-c(11110)]
    }else if(nrow(myToys)==11110){
        schedule <- xbest
    }else if(nrow(myToys)==11111){
        schedule <- xbest
        schedule[11111] <- 11111
    }else if(nrow(myToys)==11112){
        schedule <- xbest
        schedule[11111] <- 11111
        schedule[11112] <- 11112
    }else if(nrow(myToys)==11113){
        schedule <- xbest
        schedule[11111] <- 11111
        schedule[11112] <- 11112
        schedule[11113] <- 11113
    }
    outcome <- solution_Elf_submit(myToys, myelves, schedule)
    outcome_all <- rbind(outcome_all, outcome)
    cat('\nsuccess! no:', index_num, 'score:', solution_Elf(myToys, myelves, schedule))
}

submissions_output <- data.frame(ToyId = as.integer(outcome_all[,1]), 
                                 ElfId = as.integer(outcome_all[,2]), 
                                 StartTime = convert_to_chardate(outcome_all[,3]), 
                                 Duration = as.integer(outcome_all[,4]), stringsAsFactors = FALSE)
# schedule <- data.matrix(submissions[,1:2])
# save(schedule, file='baseSchedule.RData')
write.csv(submissions_output, 'toys_submission_18_Dec.csv', row.names = FALSE)

(outcome_all[which.max(outcome_all[,3]),3]+outcome_all[which.max(outcome_all[,3]), 4])*log(901)
