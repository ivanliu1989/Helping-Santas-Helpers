sum_index <- c()
for(i in 1:900){
    sum_index <- c(sum_index,length(index[[i]]))
}

table(sum_index)

load('elf_900.RData')
load('data/toys.RData')
source('R code/Functions.R')
source('R code/c_Functions.r')
sourceCpp("R code/c_Functions.cpp")
toys_dat <- data.frame(toys)

NUM_ELVES <- 1
myelves <- create_elves(NUM_ELVES)
outcome_all <- matrix(0, nrow = 0, ncol = 5, 
                   dimnames = list(NULL, c('ToyId', 'ElfId', 'StartTime', 'Duration', 'current_rating')))

for (index_num in 1:2){
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
    outcome_all <- rbind(outcome_all_2, outcome)
    cat('\nsuccess! no:', index_num, 'score:', solution_Elf(myToys, myelves, schedule))
}