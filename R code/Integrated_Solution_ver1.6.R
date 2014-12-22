#################
### Main Loop ###
#################
solution_sortedElf <- function(myToys, myelves){
    cat(format(Sys.time(),format = '%Y-%m-%d %H:%M:%S'))
    outcomes <- matrix(0, nrow = nrow(myToys), ncol = 5, 
     dimnames = list(NULL, c('ToyId', 'ElfId', 'StartTime', 'Duration', 'current_rating')))
    
    for(i in 1:10000000){
        next_elf <- myelves[which.min(myelves[,3]),]
        c_elf_id <- next_elf[1]
        c_elf_start_time <- next_elf[3]
        c_elf_rating <- next_elf[2]
        
        if(c_elf_rating == 4.0){
            myToys <- myToys[which(myToys[,'Size']==3),]
        }else if(c_elf_rating > 3){
            myToys <- myToys[which(myToys[,'Size']==2),]
        }else{
            myToys <- myToys[which(myToys[,'Size']==1),]
        }

        myToys[,'r_duration'] <- as.integer(ceiling(myToys[,'Duration']/c_elf_rating))
        for(j in 1:20){
            myToys[j,'start_time'] <- max(c_elf_start_time, myToys[j, 'Arrival_time']) #
            myToys[j,'rate_f'] <- updateProductivity(myToys[j,'rate_f'], myToys[j,'r_duration'], c_elf_rating)
            myToys[j,'refresh'] <- updateNextAvailableMinute(myToys[j,'start_time'], myToys[j,'r_duration'])
        }
        myToys[,'finish'] <- myToys[,'r_duration']+ myToys[,'start_time']
        

        c_toy <- myToys[order(-myToys[,'rate_f'],myToys[,'refresh'],myToys[,'finish']),][1,]
        c_toy_id <- c_toy[1]
        c_elf_start_time <- c_toy[6] 
        work_duration <- c_toy[5]

        myelves[c_elf_id, 'next_available_time'] <- myToys[c_toy_id,'refresh']
        myelves[c_elf_id, 'current_rating'] <- myToys[c_toy_id,'rate_f']
        outcomes[i,] <- c(c_toy_id, c_elf_id, c_elf_start_time, work_duration, c_elf_rating)
        myToys <- myToys[-c_toy_id,]
        
        if(i %% 100000 == 0) cat('\nCompleted', i/1000000, 'mil toys, makespan', c_elf_start_time, 'minutes',
            format(Sys.time(),format = '%Y-%m-%d %H:%M:%S'))    
        }
        cat('\nCompleted 10 mil toys at', convert_to_chardate(c_elf_start_time)) 
        return(outcomes)
    }
############
### MAIN ###
############
setwd('/Users/ivan/Work_directory/FICO/Helping-Santas-Helpers/')
setwd('C:/Users/Ivan.Liuyanfeng/Desktop/Data_Mining_Work_Space/FICO/Helping-Santas-Helpers')
setwd('H:/Machine_Learning/FICO/Helping-Santas-Helpers')
gc(); rm(list=ls()); source('R code/Functions.R'); source('R code/c_Functions.r')
load('data/toys.RData')

NUM_ELVES <- 900
myelves <- create_elves(NUM_ELVES)

toy_break1 <- 600; toy_break2 <- 2880
toys <- data.matrix(transform(toys, Size = 0))
toys <- data.matrix(transform(toys, r_duration = 0))
toys <- data.matrix(transform(toys, start_time = 0))
toys <- data.matrix(transform(toys, finish = 0))
toys <- data.matrix(transform(toys, rate_f = 0))
toys <- data.matrix(transform(toys, refresh = 0))
toys <- data.matrix(transform(toys, evaluate = 0))
toys[which(toys[,'Duration']>toy_break2),'Size'] <- 3 # overwork
toys[which(toys[,'Duration']<=toy_break2),'Size'] <- 2 # 48 hour
toys[which(toys[,'Duration']<=toy_break1),'Size'] <- 1 # 10 hour
myToys_1<- toys[which(toys[,'Size']==1),]
myToys_2<- toys[which(toys[,'Size']==2),]
myToys_3<- toys[which(toys[,'Size']==3),]

submissions <- solution_sortedElf(myToys_1, myToys_2, myToys_3, myelves)
submissions_output <- data.frame(ToyId = as.integer(submissions[,1]), 
   ElfId = as.integer(submissions[,2]), 
   StartTime = convert_to_chardate(submissions[,3]), 
   Duration = as.integer(submissions[,4]), stringsAsFactors = FALSE)
(submissions[which.max(submissions),3]+submissions[which.max(submissions[,3]), 4])*log(901)
write.csv(submissions_output, 'toys_submission_classification_sort.csv', row.names = FALSE)
