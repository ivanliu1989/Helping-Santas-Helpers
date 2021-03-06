#########################
### Schedule Strategy ###
#########################
assign_elf <- function(myelves, c_toy_size) {
    if(sum(myelves[,'score']==c_toy_size)<1) c_toy_size <- 2
    if(sum(myelves[,'score']==c_toy_size)<1) c_toy_size <- 1
    if(sum(myelves[,'score']==c_toy_size)<1) c_toy_size <- 3
    myelves <- myelves[which(myelves[,'score']==c_toy_size),]
    if(length(myelves)==4){
        assigned_elf <- as.integer(myelves['elf_id'])
    }else{
        assigned_elf <-as.integer(myelves[which.min(myelves[,'next_available_time']) ,'elf_id'][1])
    }
    return(assigned_elf)
}

#################
### Main Loop ###
#################
solution_sortedElf <- function(myToys, myelves){
    cat(format(Sys.time(),format = '%Y-%m-%d %H:%M:%S'))
    outcomes <- matrix(0, nrow = nrow(myToys), ncol = 5, 
                       dimnames = list(NULL, c('ToyId', 'ElfId', 'StartTime', 'Duration', 'current_rating')))
    
    for(current_toy in 1:nrow(myToys)){
        
        c_toy_id <- myToys[current_toy,'ToyId']
        c_toy_arrival <- myToys[current_toy, 'Arrival_time'] 
        c_toy_duration <- myToys[current_toy,'Duration']
        c_toy_size <- myToys[current_toy,'Size']
        
        myelves[which(myelves[,'current_rating']<overwork_elf),'score'] <- 2
        myelves[which(myelves[,'current_rating']<=train_elf),'score'] <- 1
        myelves[which(myelves[,'current_rating']>=overwork_elf),'score'] <- 3
        next_elf <- assign_elf(myelves, c_toy_size)
        
        c_elf_id <- myelves[next_elf, 'elf_id']
        c_elf_start_time <- myelves[next_elf, 'next_available_time']
        c_elf_rating <- myelves[next_elf, 'current_rating']
        
        if(c_elf_start_time < c_toy_arrival) c_elf_start_time <- c_toy_arrival    
        
        work_duration <- as.integer(ceiling(c_toy_duration/c_elf_rating))
        
        myelves[next_elf, 'next_available_time'] <- updateNextAvailableMinute(c_elf_start_time, work_duration)
        
        myelves[next_elf, 'current_rating'] <- updateProductivity(c_elf_start_time, work_duration, c_elf_rating)
        
        outcomes[current_toy,] <- c(c_toy_id, c_elf_id, c_elf_start_time, work_duration, c_elf_rating)
        
        if(current_toy %% 100000 == 0) cat('\nCompleted', current_toy/1000000, 'mil toys, makespan', c_elf_start_time, 'minutes',
                                           format(Sys.time(),format = '%Y-%m-%d %H:%M:%S'))    
    }
    cat('\nCompleted 10 mil toys at', convert_to_chardate(c_elf_start_time)) 
    return(outcomes)
}

############
### MAIN ###
############
# elf: Retrain-1 (0.25, 0.5) | Train-2 (0.5, 4.0) | Overwork-3 (4.0)
# toy: Small-1 | Median-2 | Large-3
# toy: 57% (0,100) mins | 20% (100,720) mins | 23% (720, inf) mins

setwd('/Users/ivan/Work_directory/FICO/Helping-Santas-Helpers/')
setwd('C:/Users/Ivan.Liuyanfeng/Desktop/Data_Mining_Work_Space/FICO/Helping-Santas-Helpers')
setwd('H:/Machine_Learning/FICO/Helping-Santas-Helpers')
gc(); rm(list=ls())
load('data/toys.RData')
NUM_ELVES <- 900; s_toy <- 100; l_toy <- 720; train_elf <- 0.5; overwork_elf <- 3.9

    toys <- data.matrix(transform(toys, Size = 0))
    toys[which(toys[,'Duration']<=l_toy),'Size'] <- 2 # Median 19.3%
    toys[which(toys[,'Duration']<=s_toy),'Size'] <- 1 # Small 57.5%
    toys[which(toys[,'Duration']>l_toy),'Size'] <- 3 # Large 23.2%
    save(toys, file='data/toys_classified.RData')

load('data/toys_regulated.RData')
toys <- toys[order(toys[,2]+toys[,3], toys[,2]),]

# par(mfcol=c(1,1))
# plot(density(toys[,'Arrival_time']))
# plot(table(toys[,'Duration']))
# boxplot(log(toys[,'Duration']))

myelves <- create_elves(NUM_ELVES)
submissions <- solution_sortedElf(toys, myelves)
submissions_output <- data.frame(ToyId = as.integer(submissions[,1]), 
                                 ElfId = as.integer(submissions[,2]), 
                                 StartTime = convert_to_chardate(submissions[,3]), 
                                 Duration = as.integer(submissions[,4]), stringsAsFactors = FALSE)
schedule <- data.matrix(submissions[,1:2])
save(schedule, file='baseSchedule.RData')
write.csv(submissions_output, 'toys_submission_18_Dec.csv', row.names = FALSE)

model_score <- (submissions[which.max(submissions[,3]),3]+submissions[which.max(submissions[,3]), 4])*log(901)
