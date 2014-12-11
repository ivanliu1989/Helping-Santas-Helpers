######################
### Init Parameter ###
######################
hours_per_day <- 10
day_start <- 9 * 60
day_end <- (9 + hours_per_day) * 60
minutes_in_24h <- 24 * 60
reference_time <- as.POSIXct('2014 1 1 0 0', '%Y %m %d %H %M', tz = 'UTC')
rating_increase <- 1.02
rating_decrease <- 0.90

#############
### Hours ###
#############
convert_to_minute <- function(arrival) {
    arrive_time <- as.POSIXct(arrival, '%Y %m %d %H %M', tz = 'UTC')
    age <- as.integer(difftime(arrive_time, reference_time, units = 'mins', tz = 'UTC'))
    return(age)
}

convert_to_chardate <- function(arrive_int) {
    char_date <- format(reference_time + arrive_int * 60, format = '%Y %m %d %H %M', tz = 'UTC')
    return(char_date)
}

is_sanctioned_time <- function(minute) {
    is_sanctioned <- ((minute - day_start) %% minutes_in_24h) < (hours_per_day * 60)
    return(is_sanctioned)
}

get_sanctioned_breakdown <- function(start_minute, work_duration) {
    full_days <- as.integer(work_duration / minutes_in_24h)
    sanctioned <- full_days * hours_per_day * 60
    unsanctioned <- full_days * (24 - hours_per_day) * 60
    remainder_start <- start_minute + full_days * minutes_in_24h
    remainder_end <- start_minute + work_duration - 1 # to avoid off-by-one per R iterator
    if(remainder_end >= remainder_start) {
        sanctioned <- sanctioned + sum(is_sanctioned_time(remainder_start:remainder_end))
        unsanctioned <- unsanctioned + sum(!is_sanctioned_time(remainder_start:remainder_end))
    }
    return(c(sanctioned, unsanctioned))
}

next_sanctioned_minute <- function(minute) {
    if(is_sanctioned_time(minute) && is_sanctioned_time(minute + 1)) {
        next_min <- minute + 1
    } else {
        num_days <- as.integer(minute / minutes_in_24h)
        am_or_pm <- as.integer(((minute %% minutes_in_24h)/day_start)) 
        next_min <- day_start + (num_days + am_or_pm / 2) * minutes_in_24h
    }
    return(next_min)
}

apply_resting_period <- function(start, num_unsanctioned){
    num_days_since_jan1 <- as.integer(start / minutes_in_24h)
    rest_time <- num_unsanctioned
    rest_time_in_working_days <- as.integer(rest_time / (60 * hours_per_day))
    rest_time_remaining_minutes <- rest_time %% (60 * hours_per_day)
    local_start <- start %% minutes_in_24h
    if(local_start < day_start){
        local_start <- day_start
    }else if(local_start > day_end){
        num_days_since_jan1 <- num_days_since_jan1 + 1
        local_start <- day_start
    }
    if((local_start + rest_time_remaining_minutes) > day_end){
        rest_time_in_working_days <- rest_time_in_working_days + 1
        rest_time_remaining_minutes <- rest_time_remaining_minutes - (day_end - local_start)
        local_start <- day_start
    }
    total_days <- num_days_since_jan1 + rest_time_in_working_days
    return (total_days * minutes_in_24h + local_start + rest_time_remaining_minutes)
}

###########
### Elf ###
###########
create_elves <- function(num_elves){
    # col_names <- c('elf_id', 'current_rating', 'next_available_time')
    col_names <- c('elf_id', 'current_rating', 'next_available_time','score')
    elf_mat <- matrix(0, nrow = num_elves, ncol = length(col_names), dimnames = list(NULL,col_names))
    elf_mat[,'elf_id'] <- seq_len(num_elves)
    elf_mat[,'current_rating'] <- 1.0
    elf_mat[,'next_available_time'] <- 540 
    elf_mat[,'score'] <- 0
    return(elf_mat)
}

update_next_available_minute <- function(start_minute, work_duration) {
    sanctioned_breakdown <- get_sanctioned_breakdown(start_minute, work_duration)
    sanc_time <- sanctioned_breakdown[1]
    unsanc_time <- sanctioned_breakdown[2]
    end_minute <- start_minute + work_duration
    if(unsanc_time == 0) {
        if(is_sanctioned_time(end_minute)) {
            next_available_time <- end_minute
        } else {
            next_available_time <- next_sanctioned_minute(end_minute)
        }
    } else {
        next_available_time <- apply_resting_period(end_minute, unsanc_time)
    }
    return(next_available_time)
}


update_productivity <- function(start_minute, work_duration, current_rating) {
    sanctioned_breakdown <- get_sanctioned_breakdown(start_minute, work_duration)
    sanc_time <- sanctioned_breakdown[1]
    unsanc_time <- sanctioned_breakdown[2]
    new_rating <- max(c(0.25, min(c(4.0, current_rating * (rating_increase ** (sanc_time/60)) * (rating_decrease ** (unsanc_time/60))))))
    return(new_rating)
}

###########
### Toy ###
###########
toy_init <- function(toys){
    toys[,'Arrival_time'] <- convert_to_minute(toys[,'Arrival_time'])
    toys <- data.matrix(toys)
    return(toys)
}

################
### Solution ###
################
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
        
        myelves[next_elf, 'next_available_time'] <- update_next_available_minute(c_elf_start_time, work_duration)
        
        myelves[next_elf, 'current_rating'] <- update_productivity(c_elf_start_time, work_duration, c_elf_rating)
        
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

NUM_ELVES <- 900; s_toy <- 100; l_toy <- 720; train_elf <- 0.5; overwork_elf <- 3.9

    toys <- data.matrix(transform(toys, Size = 0))
    toys[which(toys[,'Duration']<=l_toy),'Size'] <- 2 # Median 19.3%
    toys[which(toys[,'Duration']<=s_toy),'Size'] <- 1 # Small 57.5%
    toys[which(toys[,'Duration']>l_toy),'Size'] <- 3 # Large 23.2%
    save(toys, file='data/toys_classified.RData')

load('data/toys_classified.RData')
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

write.csv(submissions_output, 'toys_submission_naive.csv', row.names = FALSE)
write.csv(data.frame(toys, stringsAsFactors = F), 'toys.csv', row.names = FALSE)

model_score <- convert_to_minute(submissions_output[nrow(submissions_output),3]) * log(1+NUM_ELVES)
