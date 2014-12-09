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

is_sanctioned_time <- function(minute){
    return (((minute - day_start) %% minutes_in_24h) < (hours_per_day * 60))
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
    col_names <- c('elf_id', 'current_rating', 'next_available_time','score')
    elf_mat <- matrix(0, nrow = num_elves, ncol = length(col_names), dimnames = list(NULL,col_names))
    elf_mat[,'elf_id'] <- seq_len(num_elves)
    elf_mat[,'current_rating'] <- 1.0
    elf_mat[,'next_available_time'] <- 540 
    elf_mat[,'score'] <- 0
    return(elf_mat)
}

update_elf <- function(elf_list, toy, start_minute, duration){
    elf_list <- update_next_available_minute(elf_list, start_minute, duration)
    elf_list <- update_productivity(elf_list, start_minute, as.integer(ceiling(toy[4] / elf_list[2])))
    return(elf_list)
}

update_next_available_minute <- function(elf_list, start_minute, duration){
    sanctioned <- get_sanctioned_breakdown(start_minute, duration)[1]
    unsanctioned <- get_sanctioned_breakdown(start_minute, duration)[2] 
    end_minute <- start_minute + duration
    if (unsanctioned == 0 ){
        if (is_sanctioned_time(end_minute)){
            elf_list[3] <- end_minute
        }else{
            elf_list[3] <- next_sanctioned_minute(end_minute)
        }
    }else{
        elf_list[3] <- apply_resting_period(end_minute, unsanctioned)
    }
    return (elf_list)
}

update_productivity <- function(elf_list, start_minute, toy_required_minutes){
    sanctioned <- get_sanctioned_breakdown(start_minute, toy_required_minutes)[1]
    unsanctioned <- get_sanctioned_breakdown(start_minute, toy_required_minutes)[2]
    elf_list[2] <- max(0.25,min(4.0, elf_list[2] * (rating_increase ** (sanctioned/60)) * 
                                    (rating_decrease ** (unsanctioned/60))
    ))
    return(elf_list)
}

###########
### Toy ###
###########
toy_init <- function(input){
    id <- input$ToyId
    arrival_minute <- convert_to_minute(input[,2])
    duration <- as.integer(input$Duration)
    completed_minute <- 0  
    return(data.matrix(data.frame(reference_start_time = convert_to_minute(reference_time), id= id, 
                                  arrival_minute = arrival_minute, duration = duration, completed_minute = completed_minute)))
}

outside_toy_start_period <- function(arrival_minute, start_minute){
    return (as.integer(start_minute) < as.integer(arrival_minute))
}

is_complete <- function(duration, completed_minute, start_minute, elf_duration, rating){
    if (duration/rating <= elf_duration){
        completed_minute <- start_minute + ceiling(duration/rating)
        return(completed_minute)
    }else{
        return(FALSE)
    } 
}

################
### Solution ###
################
solution_sortedElf <- function(myToys, myelves){
    wcsv <- matrix(0, nrow = 0, ncol = 4, 
                   dimnames = list(NULL, c('ToyId', 'ElfId', 'StartTime', 'Duration')))   
    for(i in 1:nrow(myToys)){
        current_toy <- myToys[i,]
        
        ### select next elf ###
        myelves[,'score'] <- (max(myelves[,3], current_toy[3]) + (current_toy[4] * myelves[,2])) * 
            ifelse(myelves[,3]==540, log(1+1), log(1))
#         for (j in 1:nrow(myelves)){
#             myelves[j,'score'] <- (max(myelves[j,3], current_toy[3]) + (current_toy[4] * myelves[j,2])) * 
#                 ifelse(myelves[j,3]==540, log(1+1), log(1))
#         }
        myelves <- myelves[order(myelves[,'score']),]
#         myelves <- myelves[order(-myelves[,3], myelves[,2]),]
        for (j in 1:nrow(myelves)){
            if(current_toy[3] < myelves[j,3]){
                break   
            }
        }
        elf_available_time <- myelves[j, 3]
        current_elf <- myelves[j,]
        
        work_start_time <- elf_available_time
        if (current_toy[3] > elf_available_time){
            work_start_time <- current_toy[3]
        }
        
#         if (work_start_time < current_toy[3]){
#             stop(paste('Work_start_time:', work_start_time, 'before arrival minute:',current_toy[3]))
#         }
        
        work_duration <- as.integer(ceiling(current_toy[4] / current_elf[2]))
        current_elf <- update_elf(current_elf, current_toy, work_start_time, work_duration)
        
        # put elf back in heap
        myelves[j,] <- current_elf
        
        # write to file in correct format
        time_string <- convert_to_chardate(work_start_time)
#         wcsv[j,] <- c(current_toy[2], current_elf[1], time_string, work_duration)
        wcsv <- rbind(wcsv,c(current_toy[2], current_elf[1], time_string, work_duration))
        if(i %% 10000 == 0) cat('Completed', i/1000000, 'mil toys, makespan', time_string, 'minutes \n')
    }
    return(myelves)
}

############
### MAIN ###
############
# tips 1: new column - (p - finish time) * log(1+n)
setwd('/Users/ivan/Work_directory/FICO/Helping-Santas-Helpers/')
setwd('C:/Users/Ivan.Liuyanfeng/Desktop/Data_Mining_Work_Space/FICO/Helping-Santas-Helpers')
gc(); rm(list=ls())

start <- Sys.time()
NUM_ELVES <- 900

load('data/toys_rev2.RData')
load('data/myToys.RData')
load('data/sampleSubmission_rev1.RData')
# myToys <- toy_init(toys_rev2) 
# save(myToys, file='data/myToys.RData')

myelves <- create_elves(NUM_ELVES)
submissions_2 <- data.frame(solution_sortedElf(myToys[1:100,], myelves), stringsAsFactors = F)

print (paste('total runtime = ', as.integer(Sys.time() - start)))

write.csv(submission, 'toys_submission', row.names = FALSE)