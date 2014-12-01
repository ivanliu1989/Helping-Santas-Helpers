setwd('/Users/ivan/Work_directory/FICO/Helping-Santas-Helpers/')
setwd('C:/Users/Ivan.Liuyanfeng/Desktop/Data_Mining_Work_Space/FICO/Helping-Santas-Helpers')
gc(); rm(list=ls())

source("hours.R"); source("elf.R"); source("toy.R")
elf <- data.frame(elfid = 1:900)

####################
### create_elves ###
####################
create_elves <- function(NUM_ELVES){
    #     Elves are stored in a sorted list using heapq to maintain their order by next available time.
    #     List elements are a tuple of (next_available_time, elf object)
    #     :return: list of elves
    source("elf.R")
    elf <- data.frame(elfid = 1:900)
    list_elves <- elf_init(elf)
    return(list_elves)
}


#########################
### assign_elf_to_toy ###
#########################
assign_elf_to_toy <- function(input_time, current_elf, current_toy, hrs){
    #     Given a toy, assigns the next elf to the toy. Computes the elf's updated rating,
    #     applies the rest period (if any), and sets the next available time.
    #     :param input_time: list of tuples (next_available_time, elf)
    #     :param current_elf: elf object
    #     :param current_toy: toy object
    #     :param hrs: hours object
    #     :return: list of elves in order of next available
    source("hours.R")
    start_time <- next_sanctioned_minute(hours_init, input_time)  # double checks that work starts during sanctioned work hours
    duration <- as.integer(ceiling(current_toy$duration / current_elf$rating))
    sanctioned <- get_sanctioned_breakdown(hrs, start_time, duration)$sanctioned
    unsanctioned <- get_sanctioned_breakdown(hrs, start_time, duration)$unsanctioned
    if(unsanctioned == 0){
        return (data.frame(next_sanctioned_minute((start_time + duration)), duration))
    }else{
        return (data.frame(apply_resting_period(hrs, (start_time + duration),unsanctioned), duration))
    }
}

##################################
### solution_firstAvailableElf ###
##################################
solution_firstAvailableElf <- function(toy_file, soln_file, myelves){
    #     Creates a simple solution where the next available elf is assigned a toy. Elves do not start
    #     work outside of sanctioned hours.
    #     :param toy_file: filename for toys file (input)
    #     :param soln_file: filename for solution file (output)
    #     :param myelves: list of elves in a priority queue ordered by next available time
    #     :return:
    source("hours.R")
    hrs <- hours_init
    ref_time <- strptime(c("1.1.2014 0:0"), format = "%d.%m.%Y %H:%M")
    myToys <- toy_init(toy_file)
    wcsv <- data.frame()
    
    for(i in 1:nrow(myToys)){
        current_toy <- myToys[i,]
        
        # get next available elf
        elf_available_time <- myelves$next_available_time[1]
        current_elf <- myelves[1,]
        
        work_start_time <- elf_available_time
        if (current_toy$arrival_minute > elf_available_time){
            work_start_time <- current_toy$arrival_minute
        }
        
        # work_start_time cannot be before toy's arrival
        if (work_start_time < current_toy$arrival_minute){
            stop(paste('Work_start_time:', work_start_time, 'before arrival minute:',current_toy$arrival_minute))
        }
        current_elf$next_available_time[1] <- assign_elf_to_toy(work_start_time, current_elf, current_toy, hrs)[1]
        work_duration <- assign_elf_to_toy(work_start_time, current_elf, current_toy, hrs)[2]
        current_elf <- update_elf(current_elf, hrs, current_toy, work_start_time, work_duration)
        
        # put elf back in heap
        myelves[1,] <- current_elf
        myelves <- myelves[order(myelves$next_available_time),]
        
        # write to file in correct format
        tt <- ref_time + 60*work_start_time
        time_string <- strftime(tt, '%Y %m %d %H %M')
        wcsv <- rbind(wcsv, c(current_toy$id, current_elf$id, time_string, work_duration))
    }
    colnames(wcsv) <- c('ToyId', 'ElfId', 'StartTime', 'Duration')
    return(wcsv)
}


############
### MAIN ###
############
start <- Sys.time()

NUM_ELVES <- 900

load('data/toys_rev2.RData')
load('data/sampleSubmission_rev1.RData')

myelves <- create_elves(NUM_ELVES)
submissions <- solution_firstAvailableElf(toys_rev2, sample, myelves)

print (paste('total runtime = ', as.integer(Sys.time() - start)))
