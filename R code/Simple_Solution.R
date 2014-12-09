#########################
### assign_elf_to_toy ###
#########################
#     Given a toy, assigns the next elf to the toy. Computes the elf's updated rating,
#     applies the rest period (if any), and sets the next available time.
#     :param input_time: list of tuples (next_available_time, elf)
#     :param current_elf: elf object
#     :param current_toy: toy object
#     :param hrs: hours object
#     :return: list of elves in order of next available

assign_elf_to_toy <- function(input_time, current_elf, current_toy){
    start_time <- next_sanctioned_minute(input_time)  # double checks that work starts during sanctioned work hours
    duration <- ceiling(current_toy[4] / current_elf[2])
    sanctioned <- get_sanctioned_breakdown(start_time, duration)[1]
    unsanctioned <- get_sanctioned_breakdown(start_time, duration)[2]
    if(unsanctioned == 0){
        return (c(next_sanctioned_minute((start_time + duration)), duration))
    }else{
        return (c(apply_resting_period((start_time + duration),unsanctioned), duration))
    }
}

##################################
### solution_firstAvailableElf ###
##################################
#     Creates a simple solution where the next available elf is assigned a toy. Elves do not start
#     work outside of sanctioned hours.
#     :param toy_file: filename for toys file (input)
#     :param soln_file: filename for solution file (output)
#     :param myelves: list of elves in a priority queue ordered by next available time
#     :return:

solution_firstAvailableElf <- function(myToys, myelves){
    wcsv <- matrix(0, nrow = nrow(toys), ncol = 5, 
                       dimnames = list(NULL, c('ToyId', 'ElfId', 'StartTime', 'Duration', 'current_rating')))
    
    for(i in 1:nrow(myToys)){
        current_toy <- myToys[i,]
        
        #######################
        ### select next elf ###
        #######################
        myelves <- myelves[order(myelves[,2],decreasing = T),]
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
        # work_start_time cannot be before toy's arrival
        if (work_start_time < current_toy[3]){
            stop(paste('Work_start_time:', work_start_time, 'before arrival minute:',current_toy[3]))
        }
        
#         current_elf[3] <- assign_elf_to_toy(work_start_time, current_elf, current_toy)[1]
#         work_duration <- assign_elf_to_toy(work_start_time, current_elf, current_toy)[2]
        work_duration <- as.integer(ceiling(current_toy[4] / current_elf[2]))
        current_elf <- update_elf(current_elf, current_toy, work_start_time, work_duration)
        
        # put elf back in heap
        myelves[j,] <- current_elf
                
        # write to file in correct format
        time_string <- convert_to_chardate(work_start_time)
        wcsv <- rbind(wcsv, c(current_toy[2], current_elf[1], time_string, work_duration))
    }
    return(wcsv)
}


############
### MAIN ###
############
# tips: vectorize
setwd('/Users/ivan/Work_directory/FICO/Helping-Santas-Helpers/')
setwd('C:/Users/Ivan.Liuyanfeng/Desktop/Data_Mining_Work_Space/FICO/Helping-Santas-Helpers')
gc(); rm(list=ls())
source("R Code/hours.R"); source("R Code/elf.R"); source("R Code/toy.R")

start <- Sys.time()
NUM_ELVES <- 900

load('data/toys_rev2.RData')
# myToys <- toy_init(toys_rev2) 
# save(myToys, file='data/myToys.RData')

myelves <- create_elves(NUM_ELVES)
submissions <- solution_firstAvailableElf(myToys, myelves)

print (paste('total runtime = ', as.integer(Sys.time() - start)))
