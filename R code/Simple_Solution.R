####################
### create_elves ###
####################
create_elves <- function(NUM_ELVES){
    #     Elves are stored in a sorted list using heapq to maintain their order by next available time.
    #     List elements are a tuple of (next_available_time, elf object)
    #     :return: list of elves
    elf <- data.frame(elfid = 1:900)
    list_elves <- elf_init(elf)
    return(list_elves)
}


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
        return (matrix(c(next_sanctioned_minute((start_time + duration)), duration),nrow=1))
    }else{
        return (matrix(c(apply_resting_period((start_time + duration),unsanctioned), duration),nrow=1))
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
    ref_time <- strptime(c("1.1.2014 0:0"), format = "%d.%m.%Y %H:%M")
    wcsv <- data.matrix(data.frame('ToyId'=0, 'ElfId'=0, 'StartTime'=0, 'Duration'=0))
    
    for(i in 1:nrow(myToys)){
        current_toy <- myToys[i,]
        
        #######################
        ### select next elf ###
        #######################
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
        
        #########################
        ### assign elf to toy ###
        #########################
        current_elf[3] <- assign_elf_to_toy(work_start_time, current_elf, current_toy)[1]
        work_duration <- assign_elf_to_toy(work_start_time, current_elf, current_toy)[2]
        current_elf <- update_elf(current_elf, current_toy, work_start_time, work_duration)
        
        # put elf back in heap
        myelves[j,] <- current_elf
        ########################
        ### sorting next elf ###
        ########################
        myelves <- myelves[order(myelves[,2],decreasing = T),]
        
        # write to file in correct format
        tt <- ref_time + 60*work_start_time
        time_string <- strftime(tt, '%Y %m %d %H %M')
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
elf <- data.frame(elfid = 1:900)
start <- Sys.time()
NUM_ELVES <- 900

load('data/toys_rev2.RData')
# myToys <- toy_init(toys_rev2) 
# save(myToys, file='data/myToys.RData')

myelves <- create_elves(NUM_ELVES)
submissions <- solution_firstAvailableElf(myToys, myelves)

print (paste('total runtime = ', as.integer(Sys.time() - start)))
