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
    duration <- as.integer(ceiling(current_toy$Duration / current_elf$rating))
    sanctioned <- get_sanctioned_breakdown(start_time, duration)$sanctioned
    unsanctioned <- get_sanctioned_breakdown(start_time, duration)$unsanctioned
    if(unsanctioned == 0){
        return (data.frame(next_sanctioned_minute((start_time + duration)), duration))
    }else{
        return (data.frame(apply_resting_period((start_time + duration),unsanctioned), duration))
    }
}


##################################
### solution_firstAvailableElf ###
##################################

############
### MAIN ###
############
start <- Sys.time()

NUM_ELVES <- 900

load('data/toys_rev2.RData')
load('data/sampleSubmission_rev2.RData')

myelves <- create_elves(NUM_ELVES)
solution_firstAvailableElf(toys_rev2, sampleSubmission_rev2, myelves)

print (paste('total runtime = ', as.integer(Sys.time() - start)))
