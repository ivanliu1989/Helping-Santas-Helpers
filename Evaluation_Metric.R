setwd('/Users/ivan/Work_directory/FICO/Helping-Santas-Helpers/')
setwd('C:/Users/Ivan.Liuyanfeng/Desktop/Data_Mining_Work_Space/FICO/Helping-Santas-Helpers')
gc(); rm(list=ls())

source("hours.R"); source("elf.R"); source("toy.R")
elf <- data.frame(elfid = 1:900)

#################
### Read Toys ###
#################
read_toys <- function (toys_rev2, num_toys){
#     Reads the toy file and returns a dictionary of Toys.
#     Toy file format: ToyId, Arrival_time, Duration
#     ToyId: toy id
#     Arrival_time: time toy arrives. Format is: YYYY MM DD HH MM (space-separated)
#     Duration: duration in minutes to build toy
#     :param toy_file: toys input file
#     :param hrs: hours object
#     :param num_toys: total number of toys to build
#     :return: Dictionary of toys
    source("toy.R")
    toy_dict <- toy_init(toys_rev2)
    if(nrow(toy_dict) != num_toys){
        print(paste('\n ** Read a file with', nrow(toy_dict), 'toys, expected', num_toys, 'toys.'))
    }else{
        return(toy_dict)
    }
}
# toy_dict <- read_toys(toys_rev2, 10000000)


#################################
### Score the submission file ###
#################################


############
### MAIN ###
############
# Evaluation script for Helping Santa's Helpers, the 2014 Kaggle Holiday Optimization Competition.
start <- Sys.time()

NUM_TOYS <- 10000090
NUM_ELVES <- 900

load('data/toys_rev2.RData')
myToys <- read_toys(toys_rev2, NUM_TOYS)
print ' -- All toys read. Starting to score submission. '

load('data/sampleSubmission_rev2.RData')
hrs <- hours_init
score_submission(sampleSubmission_rev2, myToys, hrs, NUM_ELVES)

print(paste('total time =', as.integer(Sys.time() - start)))
