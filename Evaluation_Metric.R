setwd('/Users/ivan/Work_directory/FICO/Helping-Santas-Helpers/')
setwd('C:/Users/Ivan.Liuyanfeng/Desktop/Data_Mining_Work_Space/FICO/Helping-Santas-Helpers')
gc(); rm(list=ls())

load('data/toys_rev2.RData')
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