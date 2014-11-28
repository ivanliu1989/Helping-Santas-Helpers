setwd('/Users/ivan/Work_directory/FICO/Helping-Santas-Helpers/')
gc(); rm(list=ls())
load('data/toys_rev1.RData')
source("hours.R")

######################
### Init Parameter ###
######################
reference_start_time <- strftime("2014/1/1 0:0", "%Y-%m-%d %H:%M:%OS")  # set when elf starts working on toy
id = toy$ToyId
arrival_minute <- sapply(toy$Arrival_time, function(x) convert_to_minute(as.character(x)))
duration <- as.integer(toy$Duration)
completed_minute <- 0


##################################
### 开始时间是否在到达时间之后 ###
##################################
def outside_toy_start_period(self, start_minute):
# Checks that work on toy does not start outside of the allowed starting period.
# :param hrs: Hours class
# :param start_minute: minute the work is scheduled to start
# :return: True of outside of allowed starting period, False otherwise    
return start_minute < self.arrival_minute


##################################
### 是否完成根据工作时间和效率 ###
##################################
def is_complete(self, start_minute, elf_duration, rating):
# Determines if the toy is completed given duration of work and elf's productivity rating
# param start_minute: minute work started
# param elf_duration: duration of work in minutes
# param rating: elf's productivity rating
# return: Boolean
if self.duration / rating <= elf_duration:
    self.completed_minute = start_minute + int(math.ceil(self.duration/rating))
return True
else:
    return False
