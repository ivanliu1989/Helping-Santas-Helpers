setwd('/Users/ivan/Work_directory/FICO/')
gc(); rm(list=ls())
toy <- read.csv('data/toys_rev1.csv')

## init ##
init <- c(
    hours_per_day <- 10, # 10 hour day: 9 - 19
    day_start <- 9 * 60,
    day_end <- (9 + hours_per_day) * 60,
    minutes_in_24h <- 24 * 60
)

convert_to_minute <- function(arrival){
#     Converts the arrival time string to minutes since the reference start time,
#     Jan 1, 2014 at 00:00 (aka, midnight Dec 31, 2013)
#     :param arrival: string in format '2014 12 17 7 03' for Dec 17, 2014 at 7:03 am
#     :return: integer (minutes since arrival time)
        
    time = strsplit(x = arrival, split = ' ')
    time1 = paste(paste(time[[1]][1],time[[1]][2],time[[1]][3],sep = '/'),paste(time[[1]][4],time[[1]][5],sep = ":"),sep = " ")
    time1 = strftime(time1, "%Y-%m-%d %H:%M:%OS")
    time2 = strftime("2014/1/1 0:0", "%Y-%m-%d %H:%M:%OS")
    age = as.integer(difftime(time1, time2, units = "mins"))
    return(age)
}
def is_sanctioned_time(init, minute):
    """ Return boolean True or False if a given time (in minutes) is a sanctioned working day minute.  """
return ((minute - self.day_start) % self.minutes_in_24h) < (self.hours_per_day * 60)

is_sanctioned_time <- function(init, minute){
#     Return boolean True or False if a given time (in minutes) is a sanctioned working day minute.
    ((minute - init[2])%init[4]) 
}