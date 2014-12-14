setwd('/Users/ivan/Work_directory/FICO/Helping-Santas-Helpers/')
gc(); rm(list=ls())
source('R code/Functions.R')
load('data/toys.RData')

library(stringr)
library(lubridate)
library(plyr)

# df <- data.frame(DateTime = c("2010-01-01 00:00", "2010-01-01 01:00", "2010-01-01 02:00", "2010-01-01 03:00"))
# df <- mutate(df, DateTime = ymd_hm(DateTime),
#              time  = str_c(hour(DateTime), str_pad(minute(DateTime), 2, side = 'right', pad = '0'), sep = ':'))

### Transform ###
reference_time <- as.POSIXct('2014 1 1 0 0', '%Y %m %d %H %M', tz = 'UTC')

toys <- transform(toys, Arrival_time = convert_to_chardate(toys[,'Arrival_time']))
toys <- transform(toys, Arrival_time = as.POSIXct(toys[,'Arrival_time'], '%Y %m %d %H %M', tz = 'UTC'))
toys <- transform(toys, Date = paste(year(toys[,'Arrival_time']), month(toys[,'Arrival_time']), day(toys[,'Arrival_time'])))
toys <- transform(toys, Hour = hour(toys[,'Arrival_time']))

toys[toys[,'Hour'] < 9, 'Time'] <- '9 0'

toys[toys[,'Hour'] < 9, 'Time'] <- '9 0'

index_2 <- toys[,'Duration'] > 600 
toys[index_2, 'Date'] <- paste(year(toys[,'Arrival_time']), month(toys[,'Arrival_time']), day(toys[,'Arrival_time'])+1)
toys[index_2,'Time'] <- '9 0'