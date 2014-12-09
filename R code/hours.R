######################
### Init Parameter ###
######################
hours_per_day <- 10 # 10 hour day: 9 - 19
day_start <- 9 * 60
day_end <- (9 + hours_per_day) * 60
minutes_in_24h <- 24 * 60
reference_time <- as.POSIXct('2014 1 1 0 0', '%Y %m %d %H %M', tz = 'UTC')

# hours_init <- data.frame(
#     hours_per_day = hours_per_day, # 10 hour day: 9 - 19
#     day_start = day_start,
#     day_end = day_end,
#     minutes_in_24h = minutes_in_24h
# )


####################################
### 目前时间和开始时间相差分钟数 ###
####################################
#     Converts the arrival time string to minutes since the reference start time,
#     Jan 1, 2014 at 00:00 (aka, midnight Dec 31, 2013)
#     :param arrival: string in format '2014 12 17 7 03' for Dec 17, 2014 at 7:03 am
#     :return: integer (minutes since arrival time)

# convert_to_minute <- function(arrival){
#     time <- strsplit(x = arrival, split = ' ')
#     time1 <- paste(paste(time[[1]][1],time[[1]][2],time[[1]][3],sep = '/'),paste(time[[1]][4],time[[1]][5],sep = ":"),sep = " ")
#     time1 <- strftime(time1, "%Y-%m-%d %H:%M:%OS")
#     time2 <- strftime("2014/1/1 0:0", "%Y-%m-%d %H:%M:%OS")
#     age <- as.integer(difftime(time1, time2, units = "mins"))
#     return(age)
# }

convert_to_minute <- function(arrival) {
    arrive_time <- as.POSIXct(arrival, '%Y %m %d %H %M', tz = 'UTC')
    age <- as.integer(difftime(arrive_time, reference_time, units = 'mins', tz = 'UTC'))
    return(age)
}
# convert_to_minute('2014 1 1 1 1')

convert_to_chardate <- function(arrive_int) {
    char_date <- format(reference_time + arrive_int * 60, format = '%Y %m %d %H %M', tz = 'UTC')
    return(char_date)
}

######################
### 是否在工作时间 ###
######################
#     Return boolean True or False if a given time (in minutes) is a sanctioned working day minute.

is_sanctioned_time <- function(minute){
    return (((minute - day_start) %% minutes_in_24h) < (hours_per_day * 60))
}
# is_sanctioned_time(19*60)


##############################
### 分配每分钟是否工作时间 ###
##############################
#     Whole days (24-hr time periods) contribute fixed quantities of sanctioned and unsanctioned time. After
#     accounting for the whole days in the duration, the remainder minutes are tabulated as un/sanctioned.
#     :param start_minute:
#     :param duration:
#     :return:

# get_sanctioned_breakdown <- function(start_minute, duration){
#     full_days <- as.integer(duration / (minutes_in_24h))
#     sanctioned <- full_days * hours_per_day * 60
#     unsanctioned <- full_days * (24 - hours_per_day) * 60
#     remainder_start <- start_minute + full_days * minutes_in_24h
#     for(minute in remainder_start:(start_minute + duration - 1)){
#         if (is_sanctioned_time(minute)){
#             sanctioned <- sanctioned + 1
#         }else{
#             unsanctioned <- unsanctioned + 1
#         }
#     }
#     return (c(sanctioned, unsanctioned))
# }

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
# get_sanctioned_breakdown(12*60, 500)


####################################
### 给一分钟，计算下一个工作分钟 ###
####################################
#     Given a minute, finds the next sanctioned minute.
#     :param minute: integer representing a minute since reference time
#     :return: next sanctioned minute

next_sanctioned_minute <- function(minute) {
    if(is_sanctioned_time(minute) && is_sanctioned_time(minute + 1)) {
        next_min <- minute + 1
    } else {
        num_days <- as.integer(minute / minutes_in_24h)
        am_or_pm <- as.integer(((minute %% minutes_in_24h)/day_start)) 
        # This is necessary, else end-of-day unsanctioned minutes jump over an entire day.
        # David Thaler's fix works at minutes >=540, but fails at 539
        next_min <- day_start + (num_days + am_or_pm / 2) * minutes_in_24h
    }
    return(next_min)
}
# next_sanctioned_minute(1219)

################################
### 剩余时间和下次可工作时间 ###
################################
#     Enforces the rest period and returns the minute when the elf is next available for work.
#     Rest period is only applied to sanctioned work hours.
#     :param start: minute the REST period starts
#     :param num_unsanctioned: always > 0 number of unsanctioned minutes that need resting minutes
#     :return: next available minute after rest period has been applied

apply_resting_period <- function(start, num_unsanctioned){
    num_days_since_jan1 <- as.integer(start / minutes_in_24h)
    rest_time <- num_unsanctioned
    rest_time_in_working_days <- as.integer(rest_time / (60 * hours_per_day))
    rest_time_remaining_minutes <- rest_time %% (60 * hours_per_day)
    
#     rest time is only applied to sanctioned work hours. If local_start is at an unsanctioned time,
#     need to set it to be the next start of day
    local_start <- start %% minutes_in_24h  # minute of the day (relative to a current day) the work starts
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
# apply_resting_period(1140, 100)




