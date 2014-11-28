######################
### Init Parameter ###
######################
hours_init <- data.frame(
    hours_per_day = 10, # 10 hour day: 9 - 19
    day_start = 9 * 60,
    day_end = (9 + hours_per_day) * 60,
    minutes_in_24h = 24 * 60
)


####################################
### 目前时间和开始时间相差分钟数 ###
####################################
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
convert_to_minute(as.character(input$Arrival_time))


######################
### 是否在工作时间 ###
######################
is_sanctioned_time <- function(hours_init, minute){
#     Return boolean True or False if a given time (in minutes) is a sanctioned working day minute.
    return (((minute - hours_init$day_start) %% hours_init$minutes_in_24h) < (hours_init$hours_per_day * 60))
}
is_sanctioned_time(hours_init, 19*60-1)


##############################
### 分配每份周是否工作时间 ###
##############################


####################################
### 给一分钟，计算下一个工作分钟 ###
####################################


################################
### 剩余时间和下次可工作时间 ###
################################