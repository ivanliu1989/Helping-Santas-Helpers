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
### 分配每分钟是否工作时间 ###
##############################
def get_sanctioned_breakdown(self, start_minute, duration):
    """ Whole days (24-hr time periods) contribute fixed quantities of sanctioned and unsanctioned time. After
        accounting for the whole days in the duration, the remainder minutes are tabulated as un/sanctioned.
        :param start_minute:
        :param duration:
        :return:
        """
full_days = duration / (self.minutes_in_24h)
sanctioned = full_days * self.hours_per_day * 60
unsanctioned = full_days * (24 - self.hours_per_day) * 60
remainder_start = start_minute + full_days * self.minutes_in_24h
for minute in xrange(remainder_start, start_minute+duration):
    if self.is_sanctioned_time(minute):
    sanctioned += 1
else:
    unsanctioned += 1
return sanctioned, unsanctioned



####################################
### 给一分钟，计算下一个工作分钟 ###
####################################
def next_sanctioned_minute(self, minute):
    """ Given a minute, finds the next sanctioned minute.
        :param minute: integer representing a minute since reference time
        :return: next sanctioned minute
        """
# next minute is a sanctioned minute
if self.is_sanctioned_time(minute) and self.is_sanctioned_time(minute+1):
    return minute + 1
num_days = minute / self.minutes_in_24h
return self.day_start + (num_days + 1) * self.minutes_in_24h



################################
### 剩余时间和下次可工作时间 ###
################################
def apply_resting_period(self, start, num_unsanctioned):
    """ Enforces the rest period and returns the minute when the elf is next available for work.
        Rest period is only applied to sanctioned work hours.
        :param start: minute the REST period starts
        :param num_unsanctioned: always > 0 number of unsanctioned minutes that need resting minutes
        :return: next available minute after rest period has been applied
        """
num_days_since_jan1 = start / self.minutes_in_24h
rest_time = num_unsanctioned
rest_time_in_working_days = rest_time / (60 * self.hours_per_day)
rest_time_remaining_minutes = rest_time % (60 * self.hours_per_day)

# rest time is only applied to sanctioned work hours. If local_start is at an unsanctioned time,
# need to set it to be the next start of day
local_start = start % self.minutes_in_24h  # minute of the day (relative to a current day) the work starts
if local_start < self.day_start:
    local_start = self.day_start
elif local_start > self.day_end:
    num_days_since_jan1 += 1
local_start = self.day_start

if local_start + rest_time_remaining_minutes > self.day_end:
    rest_time_in_working_days += 1
rest_time_remaining_minutes -= (self.day_end - local_start)
local_start = self.day_start

total_days = num_days_since_jan1 + rest_time_in_working_days
return total_days * self.minutes_in_24h + local_start + rest_time_remaining_minutes