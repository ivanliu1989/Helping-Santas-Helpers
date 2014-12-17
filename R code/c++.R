update_next_available_minute <- function(start_minute, work_duration) {
    sanctioned_breakdown <- get_sanctioned_breakdown(start_minute, work_duration)
    sanc_time <- sanctioned_breakdown[1]
    unsanc_time <- sanctioned_breakdown[2]
    end_minute <- start_minute + work_duration
    if(unsanc_time == 0) {
        if(is_sanctioned_time(end_minute)) {
            next_available_time <- end_minute
        } else {
            next_available_time <- next_sanctioned_minute(end_minute)
        }
    } else {
        next_available_time <- apply_resting_period(end_minute, unsanc_time)
    }
    return(next_available_time)
}

update_productivity <- function(start_minute, work_duration, current_rating) {
    sanctioned_breakdown <- get_sanctioned_breakdown(start_minute, work_duration)
    sanc_time <- sanctioned_breakdown[1]
    unsanc_time <- sanctioned_breakdown[2]
    new_rating <- max(c(0.25, min(c(4.0, current_rating * (rating_increase ** (sanc_time/60)) * (rating_decrease ** (unsanc_time/60))))))
    return(new_rating)
}

###################################################################################################################################################
is_sanctioned_time <- function(minute) {
    is_sanctioned <- ((minute - day_start) %% minutes_in_24h) < (hours_per_day * 60)
    return(is_sanctioned)
}

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

next_sanctioned_minute <- function(minute) {
    if(is_sanctioned_time(minute) && is_sanctioned_time(minute + 1)) {
        next_min <- minute + 1
    } else {
        num_days <- as.integer(minute / minutes_in_24h)
        am_or_pm <- as.integer(((minute %% minutes_in_24h)/day_start)) 
        next_min <- day_start + (num_days + am_or_pm / 2) * minutes_in_24h
    }
    return(next_min)
}

apply_resting_period <- function(start, num_unsanctioned){
    num_days_since_jan1 <- as.integer(start / minutes_in_24h)
    rest_time <- num_unsanctioned
    rest_time_in_working_days <- as.integer(rest_time / (60 * hours_per_day))
    rest_time_remaining_minutes <- rest_time %% (60 * hours_per_day)
    local_start <- start %% minutes_in_24h
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
