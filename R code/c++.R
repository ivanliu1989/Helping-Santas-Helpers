require(Rcpp)
require(microbenchmark)
microbenchmark(
    isSanctionedTime(123),
    is_sanctioned_time(123)
)

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
RCPP_MODULE(yada){
    
} 
cppFunction('bool isSanctionedTime(int minute) {
        return ((minute - 540) % (60*24)) < 600;
    }')

cppFunction('NumericVector getSanctionedBreakdown(int startMinute, int duration) {
        NumericVector out(2);
        int S = 0;
        int U = 0;
        int full_days = duration / (60*24);
        S = full_days * (10*60);
        U = full_days * (14*60);
        int remainder = startMinute + full_days * (60*24);
        for (int i = remainder; i < (startMinute+duration); ++i) {
        if (isSanctionedTime(i)) S += 1;
        else U += 1;
        }
        out[1] = S; out[2] = U;
        return out;
    }')


int incrementToNextFence(int minute) {
    int day = (minute / (MID));
    int dayM = day * (MID);
    if (minute > (dayM + 60*18 + 59))        
        return dayM + (MID) + 60*9;
    else if (minute < (dayM + 60*9))
        return dayM + 60*9;
    else
        return dayM + 60*19 ;
}

int applyRestingPeriod(int current, int unsanctioned) {
    if (unsanctioned == 0) {
        if (isSanctionedTime(current)) return current;
        else return incrementToNextFence(current);
    }
    int num_days_since_jan1 = current / (60 * 24);
    int rest_time = unsanctioned;
    int rest_time_in_working_days = rest_time / 600;
    int rest_time_remaining_minutes = rest_time % 600;
    int local_start = current % (60 * 24);
    if (local_start < 540) local_start = 540;
    else if (local_start > 1140) {
        num_days_since_jan1 += 1;
        local_start = 540;
    }
    if (local_start + rest_time_remaining_minutes > 1140) {
        num_days_since_jan1 += 1;
        rest_time_remaining_minutes -= (1140 - local_start);
        local_start = 540;
    }
    int total_days = num_days_since_jan1 + rest_time_in_working_days;
    return total_days * (60*24) + local_start + rest_time_remaining_minutes;
}
