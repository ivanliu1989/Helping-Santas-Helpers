// adapted for Visual C++ from: https://github.com/rdbisch/Santa2014

#include "stdafx.h"

#include "Hours.h"


struct tm startTime; //once set, this doesn't change
time_t startTime_t; //once set, this doesn't change

void init_startTime() {
    memset(&startTime, 0, sizeof(struct tm));
    startTime.tm_year = 2014 - 1900;
    startTime.tm_mon = 0;
    startTime.tm_mday = 1;
    startTime.tm_hour = 0;
    startTime.tm_min = 0;
    startTime_t = _mkgmtime(&startTime);
}

/**
 * Poorly named function that converts the YY MM DD HH mm integers
 * into an integer number of minutes after 2004/1/1 12:00
 */
int stringToTime(int year, int month, int day, int hour, int minute) {
    struct tm nowtime;
    memset(&nowtime, 0, sizeof(struct tm));
    nowtime.tm_year = year - 1900;
    nowtime.tm_mon = month - 1;
    nowtime.tm_mday = day;
    nowtime.tm_hour = hour;
    nowtime.tm_min = minute;

    time_t nowtime_t = _mkgmtime(&nowtime);
    return int( (nowtime_t - startTime_t) / 60 );
}

/** 
 * The inverse of the above...if they above actually
 * operated on strings that is.
 * 
 * Converts integer number of minutes past start time into
 * the YY MM DD HH mm format.
 */
char* timeToString(int time,char*output) {
    //time_t afterTime = _mkgmtime(&startTime) + time * 60ULL;
    time_t afterTime = startTime_t + time * 60ULL;
    
    struct tm *after = gmtime(&afterTime);
    //char output[30];
    sprintf(output, "%d %02d %02d %02d %02d", after->tm_year + 1900, after->tm_mon + 1, after->tm_mday, after->tm_hour, after->tm_min);
    //strftime(output, sizeof(output), "%Y %m %d %H %M", after);
    //return string(output);
    return output;
}

/**
 * Returns true if "minute" is in the sanctioned time frame.
 */
bool isSanctionedTime(int minute) {
    return ((minute - 540) % MID) < 600;
}

/**
 * Increments the time to the next "fence" of sanctioned/vs. unsanctioned
 */
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

/**
 * Returns a pair of numbers that represent the sanctioned vs. 
 * unsanctioned breakdown of time.
 *
 */
int getSanctionedBreakdown(int startMinute, int duration, int*pUnsanc) {
    int S = 0;
    int U = 0;

    int full_days = duration / MID;
    S = full_days * (10*60);
    U = full_days * (14*60);
    //int remainder = startMinute + full_days * MID;
    //for (int i = remainder; i < (startMinute+duration); ++i) {
    //    if (isSanctionedTime(i)) S += 1;
    //    else U += 1;
    //}
    duration = duration % MID;
    int d;
    while (duration) {
        startMinute = startMinute%MID;
        if (isSanctionedTime(startMinute)) {
            d = 19 * 60 - startMinute;
            if (d > duration) d = duration;
            S += d;
        } else {
            d = (24 + 9) * 60 - startMinute;
            if (d > duration) d = duration;
            U += d;
        }
        startMinute += d; duration -= d;
    }
    *pUnsanc = U; return S;
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

