#include <Rcpp.h>
#include <algorithm> // std::max
using namespace Rcpp;

// [[Rcpp::export]]
int updateNextAvailableMinute(int start_minute, int work_duration){
    int next_available_time = 0;
    int S = 0;
    int U = 0;
    int full_days = work_duration / (60*24);
    S = full_days * (10*60);
    U = full_days * (14*60);
    int remainder = start_minute + full_days * (60*24);
    for (int i = remainder; i < (start_minute+work_duration); ++i) {
        bool isSanctionedTime = ((i - 540) % (60*24)) < 600;
        if (isSanctionedTime) S += 1;
        else U += 1;
    }
    int end_minute = start_minute + work_duration;
    
    if(U == 0) {
        if(((end_minute - 540) % (60*24)) < 600) {
            next_available_time = end_minute;
        } else {
            bool isSanctionedTime = ((end_minute - 540) % (60*24)) < 600;
            bool isSanctionedTime_1 = ((end_minute - 539) % (60*24)) < 600;
            int next_min = 0;
            if(isSanctionedTime && isSanctionedTime_1) next_min = end_minute + 1;
            else {
                int num_days = end_minute / (60*24);
                int am_or_pm = (end_minute % (60*24))/ 540;
                next_min = 540 + (num_days + am_or_pm / 2) * (60*24);
            }
            next_available_time = next_min;
        }
    } else {
        if (U == 0) {
            bool isSanctionedTime = ((end_minute - 540) % (60*24)) < 600;
            if (isSanctionedTime) next_available_time = end_minute;
            else{
                bool isSanctionedTime = ((end_minute - 540) % (60*24)) < 600;
                bool isSanctionedTime_1 = ((end_minute - 539) % (60*24)) < 600;
                int next_min = 0;
                if(isSanctionedTime && isSanctionedTime_1) next_min = end_minute + 1;
                else {
                    int num_days = end_minute / (60*24);
                    int am_or_pm = (end_minute % (60*24))/ 540;
                    next_min = 540 + (num_days + am_or_pm / 2) * (60*24);
                }
                next_available_time = next_min;
            }
        }
        int num_days_since_jan1 = end_minute / (60 * 24);
        int rest_time = U;
        int rest_time_in_working_days = rest_time / 600;
        int rest_time_remaining_minutes = rest_time % 600;
        int local_start = end_minute % (60 * 24);
        if (local_start < 540) local_start = 540;
        else if (local_start > 1140) {
            num_days_since_jan1 += 1;
            local_start = 540;
        }
        if (local_start + rest_time_remaining_minutes >= 1140) {
            num_days_since_jan1 += 1;
            rest_time_remaining_minutes -= (1140 - local_start);
            local_start = 540;
        }
        int total_days = num_days_since_jan1 + rest_time_in_working_days;
        
        next_available_time = total_days * (60*24) + local_start + rest_time_remaining_minutes;
    }
    return next_available_time;
}

// [[Rcpp::export]]
double updateProductivity(int start_minute, int work_duration, double current_rating){
    NumericVector out(2);
    int S = 0;
    int U = 0;
    int full_days = work_duration / (60*24);
    S = full_days * (10*60);
    U = full_days * (14*60);
    int remainder = start_minute + full_days * (60*24);
    for (int i = remainder; i < (start_minute+work_duration); ++i) {
        bool isSanctionedTime = ((i - 540) % (60*24)) < 600;
        if (isSanctionedTime) S += 1;
        else U += 1;
    }
    double new_rating = std::max(0.25, std::min(4.0, current_rating * pow(1.02, S / 60.0) * pow(0.90, U / 60.0)));
    return new_rating;
}

// [[Rcpp::export]]
int getSanctionedBreakdown(int startMinute, int duration) {
    int S = 0;
    int U = 0;
    int full_days = duration / (60*24);
    S = full_days * (10*60);
    U = full_days * (14*60);
    int remainder = startMinute + full_days * (60*24);
    for (int i = remainder; i < (startMinute+duration); ++i) {
        bool isSanctionedTime = ((i - 540) % (60*24)) < 600;
        if (isSanctionedTime) S += 1;
        else U += 1;
    }
    return S;
}

// [[Rcpp::export]]
NumericMatrix solution_Elf(NumericMatrix myToys_0, NumericMatrix myToys_1,NumericMatrix myToys_2,NumericMatrix myToys_3,NumericMatrix myToys_4,NumericMatrix myToys_5,
                           NumericMatrix myToys_6,NumericMatrix myToys_7,NumericMatrix myToys_8,NumericMatrix myToys_9,NumericMatrix myToys_10,NumericMatrix myToys_11,NumericMatrix myToys_12,
                           NumericMatrix myToys_13,NumericMatrix myToys_14,NumericMatrix myToys_15,NumericMatrix myToys_16,NumericMatrix myToys_17,NumericMatrix myelves, NumericVector myelves_rate){
    
    NumericVector toy_row(18);
    int c_toy_id, c_toy_arrival, c_toy_duration, min_val, min_row, c_elf_id, c_elf_start_time,work_duration;
    double c_elf_rating;
    int rate_count;
    int retrain_count=0;
    int delay_sum=0;
    for(unsigned long long current_toy=0; current_toy<10000000; ++current_toy){
        
        min_val = myelves(0,2);
        min_row = 0;
        rate_count = 0;
        for(int e=0; e<myelves.nrow(); e++){
            if (min_val > myelves(e,2)){
                min_val = myelves(e,2);
                min_row = e;
            }
            if(myelves_rate(e)==4.0) rate_count ++;
        }
        
        c_elf_id = myelves(min_row,0);
        c_elf_start_time = myelves(min_row,2);
        c_elf_rating = myelves_rate(min_row);
        
        //.25, .30, .37, .45, .55, .67, .82, 1, 1.22, 1.49, 1.81, 2.21, 2.69, 3.28, 4
        //remain
            int c_elf_start_time_2 = std::max(c_elf_start_time, (int)myToys_1(toy_row(1),1)); //toy1 start time 
            int act_duration = ceil(myToys_1(toy_row(1),2)/c_elf_rating); //toy1 actual work time 
            int sanc = getSanctionedBreakdown(c_elf_start_time_2, act_duration); //toy1 sanctional time
            double sanc_rate = (double)sanc/act_duration;
            Rcpp::Rcout << '\n' << c_elf_start_time << ' ' << (int)myToys_1(toy_row(1),1) << ' ' << c_elf_start_time_2;
            /*if(sanc_rate <= 0.8){
                
                int c_elf_start_time_3 = std::max(c_elf_start_time, (int)myToys_0(toy_row(0),1)); //trival start time 
                int act_duration_2 = ceil(myToys_0(toy_row(0),2)/c_elf_rating); //trival actual work time 
                int sanc_2 = getSanctionedBreakdown(c_elf_start_time_3, act_duration_2); //trival sanctional time
                double sanc_rate_2 = (double)sanc_2/act_duration_2;
                
                if((toy_row(0) < myToys_0.nrow()) && (sanc_rate_2 >= 0.95)){
                    c_toy_id = myToys_0(toy_row(0),0);
                    c_toy_arrival = myToys_0(toy_row(0),1);
                    c_toy_duration = myToys_0(toy_row(0),2);
                    toy_row(0) += 1;
                }else{
                    c_elf_start_time = 840 + sanc + c_elf_start_time_2;
                    delay_sum += sanc;
                    c_toy_id = myToys_1(toy_row(1),0);
                    c_toy_arrival = myToys_1(toy_row(1),1);
                    c_toy_duration = myToys_1(toy_row(1),2);
                    toy_row(1) += 1;   
                }
            }else{
                c_toy_id = myToys_1(toy_row(1),0);
                c_toy_arrival = myToys_1(toy_row(1),1);
                c_toy_duration = myToys_1(toy_row(1),2);
                toy_row(1) += 1; 
            }           */
        }
    return 0;
}
