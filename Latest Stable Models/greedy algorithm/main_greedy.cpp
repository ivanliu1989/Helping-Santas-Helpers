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
NumericMatrix solution_Elf(NumericMatrix myToys_0, NumericMatrix myToys_1,NumericMatrix myToys_2,NumericMatrix myToys_3,NumericMatrix myelves, NumericVector myelves_rate){
    NumericMatrix outcomes(10000000,4); //ToyId, Arrival_time, Duration, Size
    int toy_0 = 0;
    int toy_1 = 0;
    int toy_2 = 0;
    int toy_3 = 0;
    int c_toy_id, c_toy_arrival, c_toy_duration, min_val, min_row, c_elf_id, c_elf_start_time,work_duration;
    double c_elf_rating;
    
    for(unsigned long long current_toy=0; current_toy<10000000; ++current_toy){
        
        min_val = myelves(0,2);
        min_row = 0;
        for(int e=0; e<myelves.nrow(); e++){
            if (min_val > myelves(e,2)){
                min_val = myelves(e,2);
                min_row = e;
            }
        }
        
        c_elf_id = myelves(min_row,0);
        c_elf_start_time = myelves(min_row,2);
        c_elf_rating = myelves_rate(min_row);
        
        if((c_elf_rating > 3.98) & (toy_3 < myToys_3.nrow())){
            c_toy_id = myToys_3(toy_3,0);
            c_toy_arrival = myToys_3(toy_3,1);
            c_toy_duration = myToys_3(toy_3,2);
            toy_3 += 1;
        }else if((c_elf_rating >= 3.5) & (toy_2 < myToys_2.nrow())){
            c_toy_id = myToys_2(toy_2,0);
            c_toy_arrival = myToys_2(toy_2,1);
            c_toy_duration = myToys_2(toy_2,2);
            toy_2 += 1;
        }else if((c_elf_rating >= 2) & (toy_1 < myToys_1.nrow())){
            c_toy_id = myToys_1(toy_1,0);
            c_toy_arrival = myToys_1(toy_1,1);
            c_toy_duration = myToys_1(toy_1,2);
            toy_1 += 1;
        }else if(toy_0 < myToys_0.nrow()){
            c_toy_id = myToys_0(toy_0,0);
            c_toy_arrival = myToys_0(toy_0,1);
            c_toy_duration = myToys_0(toy_0,2);
            toy_0 += 1;
        }else {
            if(toy_1 < myToys_1.nrow()){
                c_toy_id = myToys_1(toy_1,0);
                c_toy_arrival = myToys_1(toy_1,1);
                c_toy_duration = myToys_1(toy_1,2);
                toy_1 += 1;
            }else if(toy_2 < myToys_2.nrow()){
                c_toy_id = myToys_2(toy_2,0);
                c_toy_arrival = myToys_2(toy_2,1);
                c_toy_duration = myToys_2(toy_2,2);
                toy_2 += 1;
            }else if(toy_3 < myToys_3.nrow()){
                c_toy_id = myToys_3(toy_3,0);
                c_toy_arrival = myToys_3(toy_3,1);
                c_toy_duration = myToys_3(toy_3,2);
                toy_3 += 1;
            }
        }
        
        c_elf_start_time = std::max((int)c_elf_start_time, (int)c_toy_arrival);
        //if c_elf_start_time late then next day
        work_duration = ceil(c_toy_duration/c_elf_rating);
        
        myelves_rate(min_row) = updateProductivity(c_elf_start_time, work_duration, c_elf_rating);
        myelves(min_row,2) = updateNextAvailableMinute(c_elf_start_time, work_duration);
        
        outcomes(current_toy,0) = c_toy_id;
        outcomes(current_toy,1) = c_elf_id;
        outcomes(current_toy,2) = c_elf_start_time;
        outcomes(current_toy,3) = work_duration;
        
        if(current_toy % 100000 == 0) Rcpp::Rcout << '\n' << (double)current_toy/1000000 << ' ' << myToys_3.nrow() << ' ' << toy_3;
    }
    Rcpp::Rcout << '\n' << 10000000/1000000 << ' ' << myToys_3.nrow() << ' ' << toy_3;
    return outcomes;
}
