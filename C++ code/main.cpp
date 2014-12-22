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
NumericMatrix solution_Elf(NumericMatrix myToys_1,NumericMatrix myToys_2,NumericMatrix myToys_3, NumericMatrix myelves){
    NumericMatrix outcomes(10,4); //ToyId, Arrival_time, Duration, Size, r_duration, start_time, finish, rate_f, refresh, evaluate

    for(int i=0; i<10; ++i){

        int min_val = myelves(0,2);
        int min_row = 0;
        for(int e=0; e<myelves.nrow(); e++){
            if (min_val > myelves(e,2)){
                min_val = myelves(e,2);
                min_row = e;
            }   
        }
        //elf_id, current_rating, next_available_time, score
        //min_element => for loop == i (row number)

        int c_elf_id = myelves(min_row,0);
        int c_elf_start_time = myelves(min_row,2);
        int c_elf_rating = myelves(min_row,1);
        NumericMatrix Toys;
        
        if(c_elf_rating == 4.0){
            Toys = myToys_3;
        }
        else if(c_elf_rating > 3){
            Toys = myToys_2; 

        }else{
            Toys = myToys_1; 
        }

        //double mean_rate, mean_refresh, mean_finish;
        double min_rate = Toys(0,7);
        double max_rate = Toys(0,7);
        double min_refresh = Toys(0,8);
        double max_refresh = Toys(0,8);
        double min_finish = Toys(0,6);
        double max_finish = Toys(0,6);

        // (e - Emin)/(Emax - Emin)
        for(int j = 0; j < Toys.nrow(); j++){ 
            Toys(j,4) = ceil(Toys(j,2)/c_elf_rating); //duration
            Toys(j,5) = std::max((int)c_elf_start_time, (int)Toys(j, 1)); //start_time
            Toys(j,7) = updateProductivity(Toys(j,7), Toys(j,4), c_elf_rating); //rate_f
            Toys(j,8) = updateNextAvailableMinute(Toys(j,5), Toys(j,4)); //refresh
            Toys(j,6) = Toys(j,4)+ Toys(j,5); //finish

            //mean_rate = (Toys(j,7) + mean_rate)/(j+1);
            //mean_refresh = (Toys(j,8) + mean_refresh)/(j+1);
            //mean_finish = (Toys(j,6) + mean_finish)/(j+1);
            if (min_rate > Toys(j,7)) min_rate = Toys(j,7);
            if (max_rate < Toys(j,7)) max_rate = Toys(j,7);
            if (min_refresh > Toys(j,8)) min_refresh = Toys(j,8);
            if (max_refresh < Toys(j,8)) max_refresh = Toys(j,8);
            if (min_finish > Toys(j,6)) min_finish = Toys(j,6);
            if (max_finish < Toys(j,6)) max_finish = Toys(j,6);
        }
        
        Toys( _, 9) = ((Toys( _, 7) - min_rate)/(max_rate - min_rate) + (Toys( _, 8) - min_refresh)/(max_refresh - min_refresh) + (Toys( _, 6) - min_finish)/(max_finish - min_finish))/3;

        double min_toy_val = Toys(0,9);
        int min_toy_row = 0;
        for(int v=0; v<Toys.nrow(); v++){
            if(Toys(v,0)<10000001){
                if (min_toy_val > Toys(v,9)){
                    min_toy_val = Toys(v,9);
                    min_toy_row = v;
                }
            }  
        }
        // (e - Emin)/(Emax - Emin)

        int c_toy_id = Toys(min_row,0);
        c_elf_start_time = (int)Toys(min_row,5); 
        int work_duration = Toys(min_row,4);
        
        myelves(c_elf_id-1, 2) = Toys(min_row,8); //next_available_time
        myelves(c_elf_id-1, 1) = Toys(min_row,7); //current_rating

        outcomes(i,0) = c_toy_id;
        outcomes(i,1) = c_elf_id;
        outcomes(i,2) = c_elf_start_time;
        outcomes(i,3) = work_duration;

        if(c_elf_rating == 4.0){
            myToys_3(min_row, 0) = 10000001;  // #################
        }
        else if(c_elf_rating > 3){
            myToys_2(min_row, 0) = 10000001;  // #################

        }else{
            myToys_1(min_row, 0) = 10000001;  // #################
        }

        //if(i % 100000 == 0) 
        Rcpp::Rcout << '\n' << i;
    }
    return outcomes;
}