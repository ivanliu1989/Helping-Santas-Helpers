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
                           NumericMatrix myToys_13,NumericMatrix myToys_14,NumericMatrix myToys_15,NumericMatrix myToys_16,NumericMatrix myToys_17,NumericMatrix myelves, NumericVector myelves_rate,
                           NumericMatrix myToys_18){
    
    NumericMatrix outcomes(10000000,4); //ToyId, Arrival_time, Duration, Size
    NumericVector toy_row(19);
    int c_toy_id, c_toy_arrival, c_toy_duration, min_val, min_row, c_elf_id, c_elf_start_time,work_duration;
    double c_elf_rating;
    int rate_count;
    int retrain_count=0;
    int delay_sum=0;
    int delay_num=0;
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
        if((c_elf_rating > 3.98) & (toy_row(1) < myToys_1.nrow())){
            
            int c_elf_start_time_2 = std::max(c_elf_start_time, (int)myToys_1(toy_row(1),1)); //toy1 start time 
            int act_duration = ceil(myToys_1(toy_row(1),2)/c_elf_rating); //toy1 actual work time 
            int sanc = getSanctionedBreakdown(c_elf_start_time_2, act_duration); //toy1 sanctional time
            double sanc_rate = (double)sanc/act_duration;
            
            if(sanc_rate <= 0.8){
        
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
                    delay_num += 1;
                    
                   /* if(sanc>100){
                        Rcpp::Rcout << '\n' << sanc_rate_2 << ' ' << sanc << ' ' << c_elf_start_time << ' ' << c_elf_start_time_2
                    << ' ' << c_elf_id << ' ' << act_duration << ' ' << myToys_0(toy_row(0),2);
                    } */
                    
                    
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
            }
            
//10 + 14 + 10            
        }else if((c_elf_rating >= 3.95) & (toy_row(18) < myToys_18.nrow())){
            
            int c_elf_start_time_2 = std::max(c_elf_start_time, (int)myToys_18(toy_row(18),1)); //toy1 start time 
            int act_duration = ceil(myToys_18(toy_row(18),2)/c_elf_rating); //toy1 actual work time 
            int sanc = getSanctionedBreakdown(c_elf_start_time_2, act_duration); //toy1 sanctional time
            int sanc_t = getSanctionedBreakdown(c_elf_start_time_2, act_duration/2); //toy1 sanctional time
            
            if(sanc <= 1100){
        
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
                    c_elf_start_time = 840 + sanc_t + c_elf_start_time_2;
                    delay_sum += sanc_t;
                    delay_num += 1;
                    
                    c_toy_id = myToys_18(toy_row(18),0);
                    c_toy_arrival = myToys_18(toy_row(18),1);
                    c_toy_duration = myToys_18(toy_row(18),2);
                    toy_row(18) += 1;   
                }
            }else{
                c_toy_id = myToys_18(toy_row(18),0);
                c_toy_arrival = myToys_18(toy_row(18),1);
                c_toy_duration = myToys_18(toy_row(18),2);
                toy_row(18) += 1; 
            }
            
//ex1            
        }else if((c_elf_rating > 3.9) & (toy_row(3) < myToys_3.nrow())){\
            c_toy_id = myToys_3(toy_row(3),0);
            c_toy_arrival = myToys_3(toy_row(3),1);
            c_toy_duration = myToys_3(toy_row(3),2);
            toy_row(3) += 1; 
            
//tr7            
        }else if((c_elf_rating >= 3.28) & (toy_row(17) < myToys_17.nrow())){
            
            int c_elf_start_time_2 = std::max(c_elf_start_time, (int)myToys_17(toy_row(17),1)); //toy1 start time 
            int act_duration = ceil(myToys_17(toy_row(17),2)/c_elf_rating); //toy1 actual work time 
            int sanc = getSanctionedBreakdown(c_elf_start_time_2, act_duration); //toy1 sanctional time
            double sanc_rate = (double)sanc/act_duration;
            
            if(sanc_rate < 1){
        
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
                    delay_num += 1;
                    c_toy_id = myToys_17(toy_row(17),0);
                    c_toy_arrival = myToys_17(toy_row(17),1);
                    c_toy_duration = myToys_17(toy_row(17),2);
                    toy_row(17) += 1;   
                }
            }else{
                c_toy_id = myToys_17(toy_row(17),0);
                c_toy_arrival = myToys_17(toy_row(17),1);
                c_toy_duration = myToys_17(toy_row(17),2);
                toy_row(17) += 1;
            }
//tr6            
        }else if((c_elf_rating >= 2.69) & (toy_row(16) < myToys_16.nrow())){
            
            
            int c_elf_start_time_2 = std::max(c_elf_start_time, (int)myToys_16(toy_row(16),1)); //toy1 start time 
            int act_duration = ceil(myToys_16(toy_row(16),2)/c_elf_rating); //toy1 actual work time 
            int sanc = getSanctionedBreakdown(c_elf_start_time_2, act_duration); //toy1 sanctional time
            double sanc_rate = (double)sanc/act_duration;
            
            if(sanc_rate < 1){
        
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
                    delay_num += 1;
                    c_toy_id = myToys_16(toy_row(16),0);
                    c_toy_arrival = myToys_16(toy_row(16),1);
                    c_toy_duration = myToys_16(toy_row(16),2);
                    toy_row(16) += 1;   
                }
            }else{
                c_toy_id = myToys_16(toy_row(16),0);
                c_toy_arrival = myToys_16(toy_row(16),1);
                c_toy_duration = myToys_16(toy_row(16),2);
                toy_row(16) += 1;
            }
//tr5            
        }else if((c_elf_rating >= 2.21) & (toy_row(15) < myToys_15.nrow())){
            
            int c_elf_start_time_2 = std::max(c_elf_start_time, (int)myToys_15(toy_row(15),1)); //toy1 start time 
            int act_duration = ceil(myToys_15(toy_row(15),2)/c_elf_rating); //toy1 actual work time 
            int sanc = getSanctionedBreakdown(c_elf_start_time_2, act_duration); //toy1 sanctional time
            double sanc_rate = (double)sanc/act_duration;
            
            if(sanc_rate < 1){
        
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
                    delay_num += 1;
                    c_toy_id = myToys_15(toy_row(15),0);
                    c_toy_arrival = myToys_15(toy_row(15),1);
                    c_toy_duration = myToys_15(toy_row(15),2);
                    toy_row(15) += 1;   
                }
            }else{
                c_toy_id = myToys_15(toy_row(15),0);
                c_toy_arrival = myToys_15(toy_row(15),1);
                c_toy_duration = myToys_15(toy_row(15),2);
                toy_row(15) += 1;
            }
            
//tr4            
        }else if((c_elf_rating >= 1.81) & (toy_row(14) < myToys_14.nrow())){
            
            int c_elf_start_time_2 = std::max(c_elf_start_time, (int)myToys_14(toy_row(14),1)); //toy1 start time 
            int act_duration = ceil(myToys_14(toy_row(14),2)/c_elf_rating); //toy1 actual work time 
            int sanc = getSanctionedBreakdown(c_elf_start_time_2, act_duration); //toy1 sanctional time
            double sanc_rate = (double)sanc/act_duration;
            
            if(sanc_rate < 1){
        
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
                    delay_num += 1;
                    c_toy_id = myToys_14(toy_row(14),0);
                    c_toy_arrival = myToys_14(toy_row(14),1);
                    c_toy_duration = myToys_14(toy_row(14),2);
                    toy_row(14) += 1;   
                }
            }else{
                c_toy_id = myToys_14(toy_row(14),0);
                c_toy_arrival = myToys_14(toy_row(14),1);
                c_toy_duration = myToys_14(toy_row(14),2);
                toy_row(14) += 1;
            }
            
//tr3            
        }else if((c_elf_rating >= 1.49) & (toy_row(13) < myToys_13.nrow())){
            
            int c_elf_start_time_2 = std::max(c_elf_start_time, (int)myToys_13(toy_row(13),1)); //toy1 start time 
            int act_duration = ceil(myToys_13(toy_row(13),2)/c_elf_rating); //toy1 actual work time 
            int sanc = getSanctionedBreakdown(c_elf_start_time_2, act_duration); //toy1 sanctional time
            double sanc_rate = (double)sanc/act_duration;
            
            if(sanc_rate < 1){
        
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
                    delay_num += 1;
                    c_toy_id = myToys_13(toy_row(13),0);
                    c_toy_arrival = myToys_13(toy_row(13),1);
                    c_toy_duration = myToys_13(toy_row(13),2);
                    toy_row(13) += 1;   
                }
            }else{
                c_toy_id = myToys_13(toy_row(13),0);
                c_toy_arrival = myToys_13(toy_row(13),1);
                c_toy_duration = myToys_13(toy_row(13),2);
                toy_row(13) += 1;
            }
            
//tr2            
        }else if((c_elf_rating >= 1.22) & (toy_row(12) < myToys_12.nrow())){
            
            int c_elf_start_time_2 = std::max(c_elf_start_time, (int)myToys_12(toy_row(12),1)); //toy1 start time 
            int act_duration = ceil(myToys_12(toy_row(12),2)/c_elf_rating); //toy1 actual work time 
            int sanc = getSanctionedBreakdown(c_elf_start_time_2, act_duration); //toy1 sanctional time
            double sanc_rate = (double)sanc/act_duration;
            
            if(sanc_rate < 1){
        
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
                    delay_num += 1;
                    c_toy_id = myToys_12(toy_row(12),0);
                    c_toy_arrival = myToys_12(toy_row(12),1);
                    c_toy_duration = myToys_12(toy_row(12),2);
                    toy_row(12) += 1;   
                }
            }else{
                c_toy_id = myToys_12(toy_row(12),0);
                c_toy_arrival = myToys_12(toy_row(12),1);
                c_toy_duration = myToys_12(toy_row(12),2);
                toy_row(12) += 1;
            }
            
//tr1            
        }else if((c_elf_rating >= 1.0) & (toy_row(11) < myToys_11.nrow())){
            
            int c_elf_start_time_2 = std::max(c_elf_start_time, (int)myToys_11(toy_row(11),1)); //toy1 start time 
            int act_duration = ceil(myToys_11(toy_row(11),2)/c_elf_rating); //toy1 actual work time 
            int sanc = getSanctionedBreakdown(c_elf_start_time_2, act_duration); //toy1 sanctional time
            double sanc_rate = (double)sanc/act_duration;
            
            if(sanc_rate < 1){
        
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
                    delay_num += 1;
                    c_toy_id = myToys_11(toy_row(11),0);
                    c_toy_arrival = myToys_11(toy_row(11),1);
                    c_toy_duration = myToys_11(toy_row(11),2);
                    toy_row(11) += 1;
                }
            }else{
                c_toy_id = myToys_11(toy_row(11),0);
                c_toy_arrival = myToys_11(toy_row(11),1);
                c_toy_duration = myToys_11(toy_row(11),2);
                toy_row(11) += 1;
            }
            
//re7            
        }else if((c_elf_rating >= .82) & (toy_row(10) < myToys_10.nrow())){
            
            int c_elf_start_time_2 = std::max(c_elf_start_time, (int)myToys_10(toy_row(10),1)); //toy1 start time 
            int act_duration = ceil(myToys_10(toy_row(10),2)/c_elf_rating); //toy1 actual work time 
            int sanc = getSanctionedBreakdown(c_elf_start_time_2, act_duration); //toy1 sanctional time
            double sanc_rate = (double)sanc/act_duration;
            
            if(sanc_rate < 1){
        
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
                    delay_num += 1;
                    c_toy_id = myToys_10(toy_row(10),0);
                    c_toy_arrival = myToys_10(toy_row(10),1);
                    c_toy_duration = myToys_10(toy_row(10),2);
                    toy_row(10) += 1;
                }
            }else{
                c_toy_id = myToys_10(toy_row(10),0);
                c_toy_arrival = myToys_10(toy_row(10),1);
                c_toy_duration = myToys_10(toy_row(10),2);
                toy_row(10) += 1;
            }
//re6            
        }else if((c_elf_rating >= .67) & (toy_row(9) < myToys_9.nrow())){
            
            int c_elf_start_time_2 = std::max(c_elf_start_time, (int)myToys_9(toy_row(9),1)); //toy1 start time 
            int act_duration = ceil(myToys_9(toy_row(9),2)/c_elf_rating); //toy1 actual work time 
            int sanc = getSanctionedBreakdown(c_elf_start_time_2, act_duration); //toy1 sanctional time
            double sanc_rate = (double)sanc/act_duration;
            
            if(sanc_rate < 1){
        
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
                    delay_num += 1;
                    c_toy_id = myToys_9(toy_row(9),0);
                    c_toy_arrival = myToys_9(toy_row(9),1);
                    c_toy_duration = myToys_9(toy_row(9),2);
                    toy_row(9) += 1;
                }
            }else{
                c_toy_id = myToys_9(toy_row(9),0);
                c_toy_arrival = myToys_9(toy_row(9),1);
                c_toy_duration = myToys_9(toy_row(9),2);
                toy_row(9) += 1;
            }
            
//re5            
        }else if((c_elf_rating >= .55) & (toy_row(8) < myToys_8.nrow())){
            
            int c_elf_start_time_2 = std::max(c_elf_start_time, (int)myToys_8(toy_row(8),1)); //toy1 start time 
            int act_duration = ceil(myToys_8(toy_row(8),2)/c_elf_rating); //toy1 actual work time 
            int sanc = getSanctionedBreakdown(c_elf_start_time_2, act_duration); //toy1 sanctional time
            double sanc_rate = (double)sanc/act_duration;
            
            if(sanc_rate < 1){
        
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
                    delay_num += 1;
                    c_toy_id = myToys_8(toy_row(8),0);
                    c_toy_arrival = myToys_8(toy_row(8),1);
                    c_toy_duration = myToys_8(toy_row(8),2);
                    toy_row(8) += 1;
                }
            }else{
                c_toy_id = myToys_8(toy_row(8),0);
                c_toy_arrival = myToys_8(toy_row(8),1);
                c_toy_duration = myToys_8(toy_row(8),2);
                toy_row(8) += 1;
            }
            
//re4            
        }else if((c_elf_rating >= .45) & (toy_row(7) < myToys_7.nrow())){
            
            int c_elf_start_time_2 = std::max(c_elf_start_time, (int)myToys_7(toy_row(7),1)); //toy1 start time 
            int act_duration = ceil(myToys_7(toy_row(7),2)/c_elf_rating); //toy1 actual work time 
            int sanc = getSanctionedBreakdown(c_elf_start_time_2, act_duration); //toy1 sanctional time
            double sanc_rate = (double)sanc/act_duration;
            
            if(sanc_rate < 1){
        
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
                    delay_num += 1;
                    c_toy_id = myToys_7(toy_row(7),0);
                    c_toy_arrival = myToys_7(toy_row(7),1);
                    c_toy_duration = myToys_7(toy_row(7),2);
                    toy_row(7) += 1;
                }
            }else{
                c_toy_id = myToys_7(toy_row(7),0);
                c_toy_arrival = myToys_7(toy_row(7),1);
                c_toy_duration = myToys_7(toy_row(7),2);
                toy_row(7) += 1;
            }
            
//re3            
        }else if((c_elf_rating >= .37) & (toy_row(6) < myToys_6.nrow())){
            
            int c_elf_start_time_2 = std::max(c_elf_start_time, (int)myToys_6(toy_row(6),1)); //toy1 start time 
            int act_duration = ceil(myToys_6(toy_row(6),2)/c_elf_rating); //toy1 actual work time 
            int sanc = getSanctionedBreakdown(c_elf_start_time_2, act_duration); //toy1 sanctional time
            double sanc_rate = (double)sanc/act_duration;
            
            if(sanc_rate < 1){
        
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
                    delay_num += 1;
                    c_toy_id = myToys_6(toy_row(6),0);
                    c_toy_arrival = myToys_6(toy_row(6),1);
                    c_toy_duration = myToys_6(toy_row(6),2);
                    toy_row(6) += 1;
                }
            }else{
                c_toy_id = myToys_6(toy_row(6),0);
                c_toy_arrival = myToys_6(toy_row(6),1);
                c_toy_duration = myToys_6(toy_row(6),2);
                toy_row(6) += 1;
            }
            
//re2            
        }else if((c_elf_rating >= .3) & (toy_row(5) < myToys_5.nrow())){
            
            int c_elf_start_time_2 = std::max(c_elf_start_time, (int)myToys_5(toy_row(5),1)); //toy1 start time 
            int act_duration = ceil(myToys_5(toy_row(5),2)/c_elf_rating); //toy1 actual work time 
            int sanc = getSanctionedBreakdown(c_elf_start_time_2, act_duration); //toy1 sanctional time
            double sanc_rate = (double)sanc/act_duration;
            
            if(sanc_rate < 1){
        
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
                    delay_num += 1;
                    c_toy_id = myToys_5(toy_row(5),0);
                    c_toy_arrival = myToys_5(toy_row(5),1);
                    c_toy_duration = myToys_5(toy_row(5),2);
                    toy_row(5) += 1;
                }
            }else{
                c_toy_id = myToys_5(toy_row(5),0);
                c_toy_arrival = myToys_5(toy_row(5),1);
                c_toy_duration = myToys_5(toy_row(5),2);
                toy_row(5) += 1;
            }
            
//re1            
        }else if((c_elf_rating >= .25) & (toy_row(4) < myToys_4.nrow())){
            
            int c_elf_start_time_2 = std::max(c_elf_start_time, (int)myToys_4(toy_row(4),1)); //toy1 start time 
            int act_duration = ceil(myToys_4(toy_row(4),2)/c_elf_rating); //toy1 actual work time 
            int sanc = getSanctionedBreakdown(c_elf_start_time_2, act_duration); //toy1 sanctional time
            double sanc_rate = (double)sanc/act_duration;
            
            if(sanc_rate < 1){
        
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
                    delay_num += 1;
                    c_toy_id = myToys_4(toy_row(4),0);
                    c_toy_arrival = myToys_4(toy_row(4),1);
                    c_toy_duration = myToys_4(toy_row(4),2);
                    toy_row(4) += 1;
                }
            }else{
                c_toy_id = myToys_4(toy_row(4),0);
                c_toy_arrival = myToys_4(toy_row(4),1);
                c_toy_duration = myToys_4(toy_row(4),2);
                toy_row(4) += 1;
            }
            
        }else{
            
            if(toy_row(0) < myToys_0.nrow()){
                
                int c_elf_start_time_2 = std::max(c_elf_start_time, (int)myToys_0(toy_row(0),1)); //toy1 start time 
                int act_duration = ceil(myToys_0(toy_row(0),2)/c_elf_rating); //toy1 actual work time 
                int sanc = getSanctionedBreakdown(c_elf_start_time_2, act_duration); //toy1 sanctional time
                double sanc_rate = (double)sanc/act_duration;
                
                if(sanc_rate < 0.9){
                    c_elf_start_time = 840 + sanc + c_elf_start_time_2;
                    
                    delay_sum += sanc;
                    delay_num += 1;
                }
                c_toy_id = myToys_0(toy_row(0),0);
                c_toy_arrival = myToys_0(toy_row(0),1);
                c_toy_duration = myToys_0(toy_row(0),2);
                toy_row(0) += 1;
                
            }else if(toy_row(4) < myToys_4.nrow()){
                c_toy_id = myToys_4(toy_row(4),0);
                c_toy_arrival = myToys_4(toy_row(4),1);
                c_toy_duration = myToys_4(toy_row(4),2);
                toy_row(4) += 1;
            }else if(toy_row(5) < myToys_5.nrow()){
                c_toy_id = myToys_5(toy_row(5),0);
                c_toy_arrival = myToys_5(toy_row(5),1);
                c_toy_duration = myToys_5(toy_row(5),2);
                toy_row(5) += 1;
            }else if(toy_row(6) < myToys_6.nrow()){
                c_toy_id = myToys_6(toy_row(6),0);
                c_toy_arrival = myToys_6(toy_row(6),1);
                c_toy_duration = myToys_6(toy_row(6),2);
                toy_row(6) += 1;
            }else if(toy_row(7) < myToys_7.nrow()){
                c_toy_id = myToys_7(toy_row(7),0);
                c_toy_arrival = myToys_7(toy_row(7),1);
                c_toy_duration = myToys_7(toy_row(7),2);
                toy_row(7) += 1;
            }else if(toy_row(8) < myToys_8.nrow()){
                c_toy_id = myToys_8(toy_row(8),0);
                c_toy_arrival = myToys_8(toy_row(8),1);
                c_toy_duration = myToys_8(toy_row(8),2);
                toy_row(8) += 1;
            }else if(toy_row(9) < myToys_9.nrow()){
                c_toy_id = myToys_9(toy_row(9),0);
                c_toy_arrival = myToys_9(toy_row(9),1);
                c_toy_duration = myToys_9(toy_row(9),2);
                toy_row(9) += 1;
            }else if(toy_row(10) < myToys_10.nrow()){
                c_toy_id = myToys_10(toy_row(10),0);
                c_toy_arrival = myToys_10(toy_row(10),1);
                c_toy_duration = myToys_10(toy_row(10),2);
                toy_row(10) += 1;
                
            }else if(toy_row(3) < myToys_3.nrow()){
                c_toy_id = myToys_3(toy_row(3),0);
                c_toy_arrival = myToys_3(toy_row(3),1);
                c_toy_duration = myToys_3(toy_row(3),2);
                toy_row(3) += 1;
            }else if(toy_row(2) < myToys_2.nrow()){
                c_toy_id = myToys_2(toy_row(2),0);
                c_toy_arrival = myToys_2(toy_row(2),1);
                c_toy_duration = myToys_2(toy_row(2),2);
                toy_row(2) += 1;
            }
        }
        
        c_elf_start_time = std::max((int)c_elf_start_time, (int)c_toy_arrival);
        work_duration = ceil(c_toy_duration/c_elf_rating);
        
        myelves_rate(min_row) = updateProductivity(c_elf_start_time, work_duration, c_elf_rating);
        myelves(min_row,2) = updateNextAvailableMinute(c_elf_start_time, work_duration);
        
        outcomes(current_toy,0) = c_toy_id;
        outcomes(current_toy,1) = c_elf_id;
        outcomes(current_toy,2) = c_elf_start_time;
        outcomes(current_toy,3) = work_duration;
        
        if((c_elf_rating==4) & (myelves_rate(min_row)<4)){
            retrain_count++;
        }
        
        if(current_toy % 100000 == 0) {
            Rcpp::Rcout << '\n' << (double)current_toy/1000000 << ' ' << rate_count << ' ' << retrain_count << ' ' << delay_sum << ' ' << delay_num
            << '\n' << myToys_0.nrow() << ' ' << toy_row(0)/myToys_0.nrow()*100
            << '\n' << myToys_1.nrow() << ' ' << toy_row(1)/myToys_1.nrow()*100
            << '\n' << myToys_2.nrow() << ' ' << toy_row(2)/myToys_2.nrow()*100
            << '\n' << myToys_3.nrow() << ' ' << toy_row(3)/myToys_3.nrow()*100
            << '\n' << myToys_4.nrow() << ' ' << toy_row(4)/myToys_4.nrow()*100
            << '\n' << myToys_5.nrow() << ' ' << toy_row(5)/myToys_5.nrow()*100
            << '\n' << myToys_6.nrow() << ' ' << toy_row(6)/myToys_6.nrow()*100
            << '\n' << myToys_7.nrow() << ' ' << toy_row(7)/myToys_7.nrow()*100
            << '\n' << myToys_8.nrow() << ' ' << toy_row(8)/myToys_8.nrow()*100
            << '\n' << myToys_9.nrow() << ' ' << toy_row(9)/myToys_9.nrow()*100
            << '\n' << myToys_10.nrow() << ' ' << toy_row(10)/myToys_10.nrow()*100
            << '\n' << myToys_11.nrow() << ' ' << toy_row(11)/myToys_11.nrow()*100
            << '\n' << myToys_12.nrow() << ' ' << toy_row(12)/myToys_12.nrow()*100
            << '\n' << myToys_13.nrow() << ' ' << toy_row(13)/myToys_13.nrow()*100
            << '\n' << myToys_14.nrow() << ' ' << toy_row(14)/myToys_14.nrow()*100
            << '\n' << myToys_15.nrow() << ' ' << toy_row(15)/myToys_15.nrow()*100
            << '\n' << myToys_16.nrow() << ' ' << toy_row(16)/myToys_16.nrow()*100
            << '\n' << myToys_17.nrow() << ' ' << toy_row(17)/myToys_17.nrow()*100
            << '\n' << myToys_18.nrow() << ' ' << toy_row(18)/myToys_18.nrow()*100 << '\n';
        }
    }
    Rcpp::Rcout << '\n' << rate_count << ' ' << retrain_count << ' ' << delay_sum<< ' ' << delay_num
    << '\n' << myToys_0.nrow() << ' ' << toy_row(0)/myToys_0.nrow()*100
    << '\n' << myToys_1.nrow() << ' ' << toy_row(1)/myToys_1.nrow()*100
    << '\n' << myToys_2.nrow() << ' ' << toy_row(2)/myToys_2.nrow()*100
    << '\n' << myToys_3.nrow() << ' ' << toy_row(3)/myToys_3.nrow()*100
    << '\n' << myToys_4.nrow() << ' ' << toy_row(4)/myToys_4.nrow()*100
    << '\n' << myToys_5.nrow() << ' ' << toy_row(5)/myToys_5.nrow()*100
    << '\n' << myToys_6.nrow() << ' ' << toy_row(6)/myToys_6.nrow()*100
    << '\n' << myToys_7.nrow() << ' ' << toy_row(7)/myToys_7.nrow()*100
    << '\n' << myToys_8.nrow() << ' ' << toy_row(8)/myToys_8.nrow()*100
    << '\n' << myToys_9.nrow() << ' ' << toy_row(9)/myToys_9.nrow()*100
    << '\n' << myToys_10.nrow() << ' ' << toy_row(10)/myToys_10.nrow()*100
    << '\n' << myToys_11.nrow() << ' ' << toy_row(11)/myToys_11.nrow()*100
    << '\n' << myToys_12.nrow() << ' ' << toy_row(12)/myToys_12.nrow()*100
    << '\n' << myToys_13.nrow() << ' ' << toy_row(13)/myToys_13.nrow()*100
    << '\n' << myToys_14.nrow() << ' ' << toy_row(14)/myToys_14.nrow()*100
    << '\n' << myToys_15.nrow() << ' ' << toy_row(15)/myToys_15.nrow()*100
    << '\n' << myToys_16.nrow() << ' ' << toy_row(16)/myToys_16.nrow()*100
    << '\n' << myToys_17.nrow() << ' ' << toy_row(17)/myToys_17.nrow()*100
    << '\n' << myToys_18.nrow() << ' ' << toy_row(18)/myToys_18.nrow()*100 << '\n';
    return outcomes;
}
