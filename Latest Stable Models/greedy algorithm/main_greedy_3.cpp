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
    
    NumericMatrix outcomes(10000000,4); //ToyId, Arrival_time, Duration, Size
    NumericVector toy_row(18);
    int c_toy_id, c_toy_arrival, c_toy_duration, min_val, min_row, c_elf_id, c_elf_start_time,work_duration;
    double c_elf_rating;
    double act_duration, sanc;
    double rate_duration;
    
    for(unsigned long long current_toy=0; current_toy<10000000; ++current_toy){
        
        min_val = myelves(0,2);
        min_row = 0;
        for(int e=0; e<myelves.nrow(); e++){
            if (min_val > myelves(e,2)){
                min_val = myelves(e,2);
                min_row = e;
            }
        }
        
        //min_row=0;
        c_elf_id = myelves(min_row,0);
        c_elf_start_time = myelves(min_row,2);
        c_elf_rating = myelves_rate(min_row);
        
        //.25, .30, .37, .45, .55, .67, .82, 1, 1.22, 1.49, 1.81, 2.21, 2.69, 3.28, 4
        if((c_elf_rating > 3.95) & (toy_row(1) < myToys_1.nrow())){
            c_elf_start_time = std::max((int)c_elf_start_time, (int)myToys_0(toy_row(0),1));
            act_duration = ceil(myToys_1(toy_row(1),2)/c_elf_rating);
            sanc = getSanctionedBreakdown(c_elf_start_time, act_duration);
            double a = updateProductivity(c_elf_start_time,(int)act_duration, c_elf_rating)/c_elf_rating;
            
            if((a<=0.9) & (toy_row(0) < myToys_0.nrow())){
                c_toy_id = myToys_0(toy_row(0),0);
                c_toy_arrival = myToys_0(toy_row(0),1);
                c_toy_duration = myToys_0(toy_row(0),2);
                toy_row(0) += 1;
            }else{
                c_toy_id = myToys_1(toy_row(1),0);
                c_toy_arrival = myToys_1(toy_row(1),1);
                c_toy_duration = myToys_1(toy_row(1),2);
                toy_row(1) += 1;
            }
            
        }else if((c_elf_rating > 3.95) & (toy_row(2) < myToys_2.nrow()/2)){
            c_toy_id = myToys_2(toy_row(2),0);
            c_toy_arrival = myToys_2(toy_row(2),1);
            c_toy_duration = myToys_2(toy_row(2),2);
            toy_row(2) += 1;
            
        }else if((c_elf_rating >= 3.28) & (toy_row(17) < myToys_17.nrow())){
            c_elf_start_time = std::max((int)c_elf_start_time, (int)myToys_0(toy_row(17),1));
            act_duration = ceil(myToys_17(toy_row(17),2)/c_elf_rating);
            sanc = getSanctionedBreakdown(c_elf_start_time, act_duration);
            rate_duration = sanc/act_duration;
            while(rate_duration<=0.03){
                c_elf_start_time = c_elf_start_time + 845;
                sanc = getSanctionedBreakdown(c_elf_start_time, act_duration);
                rate_duration = sanc/act_duration;
            }
            if((sanc/act_duration<=0.95) & (toy_row(0) < myToys_0.nrow())){
                c_toy_id = myToys_0(toy_row(0),0);
                c_toy_arrival = myToys_0(toy_row(0),1);
                c_toy_duration = myToys_0(toy_row(0),2);
                toy_row(0) += 1;
            }else{
               c_toy_id = myToys_17(toy_row(17),0);
                c_toy_arrival = myToys_17(toy_row(17),1);
                c_toy_duration = myToys_17(toy_row(17),2);
                toy_row(17) += 1;
            }
            
        }else if((c_elf_rating >= 2.69) & (toy_row(16) < myToys_16.nrow())){
            c_elf_start_time = std::max((int)c_elf_start_time, (int)myToys_0(toy_row(16),1));
            act_duration = ceil(myToys_16(toy_row(16),2)/c_elf_rating);
            sanc = getSanctionedBreakdown(c_elf_start_time, act_duration);
            rate_duration = sanc/act_duration;
            while(rate_duration<=0.03){
                c_elf_start_time = c_elf_start_time + 845;
                sanc = getSanctionedBreakdown(c_elf_start_time, act_duration);
                rate_duration = sanc/act_duration;
            }
            if((sanc/act_duration<=0.95) & (toy_row(0) < myToys_0.nrow())){
                c_toy_id = myToys_0(toy_row(0),0);
                c_toy_arrival = myToys_0(toy_row(0),1);
                c_toy_duration = myToys_0(toy_row(0),2);
                toy_row(0) += 1;
            }else{
                c_toy_id = myToys_16(toy_row(16),0);
                c_toy_arrival = myToys_16(toy_row(16),1);
                c_toy_duration = myToys_16(toy_row(16),2);
                toy_row(16) += 1;
            }
            
        }else if((c_elf_rating >= 2.21) & (toy_row(15) < myToys_15.nrow())){
            c_elf_start_time = std::max((int)c_elf_start_time, (int)myToys_0(toy_row(15),1));
            act_duration = ceil(myToys_15(toy_row(15),2)/c_elf_rating);
            sanc = getSanctionedBreakdown(c_elf_start_time, act_duration);
            rate_duration = sanc/act_duration;
            while(rate_duration<=0.03){
                c_elf_start_time = c_elf_start_time + 845;
                sanc = getSanctionedBreakdown(c_elf_start_time, act_duration);
                rate_duration = sanc/act_duration;
            }
            if((sanc/act_duration<=0.95) & (toy_row(0) < myToys_0.nrow())){
                c_toy_id = myToys_0(toy_row(0),0);
                c_toy_arrival = myToys_0(toy_row(0),1);
                c_toy_duration = myToys_0(toy_row(0),2);
                toy_row(0) += 1;
            }else{
                c_toy_id = myToys_15(toy_row(15),0);
                c_toy_arrival = myToys_15(toy_row(15),1);
                c_toy_duration = myToys_15(toy_row(15),2);
                toy_row(15) += 1;
            }
            
        }else if((c_elf_rating >= 1.81) & (toy_row(14) < myToys_14.nrow())){
            c_elf_start_time = std::max((int)c_elf_start_time, (int)myToys_0(toy_row(14),1));
            act_duration = ceil(myToys_14(toy_row(14),2)/c_elf_rating);
            sanc = getSanctionedBreakdown(c_elf_start_time, act_duration);
            rate_duration = sanc/act_duration;
            while(rate_duration<=0.03){
                c_elf_start_time = c_elf_start_time + 845;
                sanc = getSanctionedBreakdown(c_elf_start_time, act_duration);
                rate_duration = sanc/act_duration;
            }
            if((sanc/act_duration<=0.95) & (toy_row(0) < myToys_0.nrow())){
                c_toy_id = myToys_0(toy_row(0),0);
                c_toy_arrival = myToys_0(toy_row(0),1);
                c_toy_duration = myToys_0(toy_row(0),2);
                toy_row(0) += 1;
            }else{
                c_toy_id = myToys_14(toy_row(14),0);
                c_toy_arrival = myToys_14(toy_row(14),1);
                c_toy_duration = myToys_14(toy_row(14),2);
                toy_row(14) += 1;
            }
            
        }else if((c_elf_rating >= 1.49) & (toy_row(13) < myToys_13.nrow())){
            c_elf_start_time = std::max((int)c_elf_start_time, (int)myToys_0(toy_row(13),1));
            act_duration = ceil(myToys_13(toy_row(13),2)/c_elf_rating);
            sanc = getSanctionedBreakdown(c_elf_start_time, act_duration);
            rate_duration = sanc/act_duration;
            while(rate_duration<=0.03){
                c_elf_start_time = c_elf_start_time + 845;
                sanc = getSanctionedBreakdown(c_elf_start_time, act_duration);
                rate_duration = sanc/act_duration;
            }
            if((sanc/act_duration<=0.95) & (toy_row(0) < myToys_0.nrow())){
                c_toy_id = myToys_0(toy_row(0),0);
                c_toy_arrival = myToys_0(toy_row(0),1);
                c_toy_duration = myToys_0(toy_row(0),2);
                toy_row(0) += 1;
            }else{
                c_toy_id = myToys_13(toy_row(13),0);
                c_toy_arrival = myToys_13(toy_row(13),1);
                c_toy_duration = myToys_13(toy_row(13),2);
                toy_row(13) += 1;
            }
            
        }else if((c_elf_rating >= 1.22) & (toy_row(12) < myToys_12.nrow())){
            c_elf_start_time = std::max((int)c_elf_start_time, (int)myToys_12(toy_row(12),1));
            act_duration = ceil(myToys_12(toy_row(12),2)/c_elf_rating);
            sanc = getSanctionedBreakdown(c_elf_start_time, act_duration);
            rate_duration = sanc/act_duration;
            while(rate_duration<=0.03){
                c_elf_start_time = c_elf_start_time + 845;
                sanc = getSanctionedBreakdown(c_elf_start_time, act_duration);
                rate_duration = sanc/act_duration;
            }
            if((sanc/act_duration<=0.95) & (toy_row(0) < myToys_0.nrow())){
                c_toy_id = myToys_0(toy_row(0),0);
                c_toy_arrival = myToys_0(toy_row(0),1);
                c_toy_duration = myToys_0(toy_row(0),2);
                toy_row(0) += 1;
            }else{
                c_toy_id = myToys_12(toy_row(12),0);
                c_toy_arrival = myToys_12(toy_row(12),1);
                c_toy_duration = myToys_12(toy_row(12),2);
                toy_row(12) += 1;
            }
            
        }else if((c_elf_rating >= 1.0) & (toy_row(11) < myToys_11.nrow())){
            c_elf_start_time = std::max((int)c_elf_start_time, (int)myToys_0(toy_row(11),1));
            act_duration = ceil(myToys_11(toy_row(11),2)/c_elf_rating);
            sanc = getSanctionedBreakdown(c_elf_start_time, act_duration);
            rate_duration = sanc/act_duration;
            
            while(rate_duration<=0.03){
                c_elf_start_time = c_elf_start_time + 845;
                sanc = getSanctionedBreakdown(c_elf_start_time, act_duration);
                rate_duration = sanc/act_duration;
            }
            if((rate_duration<=0.95) & (toy_row(0) < myToys_0.nrow())){
                //Rcpp::Rcout << '\n' << getSanctionedBreakdown(c_elf_start_time,myToys_11(toy_row(11),2))/myToys_11(toy_row(11),2);
            
                c_toy_id = myToys_0(toy_row(0),0);
                c_toy_arrival = myToys_0(toy_row(0),1);
                c_toy_duration = myToys_0(toy_row(0),2);
                toy_row(0) += 1;
            }else{
                c_toy_id = myToys_11(toy_row(11),0);
                c_toy_arrival = myToys_11(toy_row(11),1);
                c_toy_duration = myToys_11(toy_row(11),2);
                toy_row(11) += 1;
                
            }
            
        }else if((c_elf_rating >= .82) & (toy_row(10) < myToys_10.nrow())){
            c_elf_start_time = std::max((int)c_elf_start_time, (int)myToys_10(toy_row(10),1));
            act_duration = ceil(myToys_10(toy_row(10),2)/c_elf_rating);
            sanc = getSanctionedBreakdown(c_elf_start_time, act_duration);
            rate_duration = sanc/act_duration;
            while(rate_duration<=0.03){
                c_elf_start_time = c_elf_start_time + 845;
                sanc = getSanctionedBreakdown(c_elf_start_time, act_duration);
                rate_duration = sanc/act_duration;
            }
            if((sanc/act_duration<=0.95) & (toy_row(0) < myToys_0.nrow())){
                c_toy_id = myToys_0(toy_row(0),0);
                c_toy_arrival = myToys_0(toy_row(0),1);
                c_toy_duration = myToys_0(toy_row(0),2);
                toy_row(0) += 1;
            }else{
                c_toy_id = myToys_10(toy_row(10),0);
                c_toy_arrival = myToys_10(toy_row(10),1);
                c_toy_duration = myToys_10(toy_row(10),2);
                toy_row(10) += 1;
            }
            
        }else if((c_elf_rating >= .67) & (toy_row(9) < myToys_9.nrow())){
            c_elf_start_time = std::max((int)c_elf_start_time, (int)myToys_9(toy_row(9),1));
            act_duration = ceil(myToys_9(toy_row(9),2)/c_elf_rating);
            sanc = getSanctionedBreakdown(c_elf_start_time, act_duration);
            rate_duration = sanc/act_duration;
            while(rate_duration<=0.03){
                c_elf_start_time = c_elf_start_time + 845;
                sanc = getSanctionedBreakdown(c_elf_start_time, act_duration);
                rate_duration = sanc/act_duration;
            }
            if((sanc/act_duration<=0.95) & (toy_row(0) < myToys_0.nrow())){
                c_toy_id = myToys_0(toy_row(0),0);
                c_toy_arrival = myToys_0(toy_row(0),1);
                c_toy_duration = myToys_0(toy_row(0),2);
                toy_row(0) += 1;
            }else{
                c_toy_id = myToys_9(toy_row(9),0);
                c_toy_arrival = myToys_9(toy_row(9),1);
                c_toy_duration = myToys_9(toy_row(9),2);
                toy_row(9) += 1;
            }
            
        }else if((c_elf_rating >= .55) & (toy_row(8) < myToys_8.nrow())){
            c_elf_start_time = std::max((int)c_elf_start_time, (int)myToys_8(toy_row(8),1));
            act_duration = ceil(myToys_8(toy_row(8),2)/c_elf_rating);
            sanc = getSanctionedBreakdown(c_elf_start_time, act_duration);
            rate_duration = sanc/act_duration;
            while(rate_duration<=0.03){
                c_elf_start_time = c_elf_start_time + 845;
                sanc = getSanctionedBreakdown(c_elf_start_time, act_duration);
                rate_duration = sanc/act_duration;
            }
            if((sanc/act_duration<=0.95) & (toy_row(0) < myToys_0.nrow())){
                c_toy_id = myToys_0(toy_row(0),0);
                c_toy_arrival = myToys_0(toy_row(0),1);
                c_toy_duration = myToys_0(toy_row(0),2);
                toy_row(0) += 1;
            }else{
                c_toy_id = myToys_8(toy_row(8),0);
                c_toy_arrival = myToys_8(toy_row(8),1);
                c_toy_duration = myToys_8(toy_row(8),2);
                toy_row(8) += 1;
            }
            
        }else if((c_elf_rating >= .45) & (toy_row(7) < myToys_7.nrow())){
            c_elf_start_time = std::max((int)c_elf_start_time, (int)myToys_7(toy_row(7),1));
            act_duration = ceil(myToys_7(toy_row(7),2)/c_elf_rating);
            sanc = getSanctionedBreakdown(c_elf_start_time, act_duration);
            rate_duration = sanc/act_duration;
            while(rate_duration<=0.03){
                c_elf_start_time = c_elf_start_time + 845;
                sanc = getSanctionedBreakdown(c_elf_start_time, act_duration);
                rate_duration = sanc/act_duration;
            }
            if((sanc/act_duration<=0.95) & (toy_row(0) < myToys_0.nrow())){
                c_toy_id = myToys_0(toy_row(0),0);
                c_toy_arrival = myToys_0(toy_row(0),1);
                c_toy_duration = myToys_0(toy_row(0),2);
                toy_row(0) += 1;
            }else{
                c_toy_id = myToys_7(toy_row(7),0);
                c_toy_arrival = myToys_7(toy_row(7),1);
                c_toy_duration = myToys_7(toy_row(7),2);
                toy_row(7) += 1;
            }
            
        }else if((c_elf_rating >= .37) & (toy_row(6) < myToys_6.nrow())){
            c_elf_start_time = std::max((int)c_elf_start_time, (int)myToys_6(toy_row(6),1));
            act_duration = ceil(myToys_6(toy_row(6),2)/c_elf_rating);
            sanc = getSanctionedBreakdown(c_elf_start_time, act_duration);
            rate_duration = sanc/act_duration;
            while(rate_duration<=0.03){
                c_elf_start_time = c_elf_start_time + 845;
                sanc = getSanctionedBreakdown(c_elf_start_time, act_duration);
                rate_duration = sanc/act_duration;
            }
            if((sanc/act_duration<=0.95) & (toy_row(0) < myToys_0.nrow())){
                c_toy_id = myToys_0(toy_row(0),0);
                c_toy_arrival = myToys_0(toy_row(0),1);
                c_toy_duration = myToys_0(toy_row(0),2);
                toy_row(0) += 1;
            }else{
                c_toy_id = myToys_6(toy_row(6),0);
                c_toy_arrival = myToys_6(toy_row(6),1);
                c_toy_duration = myToys_6(toy_row(6),2);
                toy_row(6) += 1;
            }
            
        }else if((c_elf_rating >= .3) & (toy_row(5) < myToys_5.nrow())){
            c_elf_start_time = std::max((int)c_elf_start_time, (int)myToys_5(toy_row(5),1));
            act_duration = ceil(myToys_5(toy_row(5),2)/c_elf_rating);
            sanc = getSanctionedBreakdown(c_elf_start_time, act_duration);
            rate_duration = sanc/act_duration;
            while(rate_duration<=0.03){
                c_elf_start_time = c_elf_start_time + 845;
                sanc = getSanctionedBreakdown(c_elf_start_time, act_duration);
                rate_duration = sanc/act_duration;
            }
            if((sanc/act_duration<=0.95) & (toy_row(0) < myToys_0.nrow())){
                c_toy_id = myToys_0(toy_row(0),0);
                c_toy_arrival = myToys_0(toy_row(0),1);
                c_toy_duration = myToys_0(toy_row(0),2);
                toy_row(0) += 1;
            }else{
                c_toy_id = myToys_5(toy_row(5),0);
                c_toy_arrival = myToys_5(toy_row(5),1);
                c_toy_duration = myToys_5(toy_row(5),2);
                toy_row(5) += 1;
            }
            
        }else if((c_elf_rating >= .25) & (toy_row(4) < myToys_4.nrow())){
            c_elf_start_time = std::max((int)c_elf_start_time, (int)myToys_4(toy_row(4),1));
            act_duration = ceil(myToys_4(toy_row(4),2)/c_elf_rating);
            sanc = getSanctionedBreakdown(c_elf_start_time, act_duration);
            rate_duration = sanc/act_duration;
            while(rate_duration<=0.03){
                c_elf_start_time = c_elf_start_time + 845;
                sanc = getSanctionedBreakdown(c_elf_start_time, act_duration);
                rate_duration = sanc/act_duration;
            }
            if((sanc/act_duration<=0.95) & (toy_row(0) < myToys_0.nrow())){
                c_toy_id = myToys_0(toy_row(0),0);
                c_toy_arrival = myToys_0(toy_row(0),1);
                c_toy_duration = myToys_0(toy_row(0),2);
                toy_row(0) += 1;
            }else{
                c_toy_id = myToys_4(toy_row(4),0);
                c_toy_arrival = myToys_4(toy_row(4),1);
                c_toy_duration = myToys_4(toy_row(4),2);
                toy_row(4) += 1;
            }
            
        }else{
            
            if(toy_row(0) < myToys_0.nrow()){
                c_toy_id = myToys_0(toy_row(0),0);
                c_toy_arrival = myToys_0(toy_row(0),1);
                c_toy_duration = myToys_0(toy_row(0),2);
                toy_row(0) += 1;
            }else if(toy_row(17) < myToys_17.nrow()){
                c_toy_id = myToys_17(toy_row(17),0);
                c_toy_arrival = myToys_17(toy_row(17),1);
                c_toy_duration = myToys_17(toy_row(17),2);
                toy_row(17) += 1;
            }else if(toy_row(16) < myToys_16.nrow()){
                c_toy_id = myToys_16(toy_row(16),0);
                c_toy_arrival = myToys_16(toy_row(16),1);
                c_toy_duration = myToys_16(toy_row(16),2);
                toy_row(16) += 1;
            }else if(toy_row(15) < myToys_15.nrow()){
                c_toy_id = myToys_15(toy_row(15),0);
                c_toy_arrival = myToys_15(toy_row(15),1);
                c_toy_duration = myToys_15(toy_row(15),2);
                toy_row(15) += 1;
            }else if(toy_row(14) < myToys_14.nrow()){
                c_toy_id = myToys_14(toy_row(14),0);
                c_toy_arrival = myToys_14(toy_row(14),1);
                c_toy_duration = myToys_14(toy_row(14),2);
                toy_row(14) += 1;
            }else if(toy_row(13) < myToys_13.nrow()){
                c_toy_id = myToys_13(toy_row(13),0);
                c_toy_arrival = myToys_13(toy_row(13),1);
                c_toy_duration = myToys_13(toy_row(13),2);
                toy_row(13) += 1;
            }else if(toy_row(12) < myToys_12.nrow()){
                c_toy_id = myToys_12(toy_row(12),0);
                c_toy_arrival = myToys_12(toy_row(12),1);
                c_toy_duration = myToys_12(toy_row(12),2);
                toy_row(12) += 1;
            }else if(toy_row(11) < myToys_11.nrow()){
                c_toy_id = myToys_11(toy_row(11),0);
                c_toy_arrival = myToys_11(toy_row(11),1);
                c_toy_duration = myToys_11(toy_row(11),2);
                toy_row(11) += 1;
            }else if(toy_row(10) < myToys_10.nrow()){
                c_toy_id = myToys_10(toy_row(10),0);
                c_toy_arrival = myToys_10(toy_row(10),1);
                c_toy_duration = myToys_10(toy_row(10),2);
                toy_row(10) += 1;
            }else if(toy_row(9) < myToys_9.nrow()){
                c_toy_id = myToys_9(toy_row(9),0);
                c_toy_arrival = myToys_9(toy_row(9),1);
                c_toy_duration = myToys_9(toy_row(9),2);
                toy_row(9) += 1;
            }else if(toy_row(8) < myToys_8.nrow()){
                c_toy_id = myToys_8(toy_row(8),0);
                c_toy_arrival = myToys_8(toy_row(8),1);
                c_toy_duration = myToys_8(toy_row(8),2);
                toy_row(8) += 1;
            }else if(toy_row(7) < myToys_7.nrow()){
                c_toy_id = myToys_7(toy_row(7),0);
                c_toy_arrival = myToys_7(toy_row(7),1);
                c_toy_duration = myToys_7(toy_row(7),2);
                toy_row(7) += 1;
            }else if(toy_row(6) < myToys_6.nrow()){
                c_toy_id = myToys_6(toy_row(6),0);
                c_toy_arrival = myToys_6(toy_row(6),1);
                c_toy_duration = myToys_6(toy_row(6),2);
                toy_row(6) += 1;
            }else if(toy_row(5) < myToys_5.nrow()){
                c_toy_id = myToys_5(toy_row(5),0);
                c_toy_arrival = myToys_5(toy_row(5),1);
                c_toy_duration = myToys_5(toy_row(5),2);
                toy_row(5) += 1;
            }else if(toy_row(4) < myToys_4.nrow()){
                c_toy_id = myToys_4(toy_row(4),0);
                c_toy_arrival = myToys_4(toy_row(4),1);
                c_toy_duration = myToys_4(toy_row(4),2);
                toy_row(4) += 1;
            }else if(toy_row(1) < myToys_1.nrow()){
                c_toy_id = myToys_1(toy_row(1),0);
                c_toy_arrival = myToys_1(toy_row(1),1);
                c_toy_duration = myToys_1(toy_row(1),2);
                toy_row(1) += 1;
            }else if(toy_row(2) < myToys_2.nrow()){
                c_toy_id = myToys_2(toy_row(2),0);
                c_toy_arrival = myToys_2(toy_row(2),1);
                c_toy_duration = myToys_2(toy_row(2),2);
                toy_row(2) += 1;
            }else if(toy_row(3) < myToys_3.nrow()){
                c_toy_id = myToys_3(toy_row(3),0);
                c_toy_arrival = myToys_3(toy_row(3),1);
                c_toy_duration = myToys_3(toy_row(3),2);
                toy_row(3) += 1;
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
        
        //Rcpp::Rcout << '\n' << current_toy << ' ' << c_elf_id << ' ' << c_elf_rating << ' ' <<work_duration << ' ' <<c_toy_duration << ' ' << c_elf_start_time << ' ' << rate_duration;
        if(current_toy % 100000 == 0) {
            Rcpp::Rcout << '\n' << (double)current_toy/1000000 << '\n' << myToys_0.nrow() << ' ' << toy_row(0)/myToys_0.nrow()
            << '\n' << myToys_1.nrow() << ' ' << toy_row(1)/myToys_1.nrow()
            << '\n' << myToys_2.nrow() << ' ' << toy_row(2)/myToys_2.nrow()
            << '\n' << myToys_3.nrow() << ' ' << toy_row(3)/myToys_3.nrow()
            << '\n' << myToys_4.nrow() << ' ' << toy_row(4)/myToys_4.nrow()
            << '\n' << myToys_5.nrow() << ' ' << toy_row(5)/myToys_5.nrow()
            << '\n' << myToys_6.nrow() << ' ' << toy_row(6)/myToys_6.nrow()
            << '\n' << myToys_7.nrow() << ' ' << toy_row(7)/myToys_7.nrow()
            << '\n' << myToys_8.nrow() << ' ' << toy_row(8)/myToys_8.nrow()
            << '\n' << myToys_9.nrow() << ' ' << toy_row(9)/myToys_9.nrow()
            << '\n' << myToys_10.nrow() << ' ' << toy_row(10)/myToys_10.nrow()
            << '\n' << myToys_11.nrow() << ' ' << toy_row(11)/myToys_11.nrow()
            << '\n' << myToys_12.nrow() << ' ' << toy_row(12)/myToys_12.nrow()
            << '\n' << myToys_13.nrow() << ' ' << toy_row(13)/myToys_13.nrow()
            << '\n' << myToys_14.nrow() << ' ' << toy_row(14)/myToys_14.nrow()
            << '\n' << myToys_15.nrow() << ' ' << toy_row(15)/myToys_15.nrow()
            << '\n' << myToys_16.nrow() << ' ' << toy_row(16)/myToys_16.nrow()
            << '\n' << myToys_17.nrow() << ' ' << toy_row(17)/myToys_17.nrow() << '\n';
        }
    }
    //Rcpp::Rcout << '\n' << 10000000/1000000 << ' ' << myToys_3.nrow() << ' ' << toy_row(3);
    return outcomes;
}
