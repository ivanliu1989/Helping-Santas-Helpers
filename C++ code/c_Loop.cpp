// [[Rcpp::plugins(cpp11)]]
#include <unordered_set>
#include <Rcpp.h>
#include <algorithm>    // std::swap_ranges std::shuffle
#include <vector>       // std::vector
#include <time.h>
#include <random>       // std::default_random_engine
#include <chrono>       // std::chrono::system_clock
#include <list>
using namespace Rcpp;


// [[Rcpp::export]]
int updateNextAvailableMinute(int start_minute, int work_duration){
	int next_available_time = 0;
	int S = 0;
	int U = 0;
	int full_days = work_duration / (60 * 24);
	S = full_days * (10 * 60);
	U = full_days * (14 * 60);
	int remainder = start_minute + full_days * (60 * 24);
	for (int i = remainder; i < (start_minute + work_duration); ++i) {
		bool isSanctionedTime = ((i - 540) % (60 * 24)) < 600;
		if (isSanctionedTime) S += 1;
		else U += 1;
	}
	int end_minute = start_minute + work_duration;

	if (U == 0) {
		if (((end_minute - 540) % (60 * 24)) < 600) {
			next_available_time = end_minute;
		}
		else {
			bool isSanctionedTime = ((end_minute - 540) % (60 * 24)) < 600;
			bool isSanctionedTime_1 = ((end_minute - 539) % (60 * 24)) < 600;
			int next_min = 0;
			if (isSanctionedTime && isSanctionedTime_1) next_min = end_minute + 1;
			else {
				int num_days = end_minute / (60 * 24);
				int am_or_pm = (end_minute % (60 * 24)) / 540;
				next_min = 540 + (num_days + am_or_pm / 2) * (60 * 24);
			}
			next_available_time = next_min;
		}
	}
	else {
		if (U == 0) {
			bool isSanctionedTime = ((end_minute - 540) % (60 * 24)) < 600;
			if (isSanctionedTime) next_available_time = end_minute;
			else{
				bool isSanctionedTime = ((end_minute - 540) % (60 * 24)) < 600;
				bool isSanctionedTime_1 = ((end_minute - 539) % (60 * 24)) < 600;
				int next_min = 0;
				if (isSanctionedTime && isSanctionedTime_1) next_min = end_minute + 1;
				else {
					int num_days = end_minute / (60 * 24);
					int am_or_pm = (end_minute % (60 * 24)) / 540;
					next_min = 540 + (num_days + am_or_pm / 2) * (60 * 24);
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

		next_available_time = total_days * (60 * 24) + local_start + rest_time_remaining_minutes;
	}
	return next_available_time;
}

// [[Rcpp::export]]
double updateProductivity(int start_minute, int work_duration, double current_rating){
	NumericVector out(2);
	int S = 0;
	int U = 0;
	int full_days = work_duration / (60 * 24);
	S = full_days * (10 * 60);
	U = full_days * (14 * 60);
	int remainder = start_minute + full_days * (60 * 24);
	for (int i = remainder; i < (start_minute + work_duration); ++i) {
		bool isSanctionedTime = ((i - 540) % (60 * 24)) < 600;
		if (isSanctionedTime) S += 1;
		else U += 1;
	}
	double new_rating = std::max(0.25, std::min(4.0, current_rating * pow(1.02, S / 60.0) * pow(0.90, U / 60.0)));
	return new_rating;
}

// [[Rcpp::export]]
double solution_Elf_c(NumericMatrix myToys_c, NumericVector myelves_c, NumericVector schedule_c){
	int work_duration, c_toy_id, c_toy_arrival, c_toy_duration, c_elf_id, c_elf_start_time, schedule_index;
	double c_elf_rating;
	int n_toys = myToys_c.nrow();
	NumericMatrix outcomes(n_toys, 4);

	for (int current_toy = 0; current_toy<n_toys; ++current_toy){
		schedule_index = schedule_c(current_toy) - 1;
		c_toy_id = myToys_c(schedule_index, 0);
		c_toy_arrival = myToys_c(schedule_index, 1);
		c_toy_duration = myToys_c(schedule_index, 2);

		c_elf_id = myelves_c(0);
		c_elf_start_time = myelves_c(2);
		c_elf_rating = myelves_c(1);

		if (c_elf_start_time < c_toy_arrival) c_elf_start_time = c_toy_arrival;
		work_duration = ceil(c_toy_duration / c_elf_rating);

		myelves_c(2) = updateNextAvailableMinute(c_elf_start_time, work_duration);
		myelves_c(1) = updateProductivity(c_elf_start_time, work_duration, c_elf_rating);

		outcomes(current_toy, 0) = c_toy_id;
		outcomes(current_toy, 1) = c_elf_id;
		outcomes(current_toy, 2) = c_elf_start_time;
		outcomes(current_toy, 3) = work_duration;
	}
	int n_outcomes = outcomes.nrow();
	int last_time = 0;
	for (int i = 0; i<n_outcomes; ++i){
		if (last_time < (outcomes(i, 2) + outcomes(i, 3))) last_time = (outcomes(i, 2) + outcomes(i, 3));
	}
	myelves_c(2) = 540;
	myelves_c(1) = 1;
	return last_time * log(901);
}

// [[Rcpp::export]]
NumericVector assignX1(NumericVector x1, int np, int toy_row, int num, int p1, int p2, NumericMatrix myToys_c, NumericVector myelves_c, double fbest, NumericVector xbest){
  
		unsigned seed = std::chrono::system_clock::now().time_since_epoch().count();
		shuffle(x1.begin() + p1, x1.begin() + p2, std::default_random_engine(seed)); // partition_1_2
		//std::swap_ranges(x1.begin() + partition_1, x1.end() + partition_2, x1.begin() + partition_2);
		
		double fx1 = solution_Elf_c(myToys_c, myelves_c, x1);
		double delta = fx1 - fbest;
		if (delta < 0){
			std::list<int> la(x1.begin(), x1.end());
			la.sort();
			la.unique();
			if (la.size() == x1.size()){
				xbest = x1;
				fbest = fx1;
				//Rcpp::Rcout << '\n' << round(fbest) << ' ' << la.size() << ' ' << delta;
				return xbest;
			}
			else{
				return xbest;
			}
		}else return xbest;
}
