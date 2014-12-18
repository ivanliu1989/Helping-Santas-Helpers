#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
bool isSanctionedTime(int minute) {
	return ((minute - 540) % (60*24)) < 600;
}

NumericVector getSanctionedBreakdown(int startMinute, int duration) {
	NumericVector out(2);
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
	out[0] = S; out[1] = U;
	return out;
}

int nextSanctionedMinute(int minute) {
	bool isSanctionedTime = ((minute - 540) % (60*24)) < 600;
	bool isSanctionedTime_1 = ((minute - 539) % (60*24)) < 600;
	int next_min = 0;
	if(isSanctionedTime && isSanctionedTime_1) next_min = minute + 1;
	else {
		int num_days = minute / (60*24);
		int am_or_pm = (minute % (60*24))/ 540;
		next_min = 540 + (num_days + am_or_pm / 2) * (60*24);
	}
	return next_min;
}

int applyRestingPeriod(int current, int unsanctioned) {
	if (unsanctioned == 0) {
		bool isSanctionedTime = ((current - 540) % (60*24)) < 600;
		if (isSanctionedTime) return current;
		else{
			bool isSanctionedTime = ((current - 540) % (60*24)) < 600;
			bool isSanctionedTime_1 = ((current - 539) % (60*24)) < 600;
			int next_min = 0;
			if(isSanctionedTime && isSanctionedTime_1) next_min = current + 1;
			else {
				int num_days = current / (60*24);
				int am_or_pm = (current % (60*24))/ 540;
				next_min = 540 + (num_days + am_or_pm / 2) * (60*24);
			}
			return next_min;
		}
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
	if (local_start + rest_time_remaining_minutes >= 1140) {
		num_days_since_jan1 += 1;
		rest_time_remaining_minutes -= (1140 - local_start);
		local_start = 540;
	}
	int total_days = num_days_since_jan1 + rest_time_in_working_days;
	return total_days * (60*24) + local_start + rest_time_remaining_minutes;
}
