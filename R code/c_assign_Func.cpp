#include <Rcpp.h>
#include <algorithm> // for std::sort
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix solution_Elf_submit_c(NumericMatrix myToys_c, NumericMatrix myelves_c){
    //int work_duration,c_toy_id,c_toy_arrival,c_toy_duration,c_elf_id,c_elf_start_time,schedule_index;
    //double c_elf_rating;
    //int n_toys = myToys_c.nrow();
    NumericMatrix outcomes(n_toys,4);
    
    for(int i = 0; i<10000000; ++i){
        schedule_index = schedule_c(current_toy) -1;
        c_toy_id = myToys_c(schedule_index,0);
        c_toy_arrival = myToys_c(schedule_index,1);
        c_toy_duration = myToys_c(schedule_index,2);
        
        c_elf_id = myelves_c(0);
        c_elf_start_time = myelves_c(2);
        c_elf_rating = myelves_c(1);
        
        if(c_elf_start_time < c_toy_arrival) c_elf_start_time = c_toy_arrival;
        work_duration = ceil(c_toy_duration/c_elf_rating);
        
        myelves_c(2) = updateNextAvailableMinute(c_elf_start_time, work_duration);
        myelves_c(1) = updateProductivity(c_elf_start_time, work_duration, c_elf_rating);
        
        outcomes(current_toy,0) = c_toy_id;
        outcomes(current_toy,1) = c_elf_id;
        outcomes(current_toy,2) = c_elf_start_time;
        outcomes(current_toy,3) = work_duration;
    }
    myelves_c(2) =540; 
    myelves_c(1) = 1;
    return outcomes;
}








int assignElf(NumericMatrix myelves, int c_toy_size){
	int nrow = x.nrow(), ncol = x.ncol();
	for(int i=0; i < nrow; i++){
		if(myelves[i][2]==c_toy_size)
		break;	
	}
	if(i==nrow) c_toy_size = 2;

	for(int i=0; i < nrow; i++){
		if(myelves[i][2]==c_toy_size)
		break;	
	}
	if(i==nrow) c_toy_size = 1;

	for(int i=0; i < nrow; i++){
		if(myelves[i][2]==c_toy_size)
		break;	
	}
	if(i==nrow) c_toy_size = 3;

	std::sort(&myelves[0][0], &myelves[0][0]+10*10);
}