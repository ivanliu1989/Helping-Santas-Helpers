#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix solution_Elf(NumericMatrix myToys_1，NumericMatrix myToys_2,NumericMatrix myToys_3, NumericMatrix myelves){
	NumericMatrix outcomes(n_toys,4); //ToyId, Arrival_time, Duration, Size, r_duration, start_time, finish, rate_f, refresh

	for(int i=0; i<10000000; ++i){

		NumericVector next_elf; //search elf ********* elf_id, current_rating, next_available_time, score
		//min_element => for loop == i (row number)

		int c_elf_id = next_elf(0);
		int c_elf_start_time = next_elf(2);
        int c_elf_rating = next_elf(1);

        if(c_elf_rating == 4.0){
            NumericMatrix Toys = myToys_3; 
        }else if(c_elf_rating > 3){
            NumericMatrix Toys = myToys_2; 
        }else{
            NumericMatrix Toys = myToys_1; 
        }

        for(int j = 0; j < Toys.nrow(); j++){ 
        	Toys(j,4) = ceil(Toys(j,2)/c_elf_rating); //duration
            Toys(j,5) = max(c_elf_start_time, Toys(j, 1)); //start_time
            Toys(j,7) = updateProductivity(Toys(j,7), Toys(j,4), c_elf_rating); //rate_f
            Toys(j,8) = updateNextAvailableMinute(Toys(j,5), Toys(j,4); //refresh
            Toys(j,6) <- Toys(j,4)+ Toys(j,5); //finish
        }
        
        NumericeVector c_toy = Toys[order(-Toys[,'rate_f'],Toys[,'refresh'],Toys[,'finish']),][1,]; //numericeVector *********

        int c_toy_id = c_toy(1);
        int c_elf_start_time = c_toy(6); 
        int work_duration = c_toy(5);
        
		myelves(c_elf_id-1, 2) = c_toy(8); //next_available_time
        myelves(c_elf_id-1, 1) = c_toy(7); //current_rating

        outcomes(i,0) = c_toy_id;
        outcomes(i,1) = c_elf_id;
        outcomes(i,2) = c_elf_start_time;
        outcomes(i,3) = work_duration;

        myToys <- myToys[-c_toy_id,];

        if(i % 100000 == 0) Rcpp::Rcout << '\n' << i;
        return outcomes;
	}
}