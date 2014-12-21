#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix solution_Elf(NumericMatrix myToys_1，NumericMatrix myToys_2,NumericMatrix myToys_3, NumericMatrix myelves){
	NumericMatrix outcomes(n_toys,4); //ToyId, Arrival_time, Duration, Size, r_duration, start_time, finish, rate_f, refresh, evaluate

	for(int i=0; i<10000000; ++i){

		int min_val = myelves(0,2);
		int min_row = 0;
		for(int e=0; e<myelves.nrow(); e++){
			if (min_val > myelves(e,2){
				min_val = myelves(e,2);
				min_row = e;
			} 	
		}
		//elf_id, current_rating, next_available_time, score
		//min_element => for loop == i (row number)

		int c_elf_id = myelves(e,0);
		int c_elf_start_time = myelves(e,2);
        int c_elf_rating = myelves(e,1);

        if(c_elf_rating == 4.0){
            NumericMatrix Toys = myToys_3; 
        }else if(c_elf_rating > 3){
            NumericMatrix Toys = myToys_2; 
        }else{
            NumericMatrix Toys = myToys_1; 
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
            Toys(j,5) = max(c_elf_start_time, Toys(j, 1)); //start_time
            Toys(j,7) = updateProductivity(Toys(j,7), Toys(j,4), c_elf_rating); //rate_f
            Toys(j,8) = updateNextAvailableMinute(Toys(j,5), Toys(j,4); //refresh
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

        for(int k = 0; k < Toys.nrow(); k++){ 
        	Toys(k, 9) = ((Toys(k, 7) - min_rate)/(max_rate - min_rate) + (Toys(k, 8) - min_refresh)/(max_refresh - min_refresh) + (Toys(k, 6) - min_finish)/(max_finish - min_finish))/3;
        }
        
        double min_val = myelves(0,9);
		int min_val_row = 0;
		for(int v=0; v<Toys.nrow(); v++){
			if (min_val > Toys(e,9){
				min_val = Toys(e,9);
				min_row = v;
			} 	
		}
        // (e - Emin)/(Emax - Emin)

        int c_toy_id = Toys(min_row,0);
        int c_elf_start_time = Toys(min_row,5); 
        int work_duration = Toys(min_row,4);
        
		myelves(c_elf_id-1, 2) = Toys(min_row,8); //next_available_time
        myelves(c_elf_id-1, 1) = Toys(min_row,7); //current_rating

        outcomes(i,0) = c_toy_id;
        outcomes(i,1) = c_elf_id;
        outcomes(i,2) = c_elf_start_time;
        outcomes(i,3) = work_duration;

        //myToys <- myToys[-c_toy_id,]; // #################

        if(i % 100000 == 0) Rcpp::Rcout << '\n' << i;
        return outcomes;
	}
}