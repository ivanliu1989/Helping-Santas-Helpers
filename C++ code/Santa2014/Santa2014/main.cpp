#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix solution_Elf(NumericMatrix myToys， NumericMatrix myelves){
	NumericMatrix outcomes(n_toys,4);
	for(int i=0; i<10000000; ++i){
		NumericVector next_elf; //search elf
		int c_elf_id = next_elf(0);
		int c_elf_start_time = next_elf(2);
        int c_elf_rating = next_elf(1);

        if(c_elf_rating == 4.0){
            //myToys = myToys[which(myToys[,'Size']==3),]
        }else if(c_elf_rating > 3){
            //myToys <- myToys[which(myToys[,'Size']==2),]
        }else{
            //myToys <- myToys[which(myToys[,'Size']==1),]
        }

        for(int j = 0; j < myToys.nrow(); j++){ //matrix
        	myToys(j,'r_duration') = ceil(myToys(j,'Duration')/c_elf_rating);
            myToys(j,'start_time') = max(c_elf_start_time, myToys(j, 'Arrival_time'));
            myToys(j,'rate_f') = updateProductivity(myToys(j,'rate_f'), myToys(j,'r_duration'), c_elf_rating);
            myToys(j,'refresh') = updateNextAvailableMinute(myToys(j,'start_time'), myToys(j,'r_duration');
            myToys(j,'finish') <- myToys(j,'r_duration')+ myToys(j,'start_time');
        }
        
        NumericeVector c_toy = myToys[order(-myToys[,'rate_f'],myToys[,'refresh'],myToys[,'finish']),][1,]; //numericeVector
        int c_toy_id = c_toy(1);
        int c_elf_start_time = c_toy(6); 
        int work_duration = c_toy(5);
        
		myelves(c_elf_id-1, 'next_available_time') = myToys(c_toy_id,'refresh');
        myelves(c_elf_id-1, 'current_rating') = myToys(c_toy_id,'rate_f');

        outcomes(i,0) = c_toy_id;
        outcomes(i,1) = c_elf_id;
        outcomes(i,2) = c_elf_start_time;
        outcomes(i,3) = work_duration;

        myToys <- myToys[-c_toy_id,];

        if(i % 100000 == 0) Rcpp::Rcout << '\n' << i;
        return outcomes;
	}
}