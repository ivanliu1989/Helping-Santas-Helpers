#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
int rowWithMax1s(bool mat[R][C])
{
    // Initialize first row as row with max 1s
    int max_row_index = 0;
 
    // The function first() returns index of first 1 in row 0.
    // Use this index to initialize the index of leftmost 1 seen so far
    int j = first(mat[0], 0, C-1) - 1;
    if (j == -1) // if 1 is not present in first row
      j = C - 1;
 
    for (int i = 1; i < R; i++)
    {
        // Move left until a 0 is found
        while (j >= 0 && mat[i][j] == 1)
        {
           j = j-1;  // Update the index of leftmost 1 seen so far
           max_row_index = i;  // Update max_row_index
        }
    }
    return max_row_index;
}

// [[Rcpp::export]]
NumericMatrix solution_Elf(NumericMatrix myToys， NumericMatrix myelves){
	NumericMatrix outcomes(n_toys,4);
	for(int i=0; i<10000000; ++i){

		NumericVector next_elf; //search elf *********

		int c_elf_id = next_elf(0);
		int c_elf_start_time = next_elf(2);
        int c_elf_rating = next_elf(1);

        if(c_elf_rating == 4.0){
            myToys = myToys[which(myToys[,'Size']==3),] //*********
        }else if(c_elf_rating > 3){
            myToys <- myToys[which(myToys[,'Size']==2),] //*********
        }else{
            myToys <- myToys[which(myToys[,'Size']==1),] //*********
        }

        for(int j = 0; j < myToys.nrow(); j++){ 
        	myToys(j,4) = ceil(myToys(j,2)/c_elf_rating); //duration
            myToys(j,5) = max(c_elf_start_time, myToys(j, 1)); //start_time
            myToys(j,7) = updateProductivity(myToys(j,7), myToys(j,4), c_elf_rating); //rate_f
            myToys(j,8) = updateNextAvailableMinute(myToys(j,5), myToys(j,4); //refresh
            myToys(j,6) <- myToys(j,4)+ myToys(j,5); //finish
        }
        
        NumericeVector c_toy = myToys[order(-myToys[,'rate_f'],myToys[,'refresh'],myToys[,'finish']),][1,]; //numericeVector *********

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