#include <algorithm>    // std::max
#include <Rcpp.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <vector>
#include <algorithm>
#include <time.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector assignX1(NumericVector x1, double p, int np, int toy_row, int num, double Ns, double Np){
    if(p<=0.5){
        int partition_1, partition_2, length, j;
        NumericVector rep_range;
		partition_1 = ceil(((np - 1) / num)*toy_row + 1);
		partition_2 = ceil((np / num)*toy_row);
        partition_1 = std::max(partition_1, 1);
		partition_2 = std::min(partition_2, toy_row);
        length = partition_2-partition_1;
        srand(time(0));
        for(int i =0; i<=length; ++i){
            j = rand()%length;
            std::swap(x1(i), x1(j));
            
        }; 

		return partition_1;
	}
	else{

	}
}