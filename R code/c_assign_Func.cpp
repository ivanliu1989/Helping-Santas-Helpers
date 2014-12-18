#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
cNumericVector rowSumsC(NumericMatrix x) {
	int nrow = x.nrow(), ncol = x.ncol();
	NumericVector out(nrow);

	for (int i = 0; i < nrow; i++) {
		double total = 0;
		for (int j = 0; j < ncol; j++) {
			total += x(i, j);
		}
		out[i] = total;
	}
	return out;
}

assign_elf <- function(myelves, c_toy_size) {
	if(sum(myelves[,'score']==c_toy_size)<1) c_toy_size <- 2
	if(sum(myelves[,'score']==c_toy_size)<1) c_toy_size <- 1
	if(sum(myelves[,'score']==c_toy_size)<1) c_toy_size <- 3
	myelves <- myelves[which(myelves[,'score']==c_toy_size),]
	if(length(myelves)==4){
		assigned_elf <- as.integer(myelves['elf_id'])
	}else{
		assigned_elf <-as.integer(myelves[which.min(myelves[,'next_available_time']) ,'elf_id'][1])
	}
		return(assigned_elf)
	}

int assignElf(NumericMatrix myelves, int c_toy_size){
	int nrow = x.nrow(), ncol = x.ncol();
	for(int i=0; i < nrow; i++){
		
	}
}