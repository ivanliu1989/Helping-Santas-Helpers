// [[Rcpp::plugins(cpp11)]]
#include <unordered_set>
#include <iostream>     // std::cout
#include <Rcpp.h>
#include <algorithm>    // std::swap_ranges std::shuffle
#include <vector>       // std::vector
#include <time.h>
#include <random>       // std::default_random_engine
#include <chrono>       // std::chrono::system_clock
#include <list>
using namespace Rcpp;

// [[Rcpp::export]]
std::vector<int> assignX1(std::vector<int> x1, int np, int toy_row, int num, int p1, int p2){ //, NumericMatrix myToys_c, NumericMatrix, myelves_c, double fbest
  
		unsigned seed = std::chrono::system_clock::now().time_since_epoch().count();
		shuffle(x1.begin() + p1, x1.begin() + p2, std::default_random_engine(seed)); // partition_1_2
		//std::swap_ranges(x1.begin() + partition_1, x1.end() + partition_2, x1.begin() + partition_2);
		
		//double fx1 = solution_Elf_c(myToys_c, myelves_c, x1);
		//double delta = fx1 - fbest;
		//if (delta < 0){
		std::list<int> la(x1.begin(), x1.end());
		la.sort();
		la.unique();
		Rcpp::Rcout << la.size() << std::endl;
		if (la.size() == x1.size()){
			//xbest = x1;
			//fbest = f1;
			Rcpp::Rcout << '\n +++++ Find Improvement:' << 'delta' <<'!!! Current Score:' << fbest;
		}
		else{
			Rcpp::Rcout << '\n ----- Error happened during scheduling!!! Toy Number:' << ls.size() << 'Unique Tasks:' << x1.size();
			break;
		}
		//}
		return x1;
}

