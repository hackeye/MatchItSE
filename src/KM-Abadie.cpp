#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
double cppKMsqSum(CharacterMatrix m, CharacterVector c, NumericVector w){
	int N = m.nrow(); 
	int P = m.ncol(); 
	int K = c.length();
	NumericVector km(K); 
	for (int k = 0; k < K; k++){ // geht mit der std vermutlich besser
		for (int n = 0; n < N; n++){
			for (int p = 0; p < P; p++){
				if (m(n,p) == c(k)){
					km(k) += w(n); 
				}
			}
		}
		km(k) *= km(k);
	}
	return sum(km); 
}
