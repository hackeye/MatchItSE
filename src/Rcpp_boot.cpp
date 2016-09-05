// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <RcppArmadilloExtensions/sample.h>

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector cppWeightedBootstrap(NumericVector Ys, IntegerVector ts, 
                                   NumericVector ws, int Ns, 
                                   int MaxIter = 1e4) {
	NumericVector out(2); 
	NumericVector Yd(MaxIter);
  IntegerVector candidates = seq_len(Ns) - 1;
	IntegerVector indices(Ns);
	int nt, nc; 
	float sumt, sumc; 
	for (int i = 0; i < MaxIter; i++){
		indices = RcppArmadillo::sample(candidates, Ns, TRUE, ws); 
		nt = 0; nc = 0; sumt = 0.0; sumc = 0.0; 
		for (int j = 0; j < Ns; j++){
			if (ts[indices[j]] == 1){
				sumt += Ys[indices[j]]; nt++; 
			} else {
				sumc += Ys[indices[j]]; nc++; 
			}
		}
		Yd[i] = (sumt/nt) - (sumc/nc); 
	}
	out[0] = mean(Yd); out[1] = sd(Yd); 
	return out; 
}
