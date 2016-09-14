// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

using namespace Rcpp;

// [[Rcpp::export]]
List fastReg(NumericVector y, NumericMatrix x, NumericVector w){
	int n = x.nrow(), k = x.ncol();
	arma::mat X(x.begin(), n, k, false);
	arma::colvec Y(y.begin(), n, false);
	arma::colvec W = arma::sqrt(arma::vec(w.begin(), w.length(), false));
	X = arma::diagmat(W)*X;
	Y %= W;
	int df = n - k;
	arma::colvec coef = arma::solve(X, Y);
	arma::colvec res = Y - X*coef;
	double s2 = std::inner_product(res.begin(), res.end(), res.begin(), 0.0)/df;
	arma::mat vcov = s2*arma::pinv(X.t()*X);
	return List::create(Named("coef")=coef, Named("vcov")=vcov);
}
