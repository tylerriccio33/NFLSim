#include <Rcpp.h>

// [[Rcpp::export]]
Rcpp::NumericVector slice_cpp(Rcpp::NumericVector x, Rcpp::LogicalVector i) {
  // Subset based on the logical vector
  Rcpp::NumericVector result = x[i];

  return result;
}
