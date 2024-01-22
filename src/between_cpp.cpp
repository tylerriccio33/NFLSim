#include <Rcpp.h>
#include <RcppParallel.h>
using namespace Rcpp;
using namespace RcppParallel;

// Define the worker class for parallelization
struct BetweenWorker : public Worker {
  const NumericVector x;
  double left, right;
  LogicalVector result;

  BetweenWorker(const NumericVector x, double left, double right, LogicalVector result)
    : x(x), left(left), right(right), result(result) {}

  void operator()(std::size_t begin, std::size_t end) {
    for (std::size_t i = begin; i < end; ++i) {
      result[i] = (x[i] >= left) & (x[i] <= right);
    }
  }
};

// [[Rcpp::export]]
LogicalVector between_cpp_parallel(const NumericVector& x, double left, double right) {
  int n = x.size();
  LogicalVector result(n);

  // Create worker and parallelize the loop
  BetweenWorker worker(x, left, right, result);
  parallelFor(0, n, worker);

  return result;
}
