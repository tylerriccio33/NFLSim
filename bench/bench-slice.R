
n <- 100000
test_vec <- runif(n = n)
test_i <- sample(x = c(T, F), size = n, replace = T)
results <- bench::mark(
  vctrs = vctrs::vec_slice(test_vec, test_i),
  cpp = slice_cpp(test_vec, test_i)
)


results
