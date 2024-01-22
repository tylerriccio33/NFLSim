


test_vector <- runif(n = 100000)
results <- bench::mark(
  dplyr = dplyr::between(x = test_vector, left = .25, right = .25),
  cpp = between_cpp_parallel(x = test_vector, left = .25, right = .25),
  check = T
)
