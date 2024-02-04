
test_dataclass <- data_assemble(.seasons = 2022:2023, cur_week = 22)

test_that("Game runs no error", {

  result <- Game(matchup_dataclass = slice(test_dataclass, 1))

  bench <- bench::mark(
    Game(matchup_dataclass = slice(test_dataclass, 1)),
    iterations = 5
  )

  bench

  # Bench: 3.6 median

})




