
test_dataclass <- data_assemble(.seasons = 2022:2023, cur_week = 20)

test_that("Game runs no error", {

  # Get Matchup:
  test_matchup <- test_dataclass$matchup_data %>% slice(1)


  result <- Game(dataclass = test_matchup)






  })

