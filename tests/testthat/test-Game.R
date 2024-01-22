
test_dataclass <- data_assemble(.seasons = 2022:2023, cur_week = 20)

test_that("Game runs no error", {


  result <- Game(matchup_dataclass = test_dataclass)


}




