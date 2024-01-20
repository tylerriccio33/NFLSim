
test_that("fastr_fix_team_names works", {
  test_data <- tibble::tibble(
    id_posteam = c("OAK", "SD"),
    id_defteam = c("OAK", "SD")
  )

  result <- fastr_fix_team_names(test_data)
  x_result <- tibble::tibble(
    id_posteam = c("LV", "LAC"),
    id_defteam = c("LV", "LAC")
  )
  expect_equal(result, x_result)

  # test passing dots
  result <- fastr_fix_team_names(test_data, id_posteam)
  x_result <- tibble::tibble(
    id_posteam = c("LV", "LAC"),
    id_defteam = c("OAK", "SD")
  )
  expect_equal(result, x_result)

})

test_that("fastr_fix_colnames works", {

  test_data <- tibble::tibble(posteam = 1,
                              foo_id = 1,
                              bar_id = 1,
                              foo = 1)

  result <- fastr_fix_colnames(test_data)
  x_result <- tibble::tibble(id_posteam = 1,
                             id_foo = 1,
                             id_bar = 1,
                             foo = 1)

  expect_equal(result, x_result)

})

test_that("fastr_derive_defteam works", {
  test_data <- tibble::tibble(
    id_game = c(1, 1, 2, 2),
    id_posteam = c('foo','bar','winston','duchess')
  )
  result <- fastr_derive_defteam(test_data)
  x_result <- tibble::tibble(
    id_game = c(1, 1, 2, 2),
    id_posteam = c('foo','bar','winston','duchess'),
    id_defteam = c('bar','foo','duchess','winston')
  )
  expect_equal(result, x_result)

})
