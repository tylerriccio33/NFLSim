#' Fix nflfastR Data
#'
#' @param data
#' @param ... For `fastr_fix_columns`, extra columns passed to the renaming function.
#'
#' @return Fixed data.
#'
fastr_fix_colnames <- function(data, ...) {
  rm_id <-
    function(x)
      stringr::str_remove_all(string = x, pattern = "_id$")
  add_id <- function(x)
    vctrs:::vec_paste0("id_", x)

  dots <- rlang::enexprs(...)
  if (length(dots) != 0) {
    cols <- dots
  } else {
    ends_with_id_cols <-
      purrr::keep(colnames(data),
                  \(x) stringr::str_detect(string = x,
                                            pattern = "^id_"))
    cols <- expr(any_of(
      c(
        "week",
        "season",
        "posteam",
        "defteam",
        "home_team",
        "away_team"
      )
    ))


    # cols <- purrr::discard(cols, rlang::is_empty)

  }

  data <- data %>%
    dplyr::rename_with(.fn = rm_id, .cols = dplyr::matches('id_$')) %>%
    dplyr::rename_with(.fn = add_id, .cols = !!cols)

  return(data)

}



fastr_fix_team_names <- function(data, ...) {
  name_map <- c('OAK' = 'LV',
                'SD' = 'LAC',
                'STL' = 'LA')

  dots <- rlang::enexprs(...)
  if (length(dots) != 0) {
    cols <- dots
  } else {
    cols <-
      exprs(any_of(
        c(
          "id_posteam",
          "id_defteam",
          "id_home_team",
          "id_away_team"
        )
      ))
  }
  data <-
    mutate(data, across(!!!cols, \(x) stringr::str_replace_all(string = x, name_map)))
  return(data)
}
