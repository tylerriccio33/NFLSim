#' Fix nflfastR Data
#'
#' @param data
#' @param ... For `fastr_fix_columns`, extra columns passed to the renaming function.
#'
#' @return Fixed data.
#'
fastr_fix_colnames <- function(data, ...) {
  # Define Helpers:
  # - replace _id with __temp to ensure no column name collisions
  # - add_id will also remove the temp
  rm_id <-
    function(x) {
      stringr::str_replace_all(string = x, pattern = "_id$", replacement = "__temp")
    }
  add_id <- function(x) {
    x <- stringr::str_remove_all(x, "__temp$")
    vctrs:::vec_paste0("id_", x)

  }

  data_colnames <- colnames(data)

  # Log Cols Needing Change:
  ends_with_id_cols <- data_colnames[grep("_id$", data_colnames)]
  ends_with_id_cols_without_id <- rm_id(ends_with_id_cols)
  hard_coded_cols <- c("week",
                       "season",
                       "posteam",
                       "defteam",
                       "home_team",
                       "away_team")
  add_id_to_cols <- vec_c(ends_with_id_cols_without_id, hard_coded_cols)

  # Make Changes:
  data <- data %>%
    # remove id from cols ending in id
    dplyr::rename_with(.fn = rm_id, .cols = all_of(ends_with_id_cols)) %>%
    # add id to hard coded ones and the ones where the id was removed
    dplyr::rename_with(.fn = add_id, .cols = any_of(add_id_to_cols))

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
