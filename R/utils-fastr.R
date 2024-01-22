#' Reusable Functions Fixing Fastr Data
#'
#' @description
#' * `fastr_derive_defteam` assigns a defteam to each row. For example, if each row is a game-team combo, we can assign the defteam as the "other" team in the game group.
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

    data %>%
      mutate(across(c(...), ~ stringr::str_replace_all(
        .x,
        c(
          "JAC" = "JAX",
          "STL" = "LA",
          "SL" = "LA",
          "LAR" = "LA",
          "ARZ" = "ARI",
          "BLT" = "BAL",
          "CLV" = "CLE",
          "HST" = "HOU",
          "SD" = "LAC",
          "OAK" = "LV",
          "WSH" = "WAS"
        )
      )))
}

fastr_pivot_home_away <- function(.data,
                            home_cols = '^home_',
                            away_cols = '^away_') {

  # Collect Home Cols
  home_data <- .data %>%
    # Set team type
    dplyr::mutate(pattern_match = home_cols) %>%
    # Select non-away cols
    dplyr::select(!dplyr::matches(away_cols)) %>%
    # Scrub column names
    dplyr::rename_with(~ stringr::str_remove_all(.x, home_cols))

  # Collect Away Cols
  away_data <- .data %>%
    # Set team type
    dplyr::mutate(pattern_match = away_cols) %>%
    # Select away variables
    dplyr::select(!dplyr::matches(home_cols)) %>%
    # Scrub column names
    dplyr::rename_with(~ stringr::str_remove_all(.x, away_cols))

  # Bind Home and Away Data
  combined_data <- vec_rbind(home_data, away_data) %>%
    dplyr::relocate(pattern_match)


  return(combined_data)

}

fastr_derive_defteam <- function(data) {
  assert_cols(data, id_game, id_posteam)

  data %>%
    dplyr::group_by(id_game) %>%
    dplyr::arrange(id_game, .by_group = T) %>%
    mutate(id_defteam = case_when(
      id_posteam == unique(id_posteam)[1] ~ unique(id_posteam)[2],
      id_posteam == unique(id_posteam)[2] ~ unique(id_posteam)[1]
    )) %>%
    dplyr::ungroup()
}

fastr_rm_future_week <- function(data, cur_season, cur_week) {
  data %>%
    dplyr::filter(!(.data$id_season == cur_season & .data$id_week > cur_week))
}
