#' Derive Metrics from fastr Data
#'
#' @param data
#'
#' @return
#'
fastr_derive_fg_metrics <- function(data) {
  data %>%
    mutate(
      field_goal_blocked = dplyr::if_else(field_goal_result == "blocked", 1, 0),
      field_goal_missed = dplyr::if_else(field_goal_result == "missed", 1, 0),
      field_goal_made = dplyr::if_else(field_goal_result == "made", 1, 0),
      across(
        c(field_goal_blocked, field_goal_missed, field_goal_made),
        ~ dplyr::if_else(is.na(.x), 0, .x)
      )
    )


}
