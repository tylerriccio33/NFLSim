data_get <- function(seasons) {
  # Load play-by-play data
  nflreadr::load_pbp(season = seasons) %>%
    # Convert to tibble
    tibble::as_tibble() %>%
    # Fix team names
    dplyr::mutate(across(
      c(posteam, defteam, home_team, away_team),
      ~ dplyr::case_when(.x == 'OAK' ~ 'LV',
                  .x == 'SD' ~ 'LAC',
                  .x == 'STL' ~ 'LA',
                  TRUE ~ .x)
    )) %>%
    # Apply filters
    dplyr::filter(qtr < 5,
           two_point_attempt != 1) %>%
    # Filter for Hail Mary criteria
    dplyr::filter(
      !(
        half_seconds_remaining <= 10 &
          pass_attempt == 1 &
          pass_length == "deep"
      ),
      # Filter for last play nonsense criteria
      !(
        half_seconds_remaining <= 10 &
          (lateral_reception == 1 | lateral_rush == 1)
      )
    )
}
