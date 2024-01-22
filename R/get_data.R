#' Functions For Getting Raw Data
#'
#' @param seasons int vector of season of which to select for.
#'
#' @return

get_game_data <- function(.seasons) {
  nflreadr::load_schedules(.seasons) %>%
    as_tibble() %>%
    fastr_fix_colnames() %>%
    fastr_fix_team_names()
}

get_game_team_data <- function(seasons) {
  nflreadr::load_schedules(seasons = seasons) %>%
    as_tibble() %>%
    # Pivot
    fastr_pivot_home_away() %>%
    mutate(
      location = case_when(
        location == "Neutral" ~ location,
        pattern_match == "^home_" ~ "Home",
        pattern_match == "^away_" ~ "Away"
      ),
      # Spread isn't fixed but moneyline is
      spread_line = if_else(pattern_match == "^home_", spread_line * -1, spread_line),
      id_home = pattern_match == "^home_"
    ) %>%
    select(-pattern_match) %>%
    # Fix Name Conventions
    fastr_fix_colnames() %>%
    # Remove names where IDs exist
    select(-c(qb_name,
              coach,
              stadium)) %>%
    # Removing non-relevant columns
    select(-c(overtime, gsis, id_old_game, id_nfl_detail, pfr, pff, espn)) %>%
    # Relocate for ease
    relocate(starts_with('id_')) %>%
    # clean
    fastr_fix_team_names() %>%
    rename(id_posteam = team) %>%
    fastr_derive_defteam()

}

get_data <- function(seasons) {
  # Load play-by-play data
  nflreadr::load_pbp(season = seasons) %>%
    # Convert to tibble
    tibble::as_tibble() %>%
    # Fix team names
    dplyr::mutate(dplyr::across(
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
    ) %>%
    fsubset(play_type != 'no_play') %>%
    fastr_fix_colnames() %>%
    fastr_fix_team_names()
}

get_dc <- function(SAFE_SEASONS, game_team_data) {
  # cleaning up the horrifically messy depth chart endpoint

  raw <- nflreadr::load_depth_charts(SAFE_SEASONS)
  clean <- vctrs::vec_slice(raw, raw$position %in% c('QB', 'WR', 'RB', 'TE'))
  clean <- clean %>%
    fastr_fix_colnames() %>%
    dplyr::select(
      id_season,
      id_posteam = club_code,
      game_type,
      id_week,
      id_gsis,
      position,
      full_name,
      rank = depth_team
    ) %>%
    fastr_fix_team_names(id_posteam) %>%
    tidyr::drop_na() %>%
    unique()

  # NOTES on Superbowl weeks:
  # when game_type == post, it's the Superbowl
  # this is across all seasons, not just newer
  # this week has to be row binded by itself, with the new week
  superbowls <- vctrs::vec_slice(clean, clean$game_type == 'POST')
  superbowls$id_week <- if_else(superbowls$id_week <= 2020, 21, 22)
  non_superbowls <- vctrs::vec_slice(clean, clean$game_type != 'POST')
  reordered_weeks <- vec_rbind(superbowls, non_superbowls)
  assert_same_sum(reordered_weeks, clean)

  # NOTES on end-of-season charts:
  # there are normal charts for the last week
  # then there are charts for the end of season
  # this is probably to allow teams resting starters to reset before post season
  # for our case, we have the ESPN charts for up-to-date starters
  # so we can get rid of the end-of-season charts
  SHORT_SEASON_EOS <- expr(!(id_season <= 2020 & id_week == 18))
  LONG_SEASONS_EOS <- expr(!(id_season >= 2021 & id_week == 19))
  eos_removed <- reordered_weeks %>%
    dplyr::filter(!!SHORT_SEASON_EOS, !!LONG_SEASONS_EOS)

  # now super bowls are re-arranged and eos are dropped
  # we can subtract 1 week from all postseason games
  subtracted_weeks <- eos_removed %>%
    dplyr::mutate(id_week = case_when(id_season <= 2020 & id_week >= 19 ~ id_week - 1,
                               id_season >= 2021 & id_week >= 20 ~ id_week - 1,
                               TRUE ~ id_week))
  expect_id_completion(subtracted_weeks)

  # NOTES on the duplicate player phenom:
  # it appears the depth charts natively come with duplicated players
  # most times the rank is also duplicated
  # we'll group slice for now
  deduplicated_players <- subtracted_weeks %>%
    dplyr::group_by(id_season, id_week, id_gsis) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()

  with_game_id <- deduplicated_players %>%
    dplyr::left_join(dplyr::select(game_team_data, id_game, id_week , id_season, id_posteam),
              by = c('id_season','id_posteam','id_week')) %>%
    # some bye weeks ??
    tidyr::drop_na(id_game) %>%
    dplyr::relocate(id_game)
  expect_id_completion(with_game_id)
  expect_resolved_suffix(with_game_id)

  if (2020 %in% SAFE_SEASONS) {
    no_qb_game <- tibble(
      id_game = '2020_13_DEN_KC',
      id_season = 2022,
      id_week = 13,
      id_posteam = 'DEN',
      rank = as.character(1),
      position = 'QB',
      full_name = 'unknown',
      id_gsis = 'MISSING'
    )
    with_game_id <- vec_rbind(with_game_id, no_qb_game)

  }

  assert_team_representation(with_game_id, warn = T)

  return(with_game_id)

}

get_players <- function(...) {
  nflreadr::load_players(...) %>%
    dplyr::filter(status != 'RET') %>%
    dplyr::rename(
      id_posteam = team_abbr,
      id_position = position
    ) %>%
    dplyr::mutate(display_name = nflreadr::clean_player_names(display_name)) %>%
    fastr_fix_colnames()
}

get_elo <- function(.seasons = seasons) {

  raw <- readr::read_csv('https://raw.githubusercontent.com/greerreNFL/nfeloqb/main/qb_elos.csv')

  data <- raw %>%
    # Filtering Season
    dplyr::filter(season %in% .seasons) %>%
    # Fixing team name
    fastr_fix_team_names(team1, team2) %>%
    # Pivot
    fastr_pivot_home_away("home_cols" = '1',
                    "away_cols" = '2') %>%
    dplyr::select(-pattern_match) %>%
    # Fixing Date
    dplyr::mutate(date = as.character(date)) %>%
    dplyr::rename(id_posteam = team) %>%
    fastr_fix_colnames()

  # Mini-Schedule data to turn the date into the game id
  date_lookup <- nflreadr::load_schedules() %>%
    fastr_fix_team_names(game_id, home_team, away_team) %>%
    dplyr::select(id_game = game_id,
           id_week= week ,
           home_team,
           away_team,
           date = gameday) %>%
    fastr_pivot_home_away() %>%
    dplyr::select(-pattern_match) %>%
    dplyr::rename(id_posteam = team) %>%
    tibble::as_tibble()

  # Convert date + team to game_id
  data <- data %>%
    dplyr::left_join(date_lookup) %>%
    dplyr::relocate(dplyr::starts_with('id_')) %>%
    dplyr::select(-date) %>%
    dplyr::rename_with( ~ glue("team_{.x}"), -c(dplyr::starts_with('id_'), dplyr::contains('qb')))

  # Calculating ELOA
  data <- data %>%
    dplyr::group_by(id_posteam) %>%
    dplyr::arrange(id_season, id_week, .by_group = T) %>%
    dplyr::mutate(team_eloa = dplyr::lead(team_elo_pre) - team_elo_pre) %>%
    dplyr::ungroup()

  # Some cols contain 'post' data
  # This is data after the game so it must be removed
  data <- data %>%
    dplyr::select(-dplyr::ends_with('_post')) %>%
    dplyr::rename_with(~ stringr::str_remove_all(.x, '_pre'), -dplyr::starts_with('id_'))

  return(data)

}

