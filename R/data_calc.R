calculate_weeks_from_first_group_row <- function(game_team_data) {

  game_team_data %>%
    group_by(id_posteam) %>%
    arrange(-id_season, -id_week, .by_group = T) %>%
    mutate(weeks_from_current = row_number() - 1) %>%
    ungroup()

}


calculate_total_roster_relevance <- function(cur_dc, ordered_dc) {

  # TODO: this can probably be dramatically sped up

  # Why not current play roster and current game? :
  # the current play rosters will reward light personnel
  # importance varies by play (lineman on 3rd&1 and receivers on 3rd&15)

  # Plan:
  # use the ordered dc to measure similarity between games
  # use some positional importance + rank here
  # this will smooth over some receiver bias on a per play basis

  # Why QB is removed from roster relevance
  # QB is so determinant of outcomes, they should be filtered directly
  # the situation sampler will explicitly look for the QB plays
  # if the QB is 100% new, we'll rely on roster similarity
  # if the QB is not new, roster similarity will only help guide the sampler

  ordered_dc %>%
    dplyr::left_join(dplyr::select(cur_dc,
                                   cur_pos = pos_abb,
                                   cur_rank = pos_rank,
                                   id_gsis),
                     by = 'id_gsis') %>%
    tidyr::drop_na(cur_pos) %>%
    # rank positional importance
    dplyr::filter(cur_pos != 'QB') %>%
    dplyr::mutate(pos_importance = dplyr::case_match(cur_pos,
                                              "QB" ~ 5,
                                              "WR" ~ 4,
                                              "RB" ~ 3,
                                              "TE" ~ 2,
                                              .default = 1)) %>%
    # multiply by existing rank
    dplyr::mutate(importance = cur_rank * pos_importance) %>%
    # sum each play total importance
    # the higher the number, the more common important players
    dplyr::group_by(id_game, id_posteam) %>%
    dplyr::summarize(roster_relevance = sum(importance), .groups = 'drop')

}


calculate_time_distance <- function(cur_gameday, game_team_data) {
  assert_cols(game_team_data, gameday, id_game)

  assert(
    "cur_gameday should be character yyyy-mm-dd" = is.character(cur_gameday),
    "game_team_data$gameday should be character yyyy-mm-dd" = is.character(game_team_data$gameday)
  )

  game_team_data$gameday <- lubridate::ymd(game_team_data$gameday)

  cur_gameday <- lubridate::ymd(cur_gameday)

  distances <- cur_gameday - game_team_data$gameday

  lookup <- select(game_team_data, id_game)
  lookup$time_distance <- as.numeric(distances)
  # rescale to higher numbers as closer
  lookup$weighted_time_distance <- scales::rescale(-lookup$time_distance)

  if (any(vec_detect_missing(lookup$time_distance))) {
    cli::cli_alert_danger("Some values failed to pass the time distance converter.")
  }

  lookup <- unique(lookup)

  return(lookup)


}


calculate_game_desirability <- function(matchup_data,
                                        home_safety_qb_samples,
                                        away_safety_qb_samples) {
  # find home team and away team desirability

  assert_cols(matchup_data,
              home_roster_relevances,
              away_roster_relevances,
              # id_home_team,
              # id_away_team,
              time_distance)

  time_distance <- matchup_data$time_distance[[1]]
  home_team <- matchup_data$id_home_team[[1]]
  away_team <- matchup_data$id_away_team[[1]]

  # home teams
  home_master_table <- list(
    mutate(time_distance, id_posteam = home_team),
    matchup_data$home_roster_relevances[[1]]
  )

  # away teams
  away_master_table <- list(
    mutate(time_distance, id_posteam = away_team),
    matchup_data$home_roster_relevances[[1]]
  )

  tables <- list(home_master_table,
                 away_master_table) %>%
    set_names(c('home','away')) %>%
    map( ~ {
      .x %>%
        reduce(full_join, by = c('id_game', 'id_posteam')) %>%
        # fill in time distance for games
        group_by(id_game) %>%
        fill(time_distance, .direction = "downup") %>%
        ungroup() %>%
        # fill in relevance as 0 if missing
        replace_na(list(roster_relevance = 0)) %>%
        # some duplicate rows post filling in
        unique()

    })

  tables$home <- tables$home[[1]] %>%
    mutate(roster_relevance = case_when(id_game ))


  return(tables$home)

  return(tables)


  # calculate desirability
  mutate(d_time = desirability2::d_min(time_distance,
                                       low = min(time_distance),
                                       high = max(time_distance)),
         d_roster = desirability2::d_max(roster_relevance,
                                         low = min(roster_relevance),
                                         high = max(roster_relevance)),
         d_both = desirability2::d_overall(d_time, d_roster))



}

