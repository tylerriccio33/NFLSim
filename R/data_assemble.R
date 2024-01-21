
# TODO: derive cur_week from something down here
# - have to allow for back testing too
data_assemble <- function(.seasons = 2021:2023, cur_week = 20) {

  cur_season <- max(.seasons)

  # PBP Data #
  pbp_data <- get_data(seasons = .seasons) %>%
    add_time_variables() %>%
    fsubset(total_play_time > 0) %>%
    fastr_derive_fg_metrics()


  # Game and Team Data:
  game_data <- get_game_data(.seasons)
  game_team_data <- get_game_team_data(seasons = .seasons) %>%
    # remove current week:
    fastr_rm_future_week(cur_season = cur_season, cur_week = cur_week) %>%
    calculate_weeks_from_first_group_row()

  # Pluck Future Games:
  future_games <-
    pluck_future_games(game_team_data, cur_week = cur_week,
                       cur_season = cur_season) %>%
    append_dc_to_future_games()
  assert("No future games" = nrow(future_games) != 0)
  cur_gameday <- dplyr::slice_min(future_games, gameday, n = 1, with_ties = F)$gameday

  # Participation Data:
  # TODO: ? utility of raw participation data
  raw_participation_data <-
    collect_snap_pct(.seasons, summarize = F)
  summarized_participation_data <- collect_snap_pct(.seasons, summarize = T)

  # Depth Chart:
  raw_depth_charts <- get_dc(SAFE_SEASONS = .seasons, game_team_data = game_team_data)
  ordered_depth_charts <-
    reorder_depth_chart(depth_chart = raw_depth_charts,
                        participation_data = summarized_participation_data) %>%
    bind_future_dc(future_games = future_games)

  # Calculate Time Desirability:
  distance_lookup <- calculate_time_distance(cur_gameday = cur_gameday,
                                             game_team_data = game_team_data)

  # Create Matchup Data:
  matchup_data <-
    prep_matchup(game_data = game_data,
                 cur_season = cur_season,
                 cur_week = cur_week) %>%
    append_future_dc(future_games = future_games) %>%
    append_roster_relevance(ordered_depth_charts = ordered_depth_charts)

  # Slice Sample Data:
  sample_data <- slice_play_samples(raw_data = pbp_data) %>%
    append_dc_ranks(ordered_depth_charts) %>%
    left_join(distance_lookup,
              relationship = 'many-to-one')

  # Append Samples:
  matchup_data <-
    append_samples_to_matchup(matchup_data = matchup_data, samples = sample_data)

  # Return:
  return(matchup_ata)

}


if (F) {

  dt <- data_assemble()




  dt

  # mutate(time_distance = map(
  #   gameday,
  #   \(x) calculate_time_distance(cur_gameday = x, game_team_data)
  # )) %>%


# TODO: do you need this
# elo_data <- nflModeler::get_elo(.season = SEASONS) %>%
#   select(id_game, id_posteam, qb_value, qb_adj, qb) %>%
#   left_join(
#     nflreadr::load_players() %>%
#       filter(position == 'QB') %>%
#       select(qb = display_name, id_passer = gsis_id) %>%
#       unique()
#   ) %>%
#   select(-qb)


}





