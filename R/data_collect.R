

collect_snap_pct <- function(SAFE_SEASONS, summarize = T) {
  data <-
    nflreadr::load_participation((max(SAFE_SEASONS) - 2):max(SAFE_SEASONS), include_pbp = F) %>%
    as_tibble() %>%
    fastr_fix_colnames() %>%
    # filter(id_play %in% sample_data$id_play) %>%
    select(id_game = id_nflverse_game,
           id_posteam = possession_team,
           id_play,
           offense_players) %>%
    tidyr::separate_rows(offense_players, sep = ";") %>%
    rename(id_gsis = offense_players) %>%
    mutate(id_gsis = dplyr::na_if(id_gsis, '')) %>%
    drop_na()
  if (summarize) {
    data <- data  %>%
      group_by(id_game, id_posteam) %>%
      mutate(plays = dplyr::n_distinct(id_play)) %>%
      ungroup() %>%
      group_by(id_game, id_posteam, id_gsis) %>%
      dplyr::summarize(
        snaps = dplyr::n(),
        snap_pct = snaps / dplyr::first(plays),
        .groups = 'drop'
      )
  }
  expect_id_completion(data)

  return(data)
}


collect_safety_qb_samples <- function(sample_data, elo_data, qb_id) {

  assert("Qb does not exist" = length(qb_id) != 0)

  # lookup qb in elo data
  cur_elo <- dplyr::filter(elo_data,
                    .data$id_passer == qb_id) %>%
    dplyr::arrange(desc(id_game)) %>%
    dplyr::slice(1) %>%
    dplyr::pull(qb_value)

  # find 2 similar qbs
  similar_qbs <- elo_data %>%
    dplyr::filter(.data$id_passer != qb_id) %>%
    dplyr::mutate(elo_dist = abs(cur_elo - .data$qb_value)) %>%
    dplyr::slice_min(elo_dist, n = 2) %>%
    dplyr::pull(id_passer)
  # slice qb samples
  sample_group_one <- pre_allocate_pass_samples(sample_data, similar_qbs[1])
  sample_group_two <- pre_allocate_pass_samples(sample_data, similar_qbs[2])
  all_new_samples <- vec_rbind(sample_group_one, sample_group_two)

  return(all_new_samples)

}

collect_espn_dc <- function(season, posteam, espn_team_name_tibble) {
  # use a posteam to query the most recent dc

  # espn/nfl/nflverse team name inconsistency conversion
  if(!posteam %in% espn_team_name_tibble$team_abb) {
    team_name_lookup <- fix_team_names(espn_team_name_tibble, team_abb) %>%
      select(nfl_team = team_abb) %>%
      bind_cols(espn_team_name_tibble)
    posteam <- vctrs::vec_slice(team_name_lookup, team_name_lookup$nfl_team == posteam)
    posteam <- pull(posteam, team_abb)
  }

  cur_dc <-
    espnscrapeR::get_depth_chart(season = season, team = posteam) %>%
    # filter down needs
    filter(pos_abb %in% c('WR', 'QB', 'TE', 'RB')) %>%
    filter(
      !(pos_abb == 'QB' &
          pos_rank != 1),
      !(pos_abb == 'TE' & pos_rank > 2),
      !(pos_abb == 'WR' &
          pos_rank > 3),
      !(pos_abb == 'RB' & pos_rank > 2)
    ) %>%
    # collect athlete information
    mutate(player_full_name = map(athlete_id, ~ get_athlete_safely(.x))) %>%
    select(athlete_id, pos_abb, pos_rank, player_full_name) %>%
    # Clean ID tibble
    mutate(player_full_name = nflreadr::clean_player_names(as.character(player_full_name)))

  return(cur_dc)

}


