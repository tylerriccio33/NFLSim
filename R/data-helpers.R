#' Post Data Collection Helpers and Wranglers
#'
#' @description
#' * `reorder_depth_chart` takes the raw depth charts and summarized participation data to reorder the charting ranks. The raw depth charts are from NFL data exchange, which can be wrong often. It's often a better representation of depth rank to go on participation. So here, we'll use the player's particitpation percentage to determine the rank. For example, the player with the most participation in the game will get the rank of 1, and so on.
#' * `pluck_future_games` takes game team data and filters to unplayed records.
#' * `append_samples_to_matchup` appends the home/away samples to each matchup record.
#'
#' @param depth_chart Raw depth chart data.
#' @param participation_data Summarized participation data.
#' @param game_team_data Data where each row is a game-team combination.
#' @param cur_week The current week of target games.
#' @param cur_season Int indicating the current season.
#'
#' @return
#' * `reorder_depth_chart` returns the original depth chart data with the fixed ranks.
reorder_depth_chart <- function(depth_chart, participation_data) {
  # join game participation data to re-order ranks
  # WARNING: this includes posterior participation data
  assert_cols(participation_data, snap_pct)

  depth_chart <- mutate(depth_chart, rank = as.integer(rank))

  # slice top QB
  qb_slices <- vctrs::vec_slice(depth_chart, depth_chart$position == 'QB')
  qb_slices <- qb_slices %>%
    dplyr::group_by(id_game,id_posteam) %>%
    dplyr::arrange(id_posteam, rank, .by_group = T) %>%
    dplyr::slice(1)

  # reorder WR by snap
  wr_reordered <- vctrs::vec_slice(depth_chart, depth_chart$position == 'WR')

  wr_reordered <- wr_reordered %>%
    dplyr::left_join(participation_data) %>%
    dplyr::group_by(id_game, id_posteam, position) %>%
    dplyr::arrange(-snap_pct, .by_group = T) %>%
    dplyr::mutate(rank = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::select(-c(snaps, snap_pct))
  expect_resolved_suffix(wr_reordered)
  wr_reordered <- vctrs::vec_slice(wr_reordered, wr_reordered$rank <= 3)

  # slice top TE
  te_slices <- vctrs::vec_slice(depth_chart, depth_chart$position == 'TE')
  te_slices <- vctrs::vec_slice(te_slices, te_slices$rank <= 2)

  # slice top RB
  rb_slices <- vctrs::vec_slice(depth_chart, depth_chart$position == 'RB')
  rb_slices <- vctrs::vec_slice(rb_slices, rb_slices$rank <= 2)

  # Bind all
  ordered_dc <- vec_rbind(qb_slices, wr_reordered, te_slices, rb_slices) %>%
    dplyr::arrange(id_season, id_week, id_posteam, position, rank) %>%

    # final rank clean up
    dplyr::group_by(id_game, id_posteam, position, rank) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()

  # assert
  expect_id_completion(ordered_dc)
  assert_completion(ordered_dc, rank)
  return(ordered_dc)

}

bind_future_dc <- function(depth_chart, future_games) {
  ordered_dc <-
    # bind future game dc to past ordered
    vec_rbind(
      future_games %>%
        dplyr::select(id_season, id_week, id_posteam_game = id_posteam, id_game, x_dc) %>%
        tidyr::unnest(x_dc) %>%
        dplyr::relocate(dplyr::starts_with('id_')) %>%
        dplyr::select(-c(athlete_id, id_posteam)) %>%
        dplyr::rename(id_posteam = id_posteam_game) %>%
        dplyr::rename(
          position = pos_abb,
          rank = pos_rank,
          full_name = player_full_name
        )
    )
  assert_team_representation(ordered_dc)
  assert_completion(ordered_dc, rank)
  return(ordered_dc)

}

pluck_future_games <-
  function(game_team_data, cur_week, cur_season) {
    future_games <- game_team_data %>%
      filter(is.na(score),
             id_week == cur_week,
             id_season == cur_season)
    return(future_games)
  }

append_dc_to_future_games <- function(future_games) {

  # TODO: this data needs to be selected down
  all_players <- get_players()


  future_games %>%
    mutate(x_dc = map2(
      id_posteam,
      id_season,
      ~ collect_espn_dc(
        season = .y,
        posteam = .x,
        espnscrapeR::get_nfl_teams()
      ),
      .progress = T
    )) %>%
    mutate(x_dc = pmap(
      list(id_posteam, id_season, x_dc),
      ~ join_espn_team_dc(
        posteam = ..1,
        season = ..2,
        cur_espn_dc = ..3,
        all_players = all_players
      ),
      .progress = T
    ))

}

slice_play_samples <- function(raw_data) {
  sample_data <- raw_data %>%
    drop_na(down, ydstogo, yardline_100, wp) %>%
    dplyr::filter(.data$play_type %in% c('run', 'pass', 'punt', 'field_goal')) %>%
    # filter weird situations (for now)
    dplyr::filter(!(.data$interception == 1 & .data$fumble == 1),
                  .data$safety == 0)
  expect_resolved_suffix(sample_data)
  assert_no_duplicates(sample_data, id_play, id_game)
  assert_completion(sample_data, total_play_time)
  return(sample_data)
}


append_samples_to_matchup <- function(matchup_data, sample_data) {

  clean_samples <- select(sample_data, -any_of('id_play'))

  # Collect QBs:
  matchup_data$home_qb_list <-
    map(matchup_data$id_home_team_dc, \(x) collect_qb(x))
  matchup_data$away_qb_list <-
    map(matchup_data$id_away_team_dc, \(x) collect_qb(x))
  matchup_data$home_qb_gsis <-
    map_chr(matchup_data$home_qb_list, \(x) ifelse(is.null(x$gsis), na_chr, x$gsis))
  matchup_data$away_qb_gsis <-
    map_chr(matchup_data$away_qb_list, \(x) ifelse(is.null(x$gsis), na_chr, x$gsis))

  # Slice All Samples:
  matchup_data$home_team_all_samples <- map(matchup_data$id_home_team, \(x) slice_posteam_samples(clean_samples, x))
  matchup_data$away_team_all_samples = map(matchup_data$id_away_team, \(x) slice_posteam_samples(clean_samples, x))

  # Slice Def Samples:
  matchup_data$home_team_all_def_samples <- map(matchup_data$id_home_team, \(x) slice_defteam_samples(clean_samples, x))
  matchup_data$away_team_all_def_samples = map(matchup_data$id_away_team, \(x) slice_defteam_samples(clean_samples, x))

  # Pass Samples:
  matchup_data$home_team_pass_samples <-
    map2(matchup_data$home_qb_gsis,
         matchup_data$home_roster_relevances,
         ~ {
           pre_allocate_pass_samples(clean_samples, .x) %>%
             append_roster_relevance(.y)
         })
    matchup_data$away_team_pass_samples <- map2(matchup_data$away_qb_gsis, matchup_data$away_roster_relevances, ~ {
      pre_allocate_pass_samples(clean_samples, .x) %>%
        append_roster_relevance(.y)
    })

    # Def Pass Samples:
    matchup_data$home_team_def_pass_samples <- map(matchup_data$id_home_team, \(x) pre_allocate_team_pass_samples(clean_samples, x))
    matchup_data$away_team_def_pass_samples <- map(matchup_data$id_away_team, \(x) pre_allocate_team_pass_samples(clean_samples, x))

    # Rush Samples:
    matchup_data$home_team_rush_samples <- map2(matchup_data$home_team_all_samples,
                                               matchup_data$home_roster_relevances,
                                               ~ {
                                                 slice_no_dropback(.x) %>%
                                                   append_roster_relevance(.y)
                                               })
    matchup_data$away_team_rush_samples <- map2(matchup_data$away_team_all_samples,
                                               matchup_data$away_roster_relevances,
                                               ~ {
                                                 slice_no_dropback(.x) %>%
                                                   append_roster_relevance(.y)
                                               })

    # Def Rush Samples:
    matchup_data$home_team_def_rush_samples = map(matchup_data$home_team_all_def_samples, \(x) slice_no_dropback(x))
    matchup_data$away_team_def_rush_samples = map(matchup_data$away_team_all_def_samples, \(x) slice_no_dropback(x))

    # Specials:
    matchup_data$home_team_specials = map(matchup_data$home_team_all_samples, \(x) slice_specials(x))
    matchup_data$away_team_specials = map(matchup_data$away_team_all_samples, \(x) slice_specials(x))

    # Allocate ELO Data for Safety Samples:
    elo_data <- get_elo(.season = unique(sample_data$id_season)) %>%
      dplyr::select(id_game, id_posteam, qb_value, qb_adj, qb) %>%
      dplyr::left_join(
        nflreadr::load_players() %>%
          dplyr::filter(position == 'QB') %>%
          dplyr::select(qb = display_name, id_passer = gsis_id) %>%
          unique()
      ) %>%
      dplyr::select(-qb)


    # Collect and Supplement Safety QB Samples:
    matchup_data$home_safety_qb_samples = map(
      matchup_data$id_home_qb,
      \(x) collect_safety_qb_samples(
        sample_data = clean_samples,
        elo_data = elo_data,
        qb_id = x
      )
    )
    matchup_data$away_safety_qb_samples = map(
      matchup_data$id_away_qb,
      \(x) collect_safety_qb_samples(
        sample_data = clean_samples,
        elo_data = elo_data,
        qb_id = x
      )
    )
    matchup_data$home_team_pass_samples = map2(matchup_data$home_team_pass_samples, matchup_data$home_safety_qb_samples, \(x, y) vec_rbind(x, y))
    matchup_data$home_team_pass_samples = map2(matchup_data$away_team_pass_samples, matchup_data$away_safety_qb_samples, \(x, y) vec_rbind(x, y))

    # Return:
    return(matchup_data)

}
