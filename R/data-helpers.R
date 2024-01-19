#' Post Data Collection Helpers and Wranglers
#'
#' @description
#' * `reorder_depth_chart` takes the raw depth charts and summarized participation data to reorder the charting ranks. The raw depth charts are from NFL data exchange, which can be wrong often. It's often a better representation of depth rank to go on participation. So here, we'll use the player's particitpation percentage to determine the rank. For example, the player with the most participation in the game will get the rank of 1, and so on.
#'
#' @param depth_chart Raw depth chart data.
#' @param participation_data Summarized participation data.
#'
#' @return
#' * `reorder_depth_chart` returns the original depth chart data with the fixed ranks.
reorder_depth_chart <- function(depth_chart, participation_data) {
  # join game participation data to re-order ranks
  # WARNING: this includes posterior participation data
  assert_cols(participation_data, snap_pct)

  dc <- mutate(dc, rank = as.integer(rank))

  # slice top QB
  qb_slices <- vctrs::vec_slice(dc, dc$position == 'QB')
  qb_slices <- qb_slices %>%
    group_by(id_game,id_posteam) %>%
    arrange(id_posteam, rank, .by_group = T) %>%
    slice(1)

  # reorder WR by snap
  wr_reordered <- vctrs::vec_slice(dc, dc$position == 'WR')

  wr_reordered <- wr_reordered %>%
    left_join(participation_data) %>%
    group_by(id_game, id_posteam, position) %>%
    arrange(-snap_pct, .by_group = T) %>%
    mutate(rank = row_number()) %>%
    ungroup() %>%
    select(-c(snaps, snap_pct))
  expect_resolved_suffix(wr_reordered)
  wr_reordered <- vctrs::vec_slice(wr_reordered, wr_reordered$rank <= 3)

  # slice top TE
  te_slices <- vctrs::vec_slice(dc, dc$position == 'TE')
  te_slices <- vctrs::vec_slice(te_slices, te_slices$rank <= 2)

  # slice top RB
  rb_slices <- vctrs::vec_slice(dc, dc$position == 'RB')
  rb_slices <- vctrs::vec_slice(rb_slices, rb_slices$rank <= 2)

  # Bind all
  ordered_dc <- bind_rows(qb_slices, wr_reordered, te_slices, rb_slices) %>%
    arrange(id_season, id_week, id_posteam, position, rank) %>%

    # final rank clean up
    group_by(id_game, id_posteam, position, rank) %>%
    slice(1) %>%
    ungroup()

  # assert
  expect_id_completion(ordered_dc)
  assert_completion(ordered_dc, rank)
  return(ordered_dc)

}

# TODO: have to check a lot of this
bind_future_dc <- function(depth_chart, future_games) {
  ordered_dc <-
    reorder_dc_ranks(raw_depth_charts, participation_data = summarized_participation_data) %>%
    # bind future game dc to past ordered
    bind_rows(
      future_games %>%
        select(id_season, id_week, id_posteam_game = id_posteam, id_game, x_dc) %>%
        unnest(x_dc) %>%
        relocate(starts_with('id_')) %>%
        select(-c(athlete_id, id_posteam)) %>%
        rename(id_posteam = id_posteam_game) %>%
        rename(
          position = pos_abb,
          rank = pos_rank,
          full_name = player_full_name
        )
    )
  assert_team_representation(ordered_dc)
  assert_completion(ordered_dc, rank)

}


