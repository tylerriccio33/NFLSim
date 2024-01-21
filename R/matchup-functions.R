#' Functions to Prep and Build the Matchup Profile
#'
#' @return
#' @export
#'
#' @examples
prep_matchup <- function(game_data, cur_season, cur_week) {

  game_data %>%
    as_tibble() %>%
    dplyr::filter(.data$id_season == cur_season,
           .data$id_week == cur_week) %>%
    dplyr::select(
      -c(
        away_score,
        home_score,
        result,
        total,
        overtime,
        id_old_game,
        gsis,
        id_nfl_detail,
        pfr,
        pff,
        espn,
        away_qb_name,
        home_qb_name,
        stadium
      )
    )
}

append_future_dc <- function(matchup_data, future_games) {
  matchup_data %>%
    mutate(
      id_home_team_dc = map(
        id_home_team,
        \(x) get_dc_from_future_games(x, future_games = future_games)
        ,
        .progress = T
      ),
      id_away_team_dc = map(
        id_away_team,
        \(x) get_dc_from_future_games(x, future_games = future_games)
        ,
        .progress = T
      )
    ) %>%
    filter(!(lengths(id_home_team_dc) == 0 |
               is.null(id_home_team_dc))) %>%
    filter(!(lengths(id_away_team_dc) == 0 |
               is.null(id_away_team_dc))) %>%
    # correct any empty or old teams
    reassign_teams(id_home_team, id_home_team_dc) %>%
    reassign_teams(id_away_team, id_away_team_dc)


}

append_roster_relevance <- function(matchup_data, ordered_depth_charts) {
  matchup_data %>%
  mutate(
    home_roster_relevances = map(
      id_home_team_dc,
      \(x) calculate_total_roster_relevance(cur_dc = x, ordered_depth_charts)
      ,
      .progress = T
    )
  ) %>%
    mutate(
      away_roster_relevances = map(
        id_away_team_dc,
        \(x) calculate_total_roster_relevance(cur_dc = x, ordered_depth_charts)
        ,
        .progress = T
      )
    )

}

nest_matchups <- function(matchup_data) {
  matchup_data %>%
    dplyr::group_by(matchup = dplyr::row_number()) %>%
    tidyr::nest(.key = 'matchup_data') %>%
    dplyr::ungroup()
}


