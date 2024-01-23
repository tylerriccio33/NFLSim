


assign_matchup_variables <- function(matchup) {

  try_fetch({
    # Teams:
    home_team <- matchup$id_home_team
    away_team <- matchup$id_away_team
    # DCs:
    home_dc <- matchup$id_home_team_dc[[1]]
    away_dc <- matchup$id_away_team_dc[[1]]
    # Player
    home_qb_gsis <- matchup$id_home_qb[[1]]
    away_qb_gsis <- matchup$id_away_qb[[1]]
    # Home Sample Allocation:
    home_team_all_samples <- matchup$home_team_all_samples[[1]]
    home_team_pass_samples <- matchup$home_team_pass_samples[[1]]
    home_team_def_pass_samples <- matchup$home_team_def_pass_samples[[1]]
    home_team_rush_samples <- matchup$home_team_rush_samples[[1]]
    home_team_def_rush_samples <- matchup$home_team_def_rush_samples[[1]]
    home_team_specials <- matchup$home_team_specials[[1]]
    # Away Sample Allocation
    away_team_all_samples <- matchup$away_team_all_samples[[1]]
    away_team_pass_samples <- matchup$away_team_pass_samples[[1]]
    away_team_def_pass_samples <- matchup$away_team_def_pass_samples[[1]]
    away_team_rush_samples <- matchup$away_team_rush_samples[[1]]
    away_team_def_rush_samples <- matchup$away_team_def_rush_samples[[1]]
    away_team_specials <- matchup$away_team_specials[[1]]
  }, error = function(cnd) {
    cli_abort(c("Failed to extract variables from matchup -> {cnd}.",
                i = "Did data assemble produce a valid result?"))
  })


  # Assign to Parent:
  invisible(list2env(as.list(environment()), parent.frame()) )


}

set_cur_team <- function() {

  # Get Parent:
  env <- parent.frame()

  # Get Team:
  next_play <- get("next_play", envir = env)
  cur_team <- next_play$id_posteam

  # Get QB:
  home_team <- get("home_team", envir = env)
  home_qb_gsis <- get("home_qb_gsis", envir = env)
  away_qb_gsis <- get("away_qb_gsis", envir = env)
  cur_qb <-
    ifelse(cur_team == home_team, home_qb_gsis, away_qb_gsis)

  # All Samples:
  home_team_all_samples <- get("home_team_all_samples", envir = env)
  away_team_all_samples <- get("away_team_all_samples", envir = env)
  cur_all_samples <- ifelse(cur_team == home_team,
                            list(home_team_all_samples),
                            list(away_team_all_samples))[[1]]

  # Pass Samples:
  home_team_pass_samples <- get("home_team_pass_samples", envir = env)
  away_team_pass_samples <- get("away_team_pass_samples", envir = env)
  cur_pass_samples <- ifelse(
    cur_team == home_team,
    list(home_team_pass_samples),
    list(away_team_pass_samples)
  )[[1]]

  # Rush Samples:
  home_team_rush_samples <- get("home_team_rush_samples", envir = env)
  away_team_rush_samples <- get("away_team_rush_samples", envir = env)
  cur_rush_samples <- ifelse(
    cur_team == home_team,
    list(home_team_rush_samples),
    list(away_team_rush_samples)
  )[[1]]

  # Special Samples:
  home_team_specials <- get("home_team_specials", envir = env)
  away_team_specials <- get("away_team_specials", envir = env)
  cur_special_samples <- ifelse(cur_team == home_team,
                                list(home_team_specials),
                                list(away_team_specials))[[1]]

  # Def Pass Samples:
  away_team_def_pass_samples <- get("away_team_def_pass_samples", envir = env)
  home_team_def_pass_samples <- get("home_team_def_pass_samples", envir = env)
  cur_def_pass_samples <- ifelse(
    cur_team == home_team,
    list(away_team_def_pass_samples),
    list(home_team_def_pass_samples)
  )[[1]]

  # Def Rush Samples:
  away_team_def_rush_samples <- get("away_team_def_rush_samples", envir = env)
  home_team_def_rush_samples <- get("home_team_def_rush_samples", envir = env)
  cur_def_rush_samples <- ifelse(
    cur_team == home_team,
    list(away_team_def_rush_samples),
    list(home_team_def_rush_samples)
  )[[1]]

  # Assign to Parent:
  invisible(list2env(as.list(environment()), parent.frame()))

}
