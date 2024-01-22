


expose_matchup_variables <- function(matchup) {

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


  # List Variables:
  var_names <- ls()
  variable_list <- lapply(var_names, function(var) get(var))
  names(variable_list) <- var_names

  # Read to Parent:
  parent_env <- rlang::env_parent()
  list2env(variable_list, envir = parent_env)

}


