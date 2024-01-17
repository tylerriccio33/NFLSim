#' App
#'
#' @description Main entry point for the app.
#'
#' @param supplemental_data Optional data joined to required assembled data, using `left_join`.
#' @param join_args Optional args passed to `left_join` joining the supplemental data. This is a required argument if any supplemental data is passed. The relationship must be "one-to-one" and dplyr will throw an error if not.
#' @param calc_similarity_fn Optional function to calculate similarity scores. The function takes the final dataset (supplemental joined) and must return the same dataset with a new column titled `sim_score`.
#' @param n_simulations Number of simulations to run per game.
#' @param id_game_vec Optional vector of games to limit the simulations to. Defaults to `NULL`, which runs all games.
#'
#' @return
#' @export
#'
app <- function(supplemental_data = rlang::zap(),
                calc_similarity_fn = rlang::zap(),
                n_simulations = 100,
                id_game_vec = rlang::zap()) {

  # Assemble Data #
  assembled_data <- assemble_data()
  if (!rlang::is_zap(supplemental_data)) {
    cli_abort("Not implemented")
  }

  # Calculate Similarity #
  if (!rlang::is_zap(id_game_vec)) {
    assembled_data <-
      filter(assembled_data, .data$id_game %in% id_game_vec)
  }
  if (!rlang::is_zap(calc_similarity_fn)) {
    cli_abort("Not implemented")
  }
  assembled_data <- calculate_matchup_samples(assembled_data)

  # Run Simulations #
  cli_abort("Not implemented")

}




