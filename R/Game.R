
Game <-
  function(matchup_dataclass,
           max_plays = 150,
           quiet = F) {
    assert("Matchup dataclass must be a single row." = nrow(matchup_dataclass) == 1)

    # Initialize Game:
    assign_matchup_variables(matchup = matchup_dataclass)
    next_play <- initialize_game(matchup = matchup_dataclass)
    next_play <- add_wp(next_play)
    game_plays <- tibble::new_tibble(x = list(), nrow = 0)
    n_play <- 1
    set_cur_team()
    cli::cli_h2("Starting Game")
    cli::cli_progress_bar("Playing game", total = max_plays)

    # Start Game:
    while (n_play <= max_plays) {
      cli_progress_update()
      if (next_play$id_posteam != cur_team) {
        set_cur_team()
      }

      # Fourth Down Decision:
      if (next_play$down == 4) {
        fourth_down_situational_samples <-
          Situation(samples = cur_all_samples,
                    next_play)
        play_decision <-
          simulate_fourth_down_decision(fourth_down_situational_samples)
      } else {
        play_decision <- "go"
      }

      # Sample dropback #
      if (play_decision == "go") {
        dropback_decision_samples <- Situation(
          samples = cur_all_samples, next_play)
        dropback_decision <- simulate_dropback_decision(dropback_decision_samples)
      }

      # Select Sample #
      if (play_decision == 'go') {
        if (dropback_decision) {
          pass_samples <- Situation(cur_pass_samples, next_play)
          def_pass_samples <-
            Situation(cur_def_pass_samples, next_play)
          selected_sample <-
            SelectSample(off_samples = pass_samples,
                         def_samples = def_pass_samples)
        } else {
          rush_samples <- Situation(cur_rush_samples, next_play)
          def_rush_samples <-
            Situation(cur_def_rush_samples, next_play)
          selected_sample <-
            SelectSample(off_samples = rush_samples,
                         def_samples = def_rush_samples)
        }
        if (next_play$down == 4) {
          selected_sample <- fix_turnover_on_downs(selected_sample)
        }
      } else {
        samples <- Situation(cur_special_samples, next_play)
        selected_sample <- select_special_sample(samples, decision = play_decision)
      }
      # fix issues that come with variable yardline
      selected_sample <- fix_touchdown(selected_sample)

      # Merge current game context with sample
      play <-
        decode_sample(sample = selected_sample, next_play)
      validate_core_play_equality(play, next_play)

      # Log Play #
      game_plays <- vec_rbind(game_plays, play)

      # End game #
      if (play$game_seconds_remaining <= 0) break

      # Describe Possession Changes #
      next_play <- Play(play)
      next_play <- calculate_next_play_values(next_play)
      n_play <- n_play + 1

    }

    # End Game:
    cli_progress_done()
    cli_alert_success("End game.")
    return(game_plays)

    #
    # TODO: abstract to function
    clean_game <- game_plays
    get_vars(clean_game,
             vars = .c(id_rusher_player, id_receiver_player, id_passer)) <- NULL
    # remove IDs of rusher and receiver
    # passer is kept since
    # fselect(-c(id_rusher_player, id_receiver_player, id_passer)) %>%
    clean_game <- clean_game %>%
      # append rusher
      append_player_id(dc = home_dc, pos = "rusher") %>%
      append_player_id(dc = away_dc, pos = "rusher") %>%
      # append receiver
      append_player_id(dc = home_dc, pos = "receiver") %>%
      append_player_id(dc = away_dc, pos = "receiver")

    return(clean_game)

    # TODO: abstract to function
    clean_game <- clean_game %>%
      # replace QB
      fmutate(id_passer = case_when(
        (id_posteam == id_home_team &
           qb_dropback == 1) ~ home_qb_list$gsis,
        (id_posteam == id_away_team &
           qb_dropback == 1) ~ away_qb_list$gsis,
        .default = NA
      )) %>%
      fmutate(id_passer_name = case_when(
        (id_posteam == id_home_team &
           qb_dropback == 1) ~ home_qb_list$name,
        (id_posteam == id_away_team &
           qb_dropback == 1) ~ away_qb_list$name,
        .default = NA
      )) %>%
      # rushers for scrambles
      fmutate(id_rusher = case_when(
        (id_posteam == id_home_team &
           qb_dropback == 1 & qb_scramble == 1) ~ home_qb_list$gsis,
        (id_posteam == id_away_team &
           qb_dropback == 1 & qb_scramble == 1) ~ away_qb_list$gsis,
        .default = rusher_name
      )) %>%
      fmutate(rusher_name = case_when(
        (id_posteam == id_home_team &
           qb_dropback == 1 & qb_scramble == 1) ~ home_qb_list$name,
        (id_posteam == id_away_team &
           qb_dropback == 1 & qb_scramble == 1) ~ away_qb_list$name,
        .default = rusher_name
      ))

    clean_game$id_play_new <- seq_row(clean_game)


    return(clean_game)

  }




