#' Simulate Last Two Minutes (L2M) report data
#'
#' @param n_games Number of games to simulate
#' @param n_players Number of players in the league
#' @param n_events_per_game Number of events per game
#' @param game_length_minutes Total game length in minutes
#' @param last_two_minutes Length of last two minutes period
#' @param base_rate Base log-odds for foul probability
#' @param all_star_effect Additional log-odds for pre-All-Star games
#' @param player_sd SD for player random effects
#' @param team_sd SD for team effects
#' @param period_sd SD for period effects
#' @param game_sd SD for game effects
#' @param p_correct_call Probability of correct call when foul occurs
#' @param p_incorrect_call Probability of incorrect call when foul occurs
#' @param p_correct_nocall Probability of correct non-call when no foul
#' @param p_incorrect_nocall Probability of incorrect non-call when no foul
#' @return A data frame containing simulated L2M data
#' @export
simulate_l2m <- function(
  n_games = 500,
  n_players = 250,
  n_events_per_game = 100,
  game_length_minutes = 48,
  last_two_minutes = 2,
  base_rate = -3.7,
  all_star_effect = 0.5,
  player_sd = 0.3,
  team_sd = 0.2,
  period_sd = 0.15,
  game_sd = 0.25,
  p_correct_call = 0.9,
  p_incorrect_call = 0.1,
  p_correct_nocall = 0.9,
  p_incorrect_nocall = 0.1
) {
  # Calculate events in last two minutes
  events_per_minute <- n_events_per_game / game_length_minutes
  events_in_last_two <- ceiling(events_per_minute * last_two_minutes)

  # Define positions and their baseline foul tendencies
  positions <- c("PG", "SG", "SF", "PF", "C")
  position_effects <- c(-0.2, -0.1, 0, 0.2, 0.3)
  names(position_effects) <- positions

  # Generate player effects
  set.seed(123)
  player_effects <- rnorm(n_players, 0, player_sd)
  names(player_effects) <- paste0("Player_", 1:n_players)

  # Assign positions to players
  player_positions <- sample(positions, n_players, replace = TRUE, 
                           prob = c(0.2, 0.2, 0.2, 0.2, 0.2))
  names(player_positions) <- names(player_effects)

  # Generate team effects
  n_teams <- 5
  team_effects <- rnorm(n_teams, 0, team_sd)
  teams <- paste0("Team_", LETTERS[1:n_teams])
  names(team_effects) <- teams

  # Generate game effects
  game_effects <- rnorm(n_games, 0, game_sd)
  names(game_effects) <- paste0("Game_", 1:n_games)

  # Generate period effects
  period_effects <- rnorm(4, 0, period_sd)
  names(period_effects) <- paste0("Period_", 1:4)

  # Function to simulate one game's L2M events
  simulate_single_l2m <- function(game_id) {
    # Create data frame for this game
    game_data <- data.frame()
    
    # Determine if game is before All-Star break (first half of season)
    is_pre_all_star <- game_id <= ceiling(n_games/2)
    
    # Assign teams for this game
    home_team <- sample(teams, 1)
    away_team <- sample(setdiff(teams, home_team), 1)
    
    # Track accumulated fouls for each player
    player_fouls <- rep(0, n_players)
    names(player_fouls) <- paste0("Player_", 1:n_players)
    
    # Assign players to teams (5 players per team)
    home_players <- character(5)
    away_players <- character(5)
    
    for (pos in 1:5) {
      pos_players <- names(player_positions[player_positions == positions[pos]])
      if (length(pos_players) >= 2) {
        selected <- sample(pos_players, 2)
        home_players[pos] <- selected[1]
        away_players[pos] <- selected[2]
      } else {
        available <- setdiff(names(player_positions), c(home_players, away_players))
        home_players[pos] <- sample(available, 1)
        available <- setdiff(available, home_players[pos])
        away_players[pos] <- sample(available, 1)
      }
    }
    
    # Only simulate events in the last two minutes
    start_event <- n_events_per_game - events_in_last_two + 1
    
    for (event in start_event:n_events_per_game) {
      # Determine period (4th period for last two minutes)
      current_period <- 4
      
      # For each player on defense, simulate potential foul
      defending_team <- ifelse(event %% 2 == 0, home_team, away_team)
      defending_players <- ifelse(defending_team == home_team, list(home_players), list(away_players))[[1]]
      
      for (player in defending_players) {
        # Get player position
        player_position <- player_positions[player]
        
        # Calculate foul probability with All-Star break effect
        logit_p <- base_rate + 
                   (is_pre_all_star * all_star_effect) +
                   player_effects[player] + 
                   position_effects[player_position] +
                   team_effects[defending_team] +
                   period_effects[paste0("Period_", current_period)] +
                   game_effects[paste0("Game_", game_id)]
        
        p <- plogis(logit_p)
        
        # Simulate foul occurrence
        foul_occurs <- rbinom(1, 1, p)
        
        # Simulate decision based on whether foul occurred
        if (foul_occurs) {
          decision <- sample(c("CC", "IC"), 1, prob = c(p_correct_call, p_incorrect_call))
        } else {
          decision <- sample(c("CNC", "INC"), 1, prob = c(p_correct_nocall, p_incorrect_nocall))
        }
        
        # Add row to game data
        event_data <- data.frame(
          game_id = paste0("Game_", game_id),
          number_event = event,
          period = current_period,
          player_name = player,
          position = player_position,
          slug_team = defending_team,
          slug_opp = ifelse(defending_team == home_team, away_team, home_team),
          personal_fouls_during_event = player_fouls[player],
          personal_foul_occurance_on_player = foul_occurs,
          act_type = sample(c("2pt", "3pt", "drive"), 1),
          is_pre_all_star = is_pre_all_star,
          time_remaining = (n_events_per_game - event) / events_per_minute,
          decision = decision
        )
        
        game_data <- rbind(game_data, event_data)
        
        # Update player fouls if a foul occurred
        if (foul_occurs) {
          player_fouls[player] <- player_fouls[player] + 1
        }
      }
    }
    
    return(game_data)
  }

  # Simulate all games
  sim_data <- do.call(rbind, lapply(1:n_games, simulate_single_l2m))
  return(sim_data)
} 