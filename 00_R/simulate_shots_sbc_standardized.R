#' Simulate NBA Shot and Foul Data
#'
#' This function generates simulated NBA shot and foul data based on various parameters.
#' It creates a dataset that includes shot attempts, defender information, and outcomes.
#'
#' @param n_observations Number of shots to simulate (default: 1000)
#' @param n_defenders Number of defenders to create (default: 50)
#' @param n_teams Number of teams to create (default: 10)
#' @param position_names Vector of position names (default: c("PG", "SG", "SF", "PF", "C"))
#' @param shot_distance_ranges Vector of shot distance categories
#' @param seed Random seed for reproducibility (default: 42)
#'
#' @return A list containing:
#'   - variables: List of true parameter values used in simulation
#'   - generated: Data frame containing the simulated shot data
#'
#' @examples
#' # Generate a small dataset for testing
#' small_sim <- simulate_nba_shots(n_observations = 100, n_defenders = 20)
#'
#' # Generate a larger dataset with custom parameters
#' large_sim <- simulate_nba_shots(
#'   n_observations = 5000,
#'   n_defenders = 100,
#'   n_teams = 15,
#'   seed = 123
#' )
#'
#' @export
simulate_shots_sbc_centered <- function(
    n_observations = 1000,
    n_defenders = 50,
    n_teams = 6,
    position_names = c("PG", "SG", "SF", "PF", "C"),
    shot_distance_ranges = c("0_through_9_ft", "10_through_23_ft", "24_plus"),
    seed = 42
) {
  set.seed(seed)
  
  # Create teams with offensive and defensive ratings
  teams <- tibble(
    team_id = 1:n_teams,
    team_name = paste0("Team_", LETTERS[1:n_teams]),
    offensive_rating = rnorm(n_teams, 0, 0.2),  # Team offensive effect
    defensive_rating = rnorm(n_teams, 0, 0.2)   # Team defensive effect
  )
  
  # Calculate defenders per team
  defenders_per_team <- ceiling(n_defenders / n_teams)
  
  # Creating a tibble of defenders with balanced team assignments
  defenders <- tibble(
    defender_id = 1:n_defenders,
    defender_name = paste0("Player_", 1:n_defenders),
    position_id = sample(1:length(position_names), n_defenders, replace = TRUE),
    position = position_names[position_id],
    team_id = rep(1:n_teams, length.out = n_defenders)  # Each defender assigned to exactly one team
  ) |>
    left_join(teams, by = "team_id") |>
    mutate(
      defensive_skill = rnorm(n_defenders, 0, 1) + defensive_rating  # Add team defensive effect to individual skill
    )
  
  # Function to sample 5 unique defenders for a shot
  sample_defenders <- function(defenders_df) {
    sample(defenders_df$defender_id, 5, replace = FALSE)
  }
  
  # Create a list of defender IDs for each shot
  defender_ids_list <- replicate(n_observations, sample_defenders(defenders), simplify = FALSE)
  
  # Create a long format dataset for defenders
  defender_data <- tibble(
    shot_id = rep(1:n_observations, each = 5),
    defender_id = unlist(defender_ids_list)
  ) |>
    left_join(defenders, by = "defender_id") |>
    group_by(shot_id) |>
    mutate(
      # Simulate fouls based on position and skill
      position_foul_effect = case_when(
        position == "C" ~ 1.0,
        position == "PF" ~ 0.7,
        position == "SF" ~ 0.3,
        position == "SG" ~ 0.2,
        position == "PG" ~ 0.0
      ),
      
      # Defenders with lower skill tend to foul more
      defender_foul_count = pmin(5, pmax(0, round(rpois(n(), lambda = max(0, 2 - defensive_skill + position_foul_effect)))))
    ) |>
    ungroup()
  
  # Calculate shot characteristics based on closest defender
  shot_characteristics <- defender_data |>
    group_by(shot_id) |>
    summarize(
      # Use the closest defender's characteristics for shot distance
      closest_defender_position = position[which.min(defender_foul_count)],
      shot_distance_category = case_when(
        closest_defender_position %in% c("PG", "SG") & runif(1) < 0.6 ~ "24_plus",
        closest_defender_position %in% c("SF") & runif(1) < 0.4 ~ "24_plus",
        closest_defender_position %in% c("PF", "C") & runif(1) < 0.2 ~ "24_plus",
        closest_defender_position %in% c("PF", "C") & runif(1) < 0.6 ~ "0_through_9_ft",
        TRUE ~ "10_through_23_ft"
      ),
      
      # Calculate shot success probability based on all defenders
      base_prob = case_when(
        shot_distance_category == "0_through_9_ft" ~ 0.6,
        shot_distance_category == "10_through_23_ft" ~ 0.4,
        shot_distance_category == "24_plus" ~ 0.35
      ),
      
      # Aggregate defender effects
      foul_effect = mean(defender_foul_count) * 0.02,
      defensive_skill_effect = mean(-defensive_skill * 0.3),
      team_defensive_effect = mean(-defensive_rating * 0.2),  # Team defensive effect
      
      # Sample offensive team for this shot
      offensive_team_id = sample(teams$team_id, 1),
      team_offensive_effect = teams$offensive_rating[offensive_team_id] * 0.2,  # Team offensive effect
      
      shot_prob = plogis(qlogis(base_prob) + foul_effect + defensive_skill_effect + 
                           team_defensive_effect + team_offensive_effect),
      shot_made = rbinom(1, 1, shot_prob)
    )
  
  # Create the final dataset with all defender information
  final_dataset <- defender_data |>
    left_join(shot_characteristics, by = "shot_id") |>
    mutate(
      game_id = sample(1:20, n(), replace = TRUE),
      period = sample(1:4, n(), replace = TRUE),
      shots_attempted = 1,
      # Standardize defender_foul_count before grouping
      defender_foul_count = (defender_foul_count - mean(defender_foul_count)) / sd(defender_foul_count)
    ) |>
    group_by(team_id, position, defender_foul_count, shot_distance_category) |> 
    add_count() |> 
    summarize(shots_taken = n, shots_made = sum(shot_made)) |> 
    distinct()
  
  # Generate random effects for each level
  set.seed(seed)
  
  # Position random effects
  position_intercepts <- rnorm(length(position_names), 0, 0.3)
  position_slopes <- rnorm(length(position_names), 0, 0.2)
  names(position_intercepts) <- position_names
  names(position_slopes) <- position_names
  
  r_position <- matrix(rnorm(n_observations, 0, 0.3), 
                       nrow = 5, ncol = 1,
                       dimnames = list(1:5, "Intercept"))
  
  # Shot distance random effects
  distance_intercepts <- rnorm(length(shot_distance_ranges), 0, 0.4)
  distance_slopes <- rnorm(length(shot_distance_ranges), 0, 0.3)
  names(distance_intercepts) <- shot_distance_ranges
  names(distance_slopes) <- shot_distance_ranges
  
  r_shot_distance_category <- matrix(rnorm(n_observations, 0, 0.4), 
                                     nrow = 3, ncol = 1,
                                     dimnames = list(1:3, "Intercept"))
  
  r_team_id <- matrix(rnorm(n_observations, 0, 0.5), 
                      nrow = n_teams, ncol = 1,
                      dimnames = list(1:n_teams, "Intercept"))
  
  # Team random effects
  team_intercepts <- rnorm(n_teams, 0, 0.5)
  team_slopes <- rnorm(n_teams, 0, 0.4)
  names(team_intercepts) <- 1:n_teams
  names(team_slopes) <- 1:n_teams
  
  # Extract true parameters for SBC
  true_parameters <- list(
    # Fixed effects
    b_Intercept = qlogis(0.4),
    b_defender_foul_count = 0.02,
    
    # Standard deviations
    sd_position__Intercept = 0.3,
    sd_position__defender_foul_count = 0.2,
    sd_shot_distance_category__Intercept = 0.4,
    sd_shot_distance_category__defender_foul_count = 0.3,
    sd_team_id__Intercept = 0.5,
    sd_team_id__defender_foul_count = 0.4,
    
    # Correlations
    cor_position__Intercept__defender_foul_count = 0.2,
    cor_shot_distance_category__Intercept__defender_foul_count = 0.3,
    cor_team_id__Intercept__defender_foul_count = 0.4,
    
    # Random effects
    Intercept = qlogis(0.4),
    
    # Position random effects
    r_position = r_position,
    
    # Shot distance random effects
    r_shot_distance_category = r_shot_distance_category,
    
    # Team random effects
    r_team_id = r_team_id,
    
    # Prior and log-posterior
    lprior = 0,
    lp__ = 0
  )
  
  list(
    variables = true_parameters,
    generated = final_dataset
  )
}