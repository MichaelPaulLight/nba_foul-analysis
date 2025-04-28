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
#' @param defender_proximity_ranges Vector of defender proximity categories
#' @param seed Random seed for reproducibility (default: 42)
#' @param foul_count_transform How to transform the defender_foul_count variable. Options are:
#'        "raw" (default) - leave as raw count
#'        "standardized" - standardize to mean 0, sd 1
#'        "ordered" - convert to ordered factor
#' @param summarize_data Whether to return aggregated data (TRUE) or individual shot data (FALSE)
#'        (default: TRUE)
#'
#' @return A tibble containing simulated shot and foul data. If summarize_data is TRUE, the data is
#'        aggregated by defensive team, defender, position, and defender foul count. If FALSE, the
#'        data is returned at the individual shot level.
#'
#' @examples
#' # Generate a small dataset for testing
#' small_sim <- simulate_shots(n_observations = 100, n_defenders = 20)
#'
#' # Generate a larger dataset with standardized foul counts
#' large_sim <- simulate_shots(
#'   n_observations = 5000,
#'   n_defenders = 100,
#'   n_teams = 15,
#'   seed = 123,
#'   foul_count_transform = "standardized"
#' )
#'
#' # Generate disaggregated data
#' disaggregated_data <- simulate_shots(
#'   n_observations = 1000,
#'   summarize_data = FALSE
#' )
#'
#' @export
simulate_shots <- function(
  n_observations = 1000,
  n_defenders = 50,
  n_teams = 10,
  position_names = c("PG", "SG", "SF", "PF", "C"),
  shot_distance_ranges = c("0_through_9_ft", "10_through_23_ft", "24_plus"),
  defender_proximity_ranges = c("0-2 Feet", "2-4 Feet", "4-6 Feet", "6+ Feet"),
  seed = 42,
  foul_count_transform = c("raw", "standardized", "ordered"),
  summarize_data = TRUE
) {
  set.seed(seed)
  foul_count_transform <- match.arg(foul_count_transform)
  
  # Generate team effects
  team_offensive_effect <- rnorm(n_teams, mean = 0, sd = 0.2)
  team_defensive_effect <- rnorm(n_teams, mean = 0, sd = 0.2)
  player_defensive_effect <- rnorm(n_defenders, mean = 0, sd = 1)

  
  teams <- tibble(
    team_id = 1:n_teams,
    team_name = paste0("Team_", LETTERS[1:n_teams]),
    offensive_rating = team_offensive_effect,
    defensive_rating = team_defensive_effect
  )
  
  # Calculate defenders per team
  defenders_per_team <- ceiling(n_defenders / n_teams)
    
  # Creating a tibble of defenders with balanced team assignments
  defenders <- tibble(
    defender_id = 1:n_defenders,
    defender_name = paste0("Player_", 1:n_defenders),
    position_id = sample(1:length(position_names), n_defenders, replace = TRUE),
    position = position_names[position_id],
    team_id = rep(1:n_teams, length.out = n_defenders),  # Each defender assigned to exactly one team
    player_defensive_effect = player_defensive_effect
  ) |>
    left_join(teams, by = "team_id") |>
    mutate(
      defensive_skill = player_defensive_effect + defensive_rating  # Add team defensive effect to individual skill
    )
  
  shot_df <- tibble(
    shot_id = 1:n_observations, 
    team_id = sample(defenders$team_id, size = n_observations, replace = TRUE))
  
  shot_df <- shot_df |> 
    full_join(defenders, by = "team_id", relationship = "many-to-many") |> 
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
      # Generate raw foul values using a gamma distribution, in line with the findings in Chu and Swartz (2020)
      raw_fouls = floor(rgamma(n(), 
                         shape = 0.7, 
                         scale = max(0.4, 0.8 - defensive_skill + position_foul_effect * 0.25))),
      
      # This is a bit hacky, but essentially we 
      defender_foul_count = case_when(
        raw_fouls > 5 ~ sample(3:5, n(), replace = TRUE, prob = c(0.5, 0.35, 0.15)),
        TRUE ~ raw_fouls
      )
    ) |>
    ungroup()
  
  # Transform defender_foul_count based on user choice
  if (foul_count_transform == "standardized") {
    shot_df <- shot_df |>
      mutate(defender_foul_count = scale(defender_foul_count)[,1])
  } else if (foul_count_transform == "ordered") {
    shot_df <- shot_df |>
      mutate(
        defender_foul_count_numeric = defender_foul_count,  # Keep numeric version for calculations
        defender_foul_count = factor(defender_foul_count, 
                                   levels = 0:5,
                                   ordered = TRUE)
      )
  }
  
  # Calculate shot characteristics based on which defender is playing with the most foul risk
  shot_characteristics <- shot_df |>
    group_by(team_id, shot_id) |>
    summarize(
      # Use the defender with the riskiest number of fouls to characterize shot distance choice
      riskiest_defender_position = position[which.max(if(foul_count_transform == "ordered") 
                                                     defender_foul_count_numeric 
                                                     else defender_foul_count)],
      
      # Need to generate a random number for each group
      rand_num = runif(1),
      rand_num_type = runif(1),  # Additional random number for shot type
      rand_num_movement = runif(1),  # Additional random number for shot movement
      
      # Use the random number for each group's decision
      shot_distance_category = case_when(
        riskiest_defender_position %in% c("PG", "SG") & rand_num < 0.5 ~ "24_plus",
        riskiest_defender_position %in% c("SF") & rand_num < 0.5 ~ "24_plus",
        riskiest_defender_position %in% c("PF", "C") & rand_num < 0.2 ~ "24_plus",
        riskiest_defender_position %in% c("PF", "C") & rand_num < 0.6 ~ "0_through_9_ft",
        TRUE ~ "10_through_23_ft"
      ),
      
      # Shot type basic - determine shot type based on position and distance
      shot_type_basic = case_when(
        shot_distance_category == "0_through_9_ft" & rand_num_type < 0.6 ~ "layup",
        shot_distance_category == "0_through_9_ft" & rand_num_type < 0.8 ~ "dunk",
        shot_distance_category == "0_through_9_ft" & rand_num_type < 0.9 ~ "hook shot",
        shot_distance_category == "0_through_9_ft" ~ "finger roll",
        shot_distance_category == "24_plus" ~ "jump shot",
        shot_distance_category == "10_through_23_ft" & rand_num_type < 0.9 ~ "jump shot",
        shot_distance_category == "10_through_23_ft" ~ "hook shot",
        TRUE ~ "jump shot"
      ),
      
      # Shot movement - how the player is moving
      shot_movement = case_when(
        shot_type_basic %in% c("layup", "dunk") & rand_num_movement < 0.7 ~ "driving",
        shot_type_basic == "jump shot" & rand_num_movement < 0.3 ~ "pullup",
        shot_type_basic == "jump shot" & rand_num_movement < 0.5 ~ "step back",
        shot_type_basic %in% c("layup", "dunk") & rand_num_movement < 0.8 ~ "cutting",
        shot_type_basic == "finger roll" & rand_num_movement < 0.6 ~ "floating",
        shot_type_basic == "jump shot" & rand_num_movement < 0.7 ~ "running",
        TRUE ~ "stationary"
      ),
      
      # Base probability of making a shot
      base_prob = case_when(
        shot_distance_category == "0_through_9_ft" ~ 0.6,
        shot_distance_category == "10_through_23_ft" ~ 0.4,
        shot_distance_category == "24_plus" ~ 0.35
      ),
      
      # Effect of foul count on shot probability 
      foul_effect = mean(if(foul_count_transform == "ordered") 
                        defender_foul_count_numeric 
                        else defender_foul_count) * 0.02,
      
      # Effect of defensive skill on shot probability
      defensive_skill_effect = mean(-defensive_skill * 0.3),
      
      # Effect of team defensive rating on shot probability
      team_defensive_effect = mean(-defensive_rating * 0.2),
      
      .groups = "drop"  # Explicitly drop grouping
    )
  
  # Calculate shot probability and make a shot or not
  shot_characteristics <- shot_characteristics |> 
    rename(defensive_team_id = team_id) |> 
    rowwise() |>  # Add rowwise to ensure operations happen per row
    mutate(
      # Sample offensive team for this shot, excluding the defensive team
      offensive_team_id = sample(setdiff(teams$team_id, defensive_team_id), 1),
      team_offensive_effect = teams$offensive_rating[offensive_team_id] * 0.2,
      
      shot_prob = plogis(qlogis(base_prob) + foul_effect + defensive_skill_effect + 
                           team_defensive_effect + team_offensive_effect),
      shot_made = rbinom(1, 1, shot_prob)
    ) |>
    ungroup()
  
  # Join the shot characteristics with the shot data
  final_dataset <- shot_df |>
    left_join(shot_characteristics, by = "shot_id")
  
  # Summarize the data if requested
  if (summarize_data) {
    final_dataset <- final_dataset |>
      group_by(defensive_team_id, offensive_team_id, defender_id, position, defender_foul_count, 
               shot_distance_category, shot_type_basic, shot_movement) |> 
      summarize(
        shots_taken = n(),
        shots_made = sum(shot_made, na.rm = TRUE),
        .groups = "drop"
      )
  }
  
  return(final_dataset)
} 