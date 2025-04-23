#' Simulate NBA Shot and Foul Data
#'
#' This function generates simulated NBA shot and foul data based on various parameters.
#' It creates a dataset that includes shot attempts, defender information, and outcomes.
#' The dataset is disaggregated to the individual shot level
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
#'
#' @return A tibble containing simulated shot and foul data with the following columns:
#' \itemize{
#'   \item shot_id: Unique identifier for each shot
#'   \item game_id: Game identifier
#'   \item period: Game period (1-4)
#'   \item defender_id: Unique identifier for each defender
#'   \item defender_name: Name of the defender
#'   \item position: Player position
#'   \item team: Team name
#'   \item defender_foul_count: Number of fouls by the defender (transformed according to foul_count_transform)
#'   \item defender_proximity: Distance of defender from shooter
#'   \item shot_distance_category: Category of shot distance
#'   \item shot_made: Binary indicator of whether shot was made
#'   \item defensive_skill: Defender's defensive skill rating
#' }
#'
#' @examples
#' # Generate a small dataset for testing
#' small_sim <- simulate_shots_disaggregated(n_observations = 100, n_defenders = 20)
#'
#' # Generate a larger dataset with standardized foul counts
#' large_sim <- simulate_shots_disaggregated(
#'   n_observations = 5000,
#'   n_defenders = 100,
#'   n_teams = 15,
#'   seed = 123,
#'   foul_count_transform = "standardized"
#' )
#'
#' @export
simulate_shots_disaggregated <- function(
    n_observations = 1000,
    n_defenders = 50,
    n_teams = 10,
    position_names = c("PG", "SG", "SF", "PF", "C"),
    shot_distance_ranges = c("0_through_9_ft", "10_through_23_ft", "24_plus"),
    defender_proximity_ranges = c("0-2 Feet", "2-4 Feet", "4-6 Feet", "6+ Feet"),
    seed = 42,
    foul_count_transform = c("raw", "standardized", "ordered")
) {
  set.seed(seed)
  foul_count_transform <- match.arg(foul_count_transform)
  
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
    team_id = rep(1:n_teams, length.out = n_defenders)  # Each defender assigned to exactly one team
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
      defender_foul_count = pmin(5, pmax(0, round(rpois(n(), lambda = max(0, 2 - defensive_skill + position_foul_effect)))))
    ) |>
    ungroup()
  
  # Transform defender_foul_count based on user choice
  if (foul_count_transform == "standardized") {
    shot_df <- shot_df |>
      mutate(defender_foul_count = scale(defender_foul_count)[,1])
  } else if (foul_count_transform == "ordered") {
    shot_df <- shot_df |>
      mutate(defender_foul_count = factor(defender_foul_count, 
                                         levels = 0:5,
                                         ordered = TRUE))
  }
  
  # Calculate shot characteristics based on closest defender
  shot_characteristics <- shot_df |>
    group_by(team_id, shot_id) |>
    summarize(
      # Use the defender with the riskiest number of fouls to characterize shot distance choice
      riskiest_defender_position = position[which.max(defender_foul_count)],
      
      # Need to generate a random number for each group
      rand_num = runif(1),
      
      # Use the random number for each group's decision
      shot_distance_category = case_when(
        riskiest_defender_position %in% c("PG", "SG") & rand_num < 0.5 ~ "24_plus",
        riskiest_defender_position %in% c("SF") & rand_num < 0.5 ~ "24_plus",
        riskiest_defender_position %in% c("PF", "C") & rand_num < 0.2 ~ "24_plus",
        riskiest_defender_position %in% c("PF", "C") & rand_num < 0.6 ~ "0_through_9_ft",
        TRUE ~ "10_through_23_ft"
      ),
      
      # Other summarize operations...
      base_prob = case_when(
        shot_distance_category == "0_through_9_ft" ~ 0.6,
        shot_distance_category == "10_through_23_ft" ~ 0.4,
        shot_distance_category == "24_plus" ~ 0.35
      ),
      
      foul_effect = mean(defender_foul_count) * 0.02,
      defensive_skill_effect = mean(-defensive_skill * 0.3),
      team_defensive_effect = mean(-defensive_rating * 0.2),
      .groups = "drop"  # Explicitly drop grouping
    )
  
  # Fix the shot_characteristics mutation to use rowwise operations
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
    ungroup()  # Ungroup after rowwise operations
  
  # Fix the final_dataset join and grouping
  final_dataset <- shot_df |>
    left_join(shot_characteristics, by = "shot_id")
  
  return(final_dataset)
} 