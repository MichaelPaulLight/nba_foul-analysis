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
#'   \item defender_foul_count: Number of fouls by the defender
#'   \item defender_proximity: Distance of defender from shooter
#'   \item shot_distance_category: Category of shot distance
#'   \item shot_made: Binary indicator of whether shot was made
#'   \item defensive_skill: Defender's defensive skill rating
#' }
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
simulate_nba_shots <- function(
  n_observations = 1000,
  n_defenders = 50,
  n_teams = 10,
  position_names = c("PG", "SG", "SF", "PF", "C"),
  shot_distance_ranges = c("0_through_9_ft", "10_through_23_ft", "24_plus"),
  defender_proximity_ranges = c("0-2 Feet", "2-4 Feet", "4-6 Feet", "6+ Feet"),
  seed = 42
) {
  set.seed(seed)
  
  # Create teams
  teams <- paste0("Team_", LETTERS[1:n_teams])
  
  # Creating a tibble of defenders
  defenders <- tibble(
    defender_id = 1:n_defenders,
    defender_name = paste0("Player_", 1:n_defenders),
    position_id = sample(1:length(position_names), n_defenders, replace = TRUE),
    position = position_names[position_id],
    team = sample(teams, n_defenders, replace = TRUE),
    defensive_skill = rnorm(n_defenders, 0, 1)
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
      defender_foul_count = pmin(5, pmax(0, round(rpois(n(), lambda = max(0, 2 - defensive_skill + position_foul_effect))))),
      
      # Defender proximity affected by fouls and skill
      # More fouls -> more distance
      proximity_base = 2 + defender_foul_count * 0.5 - defensive_skill,
      proximity_category_id = pmin(4, pmax(1, round(proximity_base))),
      defender_proximity = defender_proximity_ranges[proximity_category_id]
    ) |>
    ungroup()
  
  # Calculate shot characteristics based on closest defender
  shot_characteristics <- defender_data |>
    group_by(shot_id) |>
    summarize(
      # Use the closest defender's characteristics for shot distance
      closest_defender_position = position[which.min(proximity_category_id)],
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
      proximity_effect = mean(case_when(
        defender_proximity == "0-2 Feet" ~ -0.15,
        defender_proximity == "2-4 Feet" ~ -0.08,
        defender_proximity == "4-6 Feet" ~ -0.02,
        defender_proximity == "6+ Feet" ~ 0.05
      )),
      
      foul_effect = mean(defender_foul_count) * 0.02,
      defensive_skill_effect = mean(-defensive_skill * 0.3),
      
      shot_prob = plogis(qlogis(base_prob) + proximity_effect + foul_effect + defensive_skill_effect),
      shot_made = rbinom(1, 1, shot_prob)
    )
  
  # Create the final dataset with all defender information
  final_dataset <- defender_data |>
    left_join(shot_characteristics, by = "shot_id") |>
    mutate(
      game_id = sample(1:20, n(), replace = TRUE),
      period = sample(1:4, n(), replace = TRUE),
      shots_attempted = 1
    ) |>
    select(
      shot_id, game_id, period,
      defender_id, defender_name, position, team,
      defender_foul_count, defender_proximity,
      shot_distance_category, shot_made,
      defensive_skill
    )
  
  return(final_dataset)
} 