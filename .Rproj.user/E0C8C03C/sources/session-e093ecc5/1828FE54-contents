# Foul Data Processing Functions
# This file contains functions for processing play-by-play data for foul modeling

#' Process Play-by-Play Data
#' 
#' Reads and processes play-by-play data, adding lineup information
#' 
#' @param pbp_data_path Path to the play-by-play data parquet file
#' @return A processed dataframe with lineup information
process_pbp_data <- function(pbp_data_path) {
  pbp_data <- nanoparquet::read_parquet(pbp_data_path)
  
  lineup_pbp <- (
    pbp_data 
    |> group_by(game_id, slug_team)
    |> mutate(stint_home = ifelse(slug_team == team_home, cumsum(msg_type == 8) + 1, NA),
           stint_away = ifelse(slug_team == team_away, cumsum(msg_type == 8) + 1, NA)) 
    |> group_by(game_id) 
    |> mutate(across(starts_with("stint"), ~ na.locf0(., fromLast = TRUE)),
           across(starts_with("stint"), ~ na.locf(.))) 
    |> ungroup() 
    |> pivot_longer(cols = starts_with("lineup"),
                 names_to = "lineup_location",
                 values_to = "lineup",
                 names_prefix = "lineup_")
    |> mutate(pts_team = ifelse(lineup_location == "home", shot_pts_home, shot_pts_away),
           pts_opp = ifelse(lineup_location == "away", shot_pts_home, shot_pts_away),
           poss_team = ifelse(lineup_location == "home", poss_home, poss_away),
           poss_opp = ifelse(lineup_location == "away", poss_home, poss_away),
           slug_team = ifelse(lineup_location == "home", team_home, team_away),
           slug_opp = ifelse(lineup_location == "away", team_home, team_away),
           stint = ifelse(lineup_location == "home", stint_home, stint_away))
  )
  
  return(lineup_pbp)
}

#' Add Foul Information to Play-by-Play Data
#' 
#' Processes play-by-play data to add detailed foul information by player
#' 
#' @param lineup_pbp Processed play-by-play data with lineup information
#' @return A dataframe with detailed foul information by player
add_foul_information <- function(lineup_pbp) {
  pbp_with_fouls_by_player <- (lineup_pbp 
    |> filter(str_detect(description, regex("technical", ignore_case = TRUE)) == FALSE) # excluding technical fouls
    |> mutate(foul_occured_on_make = lead(total_fta) == 1)
    |> mutate(foul_occured_on_make = case_when(lead(foul_occured_on_make) == TRUE ~ 1, .default = foul_occured_on_make))
    |> mutate(foul_occured_on_make = case_when(foul_occured_on_make == 1 & str_detect(description, regex("foul:", ignore_case = T)) ~ 0, .default = foul_occured_on_make))
    |> mutate(fta_awarded_on_make = case_when(foul_occured_on_make == 1 ~ 1, .default = 0))
    |> mutate(fouled_player_on_make = case_when(lead(total_fta, n = 2) == 1 ~ lead(player3, n = 2), .default = NA))
    |> mutate(fouling_player_on_make = case_when(lead(total_fta, n = 2) == 1 ~ lead(player1, n = 2), .default = NA))
    |> mutate(checkr = case_when(fta_awarded_on_make == lead(total_fta, n = 2)~ 1, .default = 0)) # when checkr == fta_awarded_on_make, then this script is working correctly
    |> mutate(fta_awarded_on_miss = case_when(total_fta > 1 ~ total_fta, .default = 0))
    |> mutate(ft_pts_opp = case_when(fta_awarded_on_make == 1 ~ lead(pts_opp, n = 2),
                                   fta_awarded_on_miss > 0 ~ pts_opp,
                                   .default = 0))
    |> mutate(ft_pts_team = case_when(fta_awarded_on_make == 1 ~ lead(pts_team, n = 2),
                                    fta_awarded_on_miss > 0 ~ pts_team,
                                    .default = 0))
    |> separate_longer_delim(cols = lineup, delim = ", ")
    |> rename(player_name = lineup)
    |> mutate(personal_fouls_after_event = case_when(str_detect(description, regex("foul:", ignore_case = T)) & player1 == player_name ~ str_extract(description, regex("\\([^()]*\\)(?![^()]*\\))")), TRUE ~ NA))
    |> mutate(personal_fouls_after_event = case_when(fouling_player_on_make == player_name ~ lead(personal_fouls_after_event, n = 10), .default = personal_fouls_after_event))
    |> mutate(personal_foul_occurance_on_play = case_when(str_detect(description, regex("foul:", ignore_case = T)) | foul_occured_on_make == 1 ~ 1, TRUE ~ 0))
    |> mutate(personal_foul_occurance_on_player = case_when(
      str_detect(description, regex("foul:", ignore_case = T)) & player1 == player_name ~ 1,
      foul_occured_on_make == 1 & fouling_player_on_make == player_name ~ 1,
      .default = 0))
    |> mutate(personal_foul_occurance_on_teammate = case_when(personal_foul_occurance_on_play == 1 & personal_foul_occurance_on_player == 0 ~ 1, .default = 0))
    |> mutate(referee = case_when(str_detect(description, regex("foul:", ignore_case = T)) & player1 == player_name ~ str_extract(description, regex("\\([^()]*\\)(?!.*\\([^()]*\\))")), TRUE ~ NA))
    |> mutate(referee = case_when(fouling_player_on_make == player_name ~ lead(referee, n = 10), .default = referee))
    |> mutate(referee = str_remove_all(referee, "[()]"))
    |> mutate(personal_fouls_after_event = str_extract(personal_fouls_after_event, regex("\\d+")))
    |> mutate(personal_fouls_after_event = as.numeric(personal_fouls_after_event))
    |> group_by(game_id, slug_team, player_name)
    |> fill(personal_fouls_after_event, .direction = "down")
    |> mutate(personal_fouls_after_event = replace_na(personal_fouls_after_event, 0))
    |> mutate(personal_fouls_during_event = case_when(
      personal_foul_occurance_on_player == 1 ~ personal_fouls_after_event - 1,
      .default = personal_fouls_after_event))
    |> mutate(fta_awarded_on_miss = replace_na(fta_awarded_on_miss, 0))
    |> mutate(fta_awarded_on_miss = as.numeric(fta_awarded_on_miss))
    |> mutate(game_id = as.character(game_id))
    |> mutate(total_pts_team = case_when(fta_awarded_on_miss > 0 ~ pts_team,
                                       fta_awarded_on_make > 0 ~ pts_team + ft_pts_team,
                                       .default = pts_team),
            total_pts_opp = case_when(fta_awarded_on_miss > 0 ~ pts_opp,
                                      fta_awarded_on_make > 0 ~ pts_opp + ft_pts_opp,
                                      .default = pts_opp))
  )
  
  return(pbp_with_fouls_by_player)
}

#' Prepare Model Data
#' 
#' Filters and prepares data for modeling
#' 
#' @param pbp_with_fouls_by_player Play-by-play data with foul information
#' @return A filtered dataframe ready for modeling
prepare_model_data <- function(pbp_with_fouls_by_player) {
  model_data <- (
    pbp_with_fouls_by_player 
    |> filter((fta_awarded_on_miss > 0 | str_detect(description, regex("shot", ignore_case = T))) &
             !str_detect(description, regex("clock", ignore_case = T)))
    |> filter(slug_team != off_slug_team)
    |> filter(garbage_time == 0)
    |> group_by(game_id, number_event) 
    |> mutate(event_id = cur_group_id())
    |> mutate(
        teammate_name = map(1:n(), ~player_name[-.x]),
        teammate_fouls = map(1:n(), ~personal_fouls_during_event[-.x])
    )
    |> unnest(cols = c(teammate_name, teammate_fouls))
    |> select(event_id, game_id, period, description, slug_team, slug_opp, number_event, act_type, player_name, personal_fouls_during_event, teammate_name, teammate_fouls, personal_foul_occurance_on_player)
  )
  
  return(model_data)
}

#' Filter Out Games with Missing Players
#' 
#' Removes games where unrostered players appeared
#' 
#' @param model_data Prepared model data
#' @param missing_data_path Path to the file containing information about missing data by game
#' @return Filtered model data
filter_missing_player_games <- function(model_data, missing_data_path) {
  games_with_any_missing <- nanoparquet::read_parquet(missing_data_path) |> 
    filter(missing_data_shots > 0) |> 
    pull(game_id)
  
  model_data_filtered <- model_data |> 
    mutate(game_id = as.numeric(game_id)) |> 
    filter(!game_id %in% games_with_any_missing)
  
  return(model_data_filtered)
}

#' Create Stratified Sample
#' 
#' Creates a stratified sample of the data for modeling
#' 
#' @param model_data Prepared model data
#' @param sample_fraction Fraction of data to sample (default: 0.05)
#' @param seed Random seed for reproducibility (default: 123)
#' @return A stratified sample of the data
create_stratified_sample <- function(model_data, sample_fraction = 0.05, seed = 123) {
  set.seed(seed)
  
  # Creating a stratified sample
  stratified_sample <- (model_data 
    # First level of stratification: by game and period
    # This preserves the game context and time dynamics
    |> group_by(game_id, period) 
    
    # Second level: by defensive team and player foul status
    # This ensures we capture the team defensive styles and player foul situations
    |> group_by(slug_team, personal_fouls_during_event > 0, .add = TRUE) 
    
    # Third level: by foul occurrence (most important to preserve)
    # This ensures we have proper representation of the rare positive cases
    |> group_by(personal_foul_occurance_on_player, .add = TRUE) 
    
    # Sample from each stratum
    |> sample_frac(sample_fraction) 
    
    # Remove grouping
    |> ungroup()
  )
  
  return(stratified_sample)
}

#' Scale Features in Sample
#' 
#' Scales numerical features in the sample
#' 
#' @param stratified_sample Stratified sample data
#' @return Sample with scaled features
scale_sample_features <- function(stratified_sample) {
  stratified_sample_scaled <- stratified_sample |>
    mutate(
      teammate_fouls_scaled = scale(teammate_fouls)[,1],
      personal_fouls_scaled = scale(personal_fouls_during_event)[,1]
    )
  
  return(stratified_sample_scaled)
}

#' Create Validation Sample
#' 
#' Creates a validation sample from the remaining data
#' 
#' @param model_data Full model data
#' @param stratified_sample Stratified sample to exclude
#' @param validation_fraction Fraction of remaining data to use for validation (default: 0.01)
#' @return A validation sample
create_validation_sample <- function(model_data, stratified_sample, validation_fraction = 0.01) {
  # Creating a validation set from the remaining data
  remaining_data <- model_data |>
    anti_join(stratified_sample, by = "event_id")
  
  validation_sample <- remaining_data |>
    # Using the same stratification approach for consistency
    group_by(game_id, period, slug_team, 
             personal_fouls_during_event > 0, 
             personal_foul_occurance_on_player) |>
    sample_frac(validation_fraction) |>
    ungroup()
  
  # Scaling the validation sample
  validation_sample_scaled <- validation_sample |>
    mutate(
      teammate_fouls_scaled = scale(teammate_fouls)[,1],
      personal_fouls_scaled = scale(personal_fouls_during_event)[,1]
    )
  
  return(validation_sample_scaled)
}

#' Join Player Information
#' 
#' Joins player logs and roster information to the samples
#' 
#' @param sample_data Sample data to join with player information
#' @param player_logs Player game logs
#' @param rosters Team rosters
#' @return Sample data with player information joined
join_player_information <- function(sample_data, player_logs, rosters) {
  player_logs_for_join <- (
    player_logs
    |> select(player_name, player_id, team_id, team_abbreviation, game_id)
    |> ungroup()
    |> rename(slug_team = team_abbreviation)
    |> select(player_name, game_id, player_id)
    |> mutate(game_id = as.numeric(game_id))
  )
  
  sample_with_player_info <- (sample_data
    |> mutate(game_id = as.numeric(game_id))
    |> left_join(player_logs_for_join, by = join_by("player_name", "game_id"))
    |> left_join(rosters, by = join_by("player_id"))
  )
  
  return(sample_with_player_info)
}

#' Save Processed Samples
#' 
#' Saves the processed samples to parquet files
#' 
#' @param stratified_sample_scaled Scaled stratified sample
#' @param validation_sample_scaled Scaled validation sample
#' @param stratified_output_path Path to save the stratified sample
#' @param validation_output_path Path to save the validation sample
save_processed_samples <- function(stratified_sample_scaled, validation_sample_scaled, 
                                  stratified_output_path = "samples/stratified_sample_scaled.parquet",
                                  validation_output_path = "samples/validation_sample_scaled.parquet") {
  # Create samples directory if it doesn't exist
  dir.create("samples", showWarnings = FALSE, recursive = TRUE)
  
  # Saving the stratified sample for future use
  write_parquet(stratified_sample_scaled, stratified_output_path)
  
  # Saving the validation sample for future use
  write_parquet(validation_sample_scaled, validation_output_path)
}

#' Process NBA League Game Log
#' 
#' Retrieves and processes NBA league game logs
#' 
#' @param season NBA season (default: "2024-25")
#' @return Processed player game logs
get_player_logs <- function(season = "2024-25") {
  player_logs <- nba_leaguegamelog(season = season, player_or_team = "P") %>%
    pluck("LeagueGameLog") %>%
    janitor::clean_names() %>%
    mutate(team_location = ifelse(str_detect(matchup, "\\@"), "away", "home"),
           across(c(player_id, team_id), as.numeric))
  
  return(player_logs)
}

#' Load Rosters
#' 
#' Loads team rosters from a parquet file
#' 
#' @param roster_path Path to the roster parquet file
#' @return Roster data
load_rosters <- function(roster_path) {
  rosters <- nanoparquet::read_parquet(roster_path)
  return(rosters)
}

#' Full Data Processing Pipeline
#' 
#' Runs the complete data processing pipeline from raw data to saved samples
#' 
#' @param pbp_data_path Path to play-by-play data
#' @param missing_data_path Path to missing data information
#' @param roster_path Path to roster data
#' @param sample_fraction Fraction of data to sample (default: 0.05)
#' @param validation_fraction Fraction of remaining data for validation (default: 0.01)
#' @param seed Random seed (default: 123)
#' @param stratified_output_path Path to save stratified sample
#' @param validation_output_path Path to save validation sample
#' @return List containing the processed samples
process_foul_data <- function(
  pbp_data_path = "data/250225_pbp_gt.parquet",
  missing_data_path = "data/missing_data_by_game.parquet",
  roster_path = "data/250227_rosters.parquet",
  sample_fraction = 0.05,
  validation_fraction = 0.01,
  seed = 123,
  stratified_output_path = "samples/stratified_sample_scaled.parquet",
  validation_output_path = "samples/validation_sample_scaled.parquet"
) {
  # Process play-by-play data
  lineup_pbp <- process_pbp_data(pbp_data_path)
  
  # Add foul information
  pbp_with_fouls <- add_foul_information(lineup_pbp)
  
  # Prepare model data
  model_data <- prepare_model_data(pbp_with_fouls)
  
  # Filter out games with missing players
  model_data_filtered <- filter_missing_player_games(model_data, missing_data_path)
  
  # Create stratified sample
  stratified_sample <- create_stratified_sample(model_data, sample_fraction, seed)
  
  # Scale features
  stratified_sample_scaled <- scale_sample_features(stratified_sample)
  
  # Create validation sample
  validation_sample_scaled <- create_validation_sample(model_data, stratified_sample, validation_fraction)
  
  # Load rosters and player logs
  rosters <- load_rosters(roster_path)
  player_logs <- get_player_logs()
  
  # Join player information
  stratified_sample_with_player_info <- join_player_information(stratified_sample_scaled, player_logs, rosters)
  validation_sample_with_player_info <- join_player_information(validation_sample_scaled, player_logs, rosters)
  
  # Drop NA values from validation sample
  validation_sample_with_player_info <- validation_sample_with_player_info |> drop_na()
  
  # Save processed samples
  save_processed_samples(
    stratified_sample_with_player_info,
    validation_sample_with_player_info,
    stratified_output_path,
    validation_output_path
  )
  
  # Return the processed samples
  return(list(
    stratified_sample = stratified_sample_with_player_info,
    validation_sample = validation_sample_with_player_info
  ))
} 