# Load required libraries
library(hoopR)
library(tidyverse)
library(nanoparquet)
library(janitor)
library(zoo)
library(here)

# Load player logs data
player_logs <- nba_leaguegamelog(season = "2024-25", player_or_team = "P") %>%
  pluck("LeagueGameLog") %>%
  clean_names() %>%
  mutate(team_location = ifelse(str_detect(matchup, "\\@"), "away", "home"),
         across(c(player_id, team_id), as.numeric))

# Load play-by-play data
pbp_df <- read_parquet(here("data", "250225_pbp_gt.parquet"))

# Add foul information to the pbp data
pbp_df <- pbp_df %>%
  mutate(fouling_player = case_when(
    str_detect(description, regex("foul:", ignore_case = T)) ~ player1, 
    .default = NA
  )) %>%
  mutate(fouled_player = case_when(
    str_detect(description, regex("foul:", ignore_case = T)) ~ player3, 
    .default = NA
  )) %>%
  mutate(personal_fouls_after_event = case_when(
    !is.na(fouling_player) ~ str_extract(description, regex("\\([^()]*\\)(?![^()]*\\))"))
  )) %>%
  mutate(personal_fouls_after_event = str_extract(personal_fouls_after_event, regex("\\d+")))

# Create dataframe by defender
pbp_df_by_defender <- pbp_df %>%
  group_by(game_id, slug_team) %>%
  mutate(
    stint_home = ifelse(slug_team == team_home, cumsum(msg_type == 8) + 1, NA),
    stint_away = ifelse(slug_team == team_away, cumsum(msg_type == 8) + 1, NA)
  ) %>%
  group_by(game_id) %>%
  mutate(
    across(starts_with("stint"), ~ na.locf0(., fromLast = TRUE)),
    across(starts_with("stint"), ~ na.locf(.))
  ) %>%
  ungroup() %>%
  pivot_longer(
    cols = starts_with("lineup"),
    names_to = "lineup_location",
    values_to = "lineup",
    names_prefix = "lineup_"
  ) %>%
  mutate(
    pts_team = ifelse(lineup_location == "home", shot_pts_home, shot_pts_away),
    pts_opp = ifelse(lineup_location == "away", shot_pts_home, shot_pts_away),
    poss_team = ifelse(lineup_location == "home", poss_home, poss_away),
    poss_opp = ifelse(lineup_location == "away", poss_home, poss_away),
    slug_team = ifelse(lineup_location == "home", team_home, team_away),
    slug_opp = ifelse(lineup_location == "away", team_home, team_away),
    stint = ifelse(lineup_location == "home", stint_home, stint_away)
  ) %>%
  filter(off_slug_team != slug_team) %>%
  separate_longer_delim(cols = lineup, delim = ", ") %>%
  rename(player_name = lineup) %>%
  mutate(
    personal_fouls_after_event = case_when(
      fouling_player == player_name ~ personal_fouls_after_event, 
      .default = NA
    )
  ) %>%
  mutate(personal_fouls_after_event = as.numeric(personal_fouls_after_event)) %>%
  group_by(game_id, slug_team, player_name) %>%
  fill(personal_fouls_after_event) %>%
  ungroup() %>%
  mutate(personal_fouls_after_event = ifelse(is.na(personal_fouls_after_event), 0, personal_fouls_after_event)) %>%
  group_by(game_id, slug_team, player_name) %>%
  mutate(
    personal_fouls_during_event = case_when(
      fouling_player == player_name ~ personal_fouls_after_event - 1,
      TRUE ~ personal_fouls_after_event
    )
  ) %>%
  ungroup()

# Create shot-only dataframe
shot_pbp_by_defense <- pbp_df_by_defender %>%
  filter(str_detect(description, regex("shot", ignore_case = T)) & 
         !str_detect(description, regex("clock", ignore_case = T))) %>%
  mutate(shot_type = case_when(
    str_detect(description, "3pt") ~ "3pt",
    TRUE ~ "2pt"
  )) %>%
  rename(known_defender = player3)

# Create shot and steal dataframe
shot_and_steal_pbp_by_defense <- pbp_df_by_defender %>%
  filter(
    (str_detect(description, regex("shot", ignore_case = T)) & 
     !str_detect(description, regex("clock", ignore_case = T))) | 
    str_detect(description, regex("steal", ignore_case = T))
  ) %>%
  mutate(shot_type = case_when(
    str_detect(description, "3pt") ~ "3pt",
    TRUE ~ "2pt"
  )) %>%
  rename(known_defender = player3)

# Function to get team rosters
function_team_stats <- function(x) {
  tryCatch({
    # small delay to avoid rate limiting
    Sys.sleep(0.5)
    
    # gets the current season
    current_season <- "2024-25"
    
    roster <- nba_commonteamroster(
      team_id = x,
      season = current_season
    )
    
    # transforms the result into a data frame with required columns
    result <- roster$CommonTeamRoster %>%
      mutate(
        league_id = "00",
        season = current_season,
        team_id = x
      )
    
    return(result)
  }, error = function(e) {
    warning(sprintf("Error processing team_id %s: %s", x, e$message))
    return(NULL)
  })
}

# Get team rosters
teams <- player_logs %>% distinct(team_id) %>% pull(team_id)
team_stats <- map_df(teams, function_team_stats, .progress = TRUE)

# Clean team stats
team_stats <- team_stats %>%
  clean_names() %>%
  select(team_id, player_id, position, height, weight, age, exp) %>%
  mutate(
    experience = case_when(exp == "R" ~ 0, TRUE ~ as.numeric(exp))
  ) %>%
  mutate(
    player_id = as.numeric(player_id),
    team_id = as.numeric(team_id)
  )

# Save team stats
write_parquet(team_stats, here("data", "250227_rosters.parquet"))

# Prepare player logs for joining
player_logs_for_join <- player_logs %>%
  select(player_name, player_id, team_id, team_abbreviation, game_id) %>%
  ungroup() %>%
  rename(slug_team = team_abbreviation) %>%
  mutate(game_id = as.numeric(game_id))

# Join shot data with player information
shot_pbp_by_defense <- shot_pbp_by_defense %>%
  left_join(player_logs_for_join, by = join_by("player_name", "slug_team", "game_id")) %>%
  rename(
    defender_name = player_name,
    defender_id = player_id,
    player_name = player1
  ) %>%
  left_join(player_logs_for_join, by = join_by("player_name", "game_id")) %>%
  rename(
    offender_id = player_id,
    offender_name = player_name,
    player_name = player2
  ) %>%
  left_join(player_logs_for_join, by = join_by("player_name", "game_id")) %>%
  rename(
    assister_id = player_id,
    assister_name = player_name
  )

# Join with team stats for defender attributes
shot_pbp_by_defense <- shot_pbp_by_defense %>%
  left_join(team_stats, by = join_by("defender_id" == "player_id"))

# Find players without anthropometric data
no_anthro_players <- shot_pbp_by_defense %>% 
  filter(is.na(height)) %>% 
  select(defender_id) %>% 
  distinct()

# Join shot and steal data with player information
shot_and_steal_pbp_by_defense <- shot_and_steal_pbp_by_defense %>%
  left_join(player_logs_for_join, by = join_by("player_name", "slug_team", "game_id")) %>%
  rename(
    defender_name = player_name,
    defender_id = player_id,
    player_name = player1
  ) %>%
  left_join(player_logs_for_join, by = join_by("player_name", "game_id")) %>%
  rename(
    offender_id = player_id,
    offender_name = player_name,
    player_name = player2
  ) %>%
  left_join(player_logs_for_join, by = join_by("player_name", "game_id")) %>%
  rename(
    assister_id = player_id,
    assister_name = player_name
  )

# Load defender dashboard data
defender_dashboard <- nanoparquet::read_parquet(here("data", "defender_dashboard.parquet"))

defender_dashboard <- defender_dashboard %>%
  rename(defender_id = CLOSE_DEF_playerId) %>%
  filter(G == 1) %>% # G == 1 ensures stats correctly correspond to given games
  clean_names() %>%
  distinct() %>%
  filter(date != "2024-11-17") # Filter out problematic date

# Load closest defender dashboard data
closest_defender_dashboard <- nanoparquet::read_parquet(here("data", "closest_defender_shooting_dashboard.parquet"))

closest_defender_dashboard <- closest_defender_dashboard %>%
  clean_names() %>%
  rename(offender_id = player_id) %>%
  filter(g == 1) %>% # g == 1 ensures stats correctly correspond to given games
  distinct()

# Load closest defender dashboard 10+ feet data
closest_defender_dashboard_10_plus <- nanoparquet::read_parquet(here("data", "closest_defender_shooting_dash_10_plus.parquet"))

closest_defender_dashboard_10_plus <- closest_defender_dashboard_10_plus %>%
  clean_names() %>%
  rename(offender_id = player_id) %>%
  filter(g == 1) %>% # g == 1 ensures stats correctly correspond to given games
  distinct()

# Process closest defender dashboard 10+ feet data
closest_defender_dashboard_10_plus <- closest_defender_dashboard_10_plus %>%
  select(date, period, offender_id, close_def_dist_range, fgm, fga, fg2m, fg2a, fg3m, fg3a) %>%
  rename_with(
    ~ paste0(.x, "_10_plus"),
    starts_with("f")
  )

# Join and process closest defender dashboard data
closest_defender_dashboard <- closest_defender_dashboard %>%
  select(date, period, offender_id, close_def_dist_range, fgm, fga, fg2m, fg2a, fg3m, fg3a) %>%
  left_join(
    closest_defender_dashboard_10_plus, 
    by = join_by("date", "period", "offender_id", "close_def_dist_range")
  ) %>%
  mutate(across(fgm:fg3a_10_plus, ~ as.numeric(.))) %>%
  mutate(across(everything(), ~replace_na(., 0)))

# Calculate shots from different distances
closest_defender_dashboard <- closest_defender_dashboard %>%
  mutate(
    o_fga_0_through_9_ft = fg2a - fg2a_10_plus,
    o_fgm_0_through_9_ft = fg2m - fg2m_10_plus
  ) %>%
  rename(
    o_fga_10_through_23_ft = fg2a_10_plus,
    o_fgm_10_through_23_ft = fg2m_10_plus,
    o_fga_24_plus = fg3a_10_plus,
    o_fgm_24_plus = fg3m_10_plus
  ) %>%
  select(-starts_with("fg"), -starts_with("fg"))

# Reshape closest defender dashboard data
closest_defender_dashboard <- closest_defender_dashboard %>%
  pivot_longer(
    cols = starts_with("o_"),
    names_to = c("stat_type", "offender_shot_dist_range"),
    names_pattern = "o_(fg[am])_(.+)",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = stat_type,
    values_from = value
  ) %>%
  rename(offender_fga = fga, offender_fgm = fgm) %>%
  mutate(
    game_date = as_date(date),
    offender_id = as.numeric(offender_id)
  ) %>%
  select(-date)

# Process defender dashboard data
defender_dashboard <- defender_dashboard %>%
  select(date, period, defender_id, defense_category, d_fga, d_fgm) %>%
  mutate(d_fga = as.numeric(d_fga), d_fgm = as.numeric(d_fgm)) %>%
  pivot_wider(
    names_from = defense_category, 
    values_from = c(d_fgm, d_fga), 
    values_fill = 0
  ) %>%
  clean_names()

# Calculate detailed shot distance breakdowns
defender_dashboard <- defender_dashboard %>%
  mutate(
    # Calculate 15-24 ft shots
    d_fga_16_through_23_ft = d_fga_greater_than_15_ft - d_fga_3_pointers,
    d_fgm_16_through_23_ft = d_fgm_greater_than_15_ft - d_fgm_3_pointers,
    # Calculate 6-10 ft shots
    d_fga_6_through_9_ft = d_fga_less_than_10_ft - d_fga_less_than_6_ft,
    d_fgm_6_through_9_ft = d_fgm_less_than_10_ft - d_fgm_less_than_6_ft,
    # Calculate 10-15 ft shots
    d_fga_10_through_15_ft = d_fga_2_pointers - (d_fga_less_than_6_ft + d_fga_6_through_9_ft + d_fga_16_through_23_ft),
    d_fgm_10_through_15_ft = d_fgm_2_pointers - (d_fgm_less_than_6_ft + d_fgm_6_through_9_ft + d_fgm_16_through_23_ft),
    # Calculate 24+ ft shots
    d_fga_24_plus = d_fga_greater_than_15_ft - d_fga_16_through_23_ft,
    d_fgm_24_plus = d_fgm_greater_than_15_ft - d_fgm_16_through_23_ft,
    # Add verification columns
    fga_2_pt_check = d_fga_2_pointers - (d_fga_less_than_6_ft + d_fga_6_through_9_ft + d_fga_10_through_15_ft + d_fga_16_through_23_ft),
    fgm_2_pt_check = d_fgm_2_pointers - (d_fgm_less_than_6_ft + d_fgm_6_through_9_ft + d_fgm_10_through_15_ft + d_fgm_16_through_23_ft),
    fga_3_pt_check = d_fga_3_pointers - d_fga_24_plus,
    fgm_3_pt_check = d_fgm_3_pointers - d_fgm_24_plus,
    fga_overall_check = d_fga_overall - (d_fga_less_than_6_ft + d_fga_6_through_9_ft + d_fga_10_through_15_ft + d_fga_16_through_23_ft + d_fga_24_plus),
    fgm_overall_check = d_fgm_overall - (d_fgm_less_than_6_ft + d_fgm_6_through_9_ft + d_fgm_10_through_15_ft + d_fgm_16_through_23_ft + d_fgm_24_plus)
  )

# Verify calculations
verification_sums <- defender_dashboard %>% 
  summarise(
    sum(fga_2_pt_check),
    sum(fgm_2_pt_check),
    sum(fga_3_pt_check),
    sum(fgm_3_pt_check),
    sum(fga_overall_check),
    sum(fgm_overall_check)
  )

# Final reshaping of defender dashboard data
defender_dashboard <- defender_dashboard %>% 
  select(-starts_with("fga_"), -starts_with("fgm_")) %>%
  pivot_longer(
    cols = starts_with("d_"),
    names_to = c("stat_type", "defender_shot_dist_range"),
    names_pattern = "d_(fg[am])_(.+)",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = stat_type,
    values_from = value
  ) %>%
  filter(
    defender_shot_dist_range == "less_than_6_ft" | 
    defender_shot_dist_range == "6_through_9_ft" | 
    defender_shot_dist_range == "10_through_15_ft" | 
    defender_shot_dist_range == "16_through_23_ft" | 
    defender_shot_dist_range == "24_plus"
  ) %>%
  rename(defender_fga = fga, defender_fgm = fgm) %>%
  mutate(
    game_date = as_date(date),
    defender_id = as.numeric(defender_id)
  ) %>%
  select(-date)

# Get shot chart data
first_date <- player_logs %>%
  distinct(game_date) %>%
  arrange(game_date) %>%
  head(1)

shot_df <- nba_shotchartdetail(
  context_measure = "FGA",
  date_from = first_date$game_date,
  date_to = Sys.Date() - 1,
  game_id = "",
  game_segment = "",
  last_n_games = 0,
  league_id = "00",
  location = "",
  month = 0,
  opponent_team_id = 0,
  outcome = "",
  period = 0,
  player_id = 0,
  player_position = "",
  rookie_year = "",
  season = year_to_season(most_recent_nba_season() - 1),
  season_segment = "",
  season_type = "Regular Season",
  team_id = 0,
  vs_conference = "",
  vs_division = ""
) %>% 
  pluck("Shot_Chart_Detail") %>% 
  clean_names()

# Clean and format shot_df
shot_df <- shot_df %>% 
  mutate(
    game_id = as.numeric(game_id),
    period = as.numeric(period), 
    game_date = as_date(game_date),
    number_original = as.numeric(game_event_id),
    shot_distance = as.numeric(shot_distance)
  ) %>%
  select(-shot_type) %>%
  # Add shot distance categories
  mutate(
    # Categorize shots for offensive analysis
    offender_shot_dist_range = case_when(
      shot_distance < 10 ~ "0_through_9_ft",
      shot_distance >= 10 & shot_distance < 24 ~ "10_through_23_ft", 
      TRUE ~ "24_plus"
    ),
    # Categorize shots for defensive analysis
    defender_shot_dist_range = case_when(
      shot_distance < 6 ~ "less_than_6_ft",
      shot_distance >= 6 & shot_distance < 10 ~ "6_through_9_ft",
      shot_distance >= 10 & shot_distance <= 15 ~ "10_through_15_ft",
      shot_distance > 15 & shot_distance < 24 ~ "16_through_23_ft",
      TRUE ~ "24_plus"
    )
  )

# Join with defender data and create base dataset
shot_df_test <- shot_df %>%
  left_join(
    shot_pbp_by_defense, 
    by = join_by("game_id", "number_original", "period", "game_date")
  ) %>%
  select(
    # Game info
    game_date, game_id, slug_team.x, slug_opp, period, number_original,
    # Play details  
    clock, offender_name, offender_id, assister_name, assister_id,
    known_defender, locX, locY, defender_name, defender_id,
    possession, minutes_remaining, seconds_remaining,
    # Shot details
    event_type, action_type, shot_zone_basic, shot_zone_area, shot_zone_range,
    shot_distance, shot_attempted_flag, shot_made_flag, shot_type,
    offender_shot_dist_range, defender_shot_dist_range,
    # Player attributes
    position, height, weight, age, experience,
    personal_fouls_during_event, garbage_time
  ) %>%
  rename(
    slug_team_def = slug_team.x,
    slug_team_off = slug_opp
  ) %>%
  # Remove garbage time plays
  filter(garbage_time == 0)

# Analyze missing data patterns
missing_data_by_game <- shot_df_test %>%
  left_join(
    closest_defender_dashboard,
    by = join_by("game_date", "period", "offender_id", "offender_shot_dist_range")
  ) %>%
  mutate(has_close_def_data = !is.na(close_def_dist_range)) %>%
  group_by(game_id) %>%
  summarize(
    total_shots = n(),
    missing_data_shots = sum(!has_close_def_data),
    pct_missing = missing_data_shots / total_shots * 100
  ) %>%
  arrange(desc(pct_missing))

# Identify games with any missing data
games_with_any_missing <- missing_data_by_game %>%
  filter(missing_data_shots > 0) %>%
  pull(game_id)

# Save missing data analysis
write_parquet(missing_data_by_game, here("data", "missing_data_by_game.parquet"))

# Create offensive perspective dataset
shots_with_closest_defender_and_foul_data_by_shot <- shot_df_test %>%
  # Remove all games with any missing data
  filter(!(game_id %in% games_with_any_missing)) %>%
  # Join with closest defender dashboard
  left_join(
    closest_defender_dashboard,
    by = join_by("game_date", "period", "offender_id", "offender_shot_dist_range")
  ) %>%
  # Handle any remaining missing values
  mutate(
    across(ends_with("fgm"), ~replace_na(., 0)),
    across(ends_with("fga"), ~replace_na(., 0))
  ) %>%
  # Group and summarize
  group_by(
    # Game context
    game_date, game_id, period,
    # Team information
    shooting_team = slug_team_off, 
    defending_team = slug_team_def,
    # Player matchup
    shooter = offender_name, 
    defender = defender_name,
    # Shot characteristics
    shot_distance_category = offender_shot_dist_range, 
    defender_proximity = close_def_dist_range,
    # Defender attributes
    defender_position = position, 
    defender_height = height, 
    defender_weight = weight, 
    defender_foul_count = personal_fouls_during_event
  ) %>%
  summarize(
    # Shot outcomes
    shots_attempted = sum(offender_fga),
    shots_made = sum(offender_fgm),
    # Keep any other metrics that end with fgm/fga
    across(ends_with("fgm") & !matches("^offender_fgm$"), sum),
    across(ends_with("fga") & !matches("^offender_fga$"), sum),
    .groups = 'drop'
  )

# Create defensive perspective dataset
shots_defended_with_distance_and_foul_data <- shot_df_test %>%
  # Remove all games with any missing data
  filter(!(game_id %in% games_with_any_missing)) %>%
  # Join with defender dashboard
  left_join(
    defender_dashboard,
    by = join_by("game_date", "period", "defender_id", "defender_shot_dist_range")
  ) %>%
  # Handle any remaining missing values
  mutate(
    across(ends_with("fgm"), ~replace_na(., 0)),
    across(ends_with("fga"), ~replace_na(., 0))
  ) %>%
  # Group and summarize
  group_by(
    # Game context
    game_date, game_id, period,
    # Team information
    offensive_team = slug_team_off,
    defensive_team = slug_team_def,
    # Player matchup
    shooter = offender_name,
    primary_defender = defender_name,
    # Defensive context
    defense_distance_category = defender_shot_dist_range,
    # Defender attributes
    defender_position = position,
    defender_height = height,
    defender_weight = weight,
    defender_foul_count = personal_fouls_during_event
  ) %>%
  summarize(
    # Shot outcomes from defender perspective
    shots_defended_total = sum(defender_fga),
    shots_defended_made = sum(defender_fgm),
    # Keep any other metrics that end with fgm/fga
    across(ends_with("fgm") & !matches("^defender_fgm$"), sum),
    across(ends_with("fga") & !matches("^defender_fga$"), sum),
    .groups = 'drop'
  )

# Calculate summary statistics for the clean dataset
closest_defender_stats <- shots_with_closest_defender_and_foul_data_by_shot %>%
  summarize(
    num_records = n(),
    num_games = n_distinct(game_id),
    num_players = n_distinct(c(shooter, defender)),
    total_shots = sum(shots_attempted),
    avg_fg_pct = sum(shots_made) / sum(shots_attempted) * 100
  )

# Print statistics
print("Closest defender dataset statistics (all games with any missing data removed):")
print(closest_defender_stats)

defender_stats <- shots_defended_with_distance_and_foul_data %>%
  summarize(
    num_records = n(),
    num_games = n_distinct(game_id),
    num_players = n_distinct(c(shooter, primary_defender)),
    total_shots = sum(shots_defended_total),
    avg_fg_pct = sum(shots_defended_made) / sum(shots_defended_total) * 100
  )

print("Defender distance from basket dataset statistics (all games with any missing data removed):")
print(defender_stats)

# Create team-level aggregation
team_defense_by_distance_foul_and_position <- shots_with_closest_defender_and_foul_data_by_shot %>%
  group_by(
    defending_team, period,
    shot_distance_category, defender_proximity,
    defender_position, defender_foul_count
  ) %>%
  summarize(
    across(c(shots_made, shots_attempted), sum),
    .groups = 'drop'
  ) %>%
  # Scale fouls for modeling
  mutate(
    fouls_scaled = as.vector(scale(defender_foul_count))
  )

# Save processed datasets
nanoparquet::write_parquet(
  shots_with_closest_defender_and_foul_data_by_shot,
  here("data", "shots_offensive_perspective.parquet")
)

nanoparquet::write_parquet(
  shots_defended_with_distance_and_foul_data,
  here("data", "shots_defensive_perspective.parquet")
)  

# Create non-aggregated test dataset for detailed analysis
non_aggregated_data <- shot_df_test |>
  # Only include games with complete data
  filter(!(game_id %in% games_with_any_missing)) |>
  left_join(
    defender_dashboard,
    by = join_by("game_date", "period", "defender_id", "defender_shot_dist_range")
  ) |>
  left_join(
    closest_defender_dashboard,
    by = join_by("game_date", "period", "offender_id", "offender_shot_dist_range")
  ) |>
  mutate(
    across(ends_with("fgm"), ~replace_na(., 0)),
    across(ends_with("fga"), ~replace_na(., 0)),
    shot_made_flag = as.numeric(shot_made_flag)
  ) |>
  select(
    game_date:number_original,
    offender_name, defender_name,
    action_type:shot_zone_area,
    shot_distance, shot_made_flag, shot_type,
    offender_shot_dist_range:defender_shot_dist_range,
    close_def_dist_range,
    offender_fga, offender_fgm, defender_fga, defender_fgm,
    position, height, weight, personal_fouls_during_event
  )

# Rename columns in non_aggregated_data for clarity
shot_level_defender_matchup_data <- non_aggregated_data |>
  rename(
    # Game context
    game_date = game_date,
    game_id = game_id,
    period = period,
    play_number = number_original,
    
    # Player matchup
    shooter = offender_name,
    primary_defender = defender_name,

    # Shot details
    shot_action = action_type,
    shot_zone = shot_zone_basic,
    shot_area = shot_zone_area,
    shot_distance_ft = shot_distance,
    shot_made = shot_made_flag,
    shot_type = shot_type,
    shot_distance_category = offender_shot_dist_range,
    defense_distance_category = defender_shot_dist_range,
    defender_proximity = close_def_dist_range,
    
    # Aggregated metrics (kept for reference)
    shooter_fga = offender_fga,
    shooter_fgm = offender_fgm,
    defender_fga = defender_fga,
    defender_fgm = defender_fgm,
    
    # Defender attributes
    defender_position = position,
    defender_height_in = height,
    defender_weight_lb = weight,
    defender_foul_count = personal_fouls_during_event
  )

# Save as parquet file with informative name
nanoparquet::write_parquet(
  shot_level_defender_matchup_data,
  here("data", "shot_level_defender_matchup_data.parquet")
)