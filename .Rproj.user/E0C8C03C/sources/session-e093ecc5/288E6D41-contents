library(lubridate)
library(hoopR)
library(tidyverse)
library(janitor)
library(nanoparquet)
library(zoo)

# Find the most recent game date
player_logs <- nba_leaguegamelog(season = year_to_season(most_recent_nba_season() - 1), 
                                    player_or_team = "P") %>%
      pluck("LeagueGameLog") %>%
      clean_names() %>%
      mutate(team_location = ifelse(str_detect(matchup, "\\@"), "away", "home"),
             across(c(player_id, team_id), as.numeric)) |> 
      filter(game_date == Sys.Date() - 1)

# Only proceed if we have new games to process
if (dim(player_logs)[1] > 0) {
  # Add a try-catch block for the main processing
    game_dates <- Sys.Date() - 1
    
    
    # Create filename for today's data
    def_dash_filename <- paste0(format(game_dates, "%y%m%d"), "_defender_dashboard.parquet")
    closest_def_filename <- paste0(format(game_dates, "%y%m%d"), "_closest_defender_shooting_dashboard.parquet")

    function_defense_dash <- function(game_dates, periods){
      nba_playerdashptshotdefend(
        date_from = game_dates,
        date_to = game_dates,
        game_segment = "",
        last_n_games = 0,
        league_id = "00",
        location = "",
        month = 0,
        opponent_team_id = 0,
        outcome = "",
        per_mode = "Totals",
        period = periods,
        player_id = 0,
        season = year_to_season(most_recent_nba_season() - 1),
        season_segment = "",
        season_type = "Regular Season",
        team_id = 0,
        vs_conference = "",
        vs_division = ""
        ) |> 
        pluck("DefendingShots") |>
        mutate(date = game_dates,
              period = periods)
    }

    game_dates <- player_logs %>%
      distinct(game_date) |> 
      pull(game_date)

    periods <- c(1:6)

    game_by_period <- expand_grid(game_dates, periods)

    def_dash <- game_by_period |> 
      pmap(possibly(function_defense_dash, NULL)) 

    def_dash <- def_dash |> list_rbind()

    # creating the closest defender shooting dashboard

    function_closest_defender_shooting_dash <- function(game_dates, periods){
      nba_playerdashptshots(
        date_from = game_dates,
        date_to = game_dates,
        game_segment = "",
        last_n_games = 0,
        league_id = "00",
        location = "",
        month = 0,
        opponent_team_id = 0,
        outcome = "",
        per_mode = "Totals",
        period = periods,
        player_id = 0,
        season = year_to_season(most_recent_nba_season() - 1),
        season_segment = "",
        season_type = "Regular Season",
        team_id = 0,
        vs_conference = "",
        vs_division = ""
        ) |> 
        pluck("ClosestDefenderShooting") |>
        mutate(date = game_dates,
              period = periods)
    }

    closest_defender_shooting_dash <- game_by_period |> 
      pmap(possibly(function_closest_defender_shooting_dash, NULL)) 

    closest_defender_shooting_dash <- closest_defender_shooting_dash |> list_rbind()
    
    function_closest_defender_shooting_dash_10_plus <- function(game_dates, periods){
      nba_playerdashptshots(
        date_from = game_dates,
        date_to = game_dates,
        game_segment = "",
        last_n_games = 0,
        league_id = "00",
        location = "",
        month = 0,
        opponent_team_id = 0,
        outcome = "",
        per_mode = "Totals",
        period = periods,
        player_id = 0,
        season = year_to_season(most_recent_nba_season() - 1),
        season_segment = "",
        season_type = "Regular Season",
        team_id = 0,
        vs_conference = "",
        vs_division = ""
      ) |> 
        pluck("ClosestDefender10ftPlusShooting") |>
        mutate(date = game_dates,
               period = periods)
    }
    
    closest_defender_shooting_dash_10_plus <- game_by_period |> 
      pmap(possibly(function_closest_defender_shooting_dash_10_plus, NULL)) 
    
    closest_defender_shooting_dash_10_plus <- closest_defender_shooting_dash_10_plus |> list_rbind()

    combined_def_file <- "../data/defender_dashboard.parquet"
    if (file.exists(combined_def_file)) {
      existing_def_dash <- read_parquet(combined_def_file)
      def_dash <- bind_rows(existing_def_dash, def_dash)
    }
    write_parquet(def_dash, "../data/defender_dashboard.parquet")
    
    # Read and combine closest defender data
    combined_closest_file <- "../data/closest_defender_shooting_dashboard.parquet"
    if (file.exists(combined_closest_file)) {
      existing_closest <- read_parquet(combined_closest_file)
      closest_defender_shooting_dash <- bind_rows(existing_closest, closest_defender_shooting_dash)
    }
    write_parquet(closest_defender_shooting_dash, "../data/closest_defender_shooting_dashboard.parquet")
    
    combined_closest_10_plus_file <- "../data/closest_defender_shooting_dash_10_plus.parquet"
    if (file.exists(combined_closest_10_plus_file)) {
      existing_closest_10_plus <- read_parquet(combined_closest_10_plus_file)
      closest_defender_shooting_dash_10_plus <- bind_rows(existing_closest_10_plus, closest_defender_shooting_dash_10_plus)
    }
    write_parquet(closest_defender_shooting_dash_10_plus, "../data/closest_defender_shooting_dash_10_plus.parquet")
    
    message("Successfully updated combined files")
    
} else {
  message("No new data to process")
}
