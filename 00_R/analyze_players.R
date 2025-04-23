# analyze_players.R
# Function to analyze defensive performance metrics for NBA players

library(tidyverse)

#' Analyze defensive performance of specified players
#'
#' This function analyzes defensive performance metrics for specified players,
#' including shot success rates by distance category and foul count.
#'
#' @param player_ids Vector of player IDs to analyze
#' @param data Dataframe containing shot data (must include: defender_id, 
#'             shot_distance_category, shot_made, defender_foul_count)
#' @return A list containing:
#'   \item{distance_data}{Dataframe of shot success by distance category}
#'   \item{foul_data}{Dataframe of shot success by foul count}
#'   \item{distance_plot}{ggplot of shot success by distance category}
#'   \item{foul_plot}{ggplot of shot success by foul count}
#'
analyze_players <- function(player_ids, data) {
  # Distance analysis for all players
  distance_analysis <- data %>%
    filter(defender_id %in% player_ids) %>%
    group_by(defender_id, shot_distance_category) %>%
    summarise(
      shots_faced = n(),
      shots_made = sum(shot_made),
      shots_made_pct = mean(shot_made),
      avg_foul_count = mean(defender_foul_count),
      .groups = "drop"
    ) %>%
    arrange(defender_id, shot_distance_category)
  
  # Foul count analysis for all players
  foul_analysis <- data %>%
    filter(defender_id %in% player_ids) %>%
    group_by(defender_id, defender_foul_count) %>%
    summarise(
      shots_faced = n(),
      shots_made = sum(shot_made),
      shots_made_pct = mean(shot_made),
      .groups = "drop"
    ) %>%
    arrange(defender_id, defender_foul_count)
  
  # Create distance comparison plot
  distance_plot <- ggplot(distance_analysis, 
                         aes(x = shot_distance_category, 
                             y = shots_made_pct, 
                             fill = factor(defender_id))) +
    geom_col(position = position_dodge(width = 0.8), alpha = 0.7) +
    geom_text(aes(label = paste0(round(shots_made_pct * 100, 1), "%\n(", 
                                shots_made, "/", shots_faced, ")")),
              position = position_dodge(width = 0.8),
              vjust = -0.5, size = 3) +
    labs(
      title = "Defensive Performance by Shot Distance",
      subtitle = "Percentage of shots made when defended",
      x = "Shot Distance Category",
      y = "Shot Success Rate",
      fill = "Player ID"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Create foul count comparison plot
  foul_plot <- ggplot(foul_analysis, 
                     aes(x = defender_foul_count, 
                         y = shots_made_pct, 
                         color = factor(defender_id))) +
    geom_point(size = 3) +
    geom_line() +
    geom_text(aes(label = paste0(round(shots_made_pct * 100, 1), "%\n(", 
                                shots_made, "/", shots_faced, ")")),
              vjust = -1, size = 3) +
    labs(
      title = "Defensive Performance by Foul Count",
      subtitle = "How foul count affects shot success rate",
      x = "Defender Foul Count",
      y = "Shot Success Rate",
      color = "Player ID"
    ) +
    facet_wrap(vars(defender_id), scales = "free_y") +
    theme_minimal()
  
  # Return both plots and data frames
  return(list(
    distance_data = distance_analysis,
    foul_data = foul_analysis,
    distance_plot = distance_plot,
    foul_plot = foul_plot
  ))
}

# Example usage:
# player_analysis <- analyze_players(c(9, 37, 47), small_sim_unsummarized)
# player_analysis$distance_plot
# player_analysis$foul_plot 