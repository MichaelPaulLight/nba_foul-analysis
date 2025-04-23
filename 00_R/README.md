Current state:

simulate_shots.R is the workhorse for simulation, allows a user to generate synthetic datasets with standardized, unstandardized, or ordered-factor transformations of defender_foul_count

simulate_shots_2.R improves the utility of simulate_shots, adding offensive_team_id and shot_movement to the returned dataframe

simulate_shots_3.R improves simulate_shots further, adding more customization arguments that make it easier for users to create more general or more customizable simulations. Users can define their own positions and position effects. This is the version used to prepare report_foul-analysis.qmd