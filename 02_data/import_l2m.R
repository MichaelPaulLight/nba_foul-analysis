library(tidyverse)
library(here)

data <- read_csv("https://raw.githubusercontent.com/atlhawksfanatic/L2M/refs/heads/master/1-tidy/L2M/L2M_stats_nba.csv")

nanoparquet::write_parquet(data, here::here("02_data", "02-1_data_raw/ltm_raw.parquet"))