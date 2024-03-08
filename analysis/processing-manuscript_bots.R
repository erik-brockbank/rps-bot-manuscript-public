#
# This script contains the data processing of the *stable bot* experiment
# for the rps bot journal submission
#



setwd("/Users/erikbrockbank/dev/research/vullab/rps-bot-manuscript-public/analysis")
library(tidyverse)


# GLOBALS ====
DATA_PATH = "../data" # pathway to data file
DATA_FILE = "rps_v2_data.csv" # name of file containing full dataset for all rounds
GAME_ROUNDS = 300
STRATEGY_LEVELS = c("prev_move_positive", "prev_move_negative",
                    "opponent_prev_move_positive", "opponent_prev_move_nil",
                    "win_nil_lose_positive", "win_positive_lose_negative",
                    "outcome_transition_dual_dependency")


# READ DATA ====
# Note: the raw data csv has more unique participant data than we keep in this function.
# Some participants did not complete the full set of rounds.
# Several managed to complete 300 rounds twice with the same survey code.
# In this instance, we keep the earlier of the two to ensure parity with other participants.
# One participant emailed saying she had completed the full set of rounds, but we don't have complete data.
# All told, 218 students were given credit; the below produces 217 complete participant data sets.
read_bot_data = function(filename, strategy_levels, game_rounds) {
  # Read csv
  data = read_csv(filename)
  data$bot_strategy = factor(data$bot_strategy, levels = strategy_levels)
  # Remove all incomplete games
  incomplete_games = data %>%
    group_by(game_id, player_id) %>%
    summarize(rounds = max(round_index)) %>%
    filter(rounds < game_rounds) %>%
    select(game_id) %>%
    unique()
  data = data %>%
    filter(!(game_id %in% incomplete_games$game_id))
  # Remove any duplicate complete games that have the same completion code
  # NB: this can happen if somebody played all the way through but exited before receiving credit
  # First, fetch survey codes with multiple complete games
  repeat_codes = data %>%
    group_by(sona_survey_code) %>%
    filter(is_bot == 0) %>%
    summarize(trials = n()) %>%
    filter(trials > game_rounds) %>%
    select(sona_survey_code)
  # Next, get game id for the later complete game and remove it (below)
  duplicate_games = data %>%
    filter(sona_survey_code %in% repeat_codes$sona_survey_code &
             is_bot == 0  &
             round_index == game_rounds) %>%
    select(sona_survey_code, game_id, player_id, round_begin_ts) %>%
    group_by(sona_survey_code) %>%
    filter(round_begin_ts == max(round_begin_ts)) %>%
    distinct(game_id)
  data = data %>%
    filter(!game_id %in% duplicate_games$game_id)

  return(data)
}


# Read data
bot_data = read_bot_data(paste(DATA_PATH, DATA_FILE, sep = "/"),
                         STRATEGY_LEVELS,
                         GAME_ROUNDS)
# Save to RData
save(bot_data, file = paste(DATA_PATH, "rps_v2_data.RData", sep = "/"))
