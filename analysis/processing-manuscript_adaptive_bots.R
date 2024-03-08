
#
# This script contains the data processing of the *adaptive bot* experiment for the rps bot journal submission
#


# SETUP ====
# setwd("/Users/erikbrockbank/dev/research/vullab/rps-bot-manuscript-public/analysis")
library(tidyverse)

# Globals
DATA_PATH = "../data"
DATA_FILE = "rps_v3_data.csv" # name of file containing full dataset for all rounds

GAME_ROUNDS = 300 # number of rounds in each complete game
STRATEGY_LEVELS = c( # In order of complexity
  # 1x3
  # "opponent_moves",
  "opponent_transitions",
  "opponent_courn_transitions",
  # 3x3
  "opponent_prev_move",
  "bot_prev_move",
  "opponent_outcome_transitions",
  # 9x3
  "opponent_bot_prev_move",
  "opponent_prev_two_moves",
  # "bot_prev_two_moves",
  "opponent_outcome_prev_transition_dual"
)


# READ DATA ====

bot_data = read_csv(paste(DATA_PATH, DATA_FILE, sep = "/"))
bot_data$bot_strategy = factor(bot_data$bot_strategy, levels = STRATEGY_LEVELS)

# Remove all incomplete games
incomplete_games = bot_data %>%
  group_by(game_id, player_id) %>%
  summarize(rounds = max(round_index)) %>%
  filter(rounds < GAME_ROUNDS) %>%
  select(game_id) %>%
  unique()
incomplete_games

bot_data = bot_data %>%
  filter(!(game_id %in% incomplete_games$game_id))

# Players with "NA" moves
# (processing python script writes NA for empty move values)
tmp = bot_data %>% filter(is.na(player_move))
tmp %>% group_by(sona_survey_code) %>% summarize(n())
bot_data = bot_data %>% filter(!is.na(player_move))


# Remove any duplicate complete games that have the same survey code
# NB: this can happen if somebody played all the way through but exited before receiving credit
# First, fetch sona survey codes with multiple complete games
repeat_codes = bot_data %>%
  group_by(sona_survey_code) %>%
  filter(is_bot == 0) %>%
  summarize(trials = n()) %>%
  filter(trials > GAME_ROUNDS) %>%
  select(sona_survey_code)
repeat_codes
# Next, get game id for the earlier complete game
# NB: commented out code checks that we have slider/free resp data for at least one of the games
duplicate_games = bot_data %>%
  filter(sona_survey_code %in% repeat_codes$sona_survey_code &
           is_bot == 0  &
           round_index == GAME_ROUNDS) %>%
  select(sona_survey_code, game_id, player_id, round_begin_ts) %>%
  # remove the later one to avoid results based on experience
  group_by(sona_survey_code) %>%
  filter(round_begin_ts == max(round_begin_ts)) %>%
  # joins below check whether we have slider/free resp data for earlier or later survey code responses
  # inner_join(fr_data, by = c("game_id", "player_id")) %>%
  # inner_join(slider_data, by = c("game_id", "player_id")) %>%
  distinct(game_id)
duplicate_games

bot_data = bot_data %>%
  filter(!game_id %in% duplicate_games$game_id)


# Sanity check: anybody with trials != 300?
trial_count = bot_data %>%
  filter(is_bot == 0) %>%
  group_by(sona_survey_code) %>%
  summarize(trials = n()) %>%
  filter(trials != GAME_ROUNDS)
trial_count


# Check that there are no rows with memory >= 300
# (this was a bug in early data)
mem = bot_data %>%
  filter(round_index == GAME_ROUNDS & is_bot == 1) %>%
  group_by(bot_strategy, game_id, sona_survey_code) %>%
  select(bot_strategy, game_id, sona_survey_code, bot_round_memory)

mem = mem %>%
  rowwise() %>%
  mutate(memory_sum =
           sum(as.numeric(unlist(regmatches(bot_round_memory, gregexpr("[[:digit:]]+", bot_round_memory))))))

mem = mem %>% filter(memory_sum >= GAME_ROUNDS)

bot_data = bot_data %>%
  filter(!sona_survey_code %in% mem$sona_survey_code)

# this person finished the experiment in 90s, chose paper 275 times, and lost 288 times
bot_data = bot_data %>%
  filter(game_id != "f7290e62-697c-46ec-b42d-51090ce3eed5")


# Save as RData
save(bot_data, file = paste(DATA_PATH, "rps_v3_data.RData", sep = "/"))


