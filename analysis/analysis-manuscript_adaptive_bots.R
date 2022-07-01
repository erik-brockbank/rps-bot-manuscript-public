#
# This script contains the final analysis of the *adaptive bot* experiment
# for the rps bot journal submission
#


# SETUP ====

rm(list = ls())
setwd("/Users/erikbrockbank/web/vullab/rps/analysis")

library(tidyverse)
library(viridis)
library(patchwork)
library(pwr)


# GLOBALS ====

DATA_FILE = "rps_v3_data.csv" # name of file containing full dataset for all rounds
GAME_ROUNDS = 300 # number of rounds in each complete game

# In order of complexity
STRATEGY_LEVELS = c(
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

STRATEGY_LOOKUP = list(
  # "opponent_moves" = "Move distribution",
  "opponent_prev_move" = "Choice given player's prior move choice",
  "bot_prev_move" = "Choice given opponent's prior choice",
  "opponent_bot_prev_move" = "Choice given player's prior choice & opponent's prior choice",
  "opponent_prev_two_moves" = "Choice given player's prior two choices",
  # "bot_prev_two_moves" = "Bot previous two moves",
  "opponent_transitions" = "Transition baserate (+/-/0)",
  "opponent_courn_transitions" = "Opponent transition baserate (+/-/0)",
  "opponent_outcome_transitions" = "Transition given prior outcome (W/L/T)",
  "opponent_outcome_prev_transition_dual" = "Transition given prior transition & prior outcome"
)

COMPLEXITY_LOOKUP = c(
  "opponent_transitions" = "3 cell memory",
  "opponent_courn_transitions" = "3 cell memory",
  "opponent_prev_move" = "9 cell memory",
  "bot_prev_move" = "9 cell memory",
  "opponent_outcome_transitions" = "9 cell memory",
  "opponent_bot_prev_move" = "27 cell memory",
  "opponent_prev_two_moves" = "27 cell memory",
  "opponent_outcome_prev_transition_dual" = "27 cell memory"
)

# NB: this is player move in rows, opponent move in columns
OUTCOME_MATRIX = matrix(c(0, -1, 1, 1, 0, -1, -1, 1, 0), nrow = 3, byrow = T,
                        dimnames = list(c("rock", "paper", "scissors"), c("rock", "paper", "scissors")))


TRANSITION_LOOKUP = matrix(
  data = c(c("0", "-", "+"), c("+", "0", "-"), c("-", "+", "0")),
  nrow = 3, ncol = 3, byrow = T,
  dimnames = list(c("rock", "paper", "scissors"), c("rock", "paper", "scissors"))
)


# ANALYSIS FUNCTIONS ====

get_bot_strategy_win_count_differential = function(data) {
  win_diff = data %>%
    group_by(bot_strategy, game_id, player_id, is_bot) %>%
    count(win_count = player_outcome == "win") %>%
    filter(win_count == TRUE) %>%
    group_by(bot_strategy, game_id) %>%
    # Win count for bots minus win count for human opponents
    # NB: if the person or bot *never* wins, this count will fail for them
    summarize(win_count_diff = n[is_bot == 1] - n[is_bot == 0]) %>%
    as.data.frame()
  return(win_diff)
}

get_bot_strategy_win_count_differential_summary = function(strategy_data) {
  strategy_data %>%
    group_by(bot_strategy) %>%
    summarize(mean_win_count_diff = mean(win_count_diff),
              n = n(),
              se = sd(win_count_diff) / sqrt(n),
              lower_se = mean_win_count_diff - se,
              upper_se = mean_win_count_diff + se)
}


# GRAPHING STYLE FUNCTIONS ====

default_plot_theme = theme(
  # titles
  plot.title = element_text(face = "bold", size = 20),
  axis.title.y = element_text(face = "bold", size = 16),
  axis.title.x = element_text(face = "bold", size = 16),
  legend.title = element_text(face = "bold", size = 16),
  # axis text
  axis.text.y = element_text(size = 14, face = "bold"),
  axis.text.x = element_text(size = 14, angle = 45, vjust = 0.5, face = "bold"),
  # legend text
  legend.text = element_text(size = 16, face = "bold"),
  # facet text
  strip.text = element_text(size = 12),
  # backgrounds, lines
  panel.background = element_blank(),
  strip.background = element_blank(),

  panel.grid = element_line(color = "gray"),
  axis.line = element_line(color = "black"),
  # positioning
  legend.position = "bottom",
  legend.key = element_rect(colour = "transparent", fill = "transparent")
)

# label_width = 48 # default: 10
label_width = 10 # default: 10

STRATEGY_LABELS = c("opponent_moves" = str_wrap(STRATEGY_LOOKUP[["opponent_moves"]], label_width),
                    "opponent_prev_move" = str_wrap(STRATEGY_LOOKUP[["opponent_prev_move"]], label_width),
                    "bot_prev_move" = str_wrap(STRATEGY_LOOKUP[["bot_prev_move"]], label_width),
                    "opponent_bot_prev_move" = str_wrap(STRATEGY_LOOKUP[["opponent_bot_prev_move"]], label_width),
                    "opponent_prev_two_moves" = str_wrap(STRATEGY_LOOKUP[["opponent_prev_two_moves"]], label_width),
                    "bot_prev_two_moves" = str_wrap(STRATEGY_LOOKUP[["bot_prev_two_moves"]], label_width),
                    "opponent_transitions" = str_wrap(STRATEGY_LOOKUP[["opponent_transitions"]], label_width),
                    "opponent_courn_transitions" = str_wrap(STRATEGY_LOOKUP[["opponent_courn_transitions"]], label_width),
                    "opponent_outcome_transitions" = str_wrap(STRATEGY_LOOKUP[["opponent_outcome_transitions"]], label_width),
                    "opponent_outcome_prev_transition_dual" = str_wrap(STRATEGY_LOOKUP[["opponent_outcome_prev_transition_dual"]], label_width))




# INITIALIZATION ====
# TODO move much of this into read data function
data = read_csv(DATA_FILE)
data$bot_strategy = factor(data$bot_strategy, levels = STRATEGY_LEVELS)

# Remove all incomplete games
incomplete_games = data %>%
  group_by(game_id, player_id) %>%
  summarize(rounds = max(round_index)) %>%
  filter(rounds < GAME_ROUNDS) %>%
  select(game_id) %>%
  unique()
incomplete_games

data = data %>%
  filter(!(game_id %in% incomplete_games$game_id))

# TODO players with "NA" moves; look into this
# (processing python script writes NA for empty move values)
tmp = data %>% filter(is.na(player_move))
tmp %>% group_by(sona_survey_code) %>% summarize(n())
data = data %>% filter(!is.na(player_move))


# Remove any duplicate complete games that have the same SONA survey code
# NB: this can happen if somebody played all the way through but exited before receiving credit
# First, fetch sona survey codes with multiple complete games
repeat_codes = data %>%
  group_by(sona_survey_code) %>%
  filter(is_bot == 0) %>%
  summarize(trials = n()) %>%
  filter(trials > GAME_ROUNDS) %>%
  select(sona_survey_code)
repeat_codes
# Next, get game id for the earlier complete game
# NB: commented out code checks that we have slider/free resp data for at least one of the games
duplicate_games = data %>%
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

data = data %>%
  filter(!game_id %in% duplicate_games$game_id)


# Sanity check: anybody with trials != 300?
trial_count = data %>%
  filter(is_bot == 0) %>%
  group_by(sona_survey_code) %>%
  summarize(trials = n()) %>%
  filter(trials != GAME_ROUNDS)
trial_count


# Check that there are no rows with memory >= 300
# (this was a bug in early data)
mem = data %>%
  filter(round_index == GAME_ROUNDS & is_bot == 1) %>%
  group_by(bot_strategy, game_id, sona_survey_code) %>%
  select(bot_strategy, game_id, sona_survey_code, bot_round_memory)

mem = mem %>%
  rowwise() %>%
  mutate(memory_sum =
           sum(as.numeric(unlist(regmatches(bot_round_memory, gregexpr("[[:digit:]]+", bot_round_memory))))))

mem = mem %>% filter(memory_sum >= GAME_ROUNDS)

data = data %>%
  filter(!sona_survey_code %in% mem$sona_survey_code)

# this person finished the experiment in 90s, chose paper 275 times, and lost 288 times
# TODO update manuscript image to exclude this
data = data %>%
  filter(game_id != "f7290e62-697c-46ec-b42d-51090ce3eed5")




# ANALYSIS: Win count differentials ====

wcd_all = get_bot_strategy_win_count_differential(data)
# TODO move this into parent function above
wcd_all = wcd_all %>%
  rowwise() %>%
  mutate(complexity = COMPLEXITY_LOOKUP[bot_strategy])
wcd_all$complexity = factor(wcd_all$complexity,
                            levels = c("3 cell memory", "9 cell memory", "27 cell memory"))


# How did bot WCD values compare to chance?
for (bot_strat in unique(wcd_all$bot_strategy)) {
  print(STRATEGY_LOOKUP[bot_strat])
  print(
    t.test(x = wcd_all$win_count_diff[wcd_all$bot_strategy == bot_strat])
  )
}

# Number of participants with WCD values < 0
# Binomial tests
for (bot_strat in unique(wcd_all$bot_strategy)) {
  print(STRATEGY_LOOKUP[bot_strat])
  print(
    binom.test(
      x = sum(wcd_all$win_count_diff[wcd_all$bot_strategy == bot_strat] < 0),
      n = length(wcd_all$win_count_diff[wcd_all$bot_strategy == bot_strat])
    )
  )
}



# FIGURE: Win count differentials ====

wcd_summary = get_bot_strategy_win_count_differential_summary(wcd_all)
# TODO move this into parent function being called above
wcd_summary = wcd_summary %>%
  rowwise() %>%
  mutate(complexity = COMPLEXITY_LOOKUP[bot_strategy])
wcd_summary$complexity = factor(wcd_summary$complexity,
                                levels = c("3 cell memory", "9 cell memory", "27 cell memory"))


wcd_summary %>%
  ggplot(aes(x = bot_strategy, y = mean_win_count_diff, color = complexity)) +
  geom_point(size = 6) +
  geom_errorbar(
    aes(ymin = lower_se, ymax = upper_se),
    width = 0.1, size = 1) +
  # geom_jitter(data = wcd_all, aes(x = bot_strategy, y = win_count_diff),
  #             size = 2, alpha = 0.75, width = 0.25, height = 0) +
  geom_hline(yintercept = 0, size = 1, linetype = "dashed") +
  labs(x = "", y = "Bot win count differential") +
  # ggtitle("Adaptive bot performance against humans") +
  scale_x_discrete(
    name = element_blank(),
    labels = STRATEGY_LABELS) +
  scale_color_viridis(discrete = T) +
  default_plot_theme +
  theme(
    plot.title = element_text(size = 32, face = "bold"),
    axis.title.y = element_text(size = 24, face = "bold"),
    # NB: axis title below is to give cushion for adding complexity labels in PPT
    # axis.title.x = element_text(size = 64),
    # axis.text.x = element_blank(),
    axis.text.x = element_text(size = 12, face = "bold", angle = 0, vjust = 1),
    axis.text.y = element_text(size = 14, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(size = 18, face = "bold"),
    legend.text = element_text(size = 16)
  )
# TODO move to function, save to pdf




# ANALYSIS: Information gain for dependencies against bots ====

# TODO move these above
# Function to get marginal probability of each transition (+/-/0) for each participant
# NB: this is different from function of same name in original manuscript analysis
get_player_transition_dist = function(data) {
  data %>%
    filter(is_bot == 0) %>%
    group_by(bot_strategy, game_id, player_id) %>%
    filter(!is.na(prev_move), # lag call above sets NA for lag on first move
           prev_move != "none",
           player_move != "none") %>%
    rowwise() %>%
    # add transition
    mutate(transition = TRANSITION_LOOKUP[player_move, prev_move]) %>%
    # calculate player transition probabilities
    count(transition) %>%
    group_by(bot_strategy, game_id, player_id) %>%
    mutate(total_transitions = sum(n),
           p_transition = n / total_transitions) %>%
    ungroup()
}


# Function to get marginal probability of each cournot transition (+/-/0) for each participant
# NB: this is different from function of same name in original manuscript analysis
get_player_cournot_transition_dist = function(data) {
  data %>%
    filter(is_bot == 0) %>%
    group_by(bot_strategy, game_id, player_id) %>%
    filter(!is.na(opponent_prev_move), # lag call above sets NA for lag on first move
           opponent_prev_move != "none",
           player_move != "none") %>%
    rowwise() %>%
    # add transition
    mutate(cournot_transition = TRANSITION_LOOKUP[player_move, opponent_prev_move]) %>%
    # calculate player transition probabilities
    count(cournot_transition) %>%
    group_by(bot_strategy, game_id, player_id) %>%
    mutate(total_cournot_transitions = sum(n),
           p_cournot_transition = n / total_cournot_transitions) %>%
    ungroup()
}


# Calculate information gain for transition distribution
get_ig_transition = function(player_summary) {
  player_summary %>%
    group_by(bot_strategy, game_id, player_id) %>%
    summarize(
      entropy_transition = -sum(p_transition * log(p_transition)),
      information_gain = -log(1/3) - entropy_transition
    )
}

# Calculate information gain for cournot transition distribution
# TODO consolidate with the above (only difference is column name)
get_ig_cournot_transition = function(player_summary) {
  player_summary %>%
    group_by(bot_strategy, game_id, player_id) %>%
    summarize(
      entropy_cournot_transition = -sum(p_cournot_transition * log(p_cournot_transition)),
      information_gain = -log(1/3) - entropy_cournot_transition
    )
}


get_ig_summary = function(data) {
  data %>%
    ungroup() %>%
    group_by(bot_strategy) %>%
    summarize(
      mean_ig = mean(information_gain),
      n = n(),
      se = sd(information_gain) / sqrt(n),
      ci_lower = mean_ig - se,
      ci_upper = mean_ig + se
    )
}


# Processing
# TODO move this to separate function or place at start

data = data %>%
  group_by(bot_strategy, game_id, player_id) %>%
  mutate(
    prev_move = lag(player_move, 1)) %>%
  ungroup()

data = data %>%
  group_by(game_id, round_index) %>%
  mutate(
    opponent_move = ifelse(is.na(lag(player_move, 1)), lead(player_move, 1), lag(player_move, 1))
  ) %>%
  ungroup()

data = data %>%
  group_by(bot_strategy, game_id, player_id) %>%
  mutate(
    opponent_prev_move = lag(opponent_move, 1)
  ) %>%
  ungroup()


# get overall probability of each transition (for each player)
transition_summary = get_player_transition_dist(data)
# get information gain for opponent of each player based on each player's transition probabilities
transition_ig = get_ig_transition(transition_summary)

# get overall probability of each cournot transition (for each player)
cournot_transition_summary = get_player_cournot_transition_dist(data)
# get information gain for opponent of each player based on each player's cournot transition probabilities
cournot_transition_ig = get_ig_cournot_transition(cournot_transition_summary)


# t-tests
t.test(
  transition_ig$information_gain[transition_ig$bot_strategy == "opponent_transitions"],
  transition_ig$information_gain[transition_ig$bot_strategy == "opponent_courn_transitions"],
  var.equal = T
)
t.test(
  transition_ig$information_gain[transition_ig$bot_strategy == "opponent_prev_move"],
  transition_ig$information_gain[transition_ig$bot_strategy == "bot_prev_move"],
  var.equal = T
)

t.test(
  cournot_transition_ig$information_gain[cournot_transition_ig$bot_strategy == "opponent_transitions"],
  cournot_transition_ig$information_gain[cournot_transition_ig$bot_strategy == "opponent_courn_transitions"],
  var.equal = T
)
t.test(
  cournot_transition_ig$information_gain[cournot_transition_ig$bot_strategy == "opponent_prev_move"],
  cournot_transition_ig$information_gain[cournot_transition_ig$bot_strategy == "bot_prev_move"],
  var.equal = T
)


# FIGURE: Information gain for dependencies against bots ====

transition_ig_summary = get_ig_summary(transition_ig)

p1 = transition_ig_summary %>%
  ggplot(aes(x = bot_strategy, y = mean_ig, color = bot_strategy)) +
  geom_point(size = 6) +
  geom_errorbar(
    aes(ymin = ci_lower, ymax = ci_upper),
    width = 0.1, size = 1) +
  # geom_jitter(data = transition_ig, aes(x = bot_strategy, y = information_gain),
              # size = 2, alpha = 0.75, width = 0.25, height = 0) +
  geom_hline(yintercept = 0, size = 1, linetype = "dashed") +
  labs(x = "", y = "Information gain") +
  ggtitle("Transition dependency exhibited against each bot") +
  scale_x_discrete(
    name = element_blank(),
    labels = STRATEGY_LABELS) +
  scale_color_viridis(discrete = T) +
  default_plot_theme +
  theme(
    plot.title = element_text(size = 24, face = "bold"),
    axis.title.y = element_text(size = 18, face = "bold"),
    axis.text.x = element_text(size = 12, face = "bold", angle = 0, vjust = 1),
    axis.text.y = element_text(size = 14, face = "bold"),
    legend.position = "none"
  )
# TODO save plot



cournot_transition_ig_summary = get_ig_summary(cournot_transition_ig)

p2 = cournot_transition_ig_summary %>%
  ggplot(aes(x = bot_strategy, y = mean_ig, color = bot_strategy)) +
  geom_point(size = 6) +
  geom_errorbar(
    aes(ymin = ci_lower, ymax = ci_upper),
    width = 0.1, size = 1) +
  # geom_jitter(data = cournot_transition_ig, aes(x = bot_strategy, y = information_gain),
              # size = 2, alpha = 0.75, width = 0.25, height = 0) +
  geom_hline(yintercept = 0, size = 1, linetype = "dashed") +
  labs(x = "", y = "Information gain") +
  ggtitle(str_wrap("Opponent transition dependency exhibited against each bot", 50)) +
  scale_x_discrete(
    name = element_blank(),
    labels = STRATEGY_LABELS) +
  scale_color_viridis(discrete = T) +
  default_plot_theme +
  theme(
    plot.title = element_text(size = 24, face = "bold"),
    axis.title.y = element_text(size = 18, face = "bold"),
    axis.text.x = element_text(size = 12, face = "bold", angle = 0, vjust = 1),
    axis.text.y = element_text(size = 14, face = "bold"),
    legend.position = "none"
  )

# TODO save plot

p1 / p2




# APPENDIX ====
# TODO should be able to get rid of all the below



# Plots for each opponent type that we're interested in
transition_ig_summary = transition_ig_summary %>%
  mutate(dependency = "Transition")
cournot_transition_ig_summary = cournot_transition_ig_summary %>%
  mutate(dependency = "Cournot Transition")
dependency_agg = bind_rows(
  transition_ig_summary,
  cournot_transition_ig_summary
)

transition_ig = transition_ig %>%
  mutate(dependency = "Transition")
cournot_transition_ig = cournot_transition_ig %>%
  mutate(dependency = "Cournot Transition")
dependency_agg_individ = bind_rows(
  transition_ig,
  cournot_transition_ig
)



# use patchwork to facet
plt_transition = dependency_agg %>%
  filter(bot_strategy == "opponent_transitions") %>%
  ggplot(aes(x = dependency, y = mean_ig, color = dependency)) +
  geom_point(
    size = 6,
    position = position_nudge(x = 0.4, y = 0)
  ) +
  geom_errorbar(
    aes(ymin = ci_lower, ymax = ci_upper),
    width = 0.1, size = 1,
    position = position_nudge(x = 0.4, y = 0)
  ) +
  # TOGGLE
  geom_jitter(data = dependency_agg_individ %>% filter(bot_strategy == "opponent_transitions"),
              aes(x = dependency, y = information_gain, color = dependency),
              size = 2, alpha = 0.5, width = 0.15, height = 0) +
  geom_hline(yintercept = 0, size = 1, linetype = "dashed") +
  # ylim(0, 0.25) +
  labs(x = "", y = "Information gain (bits)") +
  ggtitle(str_wrap("Transition baserate (+/-/0)", width = 20)) +
  scale_color_viridis(discrete = T,
                      begin = 0.25, end = 0.75) +
  default_plot_theme +
  # theme_cowplot() +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    axis.title.y = element_text(size = 18, face = "bold"),
    # axis.text.x = element_text(size = 12, face = "bold", angle = 0, vjust = 1),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 14, face = "bold"),
    legend.position = "none"
  )


plt_cournot = dependency_agg %>%
  filter(bot_strategy == "opponent_courn_transitions") %>%
  ggplot(aes(x = dependency, y = mean_ig, color = dependency)) +
  geom_point(size = 6, position = position_nudge(x = 0.4, y = 0)) +
  geom_errorbar(
    aes(ymin = ci_lower, ymax = ci_upper),
    width = 0.1, size = 1,
    position = position_nudge(x = 0.4, y = 0)
  ) +
  # TOGGLE
  geom_jitter(data = dependency_agg_individ %>% filter(bot_strategy == "opponent_courn_transitions"),
              aes(x = dependency, y = information_gain, color = dependency),
              size = 2, alpha = 0.5, width = 0.15, height = 0) +
  geom_hline(yintercept = 0, size = 1, linetype = "dashed") +
  # ylim(0, 0.25) +
  labs(x = "", y = "") +
  ggtitle(str_wrap("Opponent transition baserate (+/-/0)", width = 20)) +
  scale_color_viridis(discrete = T,
                      begin = 0.25, end = 0.75,
                      name = str_wrap("Participant dependency", 15),
                      labels = c(str_wrap("Cournot transition", 15), "Transition")
                      ) +
  default_plot_theme +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    axis.title.y = element_text(size = 18, face = "bold"),
    # axis.text.x = element_text(size = 12, face = "bold", angle = 0, vjust = 1),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 14, face = "bold"),
    legend.position = "right",
    # legend.title = element_blank(),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12)
  )


plt_transition + plt_cournot


# What's going on with these insane outliers against cournot transition opponent??
transition_ig %>% filter(information_gain > 1.0) %>% ungroup() %>% select(player_id)

# This person chose paper 275 times and scissors 25 times
data %>%
  filter(player_id == "00815ad0-27d4-4fec-a248-14165296dbbe") %>%
  count(player_move)
  # glimpse()

# Specifically, first 25 rows were all scissors, next 275 were all paper...
moves = data %>%
  filter(player_id == "00815ad0-27d4-4fec-a248-14165296dbbe") %>%
  select(player_move)
as.character(moves)

# How did this go?
data %>%
  filter(game_id == "7833ab77-0d2c-4f01-8127-a84ff09daef2",
    # player_id == "00815ad0-27d4-4fec-a248-14165296dbbe",
         round_index == 300) %>%
  glimpse()

data %>%
  filter(game_id == "7833ab77-0d2c-4f01-8127-a84ff09daef2") %>%
  glimpse()




# Let's try a raincloud plot and see if that looks better
if (!require(remotes)) {
  install.packages("remotes")
}
remotes::install_github('jorvlan/raincloudplots')
library(raincloudplots)
library(cowplot)
install.packages('PupillometryR')
library(PupillometryR)



p1 = dependency_agg_individ %>%
  filter(bot_strategy == "opponent_transitions") %>%
  ggplot(
    aes(x = dependency, y = information_gain, color = dependency, fill = dependency)
  ) +
  geom_flat_violin(
    position = position_nudge(x = .2, y = 0),
    # adjust = 1.5,
    alpha = .5,
    # trim = FALSE,
    color = NA # remove outlines around distribution
  ) +
  geom_point(
    position = position_jitter(width = 0.1, height = 0), size = 2, alpha = 0.5) +
  geom_boxplot(
    alpha = 0.5,
    width = 0.1,
    color = "black",
    outlier.shape = NA,
    position = position_nudge(x = -0.25, y = 0)
  ) +
  geom_hline(yintercept = 0, size = 1, linetype = "dashed") +
  labs(x = "", y = "Information gain (bits)") +
  ggtitle("Transition opponent") +
  scale_color_viridis(discrete = T,
                      begin = 0.25, end = 0.75) +
  scale_fill_viridis(discrete = T,
                      begin = 0.25, end = 0.75) +
  default_plot_theme +
  theme_cowplot() +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    axis.title.y = element_text(size = 18, face = "bold"),
    # axis.text.x = element_text(size = 12, face = "bold", angle = 0, vjust = 1),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 14, face = "bold"),
    legend.position = "none",
    legend.title = element_blank(),
    legend.text = element_text(size = 18, face = "bold")
  ) +
  guides(fill = FALSE)

dependency_agg_individ %>%
  filter(bot_strategy == "opponent_courn_transitions") %>%
  ggplot(
    aes(x = dependency, y = information_gain, color = dependency, fill = dependency)
  ) +
  geom_flat_violin(
    position = position_nudge(x = .2, y = 0),
    adjust = 1.5,
    alpha = .5,
    trim = FALSE,
    color = NA # remove outlines around distribution
  ) +
  geom_point(
    position = position_jitter(width = 0.1, height = 0), size = 2, alpha = 0.5) +
  geom_boxplot(
    alpha = 0.5,
    width = 0.1,
    color = "black",
    outlier.shape = NA,
    position = position_nudge(x = -0.25, y = 0)
  ) +
  geom_hline(yintercept = 0, size = 1, linetype = "dashed") +
  # ylim(0, 0.5) +
  labs(x = "", y = "Information gain (bits)") +
  ggtitle("Cournot transition opponent") +
  scale_color_viridis(discrete = T,
                      begin = 0.25, end = 0.75) +
  scale_fill_viridis(discrete = T,
                     begin = 0.25, end = 0.75) +
  default_plot_theme +
  theme_cowplot() +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    axis.title.y = element_text(size = 18, face = "bold"),
    # axis.text.x = element_text(size = 12, face = "bold", angle = 0, vjust = 1),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 14, face = "bold"),
    legend.position = "none",
    legend.title = element_blank(),
    legend.text = element_text(size = 18, face = "bold")
  ) +
  guides(fill = FALSE)




