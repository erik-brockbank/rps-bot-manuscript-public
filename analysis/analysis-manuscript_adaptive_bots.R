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
  "opponent_prev_move" = "Choice given player's prior choice",
  "bot_prev_move" = "Choice given opponent's prior choice",
  "opponent_bot_prev_move" = "Choice given player's prior choice & opponent's prior choice",
  "opponent_prev_two_moves" = "Choice given player's prior two choices",
  # "bot_prev_two_moves" = "Bot previous two moves",
  "opponent_transitions" = "Self-transition baserate (+/-/0)",
  "opponent_courn_transitions" = "Opponent-transition baserate (+/-/0)",
  "opponent_outcome_transitions" = "Transition given prior outcome (W/L/T)",
  "opponent_outcome_prev_transition_dual" = "Transition given prior transition & prior outcome"
)

COMPLEXITY_LOOKUP = c(
  "opponent_transitions" = "3-cell memory",
  "opponent_courn_transitions" = "3-cell memory",
  "opponent_prev_move" = "9-cell memory",
  "bot_prev_move" = "9-cell memory",
  "opponent_outcome_transitions" = "9-cell memory",
  "opponent_bot_prev_move" = "27-cell memory",
  "opponent_prev_two_moves" = "27-cell memory",
  "opponent_outcome_prev_transition_dual" = "27-cell memory"
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

# Players with "NA" moves
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
data = data %>%
  filter(game_id != "f7290e62-697c-46ec-b42d-51090ce3eed5")



# ANALYSIS: Power ====

# How many participants in each condition?
data %>%
  filter(is_bot == 0) %>%
  group_by(bot_strategy) %>%
  summarize(n = n_distinct(player_id))


# With 20 participants in lowest bot condition, what effect size do we have 90% power to detect?
# and what win percentage does that correspond to?
# https://cran.r-project.org/web/packages/pwr/vignettes/pwr-vignette.html
power = pwr.t.test(n = 20, sig.level = 0.05, power = 0.8, type = "one.sample", alternative = "two.sided")
# we have 90% power to detect an effect size of d = 0.77, 80% to detect d = 0.66
(1 / 3) + power$d * sqrt(1 / 12)
# d = 0.77 this is equivalent to an average win rate of about 55% assuming individual win rates are uniformly distributed (unlikely)

# TODO add clarification to this: why the sqrt(1/12)?

# Reverse calculation: how many people needed to detect effect size of bots winning 40%?
d = (.4 - (1/3)) / sqrt(1/12) # d = .23 (small effect)
pwr.t.test(sig.level = 0.05, power = 0.8, d = d, type = "one.sample", alternative = "two.sided")
# N = 200 for 90% power, 150 for 80% power...

# How much power did we have?
pwr.t.test(sig.level = 0.05, d = d, n = 20, type = "one.sample", alternative = "two.sided")
# Not much...


# Print average and SE of *bot* win rates for each strategy condition
# Note comparison to power analysis above
for (strat in unique(data$bot_strategy)) {
  print(STRATEGY_LOOKUP[strat])
  print(
    data %>%
      filter(is_bot == 0, # NOTE is bot here
             bot_strategy == strat) %>%
      group_by(player_id) %>%
      count(win = player_outcome == "win") %>%
      mutate(total = sum(n),
             win_pct = n / total) %>%
      filter(win == TRUE) %>%
      ungroup() %>%
      summarize(
        players = n(),
        win_pct_mean = mean(win_pct),
        win_pct_se = sd(win_pct) / sqrt(players)
      )
  )
}


# ANALYSIS: Win count differentials ====

wcd_all = get_bot_strategy_win_count_differential(data)
# TODO move this into parent function above
wcd_all = wcd_all %>%
  rowwise() %>%
  mutate(complexity = COMPLEXITY_LOOKUP[bot_strategy])
wcd_all$complexity = factor(wcd_all$complexity,
                            levels = c("3-cell memory", "9-cell memory", "27-cell memory"))


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
                                levels = c("3-cell memory", "9-cell memory", "27-cell memory"))


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


ggsave(filename = "v3_adaptive_bot_summary_complexity.png",
       path = "img", # TODO fix file path
       device = "png",
       units = "in",
       width = 8.5,
       height = 6.5,
       dpi = 500, # NB: this requires re-opening in preview to fix dpi
       )



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

# Add previous move column
data = data %>%
  group_by(bot_strategy, game_id, player_id) %>%
  mutate(
    prev_move = lag(player_move, 1)) %>%
  ungroup()

# Add opponent move column
data = data %>%
  group_by(game_id, round_index) %>%
  mutate(
    opponent_move = ifelse(is.na(lag(player_move, 1)), lead(player_move, 1), lag(player_move, 1))
  ) %>%
  ungroup()

# Add opponent previous move column
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

# transition information gain for participants in transition and cournot transition bot cond's
t.test(
  transition_ig$information_gain[transition_ig$bot_strategy == "opponent_transitions"],
  transition_ig$information_gain[transition_ig$bot_strategy == "opponent_courn_transitions"],
  var.equal = T
)
# transition information gain for participants in choice|prior choice and choice|opponent choice cond's
t.test(
  transition_ig$information_gain[transition_ig$bot_strategy == "opponent_prev_move"],
  transition_ig$information_gain[transition_ig$bot_strategy == "bot_prev_move"],
  var.equal = T
)

# cournot transition information gain for participants in transition and cournot transition cond's
t.test(
  cournot_transition_ig$information_gain[cournot_transition_ig$bot_strategy == "opponent_transitions"],
  cournot_transition_ig$information_gain[cournot_transition_ig$bot_strategy == "opponent_courn_transitions"],
  var.equal = T
)
# cournot transition information gain for participants in choice|prior choice and choice|opponent choice cond's
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
  geom_hline(yintercept = 0, size = 1, linetype = "dashed") +
  labs(x = "", y = "Information gain") +
  ggtitle("Self-transition dependency exhibited against each bot") +
  scale_x_discrete(
    name = element_blank(),
    labels = STRATEGY_LABELS) +
  scale_color_viridis(discrete = T) +
  default_plot_theme +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
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
  geom_hline(yintercept = 0, size = 1, linetype = "dashed") +
  labs(x = "", y = "Information gain") +
  ggtitle("Opponent-transition dependency exhibited against each bot") +
  scale_x_discrete(
    name = element_blank(),
    labels = STRATEGY_LABELS) +
  scale_color_viridis(discrete = T) +
  default_plot_theme +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    axis.title.y = element_text(size = 18, face = "bold"),
    axis.text.x = element_text(size = 12, face = "bold", angle = 0, vjust = 1),
    axis.text.y = element_text(size = 14, face = "bold"),
    legend.position = "none"
  )
# TODO save plot

p3 = p1 / p2
p3

ggsave(filename = "v3_adaptive_bot_response_summary.png",
       path = "img", # TODO fix file path
       device = "png",
       units = "in",
       width = 9,
       height = 12,
       dpi = 500, # NB: this requires re-opening in preview to fix dpi
)



# ANALYSIS: Simulate transition biases against transition bots ====

# TODO clean this up and make functions like an adult...

N_SUBJECTS = 10
N_ROUNDS = 300
MOVES = c(1, 2, 3)
MOVES_STR = c("rock", "paper", "scissors")
TRANSITIONS = c(-1, 0, 1)
TRANSITIONS_STR = c("down", "stay", "up")
TRANSITION_BIAS = -1 # values: -1, 0, 1

simulation_df = data.frame()

for(subj in seq(N_SUBJECTS)) {
  print(paste("BEGINNING SIMULATION FOR SUBJECT: ", subj))
  # Initialize subject df
  df_self_transition = data.frame(
    "subj" = numeric(),
    "round" = numeric(),
    "subj_move" = numeric(),
    "subj_move_str" = character(),
    "subj_cournot_trans" = numeric(),
    # Note these counts are inclusive of the current round, so decisions need to be made based on counts from previous round
    "subj_total_cournot_down" = numeric(),
    "subj_total_cournot_stay" = numeric(),
    "subj_total_cournot_up" = numeric(),
    # Note these probabilities are up to the previous round so don't align with counts in same row
    "subj_prob_cournot_down" = numeric(),
    "subj_prob_cournot_stay" = numeric(),
    "subj_prob_cournot_up" = numeric(),
    "bot_predicted_subj_cournot_trans" = numeric(),
    "bot_predicted_subj_move" = numeric(),
    "bot_move" = numeric(),
    "bot_move_str" = character()
  )

  # Add moves for an individual subject's simulation, round by round!
  for(round in seq(N_ROUNDS)) {
    if(round == 1) {
      subj_move = sample(MOVES, 1)
      subj_cournot_trans = NA
      subj_total_cournot_down = 0
      subj_total_cournot_stay = 0
      subj_total_cournot_up = 0
      subj_prob_cournot_down = 0
      subj_prob_cournot_stay = 0
      subj_prob_cournot_up = 0

      # Choose bot move
      bot_predicted_subj_cournot_trans = NA
      bot_predicted_subj_move = NA
      bot_move = sample(MOVES, 1)
    } else {
      # Code using mod logic below is code from Ed, very snazzy
      subj_move = ((df_self_transition$subj_move[round-1] + TRANSITION_BIAS - 1) %% 3) + 1
      bot_prev_move = df_self_transition$bot_move[round-1]
      subj_cournot_trans = ifelse(
        (subj_move - bot_prev_move) %% 3 == 2,
        -1,
        (subj_move - bot_prev_move) %% 3
      )

      subj_total_cournot_down = df_self_transition$subj_total_cournot_down[round-1] + (subj_cournot_trans == -1)
      subj_total_cournot_stay = df_self_transition$subj_total_cournot_stay[round-1] + (subj_cournot_trans == 0)
      subj_total_cournot_up = df_self_transition$subj_total_cournot_up[round-1] + (subj_cournot_trans == 1)

      total_transitions = max(sum(df_self_transition$subj_total_cournot_down[round-1], # max avoids division by 0 below
                                  df_self_transition$subj_total_cournot_stay[round-1],
                                  df_self_transition$subj_total_cournot_up[round-1]), 0.001)
      subj_prob_cournot_down = df_self_transition$subj_total_cournot_down[round-1] / total_transitions
      subj_prob_cournot_stay = df_self_transition$subj_total_cournot_stay[round-1] / total_transitions
      subj_prob_cournot_up = df_self_transition$subj_total_cournot_up[round-1] / total_transitions

      # Identify most likely human cournot transitions, break tie if needed
      bot_predicted_subj_cournot_trans = NA
      cournot_probs = c(subj_prob_cournot_down, subj_prob_cournot_stay, subj_prob_cournot_up)
      max_prob_cournot = which(cournot_probs == max(cournot_probs)) # Which of the above is max? (includes ties)
      # Single max: predicted transition is index of max in TRANSITIONS
      if(length(max_prob_cournot) == 1) {
        bot_predicted_subj_cournot_trans = TRANSITIONS[max_prob_cournot]
        # print(paste("1 max: ", round, ": ", max_prob_cournot, "choice: ", bot_predicted_subj_cournot_trans))
      }
      # All tied: sample randomly
      if(length(max_prob_cournot) == 3) {
        bot_predicted_subj_cournot_trans = sample(TRANSITIONS, 1)
        # print(paste("3-way tie: ", round, "choice: ", bot_predicted_subj_cournot_trans))
      }
      # 2-way tie. Determine which elements are tied based on sum of `max_prob_cournot`,
      # then sample transition from among relevant tied transitions
      if(length(max_prob_cournot) == 2) {
        ties = sum(max_prob_cournot)
        bot_predicted_subj_cournot_trans = case_when(
          ties == 3 ~ sample(c(TRANSITIONS[1], TRANSITIONS[2]), 1),
          ties == 4 ~ sample(c(TRANSITIONS[1], TRANSITIONS[3]), 1),
          ties == 5 ~ sample(c(TRANSITIONS[2], TRANSITIONS[3]), 1)
        )
        # print(paste("2-way tie: ", round, "choice: ", bot_predicted_subj_cournot_trans))
        # print(max_prob_cournot)
      }
      # Choose bot move based on predicted human cournot transition
      # Code using mod logic below is code from Ed, very snazzy
      bot_predicted_subj_move = ((bot_predicted_subj_cournot_trans + bot_prev_move - 1) %% 3) + 1
      bot_move = (bot_predicted_subj_move %% 3) + 1
    }

    # Add move calculations above to dataframe
    new_row = data.frame(
      "subj" = subj,
      "round" = round,
      "subj_move" = subj_move,
      "subj_move_str" = MOVES_STR[subj_move],
      "subj_cournot_trans" = subj_cournot_trans,
      "subj_total_cournot_up" = subj_total_cournot_up,
      "subj_total_cournot_down" = subj_total_cournot_down,
      "subj_total_cournot_stay" = subj_total_cournot_stay,
      "subj_prob_cournot_up" = subj_prob_cournot_up,
      "subj_prob_cournot_down" = subj_prob_cournot_down,
      "subj_prob_cournot_stay" = subj_prob_cournot_stay,
      "bot_predicted_subj_cournot_trans" = bot_predicted_subj_cournot_trans,
      "bot_predicted_subj_move" = bot_predicted_subj_move,
      "bot_move" = bot_move,
      "bot_move_str" = MOVES_STR[bot_move]
    )
    df_self_transition = bind_rows(
      df_self_transition,
      new_row
    )
  }

  # Calculate outcomes based on simulated bot moves above
  df_self_transition = df_self_transition %>%
    mutate(
      subj_outcome = ifelse(
        (subj_move - bot_move) %% 3 == 2,
        -1,
        (subj_move - bot_move) %% 3
      )
    )

  # Concatenate subject dataframe above with global df
  simulation_df = bind_rows(
    simulation_df,
    df_self_transition
  )
}




# Summarize results

subject_summary = simulation_df %>%
  group_by(subj) %>%
  summarize(
    "rounds" = n(),
    "subject_win_pct" = mean(subj_outcome)
  )
subject_summary


overall_summary = subject_summary %>%
  summarize(
    avg_win_rate = mean(subject_win_pct),
    subjects = n(),
    se = sd(subject_win_pct) / sqrt(subjects)
  )
overall_summary


# FIGURE: Simulation results
subject_summary %>%
  ggplot(aes(x = 1, y = subject_win_pct)) +
  geom_jitter(height = 0, width = 0.5) +
  # TODO add errorbars to the below
  geom_point(data = overall_summary, aes(x = 1, y = avg_win_rate), color = "red", size = 3)



# APPENDIX ====



unique(wcd_all$bot_strategy)



# Figure 7 TOP

# Check strategies that track people's *cournot* transitions (with respect to the bot)
strat = "opponent_courn_transitions"
# strat = "bot_prev_move"

# Order by highest *transition* IG people in these cournot bot conditions
transition_ig %>%
  filter(bot_strategy == strat) %>%
  arrange(desc(information_gain))

# Select games with greater than threshold IG
games = transition_ig %>%
  filter(bot_strategy == strat,
         information_gain > .05) %>%
  pull(game_id)


# Check which of these people *actually* had high WCD against hte bots (ignore pattern for people who did poorly)
wcd_all %>%
  filter(game_id %in% games)

# Count transitions by these top people
data %>%
  filter(
    game_id %in% games,
    is_bot == 0) %>%
  group_by(bot_strategy, game_id, player_id) %>%
  filter(!is.na(opponent_prev_move), # lag call above sets NA for lag on first move
         opponent_prev_move != "none",
         prev_move != "none",
         player_move != "none") %>%
  rowwise() %>%
  # add transitions
  mutate(
    transition = TRANSITION_LOOKUP[player_move, prev_move],
    cournot_transition = TRANSITION_LOOKUP[player_move, opponent_prev_move]) %>%
  # count player transitions
  count(transition) %>%
  print(n = 100)
  # OR: print full set of moves with transitions and outcomes (needs to filter a single game above, not a set)
  # select(round_index, player_move, opponent_move, player_outcome,
  #        transition,
  #        # cournot_transition
  # ) %>%
  # print(n=300)


# opponent_courn_transitions
# a52: - and +
# 7e5: +
# 80d: +
# ef2: 0
# dcb: 0
# a2f: -
# 02c: -
# 2a6: 0
# 3b5: 0
# 2b9: -

# bot_prev_move
# a4d: -
# eca: - (slight)
# 3a6: -
# 983: + (slight)
# 262: - or 0
# 394: +
#


# Fig 7 BOTTOM

# Check strategies that track people's self-transitions
strat = "opponent_transitions" # self transition baserate
strat = "opponent_prev_move" # choice given player's prior choice

# Order by highest *cournot transition* IG people in these transition bot conditions
cournot_transition_ig %>%
  filter(bot_strategy == strat) %>%
  arrange(desc(information_gain))

# Select games with greater than threshold IG
games = cournot_transition_ig %>%
  filter(bot_strategy == strat,
         information_gain > .05) %>%
  pull(game_id)


# Check which of these people *actually* had high WCD against the bots (ignore pattern for people who did poorly)
wcd_all %>%
  filter(game_id %in% games)

# Count transitions by these top people
data %>%
  filter(
    game_id %in% games,
    is_bot == 0) %>%
  group_by(bot_strategy, game_id, player_id) %>%
  filter(!is.na(opponent_prev_move), # lag call above sets NA for lag on first move
         opponent_prev_move != "none",
         prev_move != "none",
         player_move != "none") %>%
  rowwise() %>%
  # add transitions
  mutate(
    transition = TRANSITION_LOOKUP[player_move, prev_move],
    cournot_transition = TRANSITION_LOOKUP[player_move, opponent_prev_move]) %>%
  # count player transitions
  count(cournot_transition) %>%
  print(n = 100)
# OR: print full set of moves with transitions and outcomes (needs to filter a single game above, not a set)
# select(round_index, player_move, opponent_move, player_outcome,
#        transition,
#        # cournot_transition
# ) %>%
# print(n=300)



# opponent_transitions
# Not relevant (though two people with slighly positive WCD are favoring + similar to below)

# opponent_prev_move
# 138: +
# 2d7: +
# 668: + (small)
# 25a: +
# d53: +
# b2a: +
# 61d: +
# f9b: +
# e93: +


### What are people actually doing?

# It seems like the people with the *highest* WCD against a given strategy aren't
# just favoring a single transition or cournot transition (more often two of three)


# Check strategies that track people's *cournot* transitions (with respect to the bot)
strat = "opponent_courn_transitions" # opponent transition baserate
strat = "bot_prev_move" # choice given opponent's prior choice

# Order by highest WCD
wcd_all %>%
  filter(bot_strategy == strat) %>%
  arrange(win_count_diff)


# Look at transition for individuals with highest WCD

data %>%
  filter(
    # game_id == "911fe152-431f-4f86-acc7-cfa0776f674b", # -118 -> approx. equal + and 0
    # game_id == "dcfe36cc-d56f-44b6-9079-477f59de5db3", # -113 -> approx. equal + and -
    # game_id == "1b97892b-0eb9-40bd-8a8d-45d3d67034d4", # -101 -> slight bias for - (then 0)

    # game_id == "fb439c7c-84a9-4cfa-83ef-f3fb32b034eb", # -111 -> strong bias for +
    # game_id == "6bdc9905-1871-434a-a95d-b484fe08234e", # -100 -> approx. equal + and -
    game_id == "e22c447a-aa28-46e7-a954-9d834c98ba59", # -100 -> slight bias for + (then -)
    is_bot == 0) %>%
  group_by(bot_strategy, game_id, player_id) %>%
  filter(!is.na(opponent_prev_move), # lag call above sets NA for lag on first move
         opponent_prev_move != "none",
         prev_move != "none",
         player_move != "none") %>%
  rowwise() %>%
  # add transitions
  mutate(
    transition = TRANSITION_LOOKUP[player_move, prev_move],
    cournot_transition = TRANSITION_LOOKUP[player_move, opponent_prev_move]) %>%
  # count player transitions
  count(transition)
  # OR print full set of moves with transitions and outcomes (needs to filter a single game above, not a set)
  # select(round_index, player_move, opponent_move, player_outcome,
  #        transition,
  #        # cournot_transition
  # ) %>%
  # print(n=300)



# Check strategies that track people's *self* transitions
strat = "opponent_transitions" # self transition baserate
# strat = "opponent_prev_move" # choice given player's prior choice

# Order by highest WCD
wcd_all %>%
  filter(bot_strategy == strat) %>%
  arrange(win_count_diff)


# Look at cournot transition for individuals with highest WCD
data %>%
  filter(
    # game_id == "ecbdc056-66bd-4abd-bfff-6b7a0780813c", # -57 -> approx. equal + and 0
    # game_id == "abddd5ae-342e-4014-83fc-65d48b90a6f0", # -54 -> strong bias for +
    # game_id == "37c2e184-0cc9-4404-ba2e-cc9b72d261d1", # -52 -> mild bias for + (then 0)

    # game_id == "8857ca23-a687-4cb7-b0f3-8d4a35a3dd53", # -73 -> strong bias for +
    # game_id == "87d2ed74-7ca2-495e-a173-1d5a8ce5525a", # -65 -> strong bias for +
    # game_id == "d17a79e7-61ae-48a1-a859-1199d3ec461d", # -57 -> strong bias for +
    # game_id == "dc7e1ee1-8b5c-450a-8bc7-992eca12759e", # -53 -> strong bias for +
    is_bot == 0) %>%
  group_by(bot_strategy, game_id, player_id) %>%
  filter(!is.na(opponent_prev_move), # lag call above sets NA for lag on first move
         opponent_prev_move != "none",
         prev_move != "none",
         player_move != "none") %>%
  rowwise() %>%
  # add transitions
  mutate(
    transition = TRANSITION_LOOKUP[player_move, prev_move],
    cournot_transition = TRANSITION_LOOKUP[player_move, opponent_prev_move]) %>%
  # count cournot transitions
  count(cournot_transition)
  # OR print full set of moves with transitions and outcomes (needs to filter a single game above, not a set)
  # select(round_index, player_move, opponent_move, player_outcome,
  #        transition,
  #        # cournot_transition
  # ) %>%
  # print(n=300)


