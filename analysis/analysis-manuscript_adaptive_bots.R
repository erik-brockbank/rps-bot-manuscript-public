#
# This script contains the final analysis of the *adaptive bot* experiment for the rps bot journal submission
#


# SETUP ====
setwd("/Users/erikbrockbank/dev/research/vullab/rps-bot-manuscript-public/analysis")
library(tidyverse)
library(viridis)
library(patchwork)
library(pwr)

# Globals
IMG_PATH = "../figures" # pathway to "figures" folder

STRATEGY_LOOKUP = list(
  # "opponent_moves" = "Move distribution",
  "opponent_prev_move" = "Previous move",
  "bot_prev_move" = "Opponent previous move",
  "opponent_bot_prev_move" = "Previous move, opponent previous move",
  "opponent_prev_two_moves" = "Previous two moves",
  # "bot_prev_two_moves" = "Bot previous two moves",
  "opponent_transitions" = "Self-transition",
  "opponent_courn_transitions" = "Opponent-transition",
  "opponent_outcome_transitions" = "Previous outcome",
  "opponent_outcome_prev_transition_dual" = "Previous outcome, previous transition"
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


get_bot_win_pct = function(data) {
  data %>%
    filter(is_bot == 1) %>%
    group_by(bot_strategy, game_id, player_id) %>%
    count(win = player_outcome == "win") %>%
    mutate(total = sum(n),
           win_pct = n / total) %>%
    filter(win == TRUE)
}



get_condition_win_pct = function(individual_win_pct) {
  individual_win_pct %>%
    group_by(bot_strategy) %>%
    summarize(subjects = n(),
              mean_win_pct = mean(win_pct),
              se_win_pct = sd(win_pct) / sqrt(subjects))

}


# GRAPHING STYLE FUNCTIONS ====

default_plot_theme = theme(
  # text
  plot.title = element_text(size = 24, family = "Avenir", color = "black", margin = margin(b = 0.5, unit = "line")),
  axis.title.y = element_text(size = 24, family = "Avenir", color = "black", margin = margin(r = 0.5, unit = "line")),
  axis.title.x = element_text(size = 24, family = "Avenir", color = "black", margin = margin(t = 0.5, unit = "line")),
  axis.text.y = element_text(size = 18, family = "Avenir", color = "black"),
  axis.text.x = element_text(size = 14, family = "Avenir", color = "black"),
  legend.title = element_text(size = 20, family = "Avenir", color = "black"),
  legend.text = element_text(size = 18, family = "Avenir", color = "black"),
  # backgrounds, lines
  panel.background = element_blank(),
  strip.background = element_blank(),
  panel.grid.major.x = element_blank(),
  panel.grid.minor.x = element_blank(),
  panel.grid.major.y = element_blank(),
  panel.grid.minor.y = element_blank(),
  axis.line = element_line(color = "black"),
  axis.ticks = element_line(color = "black")
)

# label_width = 48 # default: 10
label_width = 22 # default: 10

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
load("../data/rps_v3_data.RData")

# ANALYSIS: Power ====

# How many participants in each condition?
bot_data %>%
  filter(is_bot == 0) %>%
  group_by(bot_strategy) %>%
  summarize(n = n_distinct(player_id))


# With 20 participants in lowest bot condition, what effect size do we have 90% power to detect?
# and what win percentage does that correspond to?
# https://cran.r-project.org/web/packages/pwr/vignettes/pwr-vignette.html
power = pwr.t.test(n = 20, sig.level = 0.05, power = 0.8, type = "one.sample", alternative = "two.sided")
# we have 90% power to detect an effect size of d = 0.77, 80% to detect d = 0.66
(1 / 3) + power$d * sqrt(1 / 12)
# d = 0.77 this is equivalent to an average win rate of about 55% assuming individual win rates are uniformly distributed


# Reverse calculation: how many people needed to detect effect size of bots winning 40%?
d = (.4 - (1/3)) / sqrt(1/12) # d = .23 (small effect)
pwr.t.test(sig.level = 0.05, power = 0.8, d = d, type = "one.sample", alternative = "two.sided")
# N = 200 for 90% power, 150 for 80% power...

# How much power did we have?
pwr.t.test(sig.level = 0.05, d = d, n = 20, type = "one.sample", alternative = "two.sided")


# Print average and SE of *bot* win rates for each strategy condition
# Note comparison to power analysis above
for (strat in unique(bot_data$bot_strategy)) {
  print(STRATEGY_LOOKUP[strat])
  print(
    bot_data %>%
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

wcd_all = get_bot_strategy_win_count_differential(bot_data)
wcd_summary = get_bot_strategy_win_count_differential_summary(wcd_all)


# FIGURE: Win count differentials ====

adaptive_bot_wcd = wcd_summary %>%
  ggplot(aes(x = bot_strategy, y = mean_win_count_diff, color = bot_strategy)) +
  geom_point(size = 6) +
  geom_errorbar(
    aes(ymin = mean_win_count_diff - se, ymax = mean_win_count_diff + se),
    width = 0, linewidth = 1) +
  geom_hline(yintercept = 0, linewidth = 1, linetype = "dashed", color = "black") +
  scale_x_discrete(
    name = element_blank(),
    labels = element_blank()
  ) +
  scale_y_continuous(
    name = "Bot win count differential",
    breaks = seq(-50, 50, by = 25),
    labels = as.character(seq(-50, 50, by = 25)),
    limits = c(-55, 50)
  ) +
  scale_color_viridis(
    discrete = T,
    name = "Human pattern",
    labels = STRATEGY_LABELS
  ) +
  default_plot_theme +
  theme(
    axis.ticks.x = element_blank(),
    legend.position = "right",
    legend.key = element_rect(colour = "transparent", fill = "transparent"),
    legend.spacing.y = unit(0, "lines"),
    legend.key.size = unit(3, "lines")
  )
adaptive_bot_wcd

ggsave(
  adaptive_bot_wcd,
  filename = "adaptive_bot_wcd.pdf",
  device = cairo_pdf,
  path = IMG_PATH,
  width = 10,
  height = 6,
  dpi = 300
)



# ANALYSIS: Win percentage ====

bot_win_pct = get_bot_win_pct(bot_data)
condition_win_pct = get_condition_win_pct(bot_win_pct)

# How did bot win percentage values compare to chance?
for (bot_strat in unique(bot_win_pct$bot_strategy)) {
  print(STRATEGY_LOOKUP[bot_strat])
  print(
    t.test(x = bot_win_pct$win_pct[bot_win_pct$bot_strategy == bot_strat],
           mu = (1/3))
  )
}



# ANALYSIS: Information gain for dependencies against bots ====

# TODO move these above
# Function to get marginal probability of each transition (+/−/0) for each participant
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


# Function to get marginal probability of each cournot transition (+/−/0) for each participant
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
bot_data = bot_data %>%
  group_by(bot_strategy, game_id, player_id) %>%
  mutate(
    prev_move = lag(player_move, 1)) %>%
  ungroup()

# Add opponent move column
bot_data = bot_data %>%
  group_by(game_id, round_index) %>%
  mutate(
    opponent_move = ifelse(is.na(lag(player_move, 1)), lead(player_move, 1), lag(player_move, 1))
  ) %>%
  ungroup()

# Add opponent previous move column
bot_data = bot_data %>%
  group_by(bot_strategy, game_id, player_id) %>%
  mutate(
    opponent_prev_move = lag(opponent_move, 1)
  ) %>%
  ungroup()


# get overall probability of each transition (for each player)
transition_summary = get_player_transition_dist(bot_data)
# get information gain for opponent of each player based on each player's transition probabilities
transition_ig = get_ig_transition(transition_summary)

# get overall probability of each cournot transition (for each player)
cournot_transition_summary = get_player_cournot_transition_dist(bot_data)
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


# TEST: correlation between information gain and win count differential
# NB: currently only looks at opponent-transition baserate bot, self-transition IG
cournot_subjects_ig = transition_ig %>%
  filter(bot_strategy == "opponent_courn_transitions")
cournot_subjects_wcd = wcd_all %>%
  filter(bot_strategy == "opponent_courn_transitions")
cournot_subjects_ig = cournot_subjects_ig %>%
  inner_join(cournot_subjects_wcd, by = c("bot_strategy", "game_id"))
cournot_subjects_ig = cournot_subjects_ig %>%
  mutate(win_count_diff_subj = -1 * win_count_diff)

cor.test(cournot_subjects_ig$information_gain,
         cournot_subjects_ig$win_count_diff_subj)
plot(cournot_subjects_ig$information_gain,
     cournot_subjects_ig$win_count_diff_subj)



# FIGURE: Information gain for dependencies against bots ====

transition_ig_summary = get_ig_summary(transition_ig)

STRATEGY_LOOKUP = list(
  # "opponent_moves" = "Move distribution",
  "opponent_prev_move" = "Previous move",
  "bot_prev_move" = "Opponent previous move",
  "opponent_bot_prev_move" = "Previous move, opponent previous move",
  "opponent_prev_two_moves" = "Previous two moves",
  # "bot_prev_two_moves" = "Bot previous two moves",
  "opponent_transitions" = "Self- transition",
  "opponent_courn_transitions" = "Opponent- transition",
  "opponent_outcome_transitions" = "Previous outcome",
  "opponent_outcome_prev_transition_dual" = "Previous outcome, previous transition"
)

label_width = 12 #

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


p1 = transition_ig_summary %>%
  ggplot(aes(x = bot_strategy, y = mean_ig, color = bot_strategy)) +
  geom_point(size = 6) +
  geom_errorbar(
    aes(ymin = mean_ig - se, ymax = mean_ig + se),
    width = 0, linewidth = 1) +
  geom_hline(yintercept = 0, linewidth = 1, linetype = "dashed", color = "black") +
  ggtitle("Self-transition dependency exhibited against each bot") +
  scale_x_discrete(
    name = element_blank(),
    labels = STRATEGY_LABELS) +
  scale_y_continuous(
    name = "Information gain"
  ) +
  scale_color_viridis(discrete = T) +
  default_plot_theme +
  theme(
    plot.title = element_text(size = 20),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 12, angle = 0, vjust = 1),
    axis.text.y = element_text(size = 14),
    legend.position = "none"
  )


cournot_transition_ig_summary = get_ig_summary(cournot_transition_ig)

p2 = cournot_transition_ig_summary %>%
  ggplot(aes(x = bot_strategy, y = mean_ig, color = bot_strategy)) +
  geom_point(size = 6) +
  geom_errorbar(
    aes(ymin = mean_ig - se, ymax = mean_ig + se),
    width = 0, linewidth = 1) +
  geom_hline(yintercept = 0, linewidth = 1, linetype = "dashed", color = "black") +
  ggtitle("Opponent-transition dependency exhibited against each bot") +
  scale_x_discrete(
    name = element_blank(),
    labels = STRATEGY_LABELS) +
  scale_y_continuous(
    name = "Information gain"
  ) +
  scale_color_viridis(discrete = T) +
  default_plot_theme +
  theme(
    plot.title = element_text(size = 20),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 12, angle = 0, vjust = 1),
    axis.text.y = element_text(size = 14),
    legend.position = "none"
  )


p3 = p1 / p2
p3

ggsave(
  p3,
  filename = "adaptive_bot_information_gain.pdf",
  device = cairo_pdf,
  path = IMG_PATH,
  width = 9,
  height = 12,
  dpi = 300
)


# ANALYSIS: Simulate self-transition biases against opponent-transition bots ====

# TODO clean this up and make actual functions

N_SUBJECTS = 1000
N_ROUNDS = 300
MOVES = c(1, 2, 3)
MOVES_STR = c("rock", "paper", "scissors")
TRANSITIONS = c(-1, 0, 1)
TRANSITIONS_STR = c("down", "stay", "up")


simulation_df = data.frame()

# NB: takes ~60s per bias for 100 participants
for(transition_bias in TRANSITIONS) {
  for(subj in seq(N_SUBJECTS)) {
    print(paste("BEGINNING SIMULATION FOR SUBJECT: ", subj, ", BIAS: ", transition_bias))
    # Initialize subject df
    df_self_transition = data.frame(
      "subj" = numeric(),
      "subj_transition_bias" = numeric(),
      "subj_transition_bias_str" = character(),
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
        subj_move = ((df_self_transition$subj_move[round-1] + transition_bias - 1) %% 3) + 1
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
        }
        # All tied: sample randomly
        if(length(max_prob_cournot) == 3) {
          bot_predicted_subj_cournot_trans = sample(TRANSITIONS, 1)
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
        }
        # Choose bot move based on predicted human cournot transition
        # Code using mod logic below is code from Ed, very snazzy
        bot_predicted_subj_move = ((bot_predicted_subj_cournot_trans + bot_prev_move - 1) %% 3) + 1
        bot_move = (bot_predicted_subj_move %% 3) + 1
      }

      # Add move calculations above to dataframe
      new_row = data.frame(
        "subj" = subj,
        "subj_transition_bias" = transition_bias,
        "subj_transition_bias_str" = TRANSITIONS_STR[which(TRANSITIONS == transition_bias)],
        "round" = round,
        "subj_move" = subj_move,
        "subj_move_str" = MOVES_STR[subj_move],
        "subj_cournot_trans" = subj_cournot_trans,
        "subj_total_cournot_down" = subj_total_cournot_down,
        "subj_total_cournot_stay" = subj_total_cournot_stay,
        "subj_total_cournot_up" = subj_total_cournot_up,
        "subj_prob_cournot_down" = subj_prob_cournot_down,
        "subj_prob_cournot_stay" = subj_prob_cournot_stay,
        "subj_prob_cournot_up" = subj_prob_cournot_up,
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
}


# Summarize results
subject_summary = simulation_df %>%
  group_by(subj, subj_transition_bias_str) %>%
  summarize(
    "rounds" = n(),
    "subject_win_pct" = mean(subj_outcome),
    "subject_win_count_diff" = sum(subj_outcome)
  )
subject_summary
# What fraction of subjects had positive win rates?
subject_summary %>%
  group_by(subj_transition_bias_str) %>%
  filter(subject_win_pct < -0.5) %>%
  summarize(
    n()
  )

subject_summary %>%
  group_by(subj_transition_bias_str) %>%
  filter(subject_win_count_diff > 40) %>%
  summarize(
    mean_wcd_per_trial = mean(subject_win_pct),
    mean_wcd = mean(subject_win_count_diff),
    min_wins = min(subject_win_count_diff),
    max_wins = max(subject_win_count_diff),
    subjects = n()
    )


overall_summary = subject_summary %>%
  group_by(subj_transition_bias_str) %>%
  summarize(
    avg_win_rate = mean(subject_win_pct),
    avg_win_count_diff = mean(subject_win_count_diff),
    subjects = n(),
    se_win_rate = sd(subject_win_pct) / sqrt(subjects),
    se_win_count_diff = sd(subject_win_count_diff) / sqrt(subjects),
    max_win_rate = max(subject_win_pct),
    max_win_count_diff = max(subject_win_count_diff)
  )
overall_summary


# FIGURE: Simulation results (subject win rates)
subject_summary %>%
  ggplot(aes(x = subj_transition_bias_str, y = subject_win_pct, color = subj_transition_bias_str)) +
  geom_jitter(height = 0, width = 0.25, alpha = 0.25) +
  # TODO add errorbars to the below
  geom_point(data = overall_summary, aes(x = subj_transition_bias_str, y = avg_win_rate, color = subj_transition_bias_str), size = 3) +
  geom_errorbar(data = overall_summary,
                aes(x = subj_transition_bias_str, y = avg_win_rate, ymin = avg_win_rate-se_win_rate, ymax = avg_win_rate+se_win_rate, color = subj_transition_bias_str),
                width = 0.1) +
  scale_color_viridis(discrete = T) +
  labs(x = "", y = "Subject win percent", color = "Self-transition bias") +
  default_plot_theme


# FIGURE: Simulation results (subject win count differentials)
subject_summary %>%
  ggplot(aes(x = subj_transition_bias_str, y = subject_win_count_diff, color = subj_transition_bias_str)) +
  geom_jitter(height = 0, width = 0.25, alpha = 0.25) +
  # TODO add errorbars to the below
  geom_point(data = overall_summary, aes(x = subj_transition_bias_str, y = avg_win_count_diff, color = subj_transition_bias_str), size = 3) +
  geom_errorbar(data = overall_summary,
                aes(x = subj_transition_bias_str, y = avg_win_count_diff, ymin = avg_win_count_diff-se_win_count_diff, ymax = avg_win_count_diff+se_win_count_diff, color = subj_transition_bias_str),
                width = 0.1) +
  scale_color_viridis(discrete = T) +
  labs(x = "", y = "Subject win count differential", color = "Self-transition bias") +
  default_plot_theme



# ANALYSIS: Simulate opponent-transition biases against self-transition bots ====

# TODO clean this up and make actual functions

N_SUBJECTS = 1000
N_ROUNDS = 300
MOVES = c(1, 2, 3)
MOVES_STR = c("rock", "paper", "scissors")
COURNOT_TRANSITIONS = c(-1, 0, 1)
COURNOT_TRANSITIONS_STR = c("down", "stay", "up")



simulation_df = data.frame()

# NB: takes ~60s per bias for every 100 simulated participants
for(transition_bias in COURNOT_TRANSITIONS) {
  for(subj in seq(N_SUBJECTS)) {
    print(paste("BEGINNING SIMULATION FOR SUBJECT: ", subj, ", BIAS: ", transition_bias))
    # Initialize subject df
    df_cournot_transition = data.frame(
      "subj" = numeric(),
      "subj_cournot_transition_bias" = numeric(),
      "subj_cournot_transition_bias_str" = character(),
      "round" = numeric(),
      "subj_move" = numeric(),
      "subj_move_str" = character(),
      "subj_self_trans" = numeric(),
      # Note these counts are inclusive of the current round, so decisions need to be made based on counts from previous round
      "subj_total_trans_down" = numeric(),
      "subj_total_trans_stay" = numeric(),
      "subj_total_trans_up" = numeric(),
      # Note these probabilities are up to the previous round so don't align with counts in same row
      "subj_prob_trans_down" = numeric(),
      "subj_prob_trans_stay" = numeric(),
      "subj_prob_trans_up" = numeric(),
      "bot_predicted_subj_self_trans" = numeric(),
      "bot_predicted_subj_move" = numeric(),
      "bot_move" = numeric(),
      "bot_move_str" = character()
    )

    # Add moves for an individual subject's simulation, round by round!
    for(round in seq(N_ROUNDS)) {
      if(round == 1) {
        subj_move = sample(MOVES, 1)
        subj_self_trans = NA
        subj_total_trans_down = 0
        subj_total_trans_stay = 0
        subj_total_trans_up = 0
        subj_prob_trans_down = 0
        subj_prob_trans_stay = 0
        subj_prob_trans_up = 0

        # Choose bot move
        bot_predicted_subj_self_trans = NA
        bot_predicted_subj_move = NA
        bot_move = sample(MOVES, 1)
      } else {
        # Code using mod logic below is code from Ed, very snazzy
        subj_move = ((df_cournot_transition$bot_move[round-1] + transition_bias - 1) %% 3) + 1
        subj_prev_move = df_cournot_transition$subj_move[round-1]
        subj_self_trans = ifelse(
          (subj_move - subj_prev_move) %% 3 == 2,
          -1,
          (subj_move - subj_prev_move) %% 3
        )

        subj_total_trans_down = df_cournot_transition$subj_total_trans_down[round-1] + (subj_self_trans == -1)
        subj_total_trans_stay = df_cournot_transition$subj_total_trans_stay[round-1] + (subj_self_trans == 0)
        subj_total_trans_up = df_cournot_transition$subj_total_trans_up[round-1] + (subj_self_trans == 1)

        total_transitions = max(sum(df_cournot_transition$subj_total_trans_down[round-1], # max avoids division by 0 below
                                    df_cournot_transition$subj_total_trans_stay[round-1],
                                    df_cournot_transition$subj_total_trans_up[round-1]), 0.001)
        subj_prob_trans_down = df_cournot_transition$subj_total_trans_down[round-1] / total_transitions
        subj_prob_trans_stay = df_cournot_transition$subj_total_trans_stay[round-1] / total_transitions
        subj_prob_trans_up = df_cournot_transition$subj_total_trans_up[round-1] / total_transitions

        # Identify most likely human self-transitions, break tie if needed
        bot_predicted_subj_self_trans = NA
        self_trans_probs = c(subj_prob_trans_down, subj_prob_trans_stay, subj_prob_trans_up)
        max_prob_self_trans = which(self_trans_probs == max(self_trans_probs)) # Which of the above is max? (includes ties)
        # Single max: predicted transition is index of max in COURNOT_TRANSITIONS
        if(length(max_prob_self_trans) == 1) {
          bot_predicted_subj_self_trans = COURNOT_TRANSITIONS[max_prob_self_trans]
          # print(paste("1 max: ", round, ": ", max_prob_cournot, "choice: ", bot_predicted_subj_cournot_trans))
        }
        # All tied: sample randomly
        if(length(max_prob_self_trans) == 3) {
          bot_predicted_subj_self_trans = sample(COURNOT_TRANSITIONS, 1)
          # print(paste("3-way tie: ", round, "choice: ", bot_predicted_subj_cournot_trans))
        }
        # 2-way tie. Determine which elements are tied based on sum of `max_prob_self_trans`,
        # then sample transition from among relevant tied transitions
        if(length(max_prob_self_trans) == 2) {
          ties = sum(max_prob_self_trans)
          bot_predicted_subj_self_trans = case_when(
            ties == 3 ~ sample(c(COURNOT_TRANSITIONS[1], COURNOT_TRANSITIONS[2]), 1),
            ties == 4 ~ sample(c(COURNOT_TRANSITIONS[1], COURNOT_TRANSITIONS[3]), 1),
            ties == 5 ~ sample(c(COURNOT_TRANSITIONS[2], COURNOT_TRANSITIONS[3]), 1)
          )
          # print(paste("2-way tie: ", round, "choice: ", bot_predicted_subj_cournot_trans))
          # print(max_prob_cournot)
        }
        # Choose bot move based on predicted human self-transition
        # Code using mod logic below is code from Ed, very snazzy
        bot_predicted_subj_move = ((bot_predicted_subj_self_trans + subj_prev_move - 1) %% 3) + 1
        bot_move = (bot_predicted_subj_move %% 3) + 1
      }

      # Add move calculations above to dataframe
      new_row = data.frame(
        "subj" = subj,
        "subj_cournot_transition_bias" = transition_bias,
        "subj_cournot_transition_bias_str" = COURNOT_TRANSITIONS_STR[which(COURNOT_TRANSITIONS == transition_bias)],
        "round" = round,
        "subj_move" = subj_move,
        "subj_move_str" = MOVES_STR[subj_move],
        "subj_self_trans" = subj_self_trans,
        "subj_total_trans_down" = subj_total_trans_down,
        "subj_total_trans_up" = subj_total_trans_up,
        "subj_total_trans_stay" = subj_total_trans_stay,
        "subj_prob_trans_down" = subj_prob_trans_down,
        "subj_prob_trans_stay" = subj_prob_trans_stay,
        "subj_prob_trans_up" = subj_prob_trans_up,
        "bot_predicted_subj_self_trans" = bot_predicted_subj_self_trans,
        "bot_predicted_subj_move" = bot_predicted_subj_move,
        "bot_move" = bot_move,
        "bot_move_str" = MOVES_STR[bot_move]
      )
      df_cournot_transition = bind_rows(
        df_cournot_transition,
        new_row
      )
    }

    # Calculate outcomes based on simulated bot moves above
    df_cournot_transition = df_cournot_transition %>%
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
      df_cournot_transition
    )
  }
}


# Summarize results
subject_summary = simulation_df %>%
  group_by(subj, subj_cournot_transition_bias_str) %>%
  summarize(
    "rounds" = n(),
    "subject_win_pct" = mean(subj_outcome),
    "subject_win_count_diff" = sum(subj_outcome)
  )
subject_summary
# What fraction of subjects had positive win rates?
subject_summary %>%
  group_by(subj_cournot_transition_bias_str) %>%
  filter(subject_win_pct < -0.5) %>%
  summarize(n())

subject_summary %>%
  group_by(subj_cournot_transition_bias_str) %>%
  filter(subject_win_pct > -0.1) %>%
  summarize(
    mean_wcd_per_trial = mean(subject_win_pct),
    mean_wcd = mean(subject_win_count_diff),
    min_wcd = min(subject_win_pct),
    max_wcd = max(subject_win_pct),
    subjects = n()
  )


overall_summary = subject_summary %>%
  group_by(subj_cournot_transition_bias_str) %>%
  summarize(
    avg_win_rate = mean(subject_win_pct),
    avg_win_count_diff = mean(subject_win_count_diff),
    subjects = n(),
    se_win_rate = sd(subject_win_pct) / sqrt(subjects),
    se_win_count_diff = sd(subject_win_count_diff) / sqrt(subjects),
    max_win_rate = max(subject_win_pct),
    max_win_count_diff = max(subject_win_count_diff)
  )
overall_summary


# FIGURE: Simulation results (subject win percentage)
subject_summary %>%
  ggplot(aes(x = subj_cournot_transition_bias_str, y = subject_win_pct, color = subj_cournot_transition_bias_str)) +
  geom_jitter(height = 0, width = 0.25, alpha = 0.25) +
  geom_point(data = overall_summary, aes(x = subj_cournot_transition_bias_str, y = avg_win_rate, color = subj_cournot_transition_bias_str), size = 3) +
  geom_errorbar(data = overall_summary,
                aes(x = subj_cournot_transition_bias_str, y = avg_win_rate, ymin = avg_win_rate-se_win_rate, ymax = avg_win_rate+se_win_rate, color = subj_cournot_transition_bias_str),
                width = 0.1) +
  scale_color_viridis(discrete = T) +
  labs(x = "", y = "Subject win percent", color = "Opponent-transition bias") +
  default_plot_theme


# FIGURE: Simulation results (subject win count differential)
subject_summary %>%
  ggplot(aes(x = subj_cournot_transition_bias_str, y = subject_win_count_diff, color = subj_cournot_transition_bias_str)) +
  geom_jitter(height = 0, width = 0.25, alpha = 0.25) +
  geom_point(data = overall_summary, aes(x = subj_cournot_transition_bias_str, y = avg_win_count_diff, color = subj_cournot_transition_bias_str), size = 3) +
  geom_errorbar(data = overall_summary,
                aes(x = subj_cournot_transition_bias_str, y = avg_win_count_diff, ymin = avg_win_count_diff-se_win_count_diff, ymax = avg_win_count_diff+se_win_count_diff, color = subj_cournot_transition_bias_str),
                width = 0.1) +
  scale_color_viridis(discrete = T) +
  labs(x = "", y = "Subject win count differential", color = "Opponent-transition bias") +
  default_plot_theme

