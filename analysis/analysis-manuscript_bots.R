#
# This script contains the final analysis of the *stable bot* experiment
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

DATA_FILE = "rps_v2_data.csv" # name of file containing full dataset for all rounds

GAME_ROUNDS = 300

# Bot analysis globals
STRATEGY_LEVELS = c("prev_move_positive", "prev_move_negative",
                    "opponent_prev_move_positive", "opponent_prev_move_nil",
                    "win_nil_lose_positive", "win_positive_lose_negative",
                    "outcome_transition_dual_dependency")
STRATEGY_LOOKUP = list("prev_move_positive" = "Previous move (+)",
                       "prev_move_negative" = "Previous move (-)",
                       "opponent_prev_move_positive" = "Opponent previous move (+)",
                       "opponent_prev_move_nil" = "Opponent previous move (0)",
                       "win_nil_lose_positive" = "Win-stay-lose-positive",
                       "win_positive_lose_negative" = "Win-positive-lose-negative",
                       "outcome_transition_dual_dependency" = "Previous outcome, previous transition")

TRANSITION_STRATEGIES = c("prev_move_positive", "prev_move_negative",
                          "opponent_prev_move_positive", "opponent_prev_move_nil")
OUTCOME_STRATEGIES = c("win_nil_lose_positive", "win_positive_lose_negative")
DUAL_DEPENDENCY_STRATEGIES = c("outcome_transition_dual_dependency")



# DATA PROCESSING FUNCTIONS ====

# Note: the raw data csv has substantially more unique participant data
# than we keep in this function. Many participants did not complete the full set of rounds.
# Several managed to complete 300 rounds twice with the same survey code: in this instance,
# we keep the earlier of the two to ensure parity with other participants.
# Finally, one participant emailed assuring that she had completed the full set of rounds,
# though we don't have complete data. Thus, 218 students are given credit in SONA,
# while the below produces 217 complete participant data sets.
read_bot_data = function(filename, strategies, game_rounds) {
  data = read_csv(filename)
  data$bot_strategy = factor(data$bot_strategy, levels = strategies)

  # Remove all incomplete games
  incomplete_games = data %>%
    group_by(game_id, player_id) %>%
    summarize(rounds = max(round_index)) %>%
    filter(rounds < game_rounds) %>%
    select(game_id) %>%
    unique()

  data = data %>%
    filter(!(game_id %in% incomplete_games$game_id))

  # Remove any duplicate complete games that have the same SONA survey code
  # NB: this can happen if somebody played all the way through but exited before receiving credit
  # First, fetch sona survey codes with multiple complete games
  repeat_codes = data %>%
    group_by(sona_survey_code) %>%
    filter(is_bot == 0) %>%
    summarize(trials = n()) %>%
    filter(trials > game_rounds) %>%
    select(sona_survey_code)

  # Next, get game id for the earlier complete game
  # NB: commented out code checks that we have slider/free resp data for at least one of the games
  duplicate_games = data %>%
    filter(sona_survey_code %in% repeat_codes$sona_survey_code &
             is_bot == 0  &
             round_index == game_rounds) %>%
    select(sona_survey_code, game_id, player_id, round_begin_ts) %>%
    # remove the earlier one since the later one has free response and slider data (confirm with joins below)
    group_by(sona_survey_code) %>%
    filter(round_begin_ts == min(round_begin_ts)) %>%
    # inner_join(fr_data, by = c("game_id", "player_id")) %>%
    # inner_join(slider_data, by = c("game_id", "player_id")) %>%
    distinct(game_id)

  data = data %>%
    filter(!game_id %in% duplicate_games$game_id)

  return(data)
}


# ANALYSIS FUNCTIONS ====

get_bot_strategy_win_count_differential = function(data) {
  win_diff = data %>%
    group_by(bot_strategy, game_id, player_id, is_bot) %>%
    count(win_count = player_outcome == "win") %>%
    filter(win_count == TRUE) %>%
    group_by(bot_strategy, game_id) %>%
    summarize(win_count_diff = n[is_bot == 0] - n[is_bot == 1]) %>%
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

# Divide each subject's trials into blocks of size blocksize (e.g. 10 trials)
# then get each subject's win percent in each block
get_subject_block_data = function(data, blocksize) {
  data %>%
    filter(is_bot == 0) %>%
    group_by(bot_strategy, round_index) %>%
    mutate(round_block = ceiling(round_index / blocksize)) %>%
    select(bot_strategy, round_index, game_id, player_id, player_outcome, round_block) %>%
    group_by(bot_strategy, game_id, player_id, round_block) %>%
    count(win = player_outcome == "win") %>%
    mutate(total = sum(n),
           win_pct = n / total) %>%
    filter(win == TRUE)
}

# Take in subject block win percent (calculated above) and summarize by bot strategy across subjects
get_block_data_summary = function(subject_block_data) {
  subject_block_data %>%
    group_by(bot_strategy, round_block) %>%
    summarize(subjects = n(),
              mean_win_pct = mean(win_pct),
              se_win_pct = sd(win_pct) / sqrt(subjects),
              lower_ci = mean_win_pct - se_win_pct,
              upper_ci = mean_win_pct + se_win_pct)
}

# Get loss percent for each bot dependent on their previous move
get_bot_prev_move_loss_pct = function(data, cutoff) {
  data %>%
    filter(round_index > cutoff, # TODO exploratory
           bot_strategy == "prev_move_positive" | bot_strategy == "prev_move_negative") %>%
    group_by(player_id) %>%
    mutate(prev_move = lag(player_move, 1)) %>%
    filter(is_bot == 1, # look only at bot prev moves
           !is.na(prev_move), # lag call above sets NA for lag on first move: ignore it here
           prev_move != "none") %>%
    group_by(bot_strategy, game_id, player_id, prev_move) %>%
    count(player_outcome) %>%
    filter(!is.na(player_outcome)) %>% # TODO why do we have NA game outcomes??
    group_by(bot_strategy, game_id, player_id, prev_move) %>%
    # player win percent calculated as bot loss percent
    summarize(player_win_pct = max(0, n[player_outcome == "loss"]) / max(1, sum(n)))
}

# Get win percent for each player dependent on their own previous move
get_player_prev_move_win_pct = function(data, cutoff) {
  data %>%
    filter(is_bot == 0,
           round_index > cutoff, # TODO exploratory
           bot_strategy == "opponent_prev_move_nil" | bot_strategy == "opponent_prev_move_positive") %>%
    group_by(player_id) %>%
    mutate(prev_move = lag(player_move, 1)) %>%
    filter(!is.na(prev_move), # lag call above sets NA for lag on first move: ignore it here
           prev_move != "none") %>%
    group_by(bot_strategy, game_id, player_id, prev_move) %>%
    count(player_outcome) %>%
    filter(!is.na(player_outcome)) %>% # TODO why do we have NA game outcomes??
    group_by(bot_strategy, game_id, player_id, prev_move) %>%
    # player win percent calculated using player win outcomes
    summarize(player_win_pct = max(0, n[player_outcome == "win"]) / max(1, sum(n)))
}

# Get loss percent for each bot dependent on the bot's previous outcome
get_bot_prev_outcome_loss_pct = function(data, cutoff) {
  data %>%
    filter(round_index > cutoff, # TODO exploratory
           bot_strategy == "win_nil_lose_positive" | bot_strategy == "win_positive_lose_negative") %>%
    group_by(player_id) %>%
    mutate(prev_outcome = lag(player_outcome, 1)) %>% # player_outcome is bot's outcome for bot rows
    filter(is_bot == 1, # look only at bot prev moves
           !is.na(prev_outcome)) %>% # lag call above sets NA for lag on first move: ignore it here
    group_by(bot_strategy, game_id, player_id, prev_outcome) %>%
    count(player_outcome) %>%
    filter(!is.na(player_outcome)) %>% # TODO why do we have NA game outcomes??
    group_by(bot_strategy, game_id, player_id, prev_outcome) %>%
    # player win percent calculated as bot loss percent
    summarize(player_win_pct = max(0, n[player_outcome == "loss"]) / max(1, sum(n)))
}

# Get summary win percent by strategy (dependent on previous move by bot or player)
get_bot_prev_move_win_pct_summary = function(prev_move_data) {
  prev_move_data %>%
    group_by(bot_strategy, prev_move) %>%
    summarize(mean_player_win_pct = mean(player_win_pct),
              n = n(),
              se = sd(player_win_pct) / sqrt(n),
              se_lower = mean_player_win_pct - se,
              se_upper = mean_player_win_pct + se)
}

# Get summary win percent by strategy (dependent on previous outcome by bot or player)
get_bot_prev_outcome_win_pct_summary = function(bot_loss_prev_outcome) {
  bot_loss_prev_outcome %>%
    group_by(bot_strategy, prev_outcome) %>%
    summarize(mean_player_win_pct = mean(player_win_pct),
              n = n(),
              se = sd(player_win_pct) / sqrt(n),
              se_lower = mean_player_win_pct - se,
              se_upper = mean_player_win_pct + se)
}



# GRAPHING STYLE FUNCTIONS ====

individ_plot_theme = theme(
  # titles
  plot.title = element_text(face = "bold", size = 24),
  axis.title.y = element_text(face = "bold", size = 20),
  axis.title.x = element_text(face = "bold", size = 20),
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


# GRAPHING FUNCTIONS ====

# Plot mean + SEM of each strategy
plot_bot_strategy_win_count_differential_summary = function(wcd_summary) {
  label_width = 10
  summary_labels = c("prev_move_positive" = str_wrap(STRATEGY_LOOKUP[["prev_move_positive"]], label_width),
                     "prev_move_negative" = str_wrap(STRATEGY_LOOKUP[["prev_move_negative"]], label_width),
                     "opponent_prev_move_positive" = str_wrap(STRATEGY_LOOKUP[["opponent_prev_move_positive"]], label_width),
                     "opponent_prev_move_nil" = str_wrap(STRATEGY_LOOKUP[["opponent_prev_move_nil"]], label_width),
                     "win_nil_lose_positive" = str_wrap(STRATEGY_LOOKUP[["win_nil_lose_positive"]], label_width),
                     "win_positive_lose_negative" = str_wrap(STRATEGY_LOOKUP[["win_positive_lose_negative"]], label_width),
                     "outcome_transition_dual_dependency" = str_wrap(STRATEGY_LOOKUP[["outcome_transition_dual_dependency"]], label_width))

  wcd_summary %>%
    ggplot(aes(x = bot_strategy, y = mean_win_count_diff)) +
    geom_point(aes(color = bot_strategy),
               size = 6) +
    geom_errorbar(aes(color = bot_strategy, ymin = lower_se, ymax = upper_se),
                  width = 0.25, size = 1) +
    geom_hline(yintercept = 0, size = 1, linetype = "dashed", color = "red") +
    labs(x = "", y = "Mean win count differential") +
    #ggtitle("Win count differential across bot strategies") +
    ggtitle("Aggregate") +
    #scale_x_discrete(labels = summary_labels) +
    scale_color_viridis(discrete = TRUE,
                        name = element_blank()) +
    individ_plot_theme +
    theme(
      # plot.title = element_text(size = 32, face = "bold"),
      axis.title.y = element_text(size = 24, face = "bold"),
      # axis.text.x = element_text(size = 20, face = "bold", angle = 0, vjust = 1),
      # axis.text.x = element_text(size = 20, face = "bold", angle = 0, vjust = 1),
      axis.text.x = element_blank(),
      # axis.text.y = element_text(face = "bold", size = 20),
      legend.position = "none"
    )
}

# Plot average of each participant's win percent in blocks of trials by strategy
plot_bot_strategy_win_pct_by_block = function(block_data_summary) {
  label_width = 12
  strategy_labels = c("prev_move_positive" = str_wrap(STRATEGY_LOOKUP[["prev_move_positive"]], label_width),
                      "prev_move_negative" = str_wrap(STRATEGY_LOOKUP[["prev_move_negative"]], label_width),
                      "opponent_prev_move_nil" = str_wrap(STRATEGY_LOOKUP[["opponent_prev_move_nil"]], label_width),
                      "opponent_prev_move_positive" = str_wrap(STRATEGY_LOOKUP[["opponent_prev_move_positive"]], label_width),
                      "win_nil_lose_positive" = str_wrap(STRATEGY_LOOKUP[["win_nil_lose_positive"]], label_width),
                      "win_positive_lose_negative" = str_wrap(STRATEGY_LOOKUP[["win_positive_lose_negative"]], label_width),
                      "outcome_transition_dual_dependency" = str_wrap(STRATEGY_LOOKUP[["outcome_transition_dual_dependency"]], label_width))

  block_labels = c("1" = "30", "2" = "60", "3" = "90", "4" = "120", "5" = "150",
                   "6" = "180", "7" = "210", "8" = "240", "9" = "270", "10" = "300")

  block_data_summary %>%
    ggplot(aes(x = round_block, y = mean_win_pct, color = bot_strategy)) +
    geom_point(size = 6, alpha = 0.75) +
    geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), size = 1, width = 0.25, alpha = 0.75) +
    geom_hline(yintercept = 1 / 3, linetype = "dashed", color = "red", size = 1) +
    labs(x = "Game round", y = "Mean win percentage") +
    # ggtitle("Participant win percentage against bot strategies") +
    ggtitle("By Round") +
    scale_color_viridis(discrete = T,
                        name = element_blank(),
                        labels = strategy_labels) +
    scale_x_continuous(labels = block_labels, breaks = seq(1:10)) +
    individ_plot_theme +
    theme(#axis.text.x = element_blank(),
      axis.title.y = element_text(size = 24, face = "bold"),
      legend.text = element_text(face = "bold", size = 14),
      legend.position = "right",
      legend.spacing.y = unit(1.0, 'lines'),
      #legend.key = element_rect(size = 2),
      legend.key.size = unit(4.75, 'lines'))
}

# Plot average win percent based on previous move dependency
plot_prev_move_win_pct = function(bot_loss_summary_prev_move, strategy, xlabel) {
  bot_loss_summary_prev_move %>%
    filter(bot_strategy == strategy) %>%
    ggplot(aes(x = prev_move, y = mean_player_win_pct, fill = strategy)) +
    # geom_bar(stat = "identity", alpha = 0.5, color = "grey50", fill = "steelblue") +
    geom_bar(stat = "identity", alpha = 0.5) +
    geom_errorbar(aes(ymin = se_lower, ymax = se_upper), width = 0.5, size = 1, color = "midnightblue") +
    geom_hline(yintercept = 1 / 3, linetype = "dashed", color = "red", size = 1) +
    scale_y_continuous(labels = seq(0, 0.9, by = 0.3),
                       breaks = seq(0, 0.9, by = 0.3),
                       limits = c(0, 0.9)) +
    scale_fill_viridis_d(begin = 0.25) +
    labs(x = xlabel, y = "Mean win percentage") +
    ggtitle(STRATEGY_LOOKUP[[strategy]]) +
    individ_plot_theme +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 0, vjust = 1))
}

# Plot average win percent based on outcome dependency
plot_outcome_win_pct = function(bot_loss_summary_prev_outcome, strategy, xlabel) {
  bot_loss_summary_prev_outcome %>%
    filter(bot_strategy == strategy) %>%
    ggplot(aes(x = prev_outcome, y = mean_player_win_pct, fill = strategy)) +
    # geom_bar(stat = "identity", alpha = 0.5, color = "grey50", fill = "steelblue") +
    geom_bar(stat = "identity", alpha = 0.5) +
    geom_errorbar(aes(ymin = se_lower, ymax = se_upper), width = 0.5, size = 1, color = "midnightblue") +
    geom_hline(yintercept = 1 / 3, linetype = "dashed", color = "red", size = 1) +
    scale_y_continuous(labels = seq(0, 0.9, by = 0.3),
                       breaks = seq(0, 0.9, by = 0.3),
                       limits = c(0, 0.9)) +
    scale_fill_viridis_d(begin = 0.75) +
    labs(x = xlabel, y = "Mean win percentage") +
    ggtitle(STRATEGY_LOOKUP[[strategy]]) +
    individ_plot_theme +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 0, vjust = 1))
}




# INITIALIZATION ====
bot_data = read_bot_data(DATA_FILE, STRATEGY_LEVELS, GAME_ROUNDS)


# ANALYSIS: Participants, completion time ====

# How many complete participants do we have for each bot strategy?
bot_data %>%
  filter(is_bot == 0) %>%
  group_by(bot_strategy) %>%
  summarize(n = n() / GAME_ROUNDS)


# How long did participants take to complete the task?
bot_data %>%
  filter(is_bot == 0) %>%
  group_by(player_id) %>%
  summarize(expt_completion_sec = (round_begin_ts[round_index == GAME_ROUNDS] - round_begin_ts[round_index == 1]) / 1000) %>%
  summarize(mean_expt_completion_sec = mean(expt_completion_sec),
            sd_expt_completion_sec = sd(expt_completion_sec))



# ANALYSIS: Power ====

# With 30 participants in lowest bot condition, what effect size do we have 90% power to detect?
# and what win percentage does that correspond to?
# https://cran.r-project.org/web/packages/pwr/vignettes/pwr-vignette.html
power = pwr.t.test(n = 30, sig.level = 0.05, power = 0.9, type = "one.sample", alternative = "two.sided")
# we have 90% power to detect an effect size of d = 0.61
(1 / 3) + power$d * sqrt(1 / 12)
# this is equivalent to an average win rate of about 51% assuming individual win rates are uniformly distributed (unlikely)

# TODO add clarification to this: why the sqrt(1/12)?


# ANALYSIS: Win rates, win count differentials ====

# Calculate WCD across all rounds
wcd_all = get_bot_strategy_win_count_differential(bot_data)

# Print average and SE of *WCD* for each strategy "group"
for (strat_class in list(TRANSITION_STRATEGIES, OUTCOME_STRATEGIES, DUAL_DEPENDENCY_STRATEGIES)) {
  print(strat_class)
  print(
    wcd_all %>%
      filter(bot_strategy %in% strat_class) %>%
      summarize(
        players = n(),
        wcd_mean = mean(win_count_diff),
        wcd_se = sd(win_count_diff) / sqrt(players)
      )
  )
}

# Print average and SE of *win rates* for each strategy "group"
# Note comparison to power analysis above
for (strat_class in list(TRANSITION_STRATEGIES, OUTCOME_STRATEGIES, DUAL_DEPENDENCY_STRATEGIES)) {
  print(strat_class)
  print(
    bot_data %>%
      filter(is_bot == 0,
             bot_strategy %in% strat_class) %>%
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


# FIGURE: Bot win count differentials ====

# Get average WCD for each strategy
wcd_summary = get_bot_strategy_win_count_differential_summary(wcd_all)
# Generate figure
fig_overall = plot_bot_strategy_win_count_differential_summary(wcd_summary)

# Calculate WCD by blocks
subject_block_data = get_subject_block_data(bot_data, blocksize = 30)
block_data_summary = get_block_data_summary(subject_block_data)
# Generate figure
fig_rounds = plot_bot_strategy_win_pct_by_block(block_data_summary)

# Draw figures
fig_overall + fig_rounds +
  plot_layout(widths = c(1, 2))
  # plot_annotation(tag_levels = 'A') &
  # theme(plot.tag = element_text(size = 24))

# TODO save to pdf


# ANALYSIS: Conditional win percentages ====

CUTOFF = 200

# 1. Bot previous move strategies
bot_loss_prev_move = get_bot_prev_move_loss_pct(bot_data, CUTOFF)
bot_loss_summary_prev_move = get_bot_prev_move_win_pct_summary(bot_loss_prev_move)

# 2. Player previous move strategies
player_win_prev_move = get_player_prev_move_win_pct(bot_data, CUTOFF)
player_win_summary_prev_move = get_bot_prev_move_win_pct_summary(player_win_prev_move)

# 3. Bot previous outcome
bot_loss_prev_outcome = get_bot_prev_outcome_loss_pct(bot_data, CUTOFF)
bot_loss_summary_prev_outcome = get_bot_prev_outcome_win_pct_summary(bot_loss_prev_outcome)



# Transition strategies ANOVA
# In all but final case below, no difference between prev move win percentages
summary(
  aov(data = bot_loss_prev_move %>% filter(bot_strategy == "prev_move_positive"),
      player_win_pct ~ prev_move + Error(player_id)
    )
)
summary(
  aov(data = bot_loss_prev_move %>% filter(bot_strategy == "prev_move_negative"),
      player_win_pct ~ prev_move + Error(player_id)
  )
)
summary(
  aov(data = player_win_prev_move %>% filter(bot_strategy == "opponent_prev_move_positive"),
      player_win_pct ~ prev_move + Error(player_id)
  )
)
summary(
  aov(data = player_win_prev_move %>% filter(bot_strategy == "opponent_prev_move_nil"),
      player_win_pct ~ prev_move + Error(player_id)
  )
)

# Transition strategies t tests
# TODO there's certainly a more efficient way to do this lol...
# All win rates significantly greater than chance
dat = bot_loss_prev_move %>% filter(bot_strategy == "prev_move_positive")
for (move in c("rock", "paper", "scissors")) {
  print(move)
  print(
    t.test(
      dat$player_win_pct[dat$prev_move == move],
      mu = 1/3
    )
  )
}
dat = bot_loss_prev_move %>% filter(bot_strategy == "prev_move_negative")
for (move in c("rock", "paper", "scissors")) {
  print(move)
  print(
    t.test(
      dat$player_win_pct[dat$prev_move == move],
      mu = 1/3
    )
  )
}
dat = player_win_prev_move %>% filter(bot_strategy == "opponent_prev_move_positive")
for (move in c("rock", "paper", "scissors")) {
  print(move)
  print(
    t.test(
      dat$player_win_pct[dat$prev_move == move],
      mu = 1/3
    )
  )
}
dat = player_win_prev_move %>% filter(bot_strategy == "opponent_prev_move_nil")
for (move in c("rock", "paper", "scissors")) {
  print(move)
  print(
    t.test(
      dat$player_win_pct[dat$prev_move == move],
      mu = 1/3
    )
  )
}



# Outcome-transition strategies ANOVA
# Significant difference in prev outcome win percentages driven by higher percentage after tie
summary(
  aov(data = bot_loss_prev_outcome %>% filter(bot_strategy == "win_nil_lose_positive"),
      player_win_pct ~ prev_outcome + Error(player_id)
  )
)
summary(
  aov(data = bot_loss_prev_outcome %>% filter(bot_strategy == "win_positive_lose_negative"),
      player_win_pct ~ prev_outcome + Error(player_id)
  )
)

# Outcome-transition strategies t tests
# TODO there's certainly a more efficient way to do this lol...
# All win rates significantly greater than chance after a *tie* only
dat = bot_loss_prev_outcome %>% filter(bot_strategy == "win_nil_lose_positive")
for (outcome in c("win", "loss", "tie")) {
  print(outcome)
  print(
    t.test(
      dat$player_win_pct[dat$prev_outcome == outcome],
      mu = 1/3
    )
  )
}
dat = bot_loss_prev_outcome %>% filter(bot_strategy == "win_positive_lose_negative")
for (outcome in c("win", "loss", "tie")) {
  print(outcome)
  print(
    t.test(
      dat$player_win_pct[dat$prev_outcome == outcome],
      mu = 1/3
    )
  )
}






# FIGURE: Conditional win percentages ====

# 1. Bot previous move strategies
prev_move_positive_plot = plot_prev_move_win_pct(bot_loss_summary_prev_move, "prev_move_positive", "Bot previous move")
prev_move_negative_plot = plot_prev_move_win_pct(bot_loss_summary_prev_move, "prev_move_negative", "Bot previous move")

# 2. Player previous move strategies
opponent_prev_move_positive_plot = plot_prev_move_win_pct(player_win_summary_prev_move, "opponent_prev_move_positive", "Player previous move")
opponent_prev_move_nil_plot = plot_prev_move_win_pct(player_win_summary_prev_move, "opponent_prev_move_nil", "Player previous move")

# 3. Bot previous outcome
win_nil_lose_positive_plot_outcome = plot_outcome_win_pct(bot_loss_summary_prev_outcome, "win_nil_lose_positive", "Bot previous outcome")
win_positive_lose_negative_plot_outcome = plot_outcome_win_pct(bot_loss_summary_prev_outcome, "win_positive_lose_negative", "Bot previous outcome")


# Plot using patchwork
prev_move_positive_plot + prev_move_negative_plot +
  opponent_prev_move_positive_plot + opponent_prev_move_nil_plot +
  win_nil_lose_positive_plot_outcome + win_positive_lose_negative_plot_outcome +
  plot_layout(ncol = 2) +
  plot_annotation(tag_levels = 'A') &
  theme(plot.tag = element_text(size = 24))


# TODO save to pdf

