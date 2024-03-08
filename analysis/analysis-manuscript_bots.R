
#
# This script contains the final analyses of the *stable bot* experiment for the rps journal submission
#


# SETUP ====

# setwd("/Users/erikbrockbank/dev/research/vullab/rps-bot-manuscript-public/analysis")
library(tidyverse)
library(viridis)
library(patchwork)
library(pwr)
library(scales)

IMG_PATH = "../figures"


# ANALYSIS FUNCTIONS ====

# Divide each subject's trials into blocks of size blocksize (e.g. 10 trials)
# then get each subject's win percent in each block
get_subject_block_win_pct = function(data, blocksize) {
  data %>%
    filter(is_bot == 0) %>%
    group_by(bot_strategy, round_index) %>%
    # NB: round_block is 0-indexed
    mutate(round_block = ceiling(round_index / blocksize) - 1) %>%
    select(bot_strategy, round_index, game_id, player_id, player_outcome, round_block) %>%
    group_by(bot_strategy, game_id, player_id, round_block) %>%
    count(win = player_outcome == "win") %>%
    mutate(total = sum(n),
           win_pct = n / total) %>%
    filter(win == TRUE)
}

# Take in subject block win percent (calculated above) and summarize by bot strategy across subjects
get_block_win_pct_summary = function(subject_block_data) {
  subject_block_data %>%
    group_by(bot_strategy, round_block) %>%
    summarize(subjects = n(),
              mean_win_pct = mean(win_pct),
              se_win_pct = sd(win_pct) / sqrt(subjects))
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



# GRAPHING STYLE ====

default_plot_theme = theme(
  # text
  plot.title = element_text(size = 24, family = "Avenir", color = "black", margin = margin(b = 0.5, unit = "line")),
  axis.title.y = element_text(size = 24, family = "Avenir", color = "black", margin = margin(r = 0.5, unit = "line")),
  axis.title.x = element_text(size = 24, family = "Avenir", color = "black", margin = margin(t = 0.5, unit = "line")),
  axis.text.y = element_text(size = 18, family = "Avenir", color = "black"),
  axis.text.x = element_text(size = 18, family = "Avenir", color = "black"),
  legend.title = element_text(size = 20, family = "Avenir", color = "black"),
  legend.text = element_text(size = 14, family = "Avenir", color = "black"),
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


# GRAPHING FUNCTIONS ====

# Plot average win percent based on previous move dependency
plot_prev_move_win_pct = function(bot_loss_summary_prev_move, strategy, title, xlabel) {
  bot_loss_summary_prev_move %>%
    filter(bot_strategy == strategy) %>%
    ggplot(aes(x = prev_move, y = mean_player_win_pct, fill = strategy)) +
    geom_bar(stat = "identity", alpha = 0.75, width = 0.75) +
    geom_errorbar(aes(ymin = se_lower, ymax = se_upper), width = 0, linewidth = 1, color = "black") +
    geom_hline(yintercept = 1/3, linewidth = 0.75, linetype = "dashed", color = "black") +
    geom_hline(yintercept = 0.9, linewidth = 0.75, linetype = "solid", color = "black") +
    scale_y_continuous(
      name = "Win percentage",
      breaks = seq(0, 0.9, by = 0.3),
      labels = as.character(seq(0, 0.9, by = 0.3)),
      limits = c(0, 0.9)
    ) +
    scale_x_discrete(
      name = xlabel
    ) +
    scale_fill_viridis_d(begin = 0.25) +
    ggtitle(title) +
    default_plot_theme +
    theme(
      axis.ticks.x = element_blank(),
      axis.text.x = element_text(size = 20, family = "Avenir", color = "black"),
      legend.position = "none"
    )
}

# Plot average win percent based on outcome dependency
plot_outcome_win_pct = function(bot_loss_summary_prev_outcome, strategy, title, xlabel) {
  bot_loss_summary_prev_outcome %>%
    filter(bot_strategy == strategy) %>%
    ggplot(aes(x = prev_outcome, y = mean_player_win_pct, fill = strategy)) +
    geom_bar(stat = "identity", alpha = 0.75, width = 0.75) +
    geom_errorbar(aes(ymin = se_lower, ymax = se_upper), width = 0, linewidth = 1, color = "black") +
    geom_hline(yintercept = 1/3, linewidth = 0.75, linetype = "dashed", color = "black") +
    geom_hline(yintercept = 0.9, linewidth = 0.75, linetype = "solid", color = "black") +
    scale_y_continuous(
      name = "Win percentage",
      breaks = seq(0, 0.9, by = 0.3),
      labels = as.character(seq(0, 0.9, by = 0.3)),
      limits = c(0, 0.9)
    ) +
    scale_x_discrete(
      name = xlabel
    ) +
    scale_fill_viridis_d(begin = 0.75) +
    ggtitle(title) +
    default_plot_theme +
    theme(
      axis.ticks.x = element_blank(),
      axis.text.x = element_text(size = 18, family = "Avenir", color = "black"),
      legend.position = "none"
    )
}




# INITIALIZATION ====
load("../data/rps_v2_data.RData")

# How many complete participants do we have for each bot strategy?
bot_data %>%
  filter(is_bot == 0) %>%
  group_by(bot_strategy) %>%
  summarize(n = n() / max(round_index))


# How long did participants take to complete the task?
bot_data %>%
  filter(is_bot == 0) %>%
  group_by(player_id) %>%
  summarize(expt_completion_sec = (round_begin_ts[round_index == max(bot_data$round_index)] - round_begin_ts[round_index == 1]) / 1000) %>%
  summarize(mean_expt_completion_sec = mean(expt_completion_sec),
            sd_expt_completion_sec = sd(expt_completion_sec))



# ANALYSIS: Power ====

# With 30 participants in lowest bot condition, what effect size do we have 90% power to detect?
# and what win percentage does that correspond to?
# https://cran.r-project.org/web/packages/pwr/vignettes/pwr-vignette.html
power = pwr.t.test(n = 30, sig.level = 0.05, power = 0.9, type = "one.sample", alternative = "two.sided")
# we have 90% power to detect an effect size of d = 0.61
(1 / 3) + power$d * sqrt(1 / 12)
# this is equivalent to an average win rate of about 51% assuming individual win rates are uniformly distributed



# ANALYSIS: Win percentages ====

# Overall win percentage
subject_win_pct = get_subject_block_win_pct(bot_data, blocksize = 300)
condition_win_pct = get_block_win_pct_summary(subject_win_pct)

# Win percentage over time
subject_block_win_pct = get_subject_block_win_pct(bot_data, blocksize = 30)
condition_block_win_pct = get_block_win_pct_summary(subject_block_win_pct)


# How did bot win percentage values compare to chance?
for (bot_strat in unique(subject_win_pct$bot_strategy)) {
  print(bot_strat)
  print(
    t.test(x = subject_win_pct$win_pct[subject_win_pct$bot_strategy == bot_strat],
           mu = (1/3))
  )
}



# FIGURE: Win percentages ====

strategy_lookup = list("prev_move_positive" = "Self-transition (+)",
                       "prev_move_negative" = "Self-transition (−)",
                       "opponent_prev_move_positive" = "Opponent-\ntransition (+)",
                       "opponent_prev_move_nil" = "Opponent-\ntransition (0)",
                       "win_nil_lose_positive" = "Previous outcome (W0L+T−)",
                       "win_positive_lose_negative" = "Previous outcome (W+L−T0)",
                       "outcome_transition_dual_dependency" = "Previous outcome, previous transition")

strategy_labels = c(
  "Self-transition (+)",
  "Self-transition (−)",
  "Opponent-\ntransition (+)",
  "Opponent-\ntransition (0)",
  "Previous outcome (W0L+T−)",
  "Previous outcome (W+L−T0)",
  "Previous outcome, previous transition"
)

condition_win_pct = condition_win_pct %>%
  rowwise() %>%
  mutate(
    bot_strategy_str = factor(strategy_lookup[[bot_strategy]],
                              levels = strategy_labels)
  )

condition_block_win_pct = condition_block_win_pct %>%
  rowwise() %>%
  mutate(
    bot_strategy_str = factor(strategy_lookup[[bot_strategy]],
                              levels = strategy_labels)
  )

# > Overall win percentage ====
win_pct_overall = condition_win_pct %>%
  ggplot(aes(x = bot_strategy_str, y = mean_win_pct, color = bot_strategy_str)) +
  geom_point(size = 6) +
  geom_errorbar(aes(ymin = mean_win_pct - se_win_pct, ymax = mean_win_pct + se_win_pct),
                width = 0, linewidth = 1) +
  geom_hline(yintercept = 1/3, linewidth = 0.75, linetype = "dashed", color = "black") +
  geom_hline(yintercept = 0.9, linewidth = 0.75, linetype = "solid", color = "black") +
  ggtitle("Aggregate") +
  scale_color_viridis(discrete = TRUE,
                      name = element_blank()) +
  scale_y_continuous(
    name = "Human win percentage",
    breaks = seq(0.3, 0.9, by = 0.1),
    labels = as.character(seq(0.3, 0.9, by = 0.1)),
    limits = c(0.3, 0.9)
  ) +
  scale_x_discrete(
    name = element_blank(),
    labels = element_blank()
  ) +
  default_plot_theme +
  theme(
    # remove X axis text and ticks
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "none",
  )

win_pct_overall


# > Win percentage over time ====

block_labels = c("0" = "30", "1" = "60", "2" = "90", "3" = "120", "4" = "150",
                 "5" = "180", "6" = "210", "7" = "240", "8" = "270", "9" = "300")


win_pct_bins = condition_block_win_pct %>%
  ggplot(aes(x = round_block, y = mean_win_pct, color = bot_strategy_str)) +
  geom_point(size = 6) +
  geom_errorbar(aes(ymin = mean_win_pct - se_win_pct, ymax = mean_win_pct + se_win_pct), linewidth = 1, width = 0) +
  geom_hline(yintercept = 1/3, linewidth = 0.75, linetype = "dashed", color = "black") +
  geom_hline(yintercept = 0.9, linewidth = 0.75, linetype = "solid", color = "black") +
  ggtitle("By Round") +
  scale_color_viridis(discrete = T,
                      name = "Bot pattern",
                      labels = label_wrap(20)) +
  scale_x_continuous(
    name = element_blank(),
    labels = block_labels,
    breaks = seq(0, 9)) +
  scale_y_continuous(
    name = element_blank(),
    breaks = seq(0.3, 0.9, by = 0.1),
    labels = c("", "", "", "", "", "", ""),
    limits = c(0.3, 0.9)
  ) +
  default_plot_theme +
  theme(
    # Make X axis text sideways
    axis.text.x = element_text(size = 14, angle = 45, vjust = 0.5, family = "Avenir", color = "black"),
    # Add legend formatting
    legend.position = "right",
    legend.key = element_rect(colour = "transparent", fill = "transparent"),
    legend.spacing.y = unit(0, "lines"),
    legend.key.size = unit(3, "lines"))

win_pct_bins


# > Combined figure ====
win_pct_combined = win_pct_overall + win_pct_bins +
  plot_layout(widths = c(1.5, 2.5))
win_pct_combined

ggsave(
  win_pct_combined,
  filename = "stable_bot_win_pct.pdf",
  device = cairo_pdf,
  path = IMG_PATH,
  width = 10,
  height = 6.5,
  dpi = 300
)




# ANALYSIS: Conditional win percentages ====

CUTOFF = 200
MOVES_ORDERED = c("rock", "paper", "scissors")
OUTCOMES_ORDERED = c("win", "tie", "loss")

# 1. Bot previous move strategies
bot_loss_prev_move = get_bot_prev_move_loss_pct(bot_data, CUTOFF)
bot_loss_summary_prev_move = get_bot_prev_move_win_pct_summary(bot_loss_prev_move)
bot_loss_summary_prev_move$prev_move = factor(
  bot_loss_summary_prev_move$prev_move,
  levels = MOVES_ORDERED
)

# 2. Player previous move strategies
player_win_prev_move = get_player_prev_move_win_pct(bot_data, CUTOFF)
player_win_summary_prev_move = get_bot_prev_move_win_pct_summary(player_win_prev_move)
player_win_summary_prev_move$prev_move = factor(
  player_win_summary_prev_move$prev_move,
  levels = MOVES_ORDERED
)

# 3. Bot previous outcome
bot_loss_prev_outcome = get_bot_prev_outcome_loss_pct(bot_data, CUTOFF)
bot_loss_summary_prev_outcome = get_bot_prev_outcome_win_pct_summary(bot_loss_prev_outcome)
bot_loss_summary_prev_outcome$prev_outcome = factor(
  bot_loss_summary_prev_outcome$prev_outcome,
  levels = OUTCOMES_ORDERED
)


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

# Transition strategies t-tests
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

# Outcome-transition strategies t-tests
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
prev_move_positive_plot = plot_prev_move_win_pct(bot_loss_summary_prev_move, "prev_move_positive", "Self-transition (+)", "Bot previous move")
prev_move_negative_plot = plot_prev_move_win_pct(bot_loss_summary_prev_move, "prev_move_negative", "Self-transition (−)", "Bot previous move")

# 2. Player previous move strategies
opponent_prev_move_positive_plot = plot_prev_move_win_pct(player_win_summary_prev_move, "opponent_prev_move_positive", "Opponent-transition (+)", "Player previous move")
opponent_prev_move_nil_plot = plot_prev_move_win_pct(player_win_summary_prev_move, "opponent_prev_move_nil", "Opponent-transition (0)", "Player previous move")

# 3. Bot previous outcome
win_nil_lose_positive_plot_outcome = plot_outcome_win_pct(bot_loss_summary_prev_outcome, "win_nil_lose_positive", "Previous outcome (W0L+T−)", "Bot previous outcome")
win_positive_lose_negative_plot_outcome = plot_outcome_win_pct(bot_loss_summary_prev_outcome, "win_positive_lose_negative", "Previous outcome (W+L−T0)", "Bot previous outcome")


# Plot using patchwork
stable_bot_win_pct_conditional = prev_move_positive_plot + prev_move_negative_plot +
  opponent_prev_move_positive_plot + opponent_prev_move_nil_plot +
  win_nil_lose_positive_plot_outcome + win_positive_lose_negative_plot_outcome +
  plot_layout(ncol = 2) +
  plot_annotation(tag_levels = 'A') &
  theme(plot.tag = element_text(size = 24, family = "Avenir"))
stable_bot_win_pct_conditional

ggsave(
  stable_bot_win_pct_conditional,
  filename = "stable_bot_win_pct_conditional.pdf",
  device = cairo_pdf,
  path = IMG_PATH,
  width = 11.5,
  height = 13,
  dpi = 300
)


# APPENDIX: self-transitions against outcome-transition opponents ====
# NB: these results outlined in the discussion

TRANSITION_LOOKUP = matrix(
  data = c(c("0", "-", "+"), c("+", "0", "-"), c("-", "+", "0")),
  nrow = 3, ncol = 3, byrow = T,
  dimnames = list(c("rock", "paper", "scissors"), c("rock", "paper", "scissors"))
)

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

# Get data for `win_nil_lose_positive` bot, last 100 rounds, player data only
bot1 = bot_data %>%
  filter(bot_strategy == "win_nil_lose_positive" &
         round_index > 200 & is_bot == 0) %>%
  group_by(player_id) %>%
  mutate(prev_move = lag(player_move, 1))

# Get each player's distribution of self-transitions in this subset
bot1_transitions = get_player_transition_dist(bot1)

# Get each player's win percentages in this subset
bot1_wins = bot1 %>%
  group_by(player_id) %>%
  count(player_outcome) %>%
  mutate(
    rounds = sum(n),
    win_pct = n / rounds
  ) %>%
  filter(player_outcome == "win") %>%
  rename("n_wins" = n)

# Join the transition data and the win percentage data
bot1_transitions = bot1_transitions %>%
  inner_join(bot1_wins, by = c("player_id"))


# What accounts for people's success after ties and in general?
bot1_transitions %>%
  filter(win_pct >= 0.5) %>%
  group_by(transition) %>%
  summarize(
    mean_pct = mean(p_transition),
    se_pct = sd(p_transition) / sqrt(n()),
    n = n(),
    mean_win_pct = mean(win_pct),
    se_win_pct = sd(win_pct) / sqrt(n())
  )
# What's going on in the above:
# Overall tendency to choose 0 transition (consistent with win-tie trap)
# ALSO, small group of participants who trapped bot in continual loss pattern
# by always shifting + (starting from after a win, this traps the bot in losses)


# Do people choose 0 transitions more than chance? And is doing so related to win pct?
t.test(
  bot1_transitions$p_transition[bot1_transitions$transition == "0"],
  mu = 1/3
)
plot(
  bot1_transitions$p_transition[bot1_transitions$transition == "0"],
  bot1_transitions$win_pct[bot1_transitions$transition == "0"]
)

# Is choosing + transitions related to win pct?
plot(
  bot1_transitions$p_transition[bot1_transitions$transition == "+"],
  bot1_transitions$win_pct[bot1_transitions$transition == "+"]
)
cor.test(
  bot1_transitions$p_transition[bot1_transitions$transition == "+"],
  bot1_transitions$win_pct[bot1_transitions$transition == "+"]
)



# Get data for `win_positive_lose_negative` bot, last 100 rounds, player data only
bot2 = bot_data %>%
  filter(bot_strategy == "win_positive_lose_negative" &
           round_index > 200 & is_bot == 0) %>%
  group_by(player_id) %>%
  mutate(prev_move = lag(player_move, 1))

# Get each player's distribution of self-transitions in this subset
bot2_transitions = get_player_transition_dist(bot2)

# Get each player's win percentages in this subset
bot2_wins = bot2 %>%
  group_by(player_id) %>%
  count(player_outcome) %>%
  mutate(
    rounds = sum(n),
    win_pct = n / rounds
  ) %>%
  filter(player_outcome == "win") %>%
  rename("n_wins" = n)

# Join the transition data and the win percentage data
bot2_transitions = bot2_transitions %>%
  inner_join(bot2_wins, by = c("player_id"))


# What accounts for people's success after ties and in general?
bot2_transitions %>%
  filter(win_pct >= 0.6) %>%
  group_by(transition) %>%
  summarize(
    mean_pct = mean(p_transition),
    se_pct = sd(p_transition) / sqrt(n()),
    n = n(),
    mean_win_pct = mean(win_pct),
    se_win_pct = sd(win_pct) / sqrt(n())
  )
# What's going on in the above:
# Overall tendency to choose + transition (consistent with win-tie trap)
# ALSO, small group of participants who trapped bot in continual loss pattern
# by always shifting - (starting from after a win, this traps the bot in losses)


# Do people choose + transitions above chance? And is doing so related to win percent?
t.test(
  bot2_transitions$p_transition[bot2_transitions$transition == "+"],
  mu = 1/3
)
plot(
  bot2_transitions$p_transition[bot2_transitions$transition == "+"],
  bot2_transitions$win_pct[bot2_transitions$transition == "+"]
)


# Is choosing - transitions related to win percent?
plot(
  bot2_transitions$p_transition[bot2_transitions$transition == "-"],
  bot2_transitions$win_pct[bot2_transitions$transition == "-"]
)
cor.test(
  bot2_transitions$p_transition[bot2_transitions$transition == "-"],
  bot2_transitions$win_pct[bot2_transitions$transition == "-"]
)




# APPENDIX: Individual learning curves ====
# NB: these results discussed with reviewers but not included in manuscript

# Add human-readable condition names
# NB: no line breaks in these labels (distinct from the ones above)
strategy_lookup = list("prev_move_positive" = "Self-transition (+)",
                       "prev_move_negative" = "Self-transition (−)",
                       "opponent_prev_move_positive" = "Opponent-transition (+)",
                       "opponent_prev_move_nil" = "Opponent-transition (0)",
                       "win_nil_lose_positive" = "Previous outcome (W0L+T−)",
                       "win_positive_lose_negative" = "Previous outcome (W+L−T0)",
                       "outcome_transition_dual_dependency" = "Previous outcome, previous transition")

strategy_labels = c(
  "Self-transition (+)",
  "Self-transition (−)",
  "Opponent-transition (+)",
  "Opponent-transition (0)",
  "Previous outcome (W0L+T−)",
  "Previous outcome (W+L−T0)",
  "Previous outcome, previous transition"
)

subject_block_win_pct = subject_block_win_pct %>%
  rowwise() %>%
  mutate(
    bot_strategy_str = factor(strategy_lookup[[bot_strategy]],
                              levels = strategy_labels)
  )
condition_block_win_pct = condition_block_win_pct %>%
  rowwise() %>%
  mutate(
    bot_strategy_str = factor(strategy_lookup[[bot_strategy]],
                              levels = strategy_labels)
  )


plot_individual_learning_curves = function(subject_data, summary_data, condition) {
  subject_data %>%
    filter(bot_strategy_str == condition) %>%
    ggplot(aes(x = round_block, y = win_pct, group = game_id)) +
    geom_line(linewidth = 2,
              # color = "light gray",
              alpha = 0.25
    ) +
    geom_point(data = summary_data %>% filter(bot_strategy_str == condition),
               aes(x = round_block, y = mean_win_pct, group = "NA"),
               color = "darkred", size = 6
    ) +
    geom_errorbar(data = summary_data %>% filter(bot_strategy_str == condition),
                  aes(x = round_block, y = mean_win_pct, group = "NA",
                      ymin = mean_win_pct - se_win_pct, ymax = mean_win_pct + se_win_pct),
                  color = "darkred", width = 0, linewidth = 1
    ) +
    geom_hline(yintercept = 1/3, linewidth = 0.75, linetype = "dashed", color = "black") +
    geom_hline(yintercept = 0.9, linewidth = 0.75, linetype = "solid", color = "black") +
    ggtitle(condition) +
    scale_x_continuous(
      name = element_blank(),
      labels = block_labels,
      breaks = seq(0, 9)) +
    scale_y_continuous(name = "Win percentage") +
    default_plot_theme +
    theme(
      # Make X axis text sideways
      axis.text.x = element_text(face = "bold", size = 14, angle = 45, vjust = 0.5, family = "Avenir", color = "black")
    )
}

strats = unique(subject_block_win_pct$bot_strategy_str)
t1 = plot_individual_learning_curves(subject_block_win_pct, condition_block_win_pct, strats[1])
t2 = plot_individual_learning_curves(subject_block_win_pct, condition_block_win_pct, strats[2])
t3 = plot_individual_learning_curves(subject_block_win_pct, condition_block_win_pct, strats[3])
t4 = plot_individual_learning_curves(subject_block_win_pct, condition_block_win_pct, strats[4])

individual_curves = t1 + t2 + t3 + t4 +
  plot_layout(
    ncol = 2,
    heights = unit(c(10, 10), c('cm', 'cm'))
  )
individual_curves

ggsave(
  individual_curves,
  filename = "individual_curves.pdf",
  device = cairo_pdf,
  path = IMG_PATH,
  width = 13,
  height = 10.5,
  dpi = 300
)

