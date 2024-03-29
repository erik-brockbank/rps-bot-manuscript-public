
#
# This script contains the final analysis of supplemental post-experiment survey data for the rps bot journal submission
#



# SETUP ====

setwd("/Users/erikbrockbank/dev/research/vullab/rps-bot-manuscript-public/analysis")
library(patchwork)
library(scales)
library(tidyverse)
library(viridis)

# Globals
DATA_PATH = "../data"
  # Experiment 1
FR_FILE_E1 = "rps_v2_data_freeResp.csv" # csv file containing free response survey data
SLIDER_FILE_E1 = "rps_v2_data_sliderData.csv" # csv file containing slider survey data
TRIAL_DATA_E1 = "rps_v2_data.RData" # RData file containing trial response data
  # Experiment 2
FR_FILE_E2 = "rps_v3_data_freeResp.csv"
SLIDER_FILE_E2 = "rps_v3_data_sliderData.csv"
TRIAL_DATA_E2 = "rps_v3_data.RData"
  # Saving figure output
IMG_PATH = "../figures/supplement"

  # Strategy labels aligned with manuscript figures
E1_STRATEGY_LEVELS = c("prev_move_positive", "prev_move_negative",
                       "opponent_prev_move_positive", "opponent_prev_move_nil",
                       "win_nil_lose_positive", "win_positive_lose_negative",
                       "outcome_transition_dual_dependency")
E1_STRATEGY_LOOKUP = list("prev_move_positive" = "Self-transition (+)",
                          "prev_move_negative" = "Self-transition (−)",
                          "opponent_prev_move_positive" = "Opponent-\ntransition (+)",
                          "opponent_prev_move_nil" = "Opponent-\ntransition (0)",
                          "win_nil_lose_positive" = "Previous outcome (W0L+T−)",
                          "win_positive_lose_negative" = "Previous outcome (W+L−T0)",
                          "outcome_transition_dual_dependency" = "Previous outcome, previous transition")
E1_STRATEGY_LABELS = c(
  "Self-transition (+)",
  "Self-transition (−)",
  "Opponent-\ntransition (+)",
  "Opponent-\ntransition (0)",
  "Previous outcome (W0L+T−)",
  "Previous outcome (W+L−T0)",
  "Previous outcome, previous transition"
)

E2_STRATEGY_LOOKUP = list(
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
E2_STRATEGY_LABELS = list(
  "Previous move",
  "Opponent previous move",
  "Previous move, opponent previous move",
  "Previous two moves",
  "Self-transition",
  "Opponent-transition",
  "Previous outcome",
  "Previous outcome, previous transition"
)


# ANALYSIS FUNCTIONS ====

# Function for generating plots of survey responses
plot_survey_summary_e1 = function(response_summary, title, ylabel, default_theme, supp_theme) {
  response_summary %>%
    ggplot(aes(x = bot_strategy_str, y = mean_resp, color = bot_strategy_str)) +
    geom_point(size = 6) +
    geom_errorbar(aes(ymin = mean_resp - se_resp, ymax = mean_resp + se_resp),
                  width = 0, linewidth = 1) +
    ggtitle(title) +
    scale_color_viridis(discrete = T,
                        name = "Bot pattern",
                        labels = label_wrap(20)) +
    scale_y_continuous(
      name = ylabel,
      breaks = seq(1, 7, by = 1),
      labels = as.character(seq(1, 7, by = 1)),
      limits = c(1, 7)
    ) +
    scale_x_discrete(
      name = element_blank(),
      labels = element_blank()
    ) +
    default_theme +
    theme(
      # remove X axis text and ticks
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      plot.title = element_text(face = "italic", size = 18, family = "Avenir", color = "black", margin = margin(b = 0.5, unit = "line")),
      axis.title.y = element_text(face = "plain", size = 18, family = "Avenir", color = "black", margin = margin(r = 0.5, unit = "line")),
      axis.text.y = element_text(face = "plain", size = 14, family = "Avenir", color = "black"),
      legend.title = element_text(face = "plain", size = 18, family = "Avenir", color = "black"),
      legend.text = element_text(face = "plain", size = 14, family = "Avenir", color = "black"),
    ) +
    supp_theme
}

default_plot_theme = theme(
  # text
  plot.title = element_text(face = "plain", size = 24, family = "Avenir", color = "black", margin = margin(b = 0.5, unit = "line")),
  axis.title.y = element_text(face = "plain", size = 24, family = "Avenir", color = "black", margin = margin(r = 0.5, unit = "line")),
  axis.title.x = element_text(face = "plain", size = 24, family = "Avenir", color = "black", margin = margin(t = 0.5, unit = "line")),
  axis.text.y = element_text(face = "plain", size = 18, family = "Avenir", color = "black"),
  axis.text.x = element_text(face = "plain", size = 18, family = "Avenir", color = "black"),
  legend.title = element_text(face = "plain", size = 20, family = "Avenir", color = "black"),
  legend.text = element_text(face = "plain", size = 14, family = "Avenir", color = "black"),
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




# Experiment 1 ====
load(paste(DATA_PATH, TRIAL_DATA_E1, sep = "/"))

e1_metadata = bot_data %>%
  filter(is_bot == 0) %>%
  select(player_id, bot_strategy) %>%
  unique()


# > Free response data ====
# "In the text box below, please describe any strategies you used to try and beat your opponent."
fr_resp_e1 = read_csv(paste(DATA_PATH, FR_FILE_E1, sep = "/"))

# Keep only participants whose data we analyzed in results
fr_resp_e1 = fr_resp_e1 %>%
  inner_join(e1_metadata, by = c("player_id"))

# Print responses
fr_resp_e1 %>%
  select(free_resp_answer) %>%
  print(n = length(unique(fr_resp_e1$game_id)))


# > Slider response data ====
# `index`: order of the question (0, 1, 2, 3, 4)
# `resp`: 1-7 slider response (1: "Strongly disagree", 7: "Strongly agree")

# Read survey responses
slider_resp_e1 = read_csv(paste(DATA_PATH, SLIDER_FILE_E1, sep = "/"))
# Keep only participants whose data we analyzed in results
slider_resp_e1 = slider_resp_e1 %>%
  inner_join(e1_metadata, by = c("player_id"))
# Add human readable condition names
slider_resp_e1 = slider_resp_e1 %>%
  rowwise() %>%
  mutate(
    bot_strategy_str = factor(E1_STRATEGY_LOOKUP[[bot_strategy]],
                              levels = E1_STRATEGY_LABELS)
  )

slider_resp_e1_summary = slider_resp_e1 %>%
  group_by(index, bot_strategy_str) %>%
  summarize(
    mean_resp = mean(resp),
    se_resp = sd(resp) / sqrt(n())
  ) %>% ungroup()

# Themes to customize figures
legend_theme = theme(
  legend.position = "right",
  legend.key = element_rect(colour = "transparent", fill = "transparent"),
  legend.spacing.y = unit(0, "lines"),
  legend.key.size = unit(2.5, "lines")
)
no_legend = theme(
  legend.position = "none"
)


# >> Q1: "My opponent was a real person and not a robot." ====
q_index = 0
str_limit = 50
p1 = plot_survey_summary_e1(
  slider_resp_e1_summary %>% filter(index == q_index),
  paste("\n", str_wrap(unique((slider_resp_e1 %>% filter(index == q_index))$statement)[1], str_limit)),
  "Response",
  default_plot_theme,
  # legend_theme
  no_legend
)
p1
ggsave(
  p1,
  filename = "e1_q1.pdf",
  path = IMG_PATH,
  device = cairo_pdf,
  width = 7.5, height = 6,
  dpi = 300
)
# Qualitative:
# - Overall, low confidence that opponent was a human across all conditions
# - Confidence that opponent was a human increased as opponents became more complex
summary(aov(
  data = slider_resp_e1 %>% filter(index == q_index),
  resp ~ bot_strategy_str
))
# Quantitative:
# - Responses do not differ by condition


# >> Q2: "I was trying to win each round against my opponent." ====
q_index = 1
str_limit = 60
p2 = plot_survey_summary_e1(
  slider_resp_e1_summary %>% filter(index == q_index),
  paste("\n", str_wrap(unique((slider_resp_e1 %>% filter(index == q_index))$statement)[1], str_limit)),
  # element_blank(),
  "",
  default_plot_theme,
  no_legend
)
p2
ggsave(
  p2,
  filename = "e1_q2.pdf",
  path = IMG_PATH,
  device = cairo_pdf,
  width = 7.5, height = 6,
  dpi = 300
)
# Qualitative:
# - Overall, participants were above mid-line on attempts to win
# - More engagement against easier opponents
summary(aov(
  data = slider_resp_e1 %>% filter(index == q_index),
  resp ~ bot_strategy_str
))
# Quantitative:
# - Responses do not differ by condition


# >> Q3: "I was focused on winning for the entire time I was playing." ====
q_index = 2
str_limit = 60
p3 = plot_survey_summary_e1(
  slider_resp_e1_summary %>% filter(index == q_index),
  paste("\n", str_wrap(unique((slider_resp_e1 %>% filter(index == q_index))$statement)[1], str_limit)),
  "",
  default_plot_theme,
  no_legend
)
p3
ggsave(
  p3,
  filename = "e1_q3.pdf",
  path = IMG_PATH,
  device = cairo_pdf,
  width = 7.5, height = 6,
  dpi = 300
)
# Qualitative:
# - Similar to focus above, participants were above mid-line *for entirety of game*
# - Also similar to above, focus was higher against easier opponents
summary(aov(
  data = slider_resp_e1 %>% filter(index == q_index),
  resp ~ bot_strategy_str
))
# Quantitative:
# - Responses do not differ by condition


# >> Q4: "I paid attention to my opponent’s moves in order to try and predict their next move." ====
q_index = 3
str_limit = 55
p4 = plot_survey_summary_e1(
  slider_resp_e1_summary %>% filter(index == q_index),
  str_wrap(unique((slider_resp_e1 %>% filter(index == q_index))$statement)[1], str_limit),
  "",
  default_plot_theme,
  no_legend
)
p4
ggsave(
  p4,
  filename = "e1_q4.pdf",
  path = IMG_PATH,
  device = cairo_pdf,
  width = 7.5, height = 6,
  dpi = 300
)
# Qualitative:
# - Relatively high reports of paying attention to predict opponent
# - Unlike previous, no obvious relation between opponent difficulty and predictions
summary(aov(
  data = slider_resp_e1 %>% filter(index == q_index),
  resp ~ bot_strategy_str
))
# Quantitative:
# - Responses do not differ by condition


# >> Q5: "There were noticeable patterns in my opponent’s moves that allowed me to predict their next move." ====
q_index = 4
str_limit = 55
p5 = plot_survey_summary_e1(
  slider_resp_e1_summary %>% filter(index == q_index),
  str_wrap(unique((slider_resp_e1 %>% filter(index == q_index))$statement)[1], str_limit),
  "",
  default_plot_theme,
  no_legend
)
p5
ggsave(
  p5,
  filename = "e1_q5.pdf",
  path = IMG_PATH,
  device = cairo_pdf,
  width = 7.5, height = 6,
  dpi = 300
)
# Qualitative:
# - Relatively high reports of noticeable patterns
# (this is surprising in conditions where people didn't win!)
# - No obvious relation between opponent difficulty and pattern ascription
summary(aov(
  data = slider_resp_e1 %>% filter(index == q_index),
  resp ~ bot_strategy_str
))
# Quantitative:
# - Responses do differ by condition
TukeyHSD(
  aov(
    data = slider_resp_e1 %>% filter(index == q_index),
    resp ~ bot_strategy_str
  )
)
# Significant ANOVA is driven primarily by pairwise difference between most complex strategy
# and three of the four transition bots (self-transition +, self-transition -, opponent-transition 0)



# Experiment 2 ====

load(paste(DATA_PATH, TRIAL_DATA_E2, sep = "/"))

e2_metadata = bot_data %>%
  filter(is_bot == 0) %>%
  select(player_id, bot_strategy) %>%
  unique()


# > Free response data ====

# "In the text box below, please describe any strategies you used to try and beat your opponent."
fr_resp_e2 = read_csv(paste(DATA_PATH, FR_FILE_E2, sep = "/"))
# Keep only participants whose data we analyzed in results
fr_resp_e2 = fr_resp_e2 %>%
  inner_join(e2_metadata,
             by = c("player_id"))

# Print responses
fr_resp_e2 %>%
  select(free_resp_answer) %>%
  print(n = length(unique(fr_resp_e2$game_id)))


# > Slider response data ====
# `index`: order of the question (0, 1, 2, 3, 4)
# `resp`: 1-7 slider response (1: "Strongly disagree", 7: "Strongly agree")

# Read survey responses
slider_resp_e2 = read_csv(paste(DATA_PATH, SLIDER_FILE_E2, sep = "/"))
# Keep only participants whose data we analyzed in results
slider_resp_e2 = slider_resp_e2 %>%
  inner_join(e2_metadata,
             by = c("player_id"))
# Add human readable condition names
# TODO this replicates code in analysis file, should just do this once and store with RData
slider_resp_e2 = slider_resp_e2 %>%
  rowwise() %>%
  mutate(
    bot_strategy_str = factor(E2_STRATEGY_LOOKUP[[bot_strategy]],
                              levels = E2_STRATEGY_LABELS)
  )

slider_resp_e2_summary = slider_resp_e2 %>%
  group_by(index, bot_strategy_str) %>%
  summarize(
    mean_resp = mean(resp),
    se_resp = sd(resp) / sqrt(n())
  ) %>% ungroup()

# Themes to customize figures
legend_theme = theme(
  legend.position = "right",
  legend.key = element_rect(colour = "transparent", fill = "transparent"),
  legend.spacing.y = unit(0, "lines"),
  legend.key.size = unit(2.5, "lines")
)
no_legend = theme(
  legend.position = "none"
)


# >> Q1: "My opponent was a real person and not a robot." ====
q_index = 0
str_limit = 50
p1 = plot_survey_summary_e1(
  slider_resp_e2_summary %>% filter(index == q_index),
  paste("\n", str_wrap(unique((slider_resp_e2 %>% filter(index == q_index))$statement)[1], str_limit)),
  "Response",
  default_plot_theme,
  # legend_theme
  no_legend
)
p1
ggsave(
  p1,
  filename = "e2_q1.pdf",
  path = IMG_PATH,
  device = cairo_pdf,
  width = 7.5, height = 6,
  dpi = 300
)
# Qualitative:
# -
summary(aov(
  data = slider_resp_e2 %>% filter(index == q_index),
  resp ~ bot_strategy_str
))
# Quantitative:
# -

# >> Q2: "I was trying to win each round against my opponent." ====
q_index = 1
str_limit = 60
p2 = plot_survey_summary_e1(
  slider_resp_e2_summary %>% filter(index == q_index),
  paste("\n", str_wrap(unique((slider_resp_e2 %>% filter(index == q_index))$statement)[1], str_limit)),
  # element_blank(),
  "",
  default_plot_theme,
  no_legend
)
p2
ggsave(
  p2,
  filename = "e2_q2.pdf",
  path = IMG_PATH,
  device = cairo_pdf,
  width = 7.5, height = 6,
  dpi = 300
)
# Qualitative:
# -
summary(aov(
  data = slider_resp_e2 %>% filter(index == q_index),
  resp ~ bot_strategy_str
))
# Quantitative:
# -

# >> Q3: "I was focused on winning for the entire time I was playing." ====
q_index = 2
str_limit = 60
p3 = plot_survey_summary_e1(
  slider_resp_e2_summary %>% filter(index == q_index),
  paste("\n", str_wrap(unique((slider_resp_e2 %>% filter(index == q_index))$statement)[1], str_limit)),
  # element_blank(),
  "",
  default_plot_theme,
  no_legend
)
p3
ggsave(
  p3,
  filename = "e2_q3.pdf",
  path = IMG_PATH,
  device = cairo_pdf,
  width = 7.5, height = 6,
  dpi = 300
)
# Qualitative:
# -
summary(aov(
  data = slider_resp_e2 %>% filter(index == q_index),
  resp ~ bot_strategy_str
))
# Quantitative:
# -


# >> Q4: "I paid attention to my opponent’s moves in order to try and predict their next move." ====
q_index = 3
str_limit = 60
p4 = plot_survey_summary_e1(
  slider_resp_e2_summary %>% filter(index == q_index),
  paste("\n", str_wrap(unique((slider_resp_e2 %>% filter(index == q_index))$statement)[1], str_limit)),
  # element_blank(),
  "",
  default_plot_theme,
  no_legend
)
p4
ggsave(
  p4,
  filename = "e2_q4.pdf",
  path = IMG_PATH,
  device = cairo_pdf,
  width = 7.5, height = 6,
  dpi = 300
)
# Qualitative:
# -
summary(aov(
  data = slider_resp_e2 %>% filter(index == q_index),
  resp ~ bot_strategy_str
))
# Quantitative:
# -


# >> Q5: "There were noticeable patterns in my opponent’s moves that allowed me to predict their next move." ====
q_index = 4
str_limit = 60
p5 = plot_survey_summary_e1(
  slider_resp_e2_summary %>% filter(index == q_index),
  paste("\n", str_wrap(unique((slider_resp_e2 %>% filter(index == q_index))$statement)[1], str_limit)),
  # element_blank(),
  "",
  default_plot_theme,
  no_legend
)
p5
ggsave(
  p5,
  filename = "e2_q5.pdf",
  path = IMG_PATH,
  device = cairo_pdf,
  width = 7.5, height = 6,
  dpi = 300
)
# Qualitative:
# -
summary(aov(
  data = slider_resp_e2 %>% filter(index == q_index),
  resp ~ bot_strategy_str
))
# Quantitative:
# -







