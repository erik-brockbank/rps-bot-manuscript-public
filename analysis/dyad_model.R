#'
#' Modeling RPS dyad responses
#'



# SETUP ====

rm(list = ls())
setwd("/Users/erikbrockbank/web/vullab/rps/analysis")


library(patchwork)
library(stats4)
library(tidyverse)
library(viridis)


# GLOBALS ====

DYAD_DATA_FILE = "rps_v1_data.csv" # name of file containing full dyad dataset
GAME_ROUNDS = 300 # number of rounds per game
EVENT_COUNT_PRIOR = 1 # baseline count of each move for model fitting
MOVE_PROBABILITY_PRIOR = 1/3 # baseline probability of each move for model fitting

# Matrix of values for each outcome:
# row: player's move choice (R, P, S); col: opponent move choice (R, P, S)
# vals: points for player given row, col move choices
OUTCOME_VALS = matrix(c(c(0, -1, 3), c(3, 0, -1), c(-1, 3, 0)),
                      nrow = 3, ncol = 3, byrow = T,
                      dimnames = list(c("p1_rock", "p1_paper", "p1_scissors"),
                                      c("opp_rock", "opp_paper", "opp_scissors")))
# Matrix of transitions for each pair of moves:
# row: previous move choice (R, P, S); col: player's move choice (R, P, S)
# vals: transition string ("up", "down", "stay")
TRANSITION_VALS = matrix(c(c("stay", "up", "down"),
                           c("down", "stay", "up"),
                           c("up", "down", "stay")),
                         nrow = 3, ncol = 3, byrow = T,
                         dimnames = list(c("rock", "paper", "scissors"),
                                         c("rock", "paper", "scissors")))


# FUNCTIONS ====

# Function to read in and structure data appropriately
read_dyad_data = function(filename, game_rounds) {
  data = read_csv(filename)

  # Remove incomplete games
  incomplete_data = data %>%
    group_by(game_id, player_id) %>%
    summarize(rounds = max(round_index)) %>%
    filter(rounds < game_rounds) %>%
    select(player_id, game_id)
  data = data %>%
    filter(!(player_id %in% incomplete_data$player_id))
  # Remove NA rows
  data = data %>% filter(!is.na(player_move))

  return(data)
}


# INITIALIZATION FUNCTIONS

# Add column for player's previous move (note this will be NA in round 1)
add_player_prev_move = function(data) {
  data %>%
    group_by(player_id) %>%
    mutate(player_prev_move = lag(player_move, 1))
}

# Add column for *opponent's* previous move
add_opponent_prev_move = function(data) {
  data %>%
    group_by(game_id, round_index) %>%
    mutate(
      # In rows for each round, opponent's prev move is "player_prev_move"
      # val of opposite row (either lead or lag)
      opponent_prev_move = ifelse(
        is.na(lag(player_prev_move, 1)),
        lead(player_prev_move, 1),
        lag(player_prev_move, 1)
      )
    )
}

# Add column for player's transition
# NB: in rows where move or previous move are "none" or NA, transition is "none"
add_transition = function(data, transition_matrix) {
  data %>%
    group_by(player_id) %>%
    rowwise() %>%
    mutate(
      transition = ifelse(
        is.na(player_move) || is.na(player_prev_move) || player_move == "none" || player_prev_move == "none",
        "none",
        transition_matrix[player_prev_move, player_move]
      )
    )
}

# Add column for player's *opponent* transition
# NB: in rows where move or previous move are "none" or NA, opponent transition is "none"
# TODO this function is almost identical to the above; possible consolidation?
add_opponent_transition = function(data, transition_matrix) {
  data %>%
    group_by(player_id) %>%
    rowwise() %>%
    mutate(
      opponent_transition = ifelse(
        is.na(player_move) || is.na(opponent_prev_move) || player_move == "none" || opponent_prev_move == "none",
        "none",
        transition_matrix[opponent_prev_move, player_move]
      ))
}


# Add column for combination of player's previous move and current move as a string
add_player_prev_move_current_move = function(data) {
  data %>%
    group_by(player_id) %>%
    rowwise() %>%
    mutate(
      player_prev_move_current_move = ifelse(
        is.na(player_move) || is.na(player_prev_move) || player_move == "none" || player_prev_move == "none",
        "none",
        paste(player_prev_move, player_move, sep = "-")
      ))

}


# MOVE COUNT FUNCTIONS

# Count cumulative number of rock, paper, and scissors moves by each player
count_moves = function(data) {
  data %>%
    group_by(player_id) %>%
    mutate(count_rock = cumsum(player_move == "rock"),
           count_paper = cumsum(player_move == "paper"),
           count_scissors = cumsum(player_move == "scissors"))
}

# Apply "prior" to move rates by increasing all counts by `count_prior`
# Counts start at `count_prior` rather than 0, increase from that amount
apply_move_count_prior = function(data, count_prior) {
  data %>%
    group_by(player_id) %>%
    rowwise() %>%
    mutate(count_rock = count_rock + count_prior,
           count_paper = count_paper + count_prior,
           count_scissors = count_scissors + count_prior)
}


# Calculate move probability on a given round based on *move* counts from previous round
# NB: the `probability_prior` is only needed to attach a probability to very first round when `lag` is NA
calculate_move_probs_move_count = function(data, probability_prior) {
  # Calculate totals for probability conversion
  data = data %>%
    rowwise() %>%
    mutate(count_total_moves = sum(count_rock, count_paper, count_scissors))
  # Calculate move probability
  data = data %>%
    group_by(game_id, player_id) %>%
    mutate(
      prob_rock = ifelse(is.na(lag(count_rock, 1)),
                         probability_prior,
                         (lag(count_rock, 1) / lag(count_total_moves, 1))),
      prob_paper = ifelse(is.na(lag(count_paper, 1)),
                          probability_prior,
                          (lag(count_paper, 1) / lag(count_total_moves, 1))),
      prob_scissors = ifelse(is.na(lag(count_scissors, 1)),
                             probability_prior,
                             (lag(count_scissors, 1) / lag(count_total_moves, 1)))
    )
  return(data)
}


# TRANSITION FUNCTIONS

# Count cumulative number of transitions by each player
count_transitions = function(data) {
  data %>%
    group_by(player_id) %>%
    mutate(count_transition_up = cumsum(transition == "up"),
           count_transition_down = cumsum(transition == "down"),
           count_transition_stay = cumsum(transition == "stay"))
}

# Apply "prior" to transition counts by increasing all counts by `count_prior`
# Counts start at `count_prior` rather than 0, increase from that amount
apply_transition_count_prior = function(data, count_prior) {
  data %>%
    group_by(player_id) %>%
    rowwise() %>%
    mutate(count_transition_up = count_transition_up + count_prior,
           count_transition_down = count_transition_down + count_prior,
           count_transition_stay = count_transition_stay + count_prior)
}


# Calculate move probability on a given round based on *transition* counts from previous round
# NB: the `probability_prior` is only needed to attach a probability to very first round when `lag` is NA
calculate_move_probs_transition = function(data, probability_prior, transition_lookup) {
  # Calculate totals for probability conversion
  data = data %>%
    rowwise() %>%
    mutate(count_total_transitions = sum(count_transition_up, count_transition_down, count_transition_stay))
  # Add probability of each transition based on counts from previous round
  data = data %>%
    # TODO can we get rid of `game_id` in groupby here? And elsewhere?
    group_by(game_id, player_id) %>%
    mutate(
      prob_transition_up = ifelse(is.na(lag(count_total_transitions, 1)), probability_prior,
                                  lag(count_transition_up, 1) / lag(count_total_transitions, 1)),
      prob_transition_down = ifelse(is.na(lag(count_total_transitions, 1)), probability_prior,
                                  lag(count_transition_down, 1) / lag(count_total_transitions, 1)),
      prob_transition_stay = ifelse(is.na(lag(count_total_transitions, 1)), probability_prior,
                                  lag(count_transition_stay, 1) / lag(count_total_transitions, 1))
    )
  # Calculate move probability
  data = data %>%
    group_by(game_id, player_id) %>%
    rowwise() %>%
    mutate(
      # Rock probability is either prior or transition probability *as of previous round* for
      # transition indicated by previous round's move -> "rock"
      prob_rock = ifelse(is.na(player_prev_move) | player_prev_move == "none",
                         # If previous move was NA or "none", probability of rock is just prior. This is a bit clumsy but conservative.
                         probability_prior,
                         # Else, probability of rock is appropriate transition probability from above for previous move -> "rock"
                         case_when(
                           transition_lookup[player_prev_move, "rock"] == "up" ~ prob_transition_up,
                           transition_lookup[player_prev_move, "rock"] == "down" ~ prob_transition_down,
                           transition_lookup[player_prev_move, "rock"] == "stay" ~ prob_transition_stay,
                           TRUE ~ probability_prior # NB: this should never be activated
                           )
                         ),
      prob_paper = ifelse(is.na(player_prev_move) | player_prev_move == "none",
                          probability_prior,
                          case_when(
                            transition_lookup[player_prev_move, "paper"] == "up" ~ prob_transition_up,
                            transition_lookup[player_prev_move, "paper"] == "down" ~ prob_transition_down,
                            transition_lookup[player_prev_move, "paper"] == "stay" ~ prob_transition_stay,
                            TRUE ~ probability_prior # NB: this should never be activated
                            )
                          ),
      prob_scissors = ifelse(is.na(player_prev_move) | player_prev_move == "none",
                             probability_prior,
                             case_when(
                               transition_lookup[player_prev_move, "scissors"] == "up" ~ prob_transition_up,
                               transition_lookup[player_prev_move, "scissors"] == "down" ~ prob_transition_down,
                               transition_lookup[player_prev_move, "scissors"] == "stay" ~ prob_transition_stay,
                               TRUE ~ probability_prior # NB: this should never be activated
                               )
                             )

    )
  return(data)
}



# OPPONENT TRANSITION FUNCTIONS

# Count cumulative number of transitions by each player
# TODO this function is almost identical to the one above for transitions; possible consolidation?
count_opponent_transitions = function(data) {
  data %>%
    group_by(player_id) %>%
    mutate(count_opponent_transition_up = cumsum(opponent_transition == "up"),
           count_opponent_transition_down = cumsum(opponent_transition == "down"),
           count_opponent_transition_stay = cumsum(opponent_transition == "stay"))
}

# Apply "prior" to opponent transition counts by increasing all counts by `count_prior`
# Counts start at `count_prior` rather than 0, increase from that amount
# TODO this function is almost identical to the one above for transitions; possible consolidation?
apply_opponent_transition_count_prior = function(data, count_prior) {
  data %>%
    group_by(player_id) %>%
    rowwise() %>%
    mutate(count_opponent_transition_up = count_opponent_transition_up + count_prior,
           count_opponent_transition_down = count_opponent_transition_down + count_prior,
           count_opponent_transition_stay = count_opponent_transition_stay + count_prior)
}

# Calculate move probability on a given round based on *opponent transition* counts from previous round
# NB: the `probability_prior` is only needed to attach a probability to very first round when `lag` is NA
calculate_move_probs_opponent_transition = function(data, probability_prior, transition_lookup) {
  # Calculate totals for probability conversion
  data = data %>%
    rowwise() %>%
    mutate(count_total_opponent_transitions = sum(count_opponent_transition_up, count_opponent_transition_down, count_opponent_transition_stay))
  # Add probability of each opponent transition based on counts from previous round
  data = data %>%
    group_by(game_id, player_id) %>%
    mutate(
      prob_opponent_transition_up = ifelse(
        is.na(lag(count_total_opponent_transitions, 1)),
        probability_prior,
        lag(count_opponent_transition_up, 1) / lag(count_total_opponent_transitions, 1)),
      prob_opponent_transition_down = ifelse(
        is.na(lag(count_total_opponent_transitions, 1)),
        probability_prior,
        lag(count_opponent_transition_down, 1) / lag(count_total_opponent_transitions, 1)),
      prob_opponent_transition_stay = ifelse(
        is.na(lag(count_total_opponent_transitions, 1)),
        probability_prior,
        lag(count_opponent_transition_stay, 1) / lag(count_total_opponent_transitions, 1))
    )
  # Calculate move probability
  data = data %>%
    group_by(game_id, player_id) %>%
    rowwise() %>%
    mutate(
      # Rock probability is either prior or opponent transition probability
      # *as of previous round* for opponent transition indicated by opponent's previous round move -> "rock"
      prob_rock = ifelse(is.na(opponent_prev_move) | opponent_prev_move == "none",
                         # If opponent previous move was NA or "none", probability of rock is just prior. This is a bit clumsy but conservative.
                         probability_prior,
                         # Else, probability of rock is appropriate *opponent* transition probability from above for opponent previous move -> "rock"
                         case_when(
                           transition_lookup[opponent_prev_move, "rock"] == "up" ~ prob_opponent_transition_up,
                           transition_lookup[opponent_prev_move, "rock"] == "down" ~ prob_opponent_transition_down,
                           transition_lookup[opponent_prev_move, "rock"] == "stay" ~ prob_opponent_transition_stay,
                           TRUE ~ probability_prior # NB: this should never be activated
                         )
      ),
      prob_paper = ifelse(is.na(opponent_prev_move) | opponent_prev_move == "none",
                          probability_prior,
                          case_when(
                            transition_lookup[opponent_prev_move, "paper"] == "up" ~ prob_opponent_transition_up,
                            transition_lookup[opponent_prev_move, "paper"] == "down" ~ prob_opponent_transition_down,
                            transition_lookup[opponent_prev_move, "paper"] == "stay" ~ prob_opponent_transition_stay,
                            TRUE ~ probability_prior # NB: this should never be activated
                          )
      ),
      prob_scissors = ifelse(is.na(opponent_prev_move) | opponent_prev_move == "none",
                             probability_prior,
                             case_when(
                               transition_lookup[opponent_prev_move, "scissors"] == "up" ~ prob_opponent_transition_up,
                               transition_lookup[opponent_prev_move, "scissors"] == "down" ~ prob_opponent_transition_down,
                               transition_lookup[opponent_prev_move, "scissors"] == "stay" ~ prob_opponent_transition_stay,
                               TRUE ~ probability_prior # NB: this should never be activated
                             )
      )

    )
  return(data)
}


# MOVE GIVEN PREVIOUS MOVE FUNCTIONS

# Count cumulative number of each previous move by each player
count_prev_move = function(data) {
  data %>%
    group_by(player_id) %>%
    mutate(
      count_prev_rock = cumsum(replace_na(player_prev_move, 0) == "rock"),
      count_prev_paper = cumsum(replace_na(player_prev_move, 0) == "paper"),
      count_prev_scissors = cumsum(replace_na(player_prev_move, 0) == "scissors"),
    )
}

# Count cumulative number of combinations of previous move, current move
# TODO could probably consolidate this with nested iteration over "rock", "paper", "scissors"
count_prev_move_current_move = function(data) {
  data %>%
    group_by(player_id) %>%
    mutate(count_rock_rock = cumsum(player_prev_move_current_move == "rock-rock"),
           count_rock_paper = cumsum(player_prev_move_current_move == "rock-paper"),
           count_rock_scissors = cumsum(player_prev_move_current_move == "rock-scissors"),
           count_paper_rock = cumsum(player_prev_move_current_move == "paper-rock"),
           count_paper_paper = cumsum(player_prev_move_current_move == "paper-paper"),
           count_paper_scissors = cumsum(player_prev_move_current_move == "paper-scissors"),
           count_scissors_rock = cumsum(player_prev_move_current_move == "scissors-rock"),
           count_scissors_paper = cumsum(player_prev_move_current_move == "scissors-paper"),
           count_scissors_scissors = cumsum(player_prev_move_current_move == "scissors-scissors")
           )
}

# Apply "prior" to previous move counts by increasing all counts by `count_prior`
# Counts start at `count_prior` rather than 0, increase from that amount
# NOTE we apply a prior count of 3 here rather than 1
apply_prev_move_count_prior = function(data, count_prior) {
  data %>%
    group_by(player_id) %>%
    rowwise() %>%
    mutate(
      count_prev_rock = count_prev_rock + count_prior,
      count_prev_paper = count_prev_paper + count_prior,
      count_prev_scissors = count_prev_scissors + count_prior
    )
}


# Apply "prior" to previous move, current move counts by increasing all counts by `count_prior`
# Counts start at `count_prior` rather than 0, increase from that amount
apply_prev_move_current_move_count_prior = function(data, count_prior) {
  data %>%
    group_by(player_id) %>%
    rowwise() %>%
    mutate(
      count_rock_rock = count_rock_rock + count_prior,
      count_rock_paper = count_rock_paper + count_prior,
      count_rock_scissors = count_rock_scissors + count_prior,
      count_paper_rock = count_paper_rock + count_prior,
      count_paper_paper = count_paper_paper + count_prior,
      count_paper_scissors = count_paper_scissors + count_prior,
      count_scissors_rock = count_scissors_rock + count_prior,
      count_scissors_paper = count_scissors_paper + count_prior,
      count_scissors_scissors = count_scissors_scissors + count_prior
    )
}

# Calculate move probability on a given round based on previous move, current move counts from previous round
# NB: the `probability_prior` is only needed to attach a probability to very first round when `lag` is NA
calculate_move_probs_move_prev_move = function(data, probability_prior) {
  # First, calculate probability of each previous move, current move transition
  # as of the previous round
  data = data %>%
    group_by(player_id) %>%
    mutate(
      prob_rock_rock = ifelse(is.na(lag(count_rock_rock, 1)),
        probability_prior,
        lag(count_rock_rock, 1) / lag(count_prev_rock, 1)
      ),
      prob_rock_paper = ifelse(is.na(lag(count_rock_paper, 1)),
        probability_prior,
        lag(count_rock_paper, 1) / lag(count_prev_rock, 1)
      ),
      prob_rock_scissors = ifelse(is.na(lag(count_rock_scissors, 1)),
        probability_prior,
        lag(count_rock_scissors, 1) / lag(count_prev_rock, 1)
      ),
      prob_paper_rock = ifelse(is.na(lag(count_paper_rock, 1)),
        probability_prior,
        lag(count_paper_rock, 1) / lag(count_prev_paper, 1)
      ),
      prob_paper_paper = ifelse(is.na(lag(count_paper_paper, 1)),
        probability_prior,
        lag(count_paper_paper, 1) / lag(count_prev_paper, 1)
      ),
      prob_paper_scissors = ifelse(is.na(lag(count_paper_scissors, 1)),
        probability_prior,
        lag(count_paper_scissors, 1) / lag(count_prev_paper, 1)
      ),
      prob_scissors_rock = ifelse(is.na(lag(count_scissors_rock, 1)),
        probability_prior,
        lag(count_scissors_rock, 1) / lag(count_prev_scissors, 1)
      ),
      prob_scissors_paper = ifelse(is.na(lag(count_scissors_paper, 1)),
        probability_prior,
        lag(count_scissors_paper, 1) / lag(count_prev_scissors, 1)
      ),
      prob_scissors_scissors = ifelse(is.na(lag(count_scissors_scissors, 1)),
        probability_prior,
        lag(count_scissors_scissors, 1) / lag(count_prev_scissors, 1)
      )
    )

  # Now, select move probabilities calculated above based on player previous move
  data = data %>%
    group_by(player_id) %>%
    rowwise() %>%
    mutate(
      prob_rock = ifelse(is.na(player_prev_move) | player_prev_move == "none",
                         # If opponent previous move was NA or "none", probability of rock is just prior. This is a bit clumsy but conservative.
                         probability_prior,
                         # Else, probability of rock is appropriate *opponent* transition probability from above for opponent previous move -> "rock"
                         case_when(
                           player_prev_move == "rock" ~ prob_rock_rock,
                           player_prev_move == "paper" ~ prob_paper_rock,
                           player_prev_move == "scissors" ~ prob_scissors_rock,
                           TRUE ~ probability_prior # NB: this should never be activated
                         )
      ),
      prob_paper = ifelse(is.na(player_prev_move) | player_prev_move == "none",
                          # If opponent previous move was NA or "none", probability of rock is just prior. This is a bit clumsy but conservative.
                          probability_prior,
                          # Else, probability of rock is appropriate *opponent* transition probability from above for opponent previous move -> "rock"
                          case_when(
                            player_prev_move == "rock" ~ prob_rock_paper,
                            player_prev_move == "paper" ~ prob_paper_paper,
                            player_prev_move == "scissors" ~ prob_scissors_paper,
                            TRUE ~ probability_prior # NB: this should never be activated
                          )
      ),
      prob_scissors = ifelse(is.na(player_prev_move) | player_prev_move == "none",
                             # If opponent previous move was NA or "none", probability of rock is just prior. This is a bit clumsy but conservative.
                             probability_prior,
                             # Else, probability of rock is appropriate *opponent* transition probability from above for opponent previous move -> "rock"
                             case_when(
                               player_prev_move == "rock" ~ prob_rock_scissors,
                               player_prev_move == "paper" ~ prob_paper_scissors,
                               player_prev_move == "scissors" ~ prob_scissors_scissors,
                               TRUE ~ probability_prior # NB: this should never be activated
                              )
      )
    )

  return(data)
}


# MODEL EV CALCULATION

# Calculate opponent's probability of rock, paper, scissors on a given round
# Transfers move probability from opponent's rows
# NB: takes 5-10s
calculate_opponent_move_probs = function(data) {
  data %>%
    group_by(game_id, round_index) %>%
    mutate(opp_prob_rock = ifelse(is.na(lag(prob_rock, 1)),
                                  lead(prob_rock, 1),
                                  lag(prob_rock, 1)),
           opp_prob_paper = ifelse(is.na(lag(prob_paper, 1)),
                                   lead(prob_paper, 1),
                                   lag(prob_paper, 1)),
           opp_prob_scissors = ifelse(is.na(lag(prob_scissors, 1)),
                                      lead(prob_scissors, 1),
                                      lag(prob_scissors, 1))
    )
}

# Calculate EV of each move based on opponent move probabilities
# `outcome_matrix` is globals with player moves in rows, opponent moves in cols, points for player in cells
# NB: this also adds column with EV value for chosen move based on the calculated EVs
calculate_move_ev = function(data, outcome_matrix) {
  # Calculate EV for each possible move
  data = data %>%
    group_by(player_id) %>%
    rowwise() %>%
    mutate(ev_rock = as.numeric(outcome_matrix["p1_rock",] %*% c(opp_prob_rock, opp_prob_paper, opp_prob_scissors)),
           ev_paper = as.numeric(outcome_matrix["p1_paper",] %*% c(opp_prob_rock, opp_prob_paper, opp_prob_scissors)),
           ev_scissors = as.numeric(outcome_matrix["p1_scissors",] %*% c(opp_prob_rock, opp_prob_paper, opp_prob_scissors))
    )

  # Add column with EV for move that was actually chosen each round (used for softmax fitting)
  # NB: takes 5-10s to run
  # TODO is there a more efficient way to do this?
  data = data %>%
    group_by(player_id) %>%
    rowwise() %>%
    mutate(
      ev_move_choice = case_when(
        player_move == "rock" ~ ev_rock,
        player_move == "paper" ~ ev_paper,
        player_move == "scissors" ~ ev_scissors,
        TRUE ~ (2/3) # For moves of "none", NA, etc. assigns EV for uniform opponent
      )
    )
  return(data)
}



# MODEL FITTING FUNCTIONS

# Fit softmax parameter to each player based on move EVs
brutefit = function(tmp) {
  ll_player_move = function(beta) {
    sum(
      -log(
        exp(beta*tmp$ev_move_choice) /
          rowSums(cbind(exp(beta*tmp$ev_rock), exp(beta*tmp$ev_paper), exp(beta*tmp$ev_scissors)))
      )
    )
  }

  fit = summary(mle(ll_player_move, start = list(beta = 1)))
  fit_vals = c(tmp$player_id[1],
               -0.5*fit@m2logL,
               length(tmp$round_index),
               fit@coef[,"Estimate"])
  names(fit_vals) = c("subject", "logL", "n", "softmax")
  return (fit_vals)
}



fit_model_to_subjects = function(data) {
  # Dataframe for storing model fits
  fit_summary = data.frame(
    "subject" = character(),
    "model" = character(),
    "logL" = numeric(),
    "n" = numeric(),
    "softmax" = numeric()
  )

  # Estimate softmax param for each individual participant
  # NB: takes ~10s to run
  for (id in unique(data$player_id)) {
    subj_data = data %>%
      filter(player_id == id)
    fit = brutefit(subj_data)
    fit_summary = rbind(fit_summary,
                        data.frame(
                          "subject" = fit["subject"],
                          model = "move_baserates",
                          "logL" = fit["logL"],
                          "n" = fit["n"],
                          "softmax" = fit["softmax"]))
  }

  # Clean up return dataframe
  fit_summary$logL = as.numeric(fit_summary$logL)
  fit_summary$n = as.numeric(fit_summary$n)
  fit_summary$softmax = as.numeric(fit_summary$softmax)
  fit_summary = fit_summary %>%
    mutate(ll_per_round = logL / n)
  return(fit_summary)
}


# GRAPH STYLE ====

default_plot_theme = theme(
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


# INITIALIZATION ====

# Read in data
dyad_data = read_dyad_data(DYAD_DATA_FILE, GAME_ROUNDS)

# Add columns
dyad_data = add_player_prev_move(dyad_data) # previous move
dyad_data = add_opponent_prev_move(dyad_data) # opponent previous move
dyad_data = add_transition(dyad_data, TRANSITION_VALS) # self-transition
dyad_data = add_opponent_transition(dyad_data, TRANSITION_VALS) # self-transition
dyad_data = add_player_prev_move_current_move(dyad_data) # combination of previous move, current move


# MODEL FITS ====

# Validate model with move baserates
dyad_data = count_moves(dyad_data) # Count each player's move choices (cumulative)
dyad_data = apply_move_count_prior(dyad_data, EVENT_COUNT_PRIOR) # Apply "prior" by setting counts to begin at `EVENT_COUNT_PRIOR`
dyad_data = calculate_move_probs_move_count(dyad_data, MOVE_PROBABILITY_PRIOR) # Calculate move probabilities based on counts from previous round
dyad_data = calculate_opponent_move_probs(dyad_data) # Calculate opponent's probability of rock, paper, scissors on a given round
dyad_data = calculate_move_ev(dyad_data, OUTCOME_VALS) # Calculate EV of each move (and chosen move) based on opponent move probabilities
fit_summary_moves = fit_model_to_subjects(dyad_data) # Fit model
fit_summary_moves = fit_summary_moves %>% mutate(model = "move baserate")



# Self-transition model
dyad_data = count_transitions(dyad_data)
dyad_data = apply_transition_count_prior(dyad_data, EVENT_COUNT_PRIOR)
# NB: the line below is the only part of the model fit that differs from the above
dyad_data = calculate_move_probs_transition(dyad_data, MOVE_PROBABILITY_PRIOR, TRANSITION_VALS) # Calculate move probabilities
dyad_data = calculate_opponent_move_probs(dyad_data)
dyad_data = calculate_move_ev(dyad_data, OUTCOME_VALS)
fit_summary_transitions = fit_model_to_subjects(dyad_data)
fit_summary_transitions = fit_summary_transitions %>% mutate(model = "transition baserate")


# Opponent-transition model
dyad_data = count_opponent_transitions(dyad_data)
dyad_data = apply_opponent_transition_count_prior(dyad_data, EVENT_COUNT_PRIOR)
# NB: the line below is the only part of the model fit that differs from the above
dyad_data = calculate_move_probs_opponent_transition(dyad_data, MOVE_PROBABILITY_PRIOR, TRANSITION_VALS) # Calculate move probabilities
dyad_data = calculate_opponent_move_probs(dyad_data)
dyad_data = calculate_move_ev(dyad_data, OUTCOME_VALS)
fit_summary_opponent_transitions = fit_model_to_subjects(dyad_data)
fit_summary_opponent_transitions = fit_summary_opponent_transitions %>% mutate(model = "opponent transition baserate")



# Move given prior move
# NB: two different "count" calls here to determine base rates (and two prior augments)
dyad_data = count_prev_move(dyad_data)
dyad_data = count_prev_move_current_move(dyad_data)
dyad_data = apply_prev_move_count_prior(dyad_data, count_prior = 3) # NOTE prior here
dyad_data = apply_prev_move_current_move_count_prior(dyad_data, EVENT_COUNT_PRIOR)
# NB: the line below is the only part of the model fit that differs from the above
dyad_data = calculate_move_probs_move_prev_move(dyad_data, MOVE_PROBABILITY_PRIOR) # Calculate move probabilities
dyad_data = calculate_opponent_move_probs(dyad_data)
dyad_data = calculate_move_ev(dyad_data, OUTCOME_VALS)
fit_summary_move_prev_move = fit_model_to_subjects(dyad_data)
fit_summary_move_prev_move = fit_summary_opponent_transitions %>% mutate(model = "move given previous move")


# Move given opponent prior move




# Outcome-transition model



# Finally: move given prior move, opponent prior move; move given prior two moves; transition given prior transition, prior outcome



# MODEL ANALYSIS ====

fit_summary = rbind(fit_summary_moves,
                    fit_summary_transitions,
                    fit_summary_opponent_transitions,
                    fit_summary_move_prev_move)



# View softmax param fits
# TODO identical vals for "opponent transition baserate" and "move given previous move" seem suspicious...

fit_summary %>% group_by(model) %>% summarize(mean(softmax))
# Plot them
fit_summary %>%
  ggplot(aes(x = model, y = softmax, color = model)) +
  geom_jitter(width = 0.1, height = 0, alpha = 0.5) +
  geom_point(stat="summary", fun="mean", size = 5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(y = "Softmax parameter estimates") +
  default_plot_theme +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())
# if predictors are uniform, softmax doesn't matter so much (should be around 0; try setting a prior?)


# View LL vals
fit_summary %>% group_by(model) %>% summarize(mean(ll_per_round))
# Plot them
fit_summary %>%
  ggplot(aes(x = model, y = ll_per_round, color = model)) +
  geom_jitter(width = 0.1, height = 0, alpha = 0.5) +
  geom_point(stat="summary", fun="mean", size = 5) +
  geom_hline(yintercept = -log(3), linetype = "dashed", color = "gray") +
  labs(y = "Log likelihood per round") +
  default_plot_theme +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())




