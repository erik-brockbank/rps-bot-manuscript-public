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
    mutate(player_prev_move = replace_na(lag(player_move, 1), "none"))
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
        player_move == "none" || player_prev_move == "none",
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
        player_move == "none" || opponent_prev_move == "none",
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
        player_move == "none" || player_prev_move == "none",
        "none",
        paste(player_prev_move, player_move, sep = "-")
      ))
}

# Add column for combination of opponent's previous move and player's current move as a string
# TODO this is very similar to `add_player_prev_move_current_move` above; possible consolidation?
add_opponent_prev_move_current_move = function(data) {
  data %>%
    group_by(player_id) %>%
    rowwise() %>%
    mutate(
      opponent_prev_move_current_move = ifelse(
        player_move == "none" || opponent_prev_move == "none",
        "none",
        paste(opponent_prev_move, player_move, sep = "-")
      ))
}

# Add column for each player's previous outcome
add_prev_outcome = function(data) {
  data %>%
    group_by(player_id) %>%
    mutate(player_prev_outcome = replace_na(lag(player_outcome, 1), "none"))
}

# Add column for combination of previous outcome and player's current transition as a string
add_outcome_transition = function(data) {
  data %>%
    group_by(player_id) %>%
    rowwise() %>%
    mutate(
      outcome_transition = ifelse(
        player_prev_outcome == "none" || transition == "none",
        "none",
        paste(player_prev_outcome, transition, sep = "-")
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
    group_by(player_id) %>%
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


# WEIGHTED MOVE COUNT FUNCTIONS

# Count *weighted* cumulative number of rock, paper, and scissors moves by each player
# Starts by splitting out data frame into a list with separate items for each player_id
# Then re-combines using map_dfr, which applies the function below to each element in the list,
# then aggregates list items into a single dataframe.
# The function takes in each player's rows, does a rowwise calculation to get the
# weighted count of rock, paper, and scissors subject to `pwr_slope` power law weighting
count_moves_w = function(data, pwr_slope) {
  data %>%
    split(data$player_id) %>%
    map_dfr(
      function(dat) {
        dat %>%
          rowwise() %>%
          mutate(
            # moves = list(.$player_move[1:round_index]), # NB: debugging to confirm that move sequence is captured
            count_rock_w = sum(
              # In each row, fetch player moves from 1:round index, reverse them
              # to weight most recent moves highest, get all previous rows where move = "rock",
              # apply exponential weight to those rows (1:round_index)^-1, then sum
              (rev(.$player_move[1:round_index]) == "rock") * (seq_len(round_index)^pwr_slope)
            ),
            count_paper_w = sum((rev(.$player_move[1:round_index]) == "paper") * (seq_len(round_index)^pwr_slope)),
            count_scissors_w = sum((rev(.$player_move[1:round_index]) == "scissors") * (seq_len(round_index)^pwr_slope))
          )
      }
    )
}



# Increase all weighted move counts by `count_prior`
# NB: this seems different from applying a prior because counts are not cumulative here
# This *shifts* all move counts; is this necessary?
apply_move_count_w_prior = function(data, count_prior) {
  data %>%
    group_by(player_id) %>%
    rowwise() %>%
    mutate(count_rock_w = count_rock_w + count_prior,
           count_paper_w = count_paper_w + count_prior,
           count_scissors_w = count_scissors_w + count_prior)
}

# Calculate move probability on a given round based on *weighted* move counts from previous round
# NB: the `probability_prior` is only needed to attach a probability to very first round when `lag` is NA
calculate_move_probs_move_count_w = function(data, probability_prior) {
  # Calculate totals for probability conversion
  data = data %>%
    rowwise() %>%
    mutate(count_total_moves_w = sum(count_rock_w, count_paper_w, count_scissors_w))
  # Calculate move probability
  data = data %>%
    group_by(player_id) %>%
    mutate(
      prob_rock = ifelse(is.na(lag(count_rock_w, 1)),
                         probability_prior,
                         (lag(count_rock_w, 1) / lag(count_total_moves_w, 1))),
      prob_paper = ifelse(is.na(lag(count_paper_w, 1)),
                          probability_prior,
                          (lag(count_paper_w, 1) / lag(count_total_moves_w, 1))),
      prob_scissors = ifelse(is.na(lag(count_scissors_w, 1)),
                             probability_prior,
                             (lag(count_scissors_w, 1) / lag(count_total_moves_w, 1)))
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
    group_by(player_id) %>%
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
    group_by(player_id) %>%
    rowwise() %>%
    mutate(
      # Rock probability is either prior or transition probability *as of previous round* for
      # transition indicated by previous round's move -> "rock"
      prob_rock = ifelse(player_prev_move == "none",
                         # If previous move was "none", probability of rock is just prior. This is a bit clumsy but conservative.
                         probability_prior,
                         # Else, probability of rock is appropriate transition probability from above for previous move -> "rock"
                         case_when(
                           transition_lookup[player_prev_move, "rock"] == "up" ~ prob_transition_up,
                           transition_lookup[player_prev_move, "rock"] == "down" ~ prob_transition_down,
                           transition_lookup[player_prev_move, "rock"] == "stay" ~ prob_transition_stay,
                           TRUE ~ probability_prior # NB: this should never be activated
                           )
                         ),
      prob_paper = ifelse(player_prev_move == "none",
                          probability_prior,
                          case_when(
                            transition_lookup[player_prev_move, "paper"] == "up" ~ prob_transition_up,
                            transition_lookup[player_prev_move, "paper"] == "down" ~ prob_transition_down,
                            transition_lookup[player_prev_move, "paper"] == "stay" ~ prob_transition_stay,
                            TRUE ~ probability_prior # NB: this should never be activated
                            )
                          ),
      prob_scissors = ifelse(player_prev_move == "none",
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


# WEIGHTED TRANSITION FUNCTIONS

# Count *weighted* cumulative number of transitions by each player
count_transitions_w = function(data, pwr_slope) {
  data %>%
    split(data$player_id) %>%
    map_dfr(
      function(dat) {
        dat %>%
          rowwise() %>%
          mutate(
            # transitions = list(.$transition[1:round_index]), # NB: debugging to confirm that transition sequence is captured
            count_transition_up_w = sum(
              # In each row, fetch player transitions from 1:round index, reverse them
              # to weight most recent transitions highest, get all previous rows where transition = "up",
              # apply exponential weight to those rows (1:round_index)^-1, then sum
              (rev(.$transition[1:round_index]) == "up") * (seq_len(round_index)^pwr_slope)
            ),
            count_transition_down_w = sum((rev(.$transition[1:round_index]) == "down") * (seq_len(round_index)^pwr_slope)),
            count_transition_stay_w = sum((rev(.$transition[1:round_index]) == "stay") * (seq_len(round_index)^pwr_slope))
          )
      }
    )
}


# Increase all weighted transition counts by `count_prior`
# NB: this seems different from applying a prior because counts are not cumulative here
# This *shifts* all transition counts; is this necessary?
apply_transition_count_w_prior = function(data, count_prior) {
  data %>%
    group_by(player_id) %>%
    rowwise() %>%
    mutate(count_transition_up_w = count_transition_up_w + count_prior,
           count_transition_down_w = count_transition_down_w + count_prior,
           count_transition_stay_w = count_transition_stay_w + count_prior)
}


# Calculate move probability on a given round based on *weighted* transition counts from previous round
# NB: the `probability_prior` is only needed to attach a probability to very first round when `lag` is NA
calculate_move_probs_transition_w = function(data, probability_prior, transition_lookup) {
  # Calculate totals for probability conversion
  data = data %>%
    rowwise() %>%
    mutate(count_total_transitions_w = sum(count_transition_up_w, count_transition_down_w, count_transition_stay_w))
  # Add probability of each weighted transition based on counts from previous round
  data = data %>%
    group_by(player_id) %>%
    mutate(
      prob_transition_up_w = ifelse(is.na(lag(count_total_transitions_w, 1)), probability_prior,
                                    lag(count_transition_up_w, 1) / lag(count_total_transitions_w, 1)),
      prob_transition_down_w = ifelse(is.na(lag(count_total_transitions_w, 1)), probability_prior,
                                      lag(count_transition_down_w, 1) / lag(count_total_transitions_w, 1)),
      prob_transition_stay_w = ifelse(is.na(lag(count_total_transitions_w, 1)), probability_prior,
                                      lag(count_transition_stay_w, 1) / lag(count_total_transitions_w, 1))
    )
  # Calculate move probability
  data = data %>%
    group_by(player_id) %>%
    rowwise() %>%
    mutate(
      # Rock probability is either prior or weighted transition probability *as of previous round* for
      # transition indicated by previous round's move -> "rock"
      prob_rock = ifelse(player_prev_move == "none",
                         # If previous move was "none", probability of rock is just prior. This is a bit clumsy but conservative.
                         probability_prior,
                         # Else, probability of rock is appropriate weighted transition probability from above for previous move -> "rock"
                         case_when(
                           transition_lookup[player_prev_move, "rock"] == "up" ~ prob_transition_up_w,
                           transition_lookup[player_prev_move, "rock"] == "down" ~ prob_transition_down_w,
                           transition_lookup[player_prev_move, "rock"] == "stay" ~ prob_transition_stay_w,
                           TRUE ~ probability_prior # NB: this should never be activated
                         )
      ),
      prob_paper = ifelse(player_prev_move == "none",
                          probability_prior,
                          case_when(
                            transition_lookup[player_prev_move, "paper"] == "up" ~ prob_transition_up_w,
                            transition_lookup[player_prev_move, "paper"] == "down" ~ prob_transition_down_w,
                            transition_lookup[player_prev_move, "paper"] == "stay" ~ prob_transition_stay_w,
                            TRUE ~ probability_prior # NB: this should never be activated
                          )
      ),
      prob_scissors = ifelse(player_prev_move == "none",
                             probability_prior,
                             case_when(
                               transition_lookup[player_prev_move, "scissors"] == "up" ~ prob_transition_up_w,
                               transition_lookup[player_prev_move, "scissors"] == "down" ~ prob_transition_down_w,
                               transition_lookup[player_prev_move, "scissors"] == "stay" ~ prob_transition_stay_w,
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
    group_by(player_id) %>%
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
    group_by(player_id) %>%
    rowwise() %>%
    mutate(
      # Rock probability is either prior or opponent transition probability
      # *as of previous round* for opponent transition indicated by opponent's previous round move -> "rock"
      prob_rock = ifelse(opponent_prev_move == "none",
                         # If opponent previous move was "none", probability of rock is just prior. This is a bit clumsy but conservative.
                         probability_prior,
                         # Else, probability of rock is appropriate *opponent* transition probability from above for opponent previous move -> "rock"
                         case_when(
                           transition_lookup[opponent_prev_move, "rock"] == "up" ~ prob_opponent_transition_up,
                           transition_lookup[opponent_prev_move, "rock"] == "down" ~ prob_opponent_transition_down,
                           transition_lookup[opponent_prev_move, "rock"] == "stay" ~ prob_opponent_transition_stay,
                           TRUE ~ probability_prior # NB: this should never be activated
                         )
      ),
      prob_paper = ifelse(opponent_prev_move == "none",
                          probability_prior,
                          case_when(
                            transition_lookup[opponent_prev_move, "paper"] == "up" ~ prob_opponent_transition_up,
                            transition_lookup[opponent_prev_move, "paper"] == "down" ~ prob_opponent_transition_down,
                            transition_lookup[opponent_prev_move, "paper"] == "stay" ~ prob_opponent_transition_stay,
                            TRUE ~ probability_prior # NB: this should never be activated
                          )
      ),
      prob_scissors = ifelse(opponent_prev_move == "none",
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


# WEIGHTED OPPONENT TRANSITION FUNCTIONS

# Count *weighted* cumulative number of opponent transitions by each player
# NB: this function is identical to `count_transitions_w` above except
# names of columns reflect *opponent* transitions
count_opponent_transitions_w = function(data, pwr_slope) {
  data %>%
    split(data$player_id) %>%
    map_dfr(
      function(dat) {
        dat %>%
          rowwise() %>%
          mutate(
            count_opponent_transition_up_w = sum(
              # In each row, fetch opponent transitions from 1:round index, reverse them
              # to weight most recent opponent transitions highest, get all previous rows where opp. transition == "up",
              # apply exponential weight to those rows (1:round_index)^-1, then sum
              (rev(.$opponent_transition[1:round_index]) == "up") * (seq_len(round_index)^pwr_slope)
            ),
            count_opponent_transition_down_w = sum((rev(.$opponent_transition[1:round_index]) == "down") * (seq_len(round_index)^pwr_slope)),
            count_opponent_transition_stay_w = sum((rev(.$opponent_transition[1:round_index]) == "stay") * (seq_len(round_index)^pwr_slope))
          )
      }
    )
}


# Increase all weighted *opponent* transition counts by `count_prior`
apply_opponent_transition_count_w_prior = function(data, count_prior) {
  data %>%
    group_by(player_id) %>%
    rowwise() %>%
    mutate(count_opponent_transition_up_w = count_opponent_transition_up_w + count_prior,
           count_opponent_transition_down_w = count_opponent_transition_down_w + count_prior,
           count_opponent_transition_stay_w = count_opponent_transition_stay_w + count_prior)
}


# Calculate move probability on a given round based on *opponent transition* counts from previous round
# NB: the `probability_prior` is only needed to attach a probability to very first round when `lag` is NA
calculate_move_probs_opponent_transition_w = function(data, probability_prior, transition_lookup) {
  # Calculate totals for probability conversion
  data = data %>%
    rowwise() %>%
    mutate(count_total_opponent_transitions_w = sum(count_opponent_transition_up_w, count_opponent_transition_down_w, count_opponent_transition_stay_w))
  # Add probability of each weighted opponent transition based on counts from previous round
  data = data %>%
    group_by(player_id) %>%
    mutate(
      prob_opponent_transition_up_w = ifelse(
        is.na(lag(count_total_opponent_transitions_w, 1)),
        probability_prior,
        lag(count_opponent_transition_up_w, 1) / lag(count_total_opponent_transitions_w, 1)),
      prob_opponent_transition_down_w = ifelse(
        is.na(lag(count_total_opponent_transitions_w, 1)),
        probability_prior,
        lag(count_opponent_transition_down_w, 1) / lag(count_total_opponent_transitions_w, 1)),
      prob_opponent_transition_stay_w = ifelse(
        is.na(lag(count_total_opponent_transitions_w, 1)),
        probability_prior,
        lag(count_opponent_transition_stay_w, 1) / lag(count_total_opponent_transitions_w, 1))
    )
  # Calculate move probability
  data = data %>%
    group_by(player_id) %>%
    rowwise() %>%
    mutate(
      # Rock probability is either prior or opponent transition probability
      # *as of previous round* for opponent transition indicated by opponent's previous round move -> "rock"
      prob_rock = ifelse(opponent_prev_move == "none",
                         # If opponent previous move was "none", probability of rock is just prior. This is a bit clumsy but conservative.
                         probability_prior,
                         # Else, probability of rock is appropriate *opponent* transition probability from above for opponent previous move -> "rock"
                         case_when(
                           transition_lookup[opponent_prev_move, "rock"] == "up" ~ prob_opponent_transition_up_w,
                           transition_lookup[opponent_prev_move, "rock"] == "down" ~ prob_opponent_transition_down_w,
                           transition_lookup[opponent_prev_move, "rock"] == "stay" ~ prob_opponent_transition_stay_w,
                           TRUE ~ probability_prior # NB: this should never be activated
                         )
      ),
      prob_paper = ifelse(opponent_prev_move == "none",
                          probability_prior,
                          case_when(
                            transition_lookup[opponent_prev_move, "paper"] == "up" ~ prob_opponent_transition_up_w,
                            transition_lookup[opponent_prev_move, "paper"] == "down" ~ prob_opponent_transition_down_w,
                            transition_lookup[opponent_prev_move, "paper"] == "stay" ~ prob_opponent_transition_stay_w,
                            TRUE ~ probability_prior # NB: this should never be activated
                          )
      ),
      prob_scissors = ifelse(opponent_prev_move == "none",
                             probability_prior,
                             case_when(
                               transition_lookup[opponent_prev_move, "scissors"] == "up" ~ prob_opponent_transition_up_w,
                               transition_lookup[opponent_prev_move, "scissors"] == "down" ~ prob_opponent_transition_down_w,
                               transition_lookup[opponent_prev_move, "scissors"] == "stay" ~ prob_opponent_transition_stay_w,
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
      count_prev_rock = cumsum(player_prev_move == "rock"),
      count_prev_paper = cumsum(player_prev_move == "paper"),
      count_prev_scissors = cumsum(player_prev_move == "scissors")
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
      prob_rock = ifelse(player_prev_move == "none",
                         # If player previous move was "none", probability of rock is just prior. This is a bit clumsy but conservative.
                         probability_prior,
                         # Else, probability of rock is probability from above for player previous move -> "rock"
                         case_when(
                           player_prev_move == "rock" ~ prob_rock_rock,
                           player_prev_move == "paper" ~ prob_paper_rock,
                           player_prev_move == "scissors" ~ prob_scissors_rock,
                           TRUE ~ probability_prior # NB: this should never be activated
                         )
      ),
      prob_paper = ifelse(player_prev_move == "none",
                          probability_prior,
                          case_when(
                            player_prev_move == "rock" ~ prob_rock_paper,
                            player_prev_move == "paper" ~ prob_paper_paper,
                            player_prev_move == "scissors" ~ prob_scissors_paper,
                            TRUE ~ probability_prior # NB: this should never be activated
                          )
      ),
      prob_scissors = ifelse(player_prev_move == "none",
                             probability_prior,
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


# WEIGHTED MOVE GIVEN PREVIOUS MOVE FUNCTIONS

# Generate *weighted* sum of each previous move by each player
count_prev_move_w = function(data, pwr_slope) {
  data %>%
    split(data$player_id) %>%
    map_dfr(
      function(dat) {
        dat %>%
          rowwise() %>%
          mutate(
            count_prev_rock_w = sum(
              # In each row, fetch player prev moves from 1:round index, reverse them
              # to weight most recent prev moves highest, get all previous rows where prev move == "rock",
              # apply exponential weight to those rows (1:round_index)^-1, then sum
              (rev(.$player_prev_move[1:round_index]) == "rock") * (seq_len(round_index)^pwr_slope)
            ),
            count_prev_paper_w = sum((rev(.$player_prev_move[1:round_index]) == "paper") * (seq_len(round_index)^pwr_slope)),
            count_prev_scissors_w = sum((rev(.$player_prev_move[1:round_index]) == "scissors") * (seq_len(round_index)^pwr_slope))
          )
      }
    )
}


# Count *weighted* cumulative number of combinations of previous move, current move
count_prev_move_current_move_w = function(data, pwr_slope) {
  data %>%
    split(data$player_id) %>%
    map_dfr(
      function(dat) {
        dat %>%
          rowwise() %>%
          mutate(
            count_rock_rock_w = sum(
              # In each row, fetch player prev move + current move from 1:round index, reverse them
              # to weight most recent moves highest, get all previous rows where prev move + current move == "rock-rock",
              # apply exponential weight to those rows (1:round_index)^-1, then sum
              (rev(.$player_prev_move_current_move[1:round_index]) == "rock-rock") * (seq_len(round_index)^pwr_slope)
            ),
            count_rock_paper_w = sum((rev(.$player_prev_move_current_move[1:round_index]) == "rock-paper") * (seq_len(round_index)^pwr_slope)),
            count_rock_scissors_w = sum((rev(.$player_prev_move_current_move[1:round_index]) == "rock-scissors") * (seq_len(round_index)^pwr_slope)),
            count_paper_rock_w = sum((rev(.$player_prev_move_current_move[1:round_index]) == "paper-rock") * (seq_len(round_index)^pwr_slope)),
            count_paper_paper_w = sum((rev(.$player_prev_move_current_move[1:round_index]) == "paper-paper") * (seq_len(round_index)^pwr_slope)),
            count_paper_scissors_w = sum((rev(.$player_prev_move_current_move[1:round_index]) == "paper-scissors") * (seq_len(round_index)^pwr_slope)),
            count_scissors_rock_w = sum((rev(.$player_prev_move_current_move[1:round_index]) == "scissors-rock") * (seq_len(round_index)^pwr_slope)),
            count_scissors_paper_w = sum((rev(.$player_prev_move_current_move[1:round_index]) == "scissors-paper") * (seq_len(round_index)^pwr_slope)),
            count_scissors_scissors_w = sum((rev(.$player_prev_move_current_move[1:round_index]) == "scissors-scissors") * (seq_len(round_index)^pwr_slope))
          )
      }
    )
}


# Apply "prior" to previous move counts by increasing all counts by `count_prior`
# Counts start at `count_prior` rather than 0, increase from that amount
# NOTE we apply a prior count of 3 times the prior for events we're conditioning on
apply_prev_move_count_w_prior = function(data, count_prior) {
  data %>%
    group_by(player_id) %>%
    rowwise() %>%
    mutate(
      count_prev_rock_w = count_prev_rock_w + count_prior,
      count_prev_paper_w = count_prev_paper_w + count_prior,
      count_prev_scissors_w = count_prev_scissors_w + count_prior
    )
}


# Apply "prior" to *weighted* previous move, current move counts by increasing all counts by `count_prior`
# Counts start at `count_prior` rather than 0, increase from that amount
apply_prev_move_current_move_count_w_prior = function(data, count_prior) {
  data %>%
    group_by(player_id) %>%
    rowwise() %>%
    mutate(
      count_rock_rock_w = count_rock_rock_w + count_prior,
      count_rock_paper_w = count_rock_paper_w + count_prior,
      count_rock_scissors_w = count_rock_scissors_w + count_prior,
      count_paper_rock_w = count_paper_rock_w + count_prior,
      count_paper_paper_w = count_paper_paper_w + count_prior,
      count_paper_scissors_w = count_paper_scissors_w + count_prior,
      count_scissors_rock_w = count_scissors_rock_w + count_prior,
      count_scissors_paper_w = count_scissors_paper_w + count_prior,
      count_scissors_scissors_w = count_scissors_scissors_w + count_prior
    )
}



# Calculate move probability on a given round based on *weighted* previous move, current move counts from previous round
# NB: the `probability_prior` is only needed to attach a probability to very first round when `lag` is NA
calculate_move_probs_move_prev_move_w = function(data, probability_prior) {
  # First, calculate probability of each previous move, current move transition
  # as of the previous round
  data = data %>%
    group_by(player_id) %>%
    mutate(
      prob_rock_rock_w = ifelse(is.na(lag(count_rock_rock_w, 1)),
                                probability_prior,
                                lag(count_rock_rock_w, 1) / lag(count_prev_rock_w, 1)
      ),
      prob_rock_paper_w = ifelse(is.na(lag(count_rock_paper_w, 1)),
                                 probability_prior,
                                 lag(count_rock_paper_w, 1) / lag(count_prev_rock_w, 1)
      ),
      prob_rock_scissors_w = ifelse(is.na(lag(count_rock_scissors_w, 1)),
                                    probability_prior,
                                    lag(count_rock_scissors_w, 1) / lag(count_prev_rock_w, 1)
      ),
      prob_paper_rock_w = ifelse(is.na(lag(count_paper_rock_w, 1)),
                                 probability_prior,
                                 lag(count_paper_rock_w, 1) / lag(count_prev_paper_w, 1)
      ),
      prob_paper_paper_w = ifelse(is.na(lag(count_paper_paper_w, 1)),
                                  probability_prior,
                                  lag(count_paper_paper_w, 1) / lag(count_prev_paper_w, 1)
      ),
      prob_paper_scissors_w = ifelse(is.na(lag(count_paper_scissors_w, 1)),
                                     probability_prior,
                                     lag(count_paper_scissors_w, 1) / lag(count_prev_paper_w, 1)
      ),
      prob_scissors_rock_w = ifelse(is.na(lag(count_scissors_rock_w, 1)),
                                    probability_prior,
                                    lag(count_scissors_rock_w, 1) / lag(count_prev_scissors_w, 1)
      ),
      prob_scissors_paper_w = ifelse(is.na(lag(count_scissors_paper_w, 1)),
                                     probability_prior,
                                     lag(count_scissors_paper_w, 1) / lag(count_prev_scissors_w, 1)
      ),
      prob_scissors_scissors_w = ifelse(is.na(lag(count_scissors_scissors_w, 1)),
                                        probability_prior,
                                        lag(count_scissors_scissors_w, 1) / lag(count_prev_scissors_w, 1)
      )
    )

  # Now, select move probabilities calculated above based on player previous move
  data = data %>%
    group_by(player_id) %>%
    rowwise() %>%
    mutate(
      prob_rock = ifelse(player_prev_move == "none",
                         # If player previous move was "none", probability of rock is just prior. This is a bit clumsy but conservative.
                         probability_prior,
                         # Else, probability of rock is probability from above for player previous move -> "rock"
                         case_when(
                           player_prev_move == "rock" ~ prob_rock_rock_w,
                           player_prev_move == "paper" ~ prob_paper_rock_w,
                           player_prev_move == "scissors" ~ prob_scissors_rock_w,
                           TRUE ~ probability_prior # NB: this should never be activated
                         )
      ),
      prob_paper = ifelse(player_prev_move == "none",
                          probability_prior,
                          case_when(
                            player_prev_move == "rock" ~ prob_rock_paper_w,
                            player_prev_move == "paper" ~ prob_paper_paper_w,
                            player_prev_move == "scissors" ~ prob_scissors_paper_w,
                            TRUE ~ probability_prior # NB: this should never be activated
                          )
      ),
      prob_scissors = ifelse(player_prev_move == "none",
                             probability_prior,
                             case_when(
                               player_prev_move == "rock" ~ prob_rock_scissors_w,
                               player_prev_move == "paper" ~ prob_paper_scissors_w,
                               player_prev_move == "scissors" ~ prob_scissors_scissors_w,
                               TRUE ~ probability_prior # NB: this should never be activated
                             )
      )
    )
  return(data)
}





# MOVE GIVEN OPPONENT PREVIOUS MOVE FUNCTIONS

# Count cumulative number of each previous move by each player's opponent
count_opponent_prev_move = function(data) {
  data %>%
    group_by(player_id) %>%
    mutate(
      count_opp_prev_rock = cumsum(opponent_prev_move == "rock"),
      count_opp_prev_paper = cumsum(opponent_prev_move == "paper"),
      count_opp_prev_scissors = cumsum(opponent_prev_move == "scissors")
    )
}

# Count cumulative number of combinations of *opponent* previous move, current move
# TODO could probably consolidate this with nested iteration over "rock", "paper", "scissors"
count_opponent_prev_move_current_move = function(data) {
  data %>%
    group_by(player_id) %>%
    mutate(count_opp_rock_rock = cumsum(opponent_prev_move_current_move == "rock-rock"),
           count_opp_rock_paper = cumsum(opponent_prev_move_current_move == "rock-paper"),
           count_opp_rock_scissors = cumsum(opponent_prev_move_current_move == "rock-scissors"),
           count_opp_paper_rock = cumsum(opponent_prev_move_current_move == "paper-rock"),
           count_opp_paper_paper = cumsum(opponent_prev_move_current_move == "paper-paper"),
           count_opp_paper_scissors = cumsum(opponent_prev_move_current_move == "paper-scissors"),
           count_opp_scissors_rock = cumsum(opponent_prev_move_current_move == "scissors-rock"),
           count_opp_scissors_paper = cumsum(opponent_prev_move_current_move == "scissors-paper"),
           count_opp_scissors_scissors = cumsum(opponent_prev_move_current_move == "scissors-scissors")
    )
}

# Apply "prior" to *opponent* previous move counts by increasing all counts by `count_prior`
# Counts start at `count_prior` rather than 0, increase from that amount
# NOTE we apply a prior count of 3 here rather than 1
apply_opponent_prev_move_count_prior = function(data, count_prior) {
  data %>%
    group_by(player_id) %>%
    rowwise() %>%
    mutate(
      count_opp_prev_rock = count_opp_prev_rock + count_prior,
      count_opp_prev_paper = count_opp_prev_paper + count_prior,
      count_opp_prev_scissors = count_opp_prev_scissors + count_prior
    )
}

# Apply "prior" to *opponent* previous move, player current move counts by increasing all counts by `count_prior`
# Counts start at `count_prior` rather than 0, increase from that amount
apply_opponent_prev_move_current_move_count_prior = function(data, count_prior) {
  data %>%
    group_by(player_id) %>%
    rowwise() %>%
    mutate(
      count_opp_rock_rock = count_opp_rock_rock + count_prior,
      count_opp_rock_paper = count_opp_rock_paper + count_prior,
      count_opp_rock_scissors = count_opp_rock_scissors + count_prior,
      count_opp_paper_rock = count_opp_paper_rock + count_prior,
      count_opp_paper_paper = count_opp_paper_paper + count_prior,
      count_opp_paper_scissors = count_opp_paper_scissors + count_prior,
      count_opp_scissors_rock = count_opp_scissors_rock + count_prior,
      count_opp_scissors_paper = count_opp_scissors_paper + count_prior,
      count_opp_scissors_scissors = count_opp_scissors_scissors + count_prior
    )
}

# Calculate move probability on a given round based on *opponent* previous move, current move counts from previous round
# NB: the `probability_prior` is only needed to attach a probability to very first round when `lag` is NA
calculate_move_probs_opponent_move_prev_move = function(data, probability_prior) {
  # First, calculate probability of each *opponent previous move*, current move transition
  # as of the previous round
  data = data %>%
    group_by(player_id) %>%
    mutate(
      prob_opp_rock_rock = ifelse(is.na(lag(count_opp_rock_rock, 1)),
                                  probability_prior,
                                  lag(count_opp_rock_rock, 1) / lag(count_opp_prev_rock, 1)
      ),
      prob_opp_rock_paper = ifelse(is.na(lag(count_opp_rock_paper, 1)),
                                   probability_prior,
                                   lag(count_opp_rock_paper, 1) / lag(count_opp_prev_rock, 1)
      ),
      prob_opp_rock_scissors = ifelse(is.na(lag(count_opp_rock_scissors, 1)),
                                      probability_prior,
                                      lag(count_opp_rock_scissors, 1) / lag(count_opp_prev_rock, 1)
      ),
      prob_opp_paper_rock = ifelse(is.na(lag(count_opp_paper_rock, 1)),
                                   probability_prior,
                                   lag(count_opp_paper_rock, 1) / lag(count_opp_prev_paper, 1)
      ),
      prob_opp_paper_paper = ifelse(is.na(lag(count_opp_paper_paper, 1)),
                                    probability_prior,
                                    lag(count_opp_paper_paper, 1) / lag(count_opp_prev_paper, 1)
      ),
      prob_opp_paper_scissors = ifelse(is.na(lag(count_opp_paper_scissors, 1)),
                                       probability_prior,
                                       lag(count_opp_paper_scissors, 1) / lag(count_opp_prev_paper, 1)
      ),
      prob_opp_scissors_rock = ifelse(is.na(lag(count_opp_scissors_rock, 1)),
                                      probability_prior,
                                      lag(count_opp_scissors_rock, 1) / lag(count_opp_prev_scissors, 1)
      ),
      prob_opp_scissors_paper = ifelse(is.na(lag(count_opp_scissors_paper, 1)),
                                       probability_prior,
                                       lag(count_opp_scissors_paper, 1) / lag(count_opp_prev_scissors, 1)
      ),
      prob_opp_scissors_scissors = ifelse(is.na(lag(count_opp_scissors_scissors, 1)),
                                          probability_prior,
                                          lag(count_opp_scissors_scissors, 1) / lag(count_opp_prev_scissors, 1)
      )
    )

  # Now, select move probabilities calculated above based on opponent previous move
  data = data %>%
    group_by(player_id) %>%
    rowwise() %>%
    mutate(
      prob_rock = ifelse(opponent_prev_move == "none",
                         # If opponent previous move was "none", probability of rock is just prior. This is a bit clumsy but conservative.
                         probability_prior,
                         # Else, probability of rock is probability from above for opponent previous move -> "rock"
                         case_when(
                           opponent_prev_move == "rock" ~ prob_opp_rock_rock,
                           opponent_prev_move == "paper" ~ prob_opp_paper_rock,
                           opponent_prev_move == "scissors" ~ prob_opp_scissors_rock,
                           TRUE ~ probability_prior # NB: this should never be activated
                         )
      ),
      prob_paper = ifelse(opponent_prev_move == "none",
                          probability_prior,
                          case_when(
                            opponent_prev_move == "rock" ~ prob_opp_rock_paper,
                            opponent_prev_move == "paper" ~ prob_opp_paper_paper,
                            opponent_prev_move == "scissors" ~ prob_opp_scissors_paper,
                            TRUE ~ probability_prior # NB: this should never be activated
                          )
      ),
      prob_scissors = ifelse(opponent_prev_move == "none",
                             probability_prior,
                             case_when(
                               opponent_prev_move == "rock" ~ prob_opp_rock_scissors,
                               opponent_prev_move == "paper" ~ prob_opp_paper_scissors,
                               opponent_prev_move == "scissors" ~ prob_opp_scissors_scissors,
                               TRUE ~ probability_prior # NB: this should never be activated
                             )
      )
    )
  return(data)
}


# OUTCOME-TRANSITION FUNCTIONS

# Count cumulative number of each previous outcome for each player
count_prev_outcomes = function(data) {
  data %>%
    group_by(player_id) %>%
    mutate(
      count_prev_win = cumsum(player_prev_outcome == "win"),
      count_prev_loss = cumsum(player_prev_outcome == "loss"),
      count_prev_tie = cumsum(player_prev_outcome == "tie")
    )
}

# Count cumulative number of combinations of previous outcome, current transition
# TODO could probably consolidate this with nested iteration over outcomes, transitions
count_prev_outcome_current_transition = function(data) {
  data %>%
    group_by(player_id) %>%
    mutate(
      count_win_up = cumsum(outcome_transition == "win-up"),
      count_win_down = cumsum(outcome_transition == "win-down"),
      count_win_stay = cumsum(outcome_transition == "win-stay"),
      count_loss_up = cumsum(outcome_transition == "loss-up"),
      count_loss_down = cumsum(outcome_transition == "loss-down"),
      count_loss_stay = cumsum(outcome_transition == "loss-stay"),
      count_tie_up = cumsum(outcome_transition == "tie-up"),
      count_tie_down = cumsum(outcome_transition == "tie-down"),
      count_tie_stay = cumsum(outcome_transition == "tie-stay")
    )
}

# Apply "prior" to previous outcome counts by increasing all counts by `count_prior`
# Counts start at `count_prior` rather than 0, increase from that amount
# NOTE we apply a prior count of 3 here rather than 1
apply_outcome_count_prior = function(data, count_prior = 3) {
  data %>%
    group_by(player_id) %>%
    rowwise() %>%
    mutate(
      count_prev_win = count_prev_win + count_prior,
      count_prev_loss = count_prev_loss + count_prior,
      count_prev_tie = count_prev_tie + count_prior
    )
}

# Apply "prior" to combination of previous outcome, current transition counts by increasing all counts by `count_prior`
# Counts start at `count_prior` rather than 0, increase from that amount
apply_outcome_transition_count_prior = function(data, count_prior) {
  data %>%
    group_by(player_id) %>%
    rowwise() %>%
    mutate(
      count_win_up = count_win_up + count_prior,
      count_win_down = count_win_down + count_prior,
      count_win_stay = count_win_stay + count_prior,
      count_loss_up = count_loss_up + count_prior,
      count_loss_down = count_loss_down + count_prior,
      count_loss_stay = count_loss_stay + count_prior,
      count_tie_up = count_tie_up + count_prior,
      count_tie_down = count_tie_down + count_prior,
      count_tie_stay = count_tie_stay + count_prior
    )
}

# Calculate move probability on a given round based on previous outcome, current transition counts from previous round
# NB: the `probability_prior` is only needed to attach a probability to very first round when `lag` is NA
calculate_move_probs_outcome_transition = function(data, probability_prior, transition_lookup) {
  # First, calculate probability of each previous outcome, current transition
  # as of the previous round
  data = data %>%
    group_by(player_id) %>%
    mutate(
      prob_win_up = ifelse(is.na(lag(count_win_up, 1)),
                           probability_prior,
                           lag(count_win_up, 1) / lag(count_prev_win, 1)
      ),
      prob_win_down = ifelse(is.na(lag(count_win_down, 1)),
                             probability_prior,
                             lag(count_win_down, 1) / lag(count_prev_win, 1)
      ),
      prob_win_stay = ifelse(is.na(lag(count_win_stay, 1)),
                             probability_prior,
                             lag(count_win_stay, 1) / lag(count_prev_win, 1)
      ),
      prob_loss_up = ifelse(is.na(lag(count_loss_up, 1)),
                            probability_prior,
                            lag(count_loss_up, 1) / lag(count_prev_loss, 1)
      ),
      prob_loss_down = ifelse(is.na(lag(count_loss_down, 1)),
                              probability_prior,
                              lag(count_loss_down, 1) / lag(count_prev_loss, 1)
      ),
      prob_loss_stay = ifelse(is.na(lag(count_loss_stay, 1)),
                              probability_prior,
                              lag(count_loss_stay, 1) / lag(count_prev_loss, 1)
      ),
      prob_tie_up = ifelse(is.na(lag(count_tie_up, 1)),
                           probability_prior,
                           lag(count_tie_up, 1) / lag(count_prev_tie, 1)
      ),
      prob_tie_down = ifelse(is.na(lag(count_tie_down, 1)),
                             probability_prior,
                             lag(count_tie_down, 1) / lag(count_prev_tie, 1)
      ),
      prob_tie_stay = ifelse(is.na(lag(count_tie_stay, 1)),
                             probability_prior,
                             lag(count_tie_stay, 1) / lag(count_prev_tie, 1)
      )
    )

  # Now, select move probabilities calculated above based on player previous outcome
  # TODO surely there's a more efficient way to do this...
  data = data %>%
    group_by(player_id) %>%
    rowwise() %>%
    mutate(
      prob_rock = ifelse(player_prev_outcome == "none",
                         # If player previous outcome was "none", probability of rock is just prior. This is a bit clumsy but conservative.
                         probability_prior,
                         # Else, probability of rock is probability from above for player previous outcome -> transition that leads to "rock"
                         case_when(
                           player_prev_outcome == "win" ~ ifelse(is.na(player_prev_move) | player_prev_move == "none",
                                                                 # If previous move was NA or "none", probability of rock is just prior. This is a bit clumsy but conservative.
                                                                 probability_prior,
                                                                 # Else, probability of rock is appropriate transition probability from above for previous outcome, previous move -> "rock"
                                                                 case_when(
                                                                   transition_lookup[player_prev_move, "rock"] == "up" ~ prob_win_up,
                                                                   transition_lookup[player_prev_move, "rock"] == "down" ~ prob_win_down,
                                                                   transition_lookup[player_prev_move, "rock"] == "stay" ~ prob_win_stay,
                                                                   TRUE ~ probability_prior)),
                           player_prev_outcome == "loss" ~ ifelse(is.na(player_prev_move) | player_prev_move == "none",
                                                                 # If previous move was NA or "none", probability of rock is just prior. This is a bit clumsy but conservative.
                                                                 probability_prior,
                                                                 # Else, probability of rock is appropriate transition probability from above for previous outcome, previous move -> "rock"
                                                                 case_when(
                                                                   transition_lookup[player_prev_move, "rock"] == "up" ~ prob_loss_up,
                                                                   transition_lookup[player_prev_move, "rock"] == "down" ~ prob_loss_down,
                                                                   transition_lookup[player_prev_move, "rock"] == "stay" ~ prob_loss_stay,
                                                                   TRUE ~ probability_prior)),
                           player_prev_outcome == "tie" ~ ifelse(is.na(player_prev_move) | player_prev_move == "none",
                                                                 # If previous move was NA or "none", probability of rock is just prior. This is a bit clumsy but conservative.
                                                                 probability_prior,
                                                                 # Else, probability of rock is appropriate transition probability from above for previous outcome, previous move -> "rock"
                                                                 case_when(
                                                                   transition_lookup[player_prev_move, "rock"] == "up" ~ prob_tie_up,
                                                                   transition_lookup[player_prev_move, "rock"] == "down" ~ prob_tie_down,
                                                                   transition_lookup[player_prev_move, "rock"] == "stay" ~ prob_tie_stay,
                                                                   TRUE ~ probability_prior)),
                           TRUE ~ probability_prior # NB: this should never be activated
                         )
      ),
      prob_paper = ifelse(player_prev_outcome == "none",
                         # If player previous outcome was "none", probability of rock is just prior. This is a bit clumsy but conservative.
                         probability_prior,
                         # Else, probability of rock is probability from above for player previous outcome -> transition that leads to "rock"
                         case_when(
                           player_prev_outcome == "win" ~ ifelse(is.na(player_prev_move) | player_prev_move == "none",
                                                                 # If previous move was NA or "none", probability of rock is just prior. This is a bit clumsy but conservative.
                                                                 probability_prior,
                                                                 # Else, probability of rock is appropriate transition probability from above for previous outcome, previous move -> "rock"
                                                                 case_when(
                                                                   transition_lookup[player_prev_move, "paper"] == "up" ~ prob_win_up,
                                                                   transition_lookup[player_prev_move, "paper"] == "down" ~ prob_win_down,
                                                                   transition_lookup[player_prev_move, "paper"] == "stay" ~ prob_win_stay,
                                                                   TRUE ~ probability_prior)),
                           player_prev_outcome == "loss" ~ ifelse(is.na(player_prev_move) | player_prev_move == "none",
                                                                  # If previous move was NA or "none", probability of rock is just prior. This is a bit clumsy but conservative.
                                                                  probability_prior,
                                                                  # Else, probability of rock is appropriate transition probability from above for previous outcome, previous move -> "rock"
                                                                  case_when(
                                                                    transition_lookup[player_prev_move, "paper"] == "up" ~ prob_loss_up,
                                                                    transition_lookup[player_prev_move, "paper"] == "down" ~ prob_loss_down,
                                                                    transition_lookup[player_prev_move, "paper"] == "stay" ~ prob_loss_stay,
                                                                    TRUE ~ probability_prior)),
                           player_prev_outcome == "tie" ~ ifelse(is.na(player_prev_move) | player_prev_move == "none",
                                                                 # If previous move was NA or "none", probability of rock is just prior. This is a bit clumsy but conservative.
                                                                 probability_prior,
                                                                 # Else, probability of rock is appropriate transition probability from above for previous outcome, previous move -> "rock"
                                                                 case_when(
                                                                   transition_lookup[player_prev_move, "paper"] == "up" ~ prob_tie_up,
                                                                   transition_lookup[player_prev_move, "paper"] == "down" ~ prob_tie_down,
                                                                   transition_lookup[player_prev_move, "paper"] == "stay" ~ prob_tie_stay,
                                                                   TRUE ~ probability_prior)),
                           TRUE ~ probability_prior # NB: this should never be activated
                         )
      ),
      prob_scissors = ifelse(player_prev_outcome == "none",
                          # If player previous outcome was "none", probability of rock is just prior. This is a bit clumsy but conservative.
                          probability_prior,
                          # Else, probability of rock is probability from above for player previous outcome -> transition that leads to "rock"
                          case_when(
                            player_prev_outcome == "win" ~ ifelse(is.na(player_prev_move) | player_prev_move == "none",
                                                                  # If previous move was NA or "none", probability of rock is just prior. This is a bit clumsy but conservative.
                                                                  probability_prior,
                                                                  # Else, probability of rock is appropriate transition probability from above for previous outcome, previous move -> "rock"
                                                                  case_when(
                                                                    transition_lookup[player_prev_move, "scissors"] == "up" ~ prob_win_up,
                                                                    transition_lookup[player_prev_move, "scissors"] == "down" ~ prob_win_down,
                                                                    transition_lookup[player_prev_move, "scissors"] == "stay" ~ prob_win_stay,
                                                                    TRUE ~ probability_prior)),
                            player_prev_outcome == "loss" ~ ifelse(is.na(player_prev_move) | player_prev_move == "none",
                                                                   # If previous move was NA or "none", probability of rock is just prior. This is a bit clumsy but conservative.
                                                                   probability_prior,
                                                                   # Else, probability of rock is appropriate transition probability from above for previous outcome, previous move -> "rock"
                                                                   case_when(
                                                                     transition_lookup[player_prev_move, "scissors"] == "up" ~ prob_loss_up,
                                                                     transition_lookup[player_prev_move, "scissors"] == "down" ~ prob_loss_down,
                                                                     transition_lookup[player_prev_move, "scissors"] == "stay" ~ prob_loss_stay,
                                                                     TRUE ~ probability_prior)),
                            player_prev_outcome == "tie" ~ ifelse(is.na(player_prev_move) | player_prev_move == "none",
                                                                  # If previous move was NA or "none", probability of rock is just prior. This is a bit clumsy but conservative.
                                                                  probability_prior,
                                                                  # Else, probability of rock is appropriate transition probability from above for previous outcome, previous move -> "rock"
                                                                  case_when(
                                                                    transition_lookup[player_prev_move, "scissors"] == "up" ~ prob_tie_up,
                                                                    transition_lookup[player_prev_move, "scissors"] == "down" ~ prob_tie_down,
                                                                    transition_lookup[player_prev_move, "scissors"] == "stay" ~ prob_tie_stay,
                                                                    TRUE ~ probability_prior)),
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



fit_model_to_subjects = function(data, model) {
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
                          model = model,
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
  legend.position = "right",
  legend.key = element_rect(colour = "transparent", fill = "transparent"),

  panel.grid.major.x = element_blank(),
  panel.grid.minor.x = element_blank(),
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
dyad_data = add_opponent_prev_move_current_move(dyad_data) # combination of *opponent* previous move, current move
dyad_data = add_prev_outcome(dyad_data) # previous outcome
dyad_data = add_outcome_transition(dyad_data) # combination of previous outcome, transition


# MODEL FITS - UNWEIGHTED (ALL) ====

# Move baserate model
dyad_data = count_moves(dyad_data) # Count each player's move choices (cumulative)
dyad_data = apply_move_count_prior(dyad_data, EVENT_COUNT_PRIOR) # Apply "prior" by setting counts to begin at `EVENT_COUNT_PRIOR`
dyad_data = calculate_move_probs_move_count(dyad_data, MOVE_PROBABILITY_PRIOR) # Calculate move probabilities based on counts from previous round
dyad_data = calculate_opponent_move_probs(dyad_data) # Calculate opponent's probability of rock, paper, scissors on a given round
dyad_data = calculate_move_ev(dyad_data, OUTCOME_VALS) # Calculate EV of each move (and chosen move) based on opponent move probabilities
fit_summary_moves = fit_model_to_subjects(dyad_data, model = "move baserate") # Fit model


# Self-transition model
dyad_data = count_transitions(dyad_data)
dyad_data = apply_transition_count_prior(dyad_data, EVENT_COUNT_PRIOR)
# NB: the line below is the only part of the model fit that differs from the previous model
dyad_data = calculate_move_probs_transition(dyad_data, MOVE_PROBABILITY_PRIOR, TRANSITION_VALS) # Calculate move probabilities
dyad_data = calculate_opponent_move_probs(dyad_data)
dyad_data = calculate_move_ev(dyad_data, OUTCOME_VALS)
fit_summary_transitions = fit_model_to_subjects(dyad_data, model = "transition baserate")


# Opponent-transition model
dyad_data = count_opponent_transitions(dyad_data)
dyad_data = apply_opponent_transition_count_prior(dyad_data, EVENT_COUNT_PRIOR)
# NB: the line below is the only part of the model fit that differs from the previous models
dyad_data = calculate_move_probs_opponent_transition(dyad_data, MOVE_PROBABILITY_PRIOR, TRANSITION_VALS) # Calculate move probabilities
dyad_data = calculate_opponent_move_probs(dyad_data)
dyad_data = calculate_move_ev(dyad_data, OUTCOME_VALS)
fit_summary_opponent_transitions = fit_model_to_subjects(dyad_data, model = "opponent transition baserate")



# Move given prior move model
# NB: two different "count" calls here to determine base rates (and two prior augments)
dyad_data = count_prev_move(dyad_data)
dyad_data = count_prev_move_current_move(dyad_data)
dyad_data = apply_prev_move_count_prior(dyad_data, count_prior = 3) # NOTE prior here
dyad_data = apply_prev_move_current_move_count_prior(dyad_data, EVENT_COUNT_PRIOR)
# NB: the line below is the only part of the model fit that differs from the previous models
dyad_data = calculate_move_probs_move_prev_move(dyad_data, MOVE_PROBABILITY_PRIOR) # Calculate move probabilities
dyad_data = calculate_opponent_move_probs(dyad_data)
dyad_data = calculate_move_ev(dyad_data, OUTCOME_VALS)
fit_summary_move_prev_move = fit_model_to_subjects(dyad_data, model = "move given previous move")


# Move given opponent prior move model
# NB: two different "count" calls here to determine base rates (and two prior augments)
dyad_data = count_opponent_prev_move(dyad_data)
dyad_data = count_opponent_prev_move_current_move(dyad_data)
dyad_data = apply_opponent_prev_move_count_prior(dyad_data, count_prior = 3) # NOTE prior here
dyad_data = apply_opponent_prev_move_current_move_count_prior(dyad_data, EVENT_COUNT_PRIOR)
# NB: the line below is the only part of the model fit that differs from the previous models
dyad_data = calculate_move_probs_opponent_move_prev_move(dyad_data, MOVE_PROBABILITY_PRIOR) # Calculate move probabilities
dyad_data = calculate_opponent_move_probs(dyad_data)
dyad_data = calculate_move_ev(dyad_data, OUTCOME_VALS)
fit_summary_opponent_move_prev_move = fit_model_to_subjects(dyad_data, model = "move given opponent previous move")


# Outcome-transition model
# NB: two different "count" calls here to determine base rates (and two prior augments)
dyad_data = count_prev_outcomes(dyad_data)
dyad_data = count_prev_outcome_current_transition(dyad_data)
dyad_data = apply_outcome_count_prior(dyad_data, count_prior = 3) # NOTE prior here
dyad_data = apply_outcome_transition_count_prior(dyad_data, EVENT_COUNT_PRIOR)
# NB: the line below is the only part of the model fit that differs from the previous models
dyad_data = calculate_move_probs_outcome_transition(dyad_data, MOVE_PROBABILITY_PRIOR, TRANSITION_VALS) # Calculate move probabilities
dyad_data = calculate_opponent_move_probs(dyad_data)
dyad_data = calculate_move_ev(dyad_data, OUTCOME_VALS)
fit_summary_outcome_transition = fit_model_to_subjects(dyad_data, model = "outcome given previous transition")


# TODO: move given prior move + opponent prior move; move given prior two moves; transition given prior transition + prior outcome



# MODEL ANALYSIS - HIGH WIN COUNTS ====

# Get player win count differentials
# TODO game_id == '0283f6be-802c-4f51-bd49-7ddbc6d9dc84' missing rounds 224-227: why??
win_diff = dyad_data %>%
  group_by(game_id, player_id) %>%
  count(win_count = player_outcome == "win") %>%
  filter(win_count == TRUE) %>%
  group_by(game_id) %>%
  mutate(win_count = ifelse(
    is.na(lag(n, 1)),
    n - lead(n, 1),
    n - lag(n, 1))) %>%
  rename("subject" = player_id) %>%
  select(game_id, subject, win_count)

# Look at data among folks with positive win counts
win_diff %>%
  filter(win_count >= 28) %>%
  nrow()
summary(win_diff$win_count[win_diff$win_count > 0])
hist(win_diff$win_count[win_diff$win_count > 0])

# TODO set this to 0 for analyses below based on full data
THRESHOLD = summary(win_diff$win_count[win_diff$win_count > 0])['3rd Qu.']

high_wc_subjects = win_diff %>%
  ungroup() %>%
  filter(win_count >= THRESHOLD) %>%
  select(subject) %>%
  unique()


fit_summary_moves_cor = fit_summary_moves %>%
  inner_join(win_diff, by = c('subject')) %>%
  filter(win_count >= 0)
fit_summary_transitions_cor = fit_summary_transitions %>%
  inner_join(win_diff, by = c('subject')) %>%
  filter(win_count >= 0)
fit_summary_opponent_transitions_cor = fit_summary_opponent_transitions %>%
  inner_join(win_diff, by = c('subject')) %>%
  filter(win_count >= 0)
fit_summary_move_prev_move_cor = fit_summary_move_prev_move %>%
  inner_join(win_diff, by = c('subject')) %>%
  filter(win_count >= 0)
fit_summary_opponent_move_prev_move_cor = fit_summary_opponent_move_prev_move %>%
  inner_join(win_diff, by = c('subject')) %>%
  filter(win_count >= 0)
fit_summary_outcome_transition_cor = fit_summary_outcome_transition %>%
  inner_join(win_diff, by = c('subject')) %>%
  filter(win_count >= 0)


# Correlation between fitted softmax vals (or log likelihood) and win count differentials
plot(fit_summary_moves_cor$ll_per_round, fit_summary_moves_cor$win_count)
plot(fit_summary_transitions_cor$ll_per_round, fit_summary_transitions_cor$win_count)
plot(fit_summary_opponent_transitions_cor$ll_per_round, fit_summary_opponent_transitions_cor$win_count)
plot(fit_summary_move_prev_move_cor$ll_per_round, fit_summary_move_prev_move_cor$win_count)
plot(fit_summary_opponent_move_prev_move_cor$ll_per_round, fit_summary_opponent_move_prev_move_cor$win_count)
plot(fit_summary_outcome_transition_cor$ll_per_round, fit_summary_outcome_transition_cor$win_count)


cor.test(fit_summary_moves_cor$ll_per_round, fit_summary_moves_cor$win_count) # N.S.
cor.test(fit_summary_transitions_cor$ll_per_round, fit_summary_transitions_cor$win_count) # N.S.
cor.test(fit_summary_opponent_transitions_cor$ll_per_round, fit_summary_opponent_transitions_cor$win_count) # N.S.
cor.test(fit_summary_move_prev_move_cor$ll_per_round, fit_summary_move_prev_move_cor$win_count) # p = .07
cor.test(fit_summary_opponent_move_prev_move_cor$ll_per_round, fit_summary_opponent_move_prev_move_cor$win_count) # N. S.
cor.test(fit_summary_outcome_transition_cor$ll_per_round, fit_summary_outcome_transition_cor$win_count) # p = 0.005




# MODEL ANALYSIS - UNWEIGHTED (ALL) ====

fit_summary = rbind(fit_summary_moves,
                    fit_summary_transitions,
                    fit_summary_opponent_transitions,
                    fit_summary_move_prev_move,
                    fit_summary_opponent_move_prev_move,
                    fit_summary_outcome_transition)
# Set order of conditions
fit_summary$model = factor(fit_summary$model,
                           levels = c("move baserate",
                                      "transition baserate", "opponent transition baserate",
                                      "move given previous move", "move given opponent previous move",
                                      "outcome given previous transition")
                           )
# Format for figure
fit_summary$model_str = str_wrap(fit_summary$model, 20)


# View softmax param fits
fit_summary %>% group_by(model) %>% summarize(mean(softmax))
# Summary plot
p1 = fit_summary %>%
  # OPTIONAL: look at only high WCD subjects
  # filter(subject %in% high_wc_subjects$subject) %>%
  # Rest of graph below is same
  ggplot(aes(x = model, y = softmax, color = model)) +
  stat_summary(fun = "mean", geom = "pointrange",
               fun.max = function(x) mean(x) + sd(x) / sqrt(length(x)),
               fun.min = function(x) mean(x) - sd(x) / sqrt(length(x)),
               size = 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1, color = "red") +
  labs(y = "") +
  scale_color_viridis(discrete = T,
                      name = element_blank(),
                      labels = unique(fit_summary$model_str)) +
  default_plot_theme +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.spacing.y = unit(1.0, 'lines'),
        legend.key.size = unit(3, 'lines')
        )
# Individual plot
p2 = fit_summary %>%
  # OPTIONAL: look at only high WCD subjects
  # filter(subject %in% high_wc_subjects$subject) %>%
  # Rest of graph below is same
  ggplot(aes(x = model, y = softmax, color = model)) +
  geom_jitter(width = 0.1, height = 0, alpha = 0.25, size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1, color = "red") +
  labs(y = "Softmax parameter") +
  scale_color_viridis(discrete = T,
                      name = element_blank(),
                      labels = unique(fit_summary$model_str)) +
  default_plot_theme +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none"
  )

p2 + p1
# if predictors are uniform, softmax doesn't matter so much (should be around 0)


# View LL vals
fit_summary %>% group_by(model) %>% summarize(mean(ll_per_round))
# Summary plot
p1 = fit_summary %>%
  # OPTIONAL: look at only high WCD subjects
  # filter(subject %in% high_wc_subjects$subject) %>%
  # Rest of graph below is same
  ggplot(aes(x = model, y = ll_per_round, color = model)) +
  stat_summary(fun = "mean", geom = "pointrange",
               fun.max = function(x) mean(x) + sd(x) / sqrt(length(x)),
               fun.min = function(x) mean(x) - sd(x) / sqrt(length(x)),
               size = 1.5) +
  geom_hline(yintercept = -log(3), linetype = "dashed", linewidth = 1, color = "red") +
  labs(y = "") +
  scale_color_viridis(discrete = T,
                      name = element_blank(),
                      labels = unique(fit_summary$model_str)) +
  default_plot_theme +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.spacing.y = unit(1.0, 'lines'),
        legend.key.size = unit(3, 'lines')
        )
# Individual plot
p2 = fit_summary %>%
  # OPTIONAL: look at only high WCD subjects
  # filter(subject %in% high_wc_subjects$subject) %>%
  # Rest of graph below is same
  ggplot(aes(x = model, y = ll_per_round, color = model)) +
  geom_jitter(width = 0.1, height = 0, alpha = 0.25, size = 2) +
  geom_hline(yintercept = -log(3), linetype = "dashed", linewidth = 1, color = "red") +
  labs(y = "LL (per round)") +
  scale_color_viridis(discrete = T,
                      name = element_blank(),
                      labels = unique(fit_summary$model_str)) +
  default_plot_theme +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none"
  )

p2 + p1






# MODEL INITIALIZATION - WEIGHTED (ALL) ====

# Slopes chosen below to give memory for roughly last 10, 5, 3 trials
PWR_SLOPES = c(-0.836, -1.094, -1.365)
MEMORY_TRIALS = sapply(X = PWR_SLOPES, function(x) {sum((1:300)^x)})

# NB: toggle the multiplier below (e.g. to 5, 10) and re-run all weighted model code
PRIOR_MULTIPLIER = 1
PRIOR_TRIALS = sapply(X = MEMORY_TRIALS, function(x) {round((PRIOR_MULTIPLIER * x) / 3)})



# MODEL FITS - WEIGHTED MOVE BASERATES ====

# Weighted move baserates
# NB: this takes ~3 mins.
fit_summary_weighted = fit_summary_moves
for (x in seq(length(PWR_SLOPES))) {
  print(x)
  dyad_data = count_moves_w(dyad_data, pwr_slope = PWR_SLOPES[x])
  dyad_data = apply_move_count_w_prior(dyad_data, count_prior = PRIOR_TRIALS[x])
  dyad_data = calculate_move_probs_move_count_w(dyad_data, MOVE_PROBABILITY_PRIOR)
  dyad_data = calculate_opponent_move_probs(dyad_data)
  dyad_data = calculate_move_ev(dyad_data, OUTCOME_VALS)
  fit_summary_moves_weighted = fit_model_to_subjects(dyad_data, model = paste("weighted move baserate, trial memory:", round(MEMORY_TRIALS[x])))
  fit_summary_weighted = rbind(fit_summary_weighted, fit_summary_moves_weighted)
}

# Sanity checks
glimpse(fit_summary_weighted)
table(fit_summary_weighted$model)



# MODEL ANALYSIS - WEIGHTED MOVE BASERATES ====

fit_summary_weighted$model = factor(fit_summary_weighted$model,
                                    levels = c("move baserate",
                                               paste("weighted move baserate, trial memory:", round(MEMORY_TRIALS[1])),
                                               paste("weighted move baserate, trial memory:", round(MEMORY_TRIALS[2])),
                                               paste("weighted move baserate, trial memory:", round(MEMORY_TRIALS[3]))
                                    )
)

# Format for figure
fit_summary_weighted$model_str = str_wrap(fit_summary_weighted$model, 20)

# View softmax param fits
fit_summary_weighted %>% group_by(model) %>% summarize(mean(softmax))
# Summary plot
p1 = fit_summary_weighted %>%
  ggplot(aes(x = model, y = softmax, color = model)) +
  stat_summary(fun = "mean", geom = "pointrange",
               fun.max = function(x) mean(x) + sd(x) / sqrt(length(x)),
               fun.min = function(x) mean(x) - sd(x) / sqrt(length(x)),
               size = 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 1, color = "red") +
  labs(y = "") +
  scale_color_viridis(discrete = T,
                      name = paste("Prior multiplier:", PRIOR_MULTIPLIER),
                      labels = unique(fit_summary_weighted$model_str)) +
  default_plot_theme +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.spacing.y = unit(1.0, 'lines'),
        legend.key.size = unit(3, 'lines')
  )
# Individual plot
p2 = fit_summary_weighted %>%
  ggplot(aes(x = model, y = softmax, color = model)) +
  geom_jitter(width = 0.1, height = 0, alpha = 0.25, size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 1, color = "red") +
  labs(y = "Softmax parameter") +
  scale_color_viridis(discrete = T,
                      name = element_blank(),
                      labels = unique(fit_summary_weighted$model_str)) +
  default_plot_theme +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none"
  )

p2 + p1


# View LL vals
fit_summary_weighted %>% group_by(model) %>% summarize(mean(ll_per_round))
# Summary plot
p1 = fit_summary_weighted %>%
  ggplot(aes(x = model, y = ll_per_round, color = model)) +
  stat_summary(fun = "mean", geom = "pointrange",
               fun.max = function(x) mean(x) + sd(x) / sqrt(length(x)),
               fun.min = function(x) mean(x) - sd(x) / sqrt(length(x)),
               size = 1.5) +
  geom_hline(yintercept = -log(3), linetype = "dashed", size = 1, color = "red") +
  labs(y = "") +
  scale_color_viridis(discrete = T,
                      name = paste("Prior multiplier:", PRIOR_MULTIPLIER),
                      labels = unique(fit_summary_weighted$model_str)) +
  default_plot_theme +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.spacing.y = unit(1.0, 'lines'),
        legend.key.size = unit(3, 'lines')
  )
# Individual plot
p2 = fit_summary_weighted %>%
  ggplot(aes(x = model, y = ll_per_round, color = model)) +
  geom_jitter(width = 0.1, height = 0, alpha = 0.25, size = 2) +
  geom_hline(yintercept = -log(3), linetype = "dashed", size = 1, color = "red") +
  labs(y = "LL (per round)") +
  scale_color_viridis(discrete = T,
                      name = element_blank(),
                      labels = unique(fit_summary_weighted$model_str)) +
  default_plot_theme +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none"
  )

p2 + p1



# MODEL FITS - WEIGHTED TRANSITIONS ====

# Weighted self-transitions
# NB: this takes ~5 mins.
fit_summary_weighted = fit_summary_transitions
for (x in seq(length(PWR_SLOPES))) {
  print(x)
  dyad_data = count_transitions_w(dyad_data, pwr_slope = PWR_SLOPES[x])
  dyad_data = apply_transition_count_w_prior(dyad_data, count_prior = PRIOR_TRIALS[x])
  dyad_data = calculate_move_probs_transition_w(dyad_data, MOVE_PROBABILITY_PRIOR, TRANSITION_VALS)
  dyad_data = calculate_opponent_move_probs(dyad_data)
  dyad_data = calculate_move_ev(dyad_data, OUTCOME_VALS)
  fit_summary_transitions_weighted = fit_model_to_subjects(dyad_data, model = paste("weighted transition baserate, trial memory:", round(MEMORY_TRIALS[x])))
  fit_summary_weighted = rbind(fit_summary_weighted, fit_summary_transitions_weighted)
}

glimpse(fit_summary_weighted)
table(fit_summary_weighted$model)


# MODEL ANALYSIS - WEIGHTED TRANSITIONS ====

fit_summary_weighted$model = factor(fit_summary_weighted$model,
                                    levels = c("transition baserate",
                                               paste("weighted transition baserate, trial memory:", round(MEMORY_TRIALS[1])),
                                               paste("weighted transition baserate, trial memory:", round(MEMORY_TRIALS[2])),
                                               paste("weighted transition baserate, trial memory:", round(MEMORY_TRIALS[3]))
                                    )
)
# Format for figure
fit_summary_weighted$model_str = str_wrap(fit_summary_weighted$model, 20)

# View softmax param fits
fit_summary_weighted %>% group_by(model) %>% summarize(mean(softmax))
# Summary plot
p1 = fit_summary_weighted %>%
  ggplot(aes(x = model, y = softmax, color = model)) +
  stat_summary(fun = "mean", geom = "pointrange",
               fun.max = function(x) mean(x) + sd(x) / sqrt(length(x)),
               fun.min = function(x) mean(x) - sd(x) / sqrt(length(x)),
               size = 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 1, color = "red") +
  labs(y = "") +
  scale_color_viridis(discrete = T,
                      name = paste("Prior multiplier:", PRIOR_MULTIPLIER),
                      labels = unique(fit_summary_weighted$model_str)) +
  default_plot_theme +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.spacing.y = unit(1.0, 'lines'),
        legend.key.size = unit(3, 'lines')
  )
# Individual plot
p2 = fit_summary_weighted %>%
  ggplot(aes(x = model, y = softmax, color = model)) +
  geom_jitter(width = 0.1, height = 0, alpha = 0.25, size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 1, color = "red") +
  labs(y = "Softmax parameter") +
  scale_color_viridis(discrete = T,
                      name = element_blank(),
                      labels = unique(fit_summary_weighted$model_str)) +
  default_plot_theme +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none"
  )

p2 + p1


# View LL vals
fit_summary_weighted %>% group_by(model) %>% summarize(mean(ll_per_round))
# Summary plot
p1 = fit_summary_weighted %>%
  ggplot(aes(x = model, y = ll_per_round, color = model)) +
  stat_summary(fun = "mean", geom = "pointrange",
               fun.max = function(x) mean(x) + sd(x) / sqrt(length(x)),
               fun.min = function(x) mean(x) - sd(x) / sqrt(length(x)),
               size = 1.5) +
  geom_hline(yintercept = -log(3), linetype = "dashed", size = 1, color = "red") +
  labs(y = "") +
  scale_color_viridis(discrete = T,
                      name = paste("Prior multiplier:", PRIOR_MULTIPLIER),
                      labels = unique(fit_summary_weighted$model_str)) +
  default_plot_theme +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.spacing.y = unit(1.0, 'lines'),
        legend.key.size = unit(3, 'lines')
  )
# Individual plot
p2 = fit_summary_weighted %>%
  ggplot(aes(x = model, y = ll_per_round, color = model)) +
  geom_jitter(width = 0.1, height = 0, alpha = 0.25, size = 2) +
  geom_hline(yintercept = -log(3), linetype = "dashed", size = 1, color = "red") +
  labs(y = "LL (per round)") +
  scale_color_viridis(discrete = T,
                      name = element_blank(),
                      labels = unique(fit_summary_weighted$model_str)) +
  default_plot_theme +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none"
  )

p2 + p1


# MODEL FITS - WEIGHTED OPPONENT TRANSITIONS ====
# NB: do we need this?

# Weighted opponent-transitions
# NB: this takes ~5 mins.
fit_summary_weighted = fit_summary_opponent_transitions
for (x in seq(length(PWR_SLOPES))) {
  print(x)
  dyad_data = count_opponent_transitions_w(dyad_data, pwr_slope = PWR_SLOPES[1])
  dyad_data = apply_opponent_transition_count_w_prior(dyad_data, count_prior = PRIOR_TRIALS[1])
  dyad_data = calculate_move_probs_opponent_transition_w(dyad_data, MOVE_PROBABILITY_PRIOR, TRANSITION_VALS)
  dyad_data = calculate_opponent_move_probs(dyad_data)
  dyad_data = calculate_move_ev(dyad_data, OUTCOME_VALS)
  fit_summary_opponent_transitions_weighted = fit_model_to_subjects(dyad_data, model = paste("weighted opponent transition baserate, trial memory:", round(MEMORY_TRIALS[x])))
  fit_summary_weighted = rbind(fit_summary_weighted, fit_summary_opponent_transitions_weighted)
}

glimpse(fit_summary_weighted)
table(fit_summary_weighted$model)


# MODEL ANALYSIS - WEIGHTED OPPONENT TRANSITIONS ====

fit_summary_weighted$model = factor(fit_summary_weighted$model,
                                    levels = c("opponent transition baserate",
                                               paste("weighted opponent transition baserate, trial memory:", round(MEMORY_TRIALS[1])),
                                               paste("weighted opponent transition baserate, trial memory:", round(MEMORY_TRIALS[2])),
                                               paste("weighted opponent transition baserate, trial memory:", round(MEMORY_TRIALS[3]))
                                    )
)
# Format for figure
fit_summary_weighted$model_str = str_wrap(fit_summary_weighted$model, 20)


# View softmax param fits
fit_summary_weighted %>% group_by(model) %>% summarize(mean(softmax))
# Summary plot
p1 = fit_summary_weighted %>%
  ggplot(aes(x = model, y = softmax, color = model)) +
  stat_summary(fun = "mean", geom = "pointrange",
               fun.max = function(x) mean(x) + sd(x) / sqrt(length(x)),
               fun.min = function(x) mean(x) - sd(x) / sqrt(length(x)),
               size = 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 1, color = "red") +
  labs(y = "") +
  scale_color_viridis(discrete = T,
                      name = paste("Prior multiplier:", PRIOR_MULTIPLIER),
                      labels = unique(fit_summary_weighted$model_str)) +
  default_plot_theme +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.spacing.y = unit(1.0, 'lines'),
        legend.key.size = unit(3, 'lines')
  )
# Individual plot
p2 = fit_summary_weighted %>%
  ggplot(aes(x = model, y = softmax, color = model)) +
  geom_jitter(width = 0.1, height = 0, alpha = 0.25, size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 1, color = "red") +
  labs(y = "Softmax parameter") +
  scale_color_viridis(discrete = T,
                      name = element_blank(),
                      labels = unique(fit_summary_weighted$model_str)) +
  default_plot_theme +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none"
  )

p2 + p1


# View LL vals
fit_summary_weighted %>% group_by(model) %>% summarize(mean(ll_per_round))
# Summary plot
p1 = fit_summary_weighted %>%
  ggplot(aes(x = model, y = ll_per_round, color = model)) +
  stat_summary(fun = "mean", geom = "pointrange",
               fun.max = function(x) mean(x) + sd(x) / sqrt(length(x)),
               fun.min = function(x) mean(x) - sd(x) / sqrt(length(x)),
               size = 1.5) +
  geom_hline(yintercept = -log(3), linetype = "dashed", size = 1, color = "red") +
  labs(y = "") +
  scale_color_viridis(discrete = T,
                      name = paste("Prior multiplier:", PRIOR_MULTIPLIER),
                      labels = unique(fit_summary_weighted$model_str)) +
  default_plot_theme +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.spacing.y = unit(1.0, 'lines'),
        legend.key.size = unit(3, 'lines')
  )
# Individual plot
p2 = fit_summary_weighted %>%
  ggplot(aes(x = model, y = ll_per_round, color = model)) +
  geom_jitter(width = 0.1, height = 0, alpha = 0.25, size = 2) +
  geom_hline(yintercept = -log(3), linetype = "dashed", size = 1, color = "red") +
  labs(y = "LL (per round)") +
  scale_color_viridis(discrete = T,
                      name = element_blank(),
                      labels = unique(fit_summary_weighted$model_str)) +
  default_plot_theme +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none"
  )

p2 + p1



# MODEL FITS - WEIGHTED MOVE GIVEN PRIOR MOVE ====

# Weighted move given prior move
fit_summary_weighted = fit_summary_move_prev_move
for (x in seq(length(PWR_SLOPES))) {
  print(x)
  dyad_data = count_prev_move_w(dyad_data, pwr_slope = PWR_SLOPES[x])
  dyad_data = count_prev_move_current_move_w(dyad_data, pwr_slope = PWR_SLOPES[x])
  dyad_data = apply_prev_move_count_w_prior(dyad_data, count_prior = PRIOR_TRIALS[x]*3) # NOTE prior here
  dyad_data = apply_prev_move_current_move_count_w_prior(dyad_data, PRIOR_TRIALS[x])
  dyad_data = calculate_move_probs_move_prev_move_w(dyad_data, MOVE_PROBABILITY_PRIOR) # Calculate move probabilities
  dyad_data = calculate_opponent_move_probs(dyad_data)
  dyad_data = calculate_move_ev(dyad_data, OUTCOME_VALS)
  fit_summary_move_prev_move_weighted = fit_model_to_subjects(dyad_data, model = paste("weighted move given previous move, trial memory:", round(MEMORY_TRIALS[x])))
  fit_summary_weighted = rbind(fit_summary_weighted, fit_summary_move_prev_move_weighted)
}

glimpse(fit_summary_weighted)
unique(fit_summary_weighted$model)



# MODEL ANALYSIS - WEIGHTED MOVE GIVEN PRIOR MOVE ====

fit_summary_weighted$model = factor(fit_summary_weighted$model,
                                    levels = c("move given previous move",
                                               paste("weighted move given previous move, trial memory:", round(MEMORY_TRIALS[1])),
                                               paste("weighted move given previous move, trial memory:", round(MEMORY_TRIALS[2])),
                                               paste("weighted move given previous move, trial memory:", round(MEMORY_TRIALS[3]))
                                    )
)
# Format for figure
fit_summary_weighted$model_str = str_wrap(fit_summary_weighted$model, 20)


# View softmax param fits
fit_summary_weighted %>% group_by(model) %>% summarize(mean(softmax))
# Summary plot
p1 = fit_summary_weighted %>%
  ggplot(aes(x = model, y = softmax, color = model)) +
  stat_summary(fun = "mean", geom = "pointrange",
               fun.max = function(x) mean(x) + sd(x) / sqrt(length(x)),
               fun.min = function(x) mean(x) - sd(x) / sqrt(length(x)),
               size = 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 1, color = "red") +
  labs(y = "") +
  scale_color_viridis(discrete = T,
                      name = paste("Prior multiplier:", PRIOR_MULTIPLIER),
                      labels = unique(fit_summary_weighted$model_str)) +
  default_plot_theme +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.spacing.y = unit(1.0, 'lines'),
        legend.key.size = unit(3, 'lines')
  )
# Individual plot
p2 = fit_summary_weighted %>%
  ggplot(aes(x = model, y = softmax, color = model)) +
  geom_jitter(width = 0.1, height = 0, alpha = 0.25, size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 1, color = "red") +
  labs(y = "Softmax parameter") +
  scale_color_viridis(discrete = T,
                      name = element_blank(),
                      labels = unique(fit_summary_weighted$model_str)) +
  default_plot_theme +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none"
  )

p2 + p1


# View LL vals
fit_summary_weighted %>% group_by(model) %>% summarize(mean(ll_per_round))
# Summary plot
p1 = fit_summary_weighted %>%
  ggplot(aes(x = model, y = ll_per_round, color = model)) +
  stat_summary(fun = "mean", geom = "pointrange",
               fun.max = function(x) mean(x) + sd(x) / sqrt(length(x)),
               fun.min = function(x) mean(x) - sd(x) / sqrt(length(x)),
               size = 1.5) +
  geom_hline(yintercept = -log(3), linetype = "dashed", size = 1, color = "red") +
  labs(y = "") +
  scale_color_viridis(discrete = T,
                      name = paste("Prior multiplier:", PRIOR_MULTIPLIER),
                      labels = unique(fit_summary_weighted$model_str)) +
  default_plot_theme +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.spacing.y = unit(1.0, 'lines'),
        legend.key.size = unit(3, 'lines')
  )
# Individual plot
p2 = fit_summary_weighted %>%
  ggplot(aes(x = model, y = ll_per_round, color = model)) +
  geom_jitter(width = 0.1, height = 0, alpha = 0.25, size = 2) +
  geom_hline(yintercept = -log(3), linetype = "dashed", size = 1, color = "red") +
  labs(y = "LL (per round)") +
  scale_color_viridis(discrete = T,
                      name = element_blank(),
                      labels = unique(fit_summary_weighted$model_str)) +
  default_plot_theme +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none"
  )

p2 + p1






# MODEL FITS - WEIGHTED (ALL) ====
# NB: this replicates the analysis of the unweighted models in `MODEL FITS - UNWEIGHTED (ALL)` above

# Weighted move baserate model
dyad_data = count_moves_w(dyad_data, pwr_slope = PWR_SLOPES[1])
dyad_data = apply_move_count_w_prior(dyad_data, count_prior = PRIOR_TRIALS[1])
dyad_data = calculate_move_probs_move_count_w(dyad_data, MOVE_PROBABILITY_PRIOR)
dyad_data = calculate_opponent_move_probs(dyad_data)
dyad_data = calculate_move_ev(dyad_data, OUTCOME_VALS)
fit_summary_moves_weighted = fit_model_to_subjects(dyad_data, model = "weighted move baserate")

# Weighted self-transition model
dyad_data = count_transitions_w(dyad_data, pwr_slope = PWR_SLOPES[1])
dyad_data = apply_transition_count_w_prior(dyad_data, count_prior = PRIOR_TRIALS[1])
dyad_data = calculate_move_probs_transition_w(dyad_data, MOVE_PROBABILITY_PRIOR, TRANSITION_VALS)
dyad_data = calculate_opponent_move_probs(dyad_data)
dyad_data = calculate_move_ev(dyad_data, OUTCOME_VALS)
fit_summary_transitions_weighted = fit_model_to_subjects(dyad_data, model = "weighted transition baserate")

# Weighted opponent-transition model
dyad_data = count_opponent_transitions_w(dyad_data, pwr_slope = PWR_SLOPES[1])
dyad_data = apply_opponent_transition_count_w_prior(dyad_data, count_prior = PRIOR_TRIALS[1])
dyad_data = calculate_move_probs_opponent_transition_w(dyad_data, MOVE_PROBABILITY_PRIOR, TRANSITION_VALS)
dyad_data = calculate_opponent_move_probs(dyad_data)
dyad_data = calculate_move_ev(dyad_data, OUTCOME_VALS)
fit_summary_opponent_transitions_weighted = fit_model_to_subjects(dyad_data, model = "weighted opponent transition baserate")


# Weighted move given prior move model
# NB: two different "count" calls here to determine base rates (and two prior augments)
# TODO is this right?? Feels like things are getting crazy...
# NOTE: for apples-to-apples comparison of softmax vals, needs to be applying priors equivalently to simpler models
dyad_data = count_prev_move_w(dyad_data, pwr_slope = PWR_SLOPES[1])
dyad_data = count_prev_move_current_move_w(dyad_data, pwr_slope = PWR_SLOPES[1])
dyad_data = apply_prev_move_count_w_prior(dyad_data, count_prior = PRIOR_TRIALS[1]*3) # NOTE prior here
dyad_data = apply_prev_move_current_move_count_w_prior(dyad_data, PRIOR_TRIALS[1])
# TODO debug the below; do these weighted probabilities feel right???
# Seems like the probabilities are almost always pretty uniform but worth confirming
dyad_data = calculate_move_probs_move_prev_move_w(dyad_data, MOVE_PROBABILITY_PRIOR) # Calculate move probabilities
dyad_data = calculate_opponent_move_probs(dyad_data)
dyad_data = calculate_move_ev(dyad_data, OUTCOME_VALS)
fit_summary_move_prev_move_weighted = fit_model_to_subjects(dyad_data, model = "weighted move given previous move")




# MODEL ANALYSIS - WEIGHTED (ALL) ====
# NB: this replicates the analysis of the unweighted models in `MODEL ANALYSIS - UNWEIGHTED (ALL)` above

fit_summary_weighted = rbind(fit_summary_moves_weighted,
                             fit_summary_transitions_weighted,
                             fit_summary_opponent_transitions_weighted,
                             fit_summary_move_prev_move_weighted
                             # fit_summary_opponent_move_prev_move,
                             # fit_summary_outcome_transition
                             )
# Set order of conditions
fit_summary_weighted$model = factor(fit_summary_weighted$model,
                                    levels = c("weighted move baserate",
                                               "weighted transition baserate", "weighted opponent transition baserate",
                                               "weighted move given previous move"
                                               # "move given opponent previous move",
                                               # "outcome given previous transition"
                                               )
)
# Format for figure
fit_summary_weighted$model_str = str_wrap(fit_summary_weighted$model, 20)


# View softmax param fits
fit_summary_weighted %>% group_by(model) %>% summarize(mean(softmax))
# Summary plot
p1 = fit_summary_weighted %>%
  # OPTIONAL: look at only high WCD subjects
  # filter(subject %in% high_wc_subjects$subject) %>%
  # Rest of graph below is same
  ggplot(aes(x = model, y = softmax, color = model)) +
  stat_summary(fun = "mean", geom = "pointrange",
               fun.max = function(x) mean(x) + sd(x) / sqrt(length(x)),
               fun.min = function(x) mean(x) - sd(x) / sqrt(length(x)),
               size = 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1, color = "red") +
  labs(y = "") +
  scale_color_viridis(discrete = T,
                      name = element_blank(),
                      labels = unique(fit_summary_weighted$model_str)) +
  default_plot_theme +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.spacing.y = unit(1.0, 'lines'),
        legend.key.size = unit(3, 'lines')
  )
# Individual plot
p2 = fit_summary_weighted %>%
  # OPTIONAL: look at only high WCD subjects
  # filter(subject %in% high_wc_subjects$subject) %>%
  # Rest of graph below is same
  ggplot(aes(x = model, y = softmax, color = model)) +
  geom_jitter(width = 0.1, height = 0, alpha = 0.25, size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1, color = "red") +
  labs(y = "Softmax parameter") +
  scale_color_viridis(discrete = T,
                      name = element_blank(),
                      labels = unique(fit_summary_weighted$model_str)) +
  default_plot_theme +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none"
  )

p2 + p1


# View LL vals
fit_summary_weighted %>% group_by(model) %>% summarize(mean(ll_per_round))
# Summary plot
p1 = fit_summary_weighted %>%
  # OPTIONAL: look at only high WCD subjects
  # filter(subject %in% high_wc_subjects$subject) %>%
  # Rest of graph below is same
  ggplot(aes(x = model, y = ll_per_round, color = model)) +
  stat_summary(fun = "mean", geom = "pointrange",
               fun.max = function(x) mean(x) + sd(x) / sqrt(length(x)),
               fun.min = function(x) mean(x) - sd(x) / sqrt(length(x)),
               size = 1.5) +
  geom_hline(yintercept = -log(3), linetype = "dashed", linewidth = 1, color = "red") +
  labs(y = "") +
  scale_color_viridis(discrete = T,
                      name = element_blank(),
                      labels = unique(fit_summary_weighted$model_str)) +
  default_plot_theme +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.spacing.y = unit(1.0, 'lines'),
        legend.key.size = unit(3, 'lines')
  )
# Individual plot
p2 = fit_summary_weighted %>%
  # OPTIONAL: look at only high WCD subjects
  # filter(subject %in% high_wc_subjects$subject) %>%
  # Rest of graph below is same
  ggplot(aes(x = model, y = ll_per_round, color = model)) +
  geom_jitter(width = 0.1, height = 0, alpha = 0.25, size = 2) +
  geom_hline(yintercept = -log(3), linetype = "dashed", linewidth = 1, color = "red") +
  labs(y = "LL (per round)") +
  scale_color_viridis(discrete = T,
                      name = element_blank(),
                      labels = unique(fit_summary_weighted$model_str)) +
  default_plot_theme +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none"
  )

p2 + p1




