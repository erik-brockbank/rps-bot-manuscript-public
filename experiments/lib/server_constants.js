/*
 * constants library for rps server (this does *not* get loaded in the browser)
 */
var cloneDeep = require('lodash.clonedeep');

// Game constants
const DATAPATH = "/../data"; // path to data folder for writing output (note .. because js files are in `/lib`)
const GAME_ROUNDS = 300; // number of rounds opponents play in each game
const VALID_MOVES = ["rock", "paper", "scissors", "none"]; // valid game moves
const OUTCOME_POINTS = {"win": 3, "loss": -1, "tie": 0}; // points awared for valid round outcomes

const WINNING_MOVE_LOOKUP = { // returns the winning move for a particular opponent move
    "rock": "paper",
    "paper": "scissors",
    "scissors": "rock"
};

const TRANSITION_LOOKUP = { // returns the transition state for a pair of sequential moves
    "rock": {"rock": "0",
                "paper": "+",
                "scissors": "-"},
    "paper": {"rock": "-",
                "paper": "0",
                "scissors": "+"},
    "scissors": {"rock": "+",
                "paper": "-",
                "scissors": "0"}
};

const TRANSITION_MOVE_MAP = { // returns the move for a particular transition, given the previous move
    "+": {"rock": "paper",
            "paper": "scissors",
            "scissors": "rock"},
    "-": {"rock": "scissors",
            "paper": "rock",
            "scissors": "paper"},
    "0": {"rock": "rock",
            "paper": "paper",
            "scissors": "scissors"}
};

// Bot strategy constants
const BOT_FIXED_MOVE_PROB = 0.9; // probability of most likely transition or move choice
const BOT_STRATEGY_SET = [ // set of available strategies
    "prev_move_positive", // high probability of positive transition (r -> p -> s) from own previous move (NB: equivalent to win-positive, lose-positive, tie-positive)
    "prev_move_negative", // high probability of negative transition (r -> s -> p) from own previous move (NB: equivalent to win-negative, lose-negative, tie-negative)
    "opponent_prev_move_positive", // high probability of positive transition from *opponent's* previous move (NB: equivalent to win-nil, lose-negative, tie-positive)
    "opponent_prev_move_nil", // high probability of nil transition from *opponent's* previous move (copies opponent) (NB: equivalent to win-negative, lose-positive, tie-nil)
    "win_nil_lose_positive", // high probability of win-nil (stay), lose-positive, tie-negative
    "win_positive_lose_negative", // high probability of win-positive, lose-negative, tie-nil
    "outcome_transition_dual_dependency" // high probability of transitions given both outcomes and previous transitions
];

// Maps from each previous move (bot or opponent's) to a dictionary with lookup probabilities for each possible move choice
const PREV_MOVE_POSITIVE_TRANSITIONS = {
    "rock": {
                "rock": (1 - BOT_FIXED_MOVE_PROB) / 2,
                "paper": BOT_FIXED_MOVE_PROB,
                "scissors": (1 - BOT_FIXED_MOVE_PROB) / 2
            },
    "paper": {
                "rock": (1 - BOT_FIXED_MOVE_PROB) / 2,
                "paper": (1 - BOT_FIXED_MOVE_PROB) / 2,
                "scissors": BOT_FIXED_MOVE_PROB
            },
    "scissors": {
                "rock": BOT_FIXED_MOVE_PROB,
                "paper": (1 - BOT_FIXED_MOVE_PROB) / 2,
                "scissors": (1 - BOT_FIXED_MOVE_PROB) / 2
            }
};

const PREV_MOVE_NEGATIVE_TRANSITIONS = {
    "rock": {
                "rock": (1 - BOT_FIXED_MOVE_PROB) / 2,
                "paper": (1 - BOT_FIXED_MOVE_PROB) / 2,
                "scissors": BOT_FIXED_MOVE_PROB
            },
    "paper": {
                "rock": BOT_FIXED_MOVE_PROB,
                "paper": (1 - BOT_FIXED_MOVE_PROB) / 2,
                "scissors": (1 - BOT_FIXED_MOVE_PROB) / 2
            },
    "scissors": {
                "rock": (1 - BOT_FIXED_MOVE_PROB) / 2,
                "paper": BOT_FIXED_MOVE_PROB,
                "scissors": (1 - BOT_FIXED_MOVE_PROB) / 2
            }
};


const PREV_MOVE_NIL_TRANSITIONS = {
    "rock": {
                "rock": BOT_FIXED_MOVE_PROB,
                "paper": (1 - BOT_FIXED_MOVE_PROB) / 2,
                "scissors": (1 - BOT_FIXED_MOVE_PROB) / 2
            },
    "paper": {
                "rock": (1 - BOT_FIXED_MOVE_PROB) / 2,
                "paper": BOT_FIXED_MOVE_PROB,
                "scissors": (1 - BOT_FIXED_MOVE_PROB) / 2
            },
    "scissors": {
                "rock": (1 - BOT_FIXED_MOVE_PROB) / 2,
                "paper": (1 - BOT_FIXED_MOVE_PROB) / 2,
                "scissors": BOT_FIXED_MOVE_PROB
            }
};

const RANDOM_MOVE_TRANSITIONS = {
    "rock": 1 / 3,
    "paper": 1 / 3,
    "scissors": 1 / 3
};

// Map from bot strategy name to transition probabilities defined above
const BOT_STRATEGY_LOOKUP = {
    // Strategies that are not outcome-dependent
    "prev_move_positive": {"win": PREV_MOVE_POSITIVE_TRANSITIONS,
                            "loss": PREV_MOVE_POSITIVE_TRANSITIONS,
                            "tie": PREV_MOVE_POSITIVE_TRANSITIONS},
    "prev_move_negative": {"win": PREV_MOVE_NEGATIVE_TRANSITIONS,
                            "loss": PREV_MOVE_NEGATIVE_TRANSITIONS,
                            "tie": PREV_MOVE_NEGATIVE_TRANSITIONS},
    "opponent_prev_move_positive": {"win": PREV_MOVE_POSITIVE_TRANSITIONS,
                                    "loss": PREV_MOVE_POSITIVE_TRANSITIONS,
                                    "tie": PREV_MOVE_POSITIVE_TRANSITIONS},
    "opponent_prev_move_nil": {"win": PREV_MOVE_NIL_TRANSITIONS,
                                "loss": PREV_MOVE_NIL_TRANSITIONS,
                                "tie": PREV_MOVE_NIL_TRANSITIONS},
    // Strategies that are outcome-dependent
    "win_nil_lose_positive": {"win": PREV_MOVE_NIL_TRANSITIONS,
                                "loss": PREV_MOVE_POSITIVE_TRANSITIONS,
                                "tie": PREV_MOVE_NEGATIVE_TRANSITIONS},
    "win_positive_lose_negative": {"win": PREV_MOVE_POSITIVE_TRANSITIONS,
                                    "loss": PREV_MOVE_NEGATIVE_TRANSITIONS,
                                    "tie": PREV_MOVE_NIL_TRANSITIONS},
    // Strategies that are both outcome and previous transition dependent
    // NB: these are expressed in opposite order (outcome -> transition) from how they were whiteboarded (transition -> outcome)
    "outcome_transition_dual_dependency": {"win": {"+": PREV_MOVE_NEGATIVE_TRANSITIONS,
                                                    "0": PREV_MOVE_POSITIVE_TRANSITIONS,
                                                    "-": PREV_MOVE_NIL_TRANSITIONS},
                                            "loss": {"+": PREV_MOVE_NIL_TRANSITIONS,
                                                        "0": PREV_MOVE_NEGATIVE_TRANSITIONS,
                                                        "-": PREV_MOVE_POSITIVE_TRANSITIONS},
                                            "tie": {"+": PREV_MOVE_POSITIVE_TRANSITIONS,
                                                        "0": PREV_MOVE_NIL_TRANSITIONS,
                                                        "-": PREV_MOVE_NEGATIVE_TRANSITIONS}}
};


// Adaptive bot constants
const ADAPTIVE_BOT_STRATEGY_SET = [ // set of available adaptive strategies
    /* Marginal */
    // "opponent_moves", // maximize against the opponent's move distribution
    /* One previous move */
    "opponent_prev_move", // maximize against the opponent's move distribution given their previous move
    "bot_prev_move", // maximize against the opponent's move distribution given the bot's previous move
    /* Two previous moves */
    "opponent_bot_prev_move", // maximize against opponent's move dist. given their previous move and the bot's previous move
    "opponent_prev_two_moves", // maximize against opponent's move dist. given their previous two moves
    // "bot_prev_two_moves", // maximize against opponent's move dist. given the bot's previous two moves
    /* Transition marginal */
    "opponent_transitions", // maximize against the opponent's transition distribution (relative to their own move)
    "opponent_courn_transitions", // maximize against the opponent's transition distribution (relative to the bot's move)
    /* Outcome-based transitions */
    "opponent_outcome_transitions", // maximize against the opponent's transition distribution given the previous outcome
    /* Previous transition and outcome based transitions */
    "opponent_outcome_prev_transition_dual" // maximize against the opponent's transition given previous outcome *and* prev. transition
];

// Supporting counter structures, not passed along to game logic
const BASE_MOVE_COUNTER = {
    "rock": 0,
    "paper": 0,
    "scissors": 0
};

const BASE_TRANSITION_COUNTER = {
    "+": 0,
    "-": 0,
    "0": 0
};

const ONE_BACK_MOVE_COUNTER = {
    "rock": cloneDeep(BASE_MOVE_COUNTER),
    "paper": cloneDeep(BASE_MOVE_COUNTER),
    "scissors": cloneDeep(BASE_MOVE_COUNTER)
};

const TWO_BACK_MOVE_COUNTER = {
    "rock": cloneDeep(ONE_BACK_MOVE_COUNTER),
    "paper": cloneDeep(ONE_BACK_MOVE_COUNTER),
    "scissors": cloneDeep(ONE_BACK_MOVE_COUNTER)
};

const OUTCOME_TRANSITION_COUNTER = {
    "win": cloneDeep(BASE_TRANSITION_COUNTER),
    "loss": cloneDeep(BASE_TRANSITION_COUNTER),
    "tie": cloneDeep(BASE_TRANSITION_COUNTER)
};

// Memory lookup constructed from counter structures above
const ADAPATIVE_BOT_MEMORY_LOOKUP = {
    "opponent_moves": cloneDeep(BASE_MOVE_COUNTER),
    "opponent_prev_move": cloneDeep(ONE_BACK_MOVE_COUNTER),
    "bot_prev_move": cloneDeep(ONE_BACK_MOVE_COUNTER),
    "opponent_bot_prev_move": cloneDeep(TWO_BACK_MOVE_COUNTER),
    "opponent_prev_two_moves": cloneDeep(TWO_BACK_MOVE_COUNTER),
    "bot_prev_two_moves": cloneDeep(TWO_BACK_MOVE_COUNTER),
    "opponent_transitions": cloneDeep(BASE_TRANSITION_COUNTER),
    "opponent_courn_transitions": cloneDeep(BASE_TRANSITION_COUNTER),
    "opponent_outcome_transitions": cloneDeep(OUTCOME_TRANSITION_COUNTER),
    "opponent_outcome_prev_transition_dual": {"+": cloneDeep(OUTCOME_TRANSITION_COUNTER),
                                                "-": cloneDeep(OUTCOME_TRANSITION_COUNTER),
                                                "0": cloneDeep(OUTCOME_TRANSITION_COUNTER)}
};


// node export structure for constants
exports.constants = {"DATAPATH": DATAPATH,
                        "GAME_ROUNDS": GAME_ROUNDS,
                        "VALID_MOVES": VALID_MOVES,
                        "POINTS": OUTCOME_POINTS,
                        "WINNING_MOVE_LOOKUP": WINNING_MOVE_LOOKUP,
                        "TRANSITION_LOOKUP": TRANSITION_LOOKUP,
                        "TRANSITION_MOVE_MAP": TRANSITION_MOVE_MAP,
                        "BOT_STRATEGY_SET": BOT_STRATEGY_SET,
                        "BOT_STRATEGY_LOOKUP": BOT_STRATEGY_LOOKUP,
                        "BOT_RANDOM_MOVES": RANDOM_MOVE_TRANSITIONS,
                        "ADAPTIVE_BOT_STRATEGY_SET": ADAPTIVE_BOT_STRATEGY_SET,
                        "ADAPATIVE_BOT_MEMORY_LOOKUP": ADAPATIVE_BOT_MEMORY_LOOKUP
                    };
