/*
 * Core game logic
 * Note: exports new GameServer() for use by app.js when clients connect
 */

var fs = require("fs");
var UUID = require("uuid");
var weighted = require("weighted");
var _ = require("lodash");
var server_constants = require("./server_constants.js"); // constants
const GAME_ROUNDS = server_constants.constants.GAME_ROUNDS;
const DATAPATH = server_constants.constants.DATAPATH;
const VALID_MOVES = server_constants.constants.VALID_MOVES;
const POINTS = server_constants.constants.POINTS;
const WINNING_MOVE_LOOKUP = server_constants.constants.WINNING_MOVE_LOOKUP;
const TRANSITION_LOOKUP = server_constants.constants.TRANSITION_LOOKUP;
const TRANSITION_MOVE_MAP = server_constants.constants.TRANSITION_MOVE_MAP;
const BOT_STRATEGY_SET = server_constants.constants.BOT_STRATEGY_SET;
const BOT_STRATEGY_LOOKUP = server_constants.constants.BOT_STRATEGY_LOOKUP;
const BOT_RANDOM_MOVES = server_constants.constants.BOT_RANDOM_MOVES;
const ADAPTIVE_BOT_STRATEGY_SET = server_constants.constants.ADAPTIVE_BOT_STRATEGY_SET;
const ADAPATIVE_BOT_MEMORY_LOOKUP = server_constants.constants.ADAPATIVE_BOT_MEMORY_LOOKUP;


/*
 * Object class for keeping track of each RPS game, i.e., 300 rounds of RPS between two players
 */
Game = function(game_id = null, version = "1", istest = false,
                // relevant SONA auto-completion info
                sona = 0, experiment_id = "", credit_token = "", survey_code = "",
                player1 = null, player2 = null, game_rounds = null) {
    this.game_id = game_id; // unique id for this game
    this.version = version; // version for this game (specifying whether players are paired or individual against a bot)
    this.istest = istest; // whether this game is a test,
    this.sona = sona; // whether this is a SONA online study (rather than in the lab)
    this.experiment_id = experiment_id; // experiment id if this is a SONA online study
    this.credit_token = credit_token; // credit token if this is a SONA online study
    this.survey_code = survey_code; // survey code if this is a SONA online study
    this.game_rounds = game_rounds; // total number of rounds to play in this game
    this.game_begin_ts = 0; // numeric: unix timestamp when both players began the game
    this.player1 = player1; // rps_player object: first player to join
    this.player1_client = null; // client connection to player 1 (used by server for passing messages)
    this.player2 = player2; // rps_player object: second player to join
    this.player2_client = null; // client connection to player 2 (used by server for passing messages)
    this.current_round_index = 0; // numeric: current round
    this.current_round = null; // rps_round object: depicts round currently in play
    this.player1_points_total = 0; // numeric: total points for player1
    this.player2_points_total = 0; // numeric: total points for player2
    this.previous_rounds = []; // list: previous rps_round objects for this game
};


/*
 * Object class for keeping track of each RPS round, i.e., one match between two players in a game
 * NB: some of these fields are redundant with fields in Game but allow each round to stand alone if need be
 */
Round = function(game) {
    this.game_id = game.game_id; // unique id for the game this round belongs to
    this.round_index = game.current_round_index; // numeric: round out of 100
    this.round_begin_ts = null; //numeric: unix timestamp when both players began the round
    this.player1_id = game.player1.client_id; // unique id for player 1 in this round
    this.player2_id = game.player2.client_id; // unique id for player 2 in this round
    this.player1_move = null; // VALID_MOVES item: player1's move
    this.player2_move = null; // VALID_MOVES item: player2's move
    this.player1_rt = 0; // numeric: time taken for player1 to select move
    this.player2_rt = 0; // numeric: time taken for player2 to select move
    this.player1_outcome = null; // rps_outcome item for player 1
    this.player2_outcome = null; // rps_outcome item for player 2
    this.player1_points = 0; // numeric: points for player1 in this round
    this.player2_points = 0; // numeric: points for player2 in this round
    this.player1_points_total = game.player1_points_total; // numeric: total points for player 1 *at beginning of round*
    this.player2_points_total = game.player2_points_total; // numeric: total points for player 2 *at beginning of round*
    this.player1_results_viewtime = 0; // numeric: time taken for player1 to view results of the round
    this.player2_results_viewtime = 0; // numeric: time taken for player2 to view results of the round
};

/*
 * Object class for keeping track of each RPS human player
 */
HumanPlayer = function(client_id) {
    this.client_id = client_id; // unique id for this client
    this.status = null; // status encoding: current status for this player
};


/*
 * Object class for keeping track of each RPS bot player
 * NB: this has the same fields as HumanPlayer but some additional functions defined for selecting moves etc.
 */
BotPlayer = function() {
    this.client_id = "bot"; // used to distinguish bots from human players
    this.uid = UUID(); // generate unique identifier for bot
    this.status = null;
    this.fixed_strategy = _.sample(BOT_STRATEGY_SET); // randomly select bot's fixed strategy
    this.fixed_strategy_transitions = BOT_STRATEGY_LOOKUP[this.fixed_strategy]; // set transition probabilities for bot's fixed strategy
};

// Utility function to evaluate whether a sequence of moves constituted a "+", "-", or "0" transition
// NB: this does not handle "none" moves so calling function must deal with that
// TODO replace this to server_constants.js:TRANSITION_LOOKUP
BotPlayer.prototype.evaluateTransition = function(move1, move2) {
    if (move1 == move2) {
        return "0";
    } else if ((move1 == "rock" && move2 == "paper") ||
                (move1 == "paper" && move2 == "scissors") ||
                (move1 == "scissors" && move2 == "rock")) {
        return "+";
    } else if ((move1 == "rock" && move2 == "scissors") ||
                (move1 == "paper" && move2 == "rock") ||
                (move1 == "scissors" && move2 == "paper")) {
        return "-";
    }
};

// Function to choose a move based on a bot's fixed strategy
BotPlayer.prototype.chooseMove = function(game) {
    if (game.current_round_index == 1 ||
            (this.fixed_strategy == "outcome_transition_dual_dependency" && game.current_round_index == 2)) {
        // sample randomly from "rock", "paper", "scissors"
        return weighted.select(BOT_RANDOM_MOVES);
    } else {
        // round index is 1-indexed, we want one behind the current round, 0-indexed
        var prev_round = game.previous_rounds[game.current_round_index - 2];
        // get bot's outcome in the previous round for determining its next move probabilities
        var prev_outcome = prev_round.player2_outcome;
        // If bot is using its opponent's previous move to determine next move, fetch its opponent's previous move
        if (this.fixed_strategy == "opponent_prev_move_positive" || this.fixed_strategy == "opponent_prev_move_nil") {
            var prev_move = prev_round.player1_move;
        } else {
        // In all other conditions, bot is using its own previous move so fetch bot's previous move
            var prev_move = prev_round.player2_move;
        }
        // If using a transition-dependent strategy (not just move or outcome), get previous transition
        if (this.fixed_strategy == "outcome_transition_dual_dependency") {
            var prev_round2 = game.previous_rounds[game.current_round_index - 3];
            var prev_move2 = prev_round2.player2_move;
            var prev_transition = this.evaluateTransition(prev_move2, prev_move);

            if (!prev_move || prev_move == "none" || !prev_outcome || !prev_move2 || prev_move2 == "none" || !prev_transition) {
                //console.log("CHOOSING MOVE. Random");
                // If we're unable to make a strategic choice because previous moves are uninformative, choose randomly
                return weighted.select(BOT_RANDOM_MOVES);
            } else {
                // Select move based on previous outcome and transition
                // NB: this object is nested one layer more than other strategy transitions
                return weighted.select(this.fixed_strategy_transitions[prev_outcome][prev_transition][prev_move]);
            }
        }

        // Choose move based on previous outcome and relevant previous move
        if (!prev_move || prev_move == "none" || !prev_outcome) {
            // If previous move was none, we have nothing to use to decide a move: sample randomly from "rock", "paper", "scissors"
            // NB: prev_move and/or prev_outcome can also be null if prev_round has a null player1_move; this can happen due to server restarts
            return weighted.select(BOT_RANDOM_MOVES);
        } else {
            // Select move based on probabilities associated with transition from whichever previous move is relevant to this strategy
            //  (and previous outcome)
            return weighted.select(this.fixed_strategy_transitions[prev_outcome][prev_move]);
        }
    }
};


// Function to select and register a move in the current game
BotPlayer.prototype.makeMove = function(game) {
    var move = this.chooseMove(game);
    console.log("game.js:\t received bot move: ", move);

    // Update current round based on bot's selected move
    var current_round = game.current_round;
    current_round.player2_move = move;
    current_round.player2_rt = 0;
    game.current_round = current_round;

    // Update bot status to allow for next round when player 1 is ready
    if (game.current_round_index == GAME_ROUNDS) {
        game.player2.status = "exited"; // end of game, set bot status
    } else {
        game.player2.status = "waiting_for_opponent"; // still in game
    }
    return game;
};


/*
 * Object class for keeping track of each *adaptive* RPS bot player
 */
AdaptiveBotPlayer = function() {
    this.client_id = "adaptive_bot";
    this.uid = UUID();
    this.status = null;
    this.adaptive_strategy = _.sample(ADAPTIVE_BOT_STRATEGY_SET); // randomly select bot's adaptive strategy
    this.memory_struct = _.cloneDeep(ADAPATIVE_BOT_MEMORY_LOOKUP[this.adaptive_strategy]); // get "memory" structure for this bot's adaptive strategy
};


// Function to select and register a move in the current game
// TODO this is currently *identical* to the same-named function for BotPlayer class
// See if there's a way to consolidate
AdaptiveBotPlayer.prototype.makeMove = function(game) {
    var move = this.chooseMove(game);
    console.log("game.js:\t received bot move: ", move);

    // Update current round based on bot's selected move
    var current_round = game.current_round;
    current_round.player2_move = move;
    current_round.player2_rt = 0;
    game.current_round = current_round;

    // Update bot status to allow for next round when player 1 is ready
    if (game.current_round_index == GAME_ROUNDS) {
        game.player2.status = "exited"; // end of game, set bot status
    } else {
        game.player2.status = "waiting_for_opponent"; // still in game
    }
    return game;
};


// Function to choose a move based on a bot's adaptive strategy
// NB: this function is similar to the same-named function for BotPlayer class
AdaptiveBotPlayer.prototype.chooseMove = function(game) {
    console.log("game.js:\t choosing adaptive bot move");
    // Returns key with highest value, or set of keys  in case of tie
    // Source: https://stackoverflow.com/questions/27376295/getting-key-with-the-highest-value-from-object
    maxMove = function(obj) {
        return Object.keys(obj).filter(x => {
            return obj[x] == Math.max.apply(null, Object.values(obj));
        });
    };
    var memory_struct = this.memory_struct;
    if (game.current_round_index <= 1 ||
        (game.current_round_index <= 2 && _.includes(
            ["opponent_prev_move", "bot_prev_move", "opponent_bot_prev_move",
                "opponent_transitions", "opponent_courn_transitions", "opponent_outcome_transitions"],
            this.adaptive_strategy)) ||
        (game.current_round_index <= 3 && _.includes(
            ["opponent_prev_two_moves", "bot_prev_two_moves", "opponent_outcome_prev_transition_dual"],
            this.adaptive_strategy))) {
        // sample randomly from "rock", "paper", "scissors"
        return weighted.select(BOT_RANDOM_MOVES);
    } else {
        var prev_round = game.previous_rounds[game.current_round_index - 2];
        if (this.adaptive_strategy == "opponent_moves") {
            // choose move that would win against most probable human opponent move
            // NB: if multiple opponent moves equally likely, chooses win against a move sampled from them
            return WINNING_MOVE_LOOKUP[_.sample(maxMove(memory_struct))];
        }
        if (this.adaptive_strategy == "opponent_prev_move") {
            // choose move that would win against most probable human opponent move, *given previous move*
            if (prev_round.player1_move in memory_struct) {
                return WINNING_MOVE_LOOKUP[_.sample(maxMove(memory_struct[prev_round.player1_move]))];
            } else {
                // If previous move was "none", choose randomly
                // TODO replace this with something smarter? back out their most common move and maximize against that?
                // Or include "none" in move tracking?
                return weighted.select(BOT_RANDOM_MOVES);
            }
        }
        if (this.adaptive_strategy == "bot_prev_move") {
            // choose move that would win against most probable human opponent give, *given bot previous move*
            if (prev_round.player2_move in memory_struct) {
                return WINNING_MOVE_LOOKUP[_.sample(maxMove(memory_struct[prev_round.player2_move]))];
            } else {
                // NB: this should never happen, means the bot made no choice in previous round
                return weighted.select(BOT_RANDOM_MOVES);
            }
        }
        if (this.adaptive_strategy == "opponent_bot_prev_move") {
            // choose move that would win against most probable human opponent move, given bot and human previous moves
            if (prev_round.player1_move in memory_struct && prev_round.player2_move in memory_struct) {
                // Nested structure of memory counts is (player previous move, bot previous move, player current move)
                return WINNING_MOVE_LOOKUP[_.sample(maxMove(memory_struct[prev_round.player1_move][prev_round.player2_move]))];
            } else {
                return weighted.select(BOT_RANDOM_MOVES);
            }
        }
        if (this.adaptive_strategy == "opponent_prev_two_moves") {
            // choose move that would win against most probably human opponent move, given previous *two* human moves
            var ante_prev_round = game.previous_rounds[game.current_round_index - 3];
            if (prev_round.player1_move in memory_struct && ante_prev_round.player1_move in memory_struct) {
                // Nested structure of memory counts is (player 2-back move, player previous move, player current move)
                return WINNING_MOVE_LOOKUP[_.sample(maxMove(memory_struct[ante_prev_round.player1_move][prev_round.player1_move]))];
            } else {
                return weighted.select(BOT_RANDOM_MOVES);
            }
        }
        if (this.adaptive_strategy == "bot_prev_two_moves") {
            // choose move that would win against most probably human opponent move, given previous *two* bot moves
            var ante_prev_round = game.previous_rounds[game.current_round_index - 3];
            if (prev_round.player2_move in memory_struct && ante_prev_round.player2_move in memory_struct) {
                // Nested structure of memory counts is (bot 2-back move, bot previous move, player current move)
                return WINNING_MOVE_LOOKUP[_.sample(maxMove(memory_struct[ante_prev_round.player2_move][prev_round.player2_move]))];
            } else {
                // NB: this should never happen, means the bot made no choice in previous round or two rounds back
                return weighted.select(BOT_RANDOM_MOVES);
            }
        }
        if (this.adaptive_strategy == "opponent_transitions") {
            // choose move that would win against most probable human opponent move, given human transition proportions (relative to own move)
            if (prev_round.player1_move && prev_round.player1_move != "none") {
                return WINNING_MOVE_LOOKUP[TRANSITION_MOVE_MAP[_.sample(maxMove(memory_struct))][prev_round.player1_move]];
            } else {
                return weighted.select(BOT_RANDOM_MOVES);
            }
        }
        if (this.adaptive_strategy == "opponent_courn_transitions") {
            // choose move that would win against most probably human opponent move, given human transition proportions (relative to bot's move)
            if (prev_round.player2_move && prev_round.player2_move != "none") {
                return WINNING_MOVE_LOOKUP[TRANSITION_MOVE_MAP[_.sample(maxMove(memory_struct))][prev_round.player2_move]];
            } else {
                // NB: this should never happen, means the bot made no choice in the previous round
                return weighted.select(BOT_RANDOM_MOVES);
            }
        }
        if (this.adaptive_strategy == "opponent_outcome_transitions") {
            // choose move that would win against most probably human opponent move, given human transition and outcome stats
            if (prev_round.player1_outcome && prev_round.player1_move && prev_round.player1_move != "none") {
                // NB: there's an error pattern on server logs consistent with prev_round.player1_outcome being null
                // but it's unclear how that can happen
                // prev_round.player1_outcome = null; // Testing
                return WINNING_MOVE_LOOKUP[TRANSITION_MOVE_MAP[_.sample(maxMove(memory_struct[prev_round.player1_outcome]))][prev_round.player1_move]];
            } else {
                return weighted.select(BOT_RANDOM_MOVES);
            }
        }
        if (this.adaptive_strategy == "opponent_outcome_prev_transition_dual") {
            var ante_prev_round = game.previous_rounds[game.current_round_index - 3];
            if (prev_round.player1_outcome && prev_round.player1_move && ante_prev_round.player1_move &&
                prev_round.player1_move != "none" && ante_prev_round.player1_move != "none") {
                var prev_transition = TRANSITION_LOOKUP[ante_prev_round.player1_move][prev_round.player1_move];
                var prev_outcome = prev_round.player1_outcome;
                // NB: there's an error pattern on server logs consistent with these being null
                // but it's unclear how that can happen...
                // var prev_transition = null; // Testing
                // var prev_outcome = null; // Testing
                var next_transition = _.sample(maxMove(memory_struct[prev_transition][prev_outcome]));
                return WINNING_MOVE_LOOKUP[TRANSITION_MOVE_MAP[next_transition][prev_round.player1_move]];
            } else {
                return weighted.select(BOT_RANDOM_MOVES);
            }
        }
    }
};

// Function to update "memory" based on results of the most recent round
AdaptiveBotPlayer.prototype.updateMemory = function(game) {
    console.log("game.js:\t updating bot memory");
    var current_round = game.current_round;
    var prev_round = null; // TODO write "get previous round" helper function that checks round index on the one it retrieves, iterates to find previous round
    var ante_prev_round = null; // Two rounds back
    if (game.current_round_index > 1) {
        // previous rounds are 0-indexed, current_round_index is 1-indexed
        prev_round = game.previous_rounds[game.current_round_index - 2];
        if (game.current_round_index > 2) {
            ante_prev_round = game.previous_rounds[game.current_round_index - 3];
        }
    }
    // Update memory structure
    if (this.adaptive_strategy == "opponent_moves") {
        // Only update if human player actually chose a move...
        if (current_round.player1_move in this.memory_struct) {
            this.memory_struct[current_round.player1_move] += 1;
        }
    }
    if (this.adaptive_strategy == "opponent_prev_move") {
        // Only update if there was a previous round and player actually chose a move in this round and the last
        if (prev_round &&
            prev_round.player1_move in this.memory_struct &&
            current_round.player1_move in this.memory_struct) {
                this.memory_struct[prev_round.player1_move][current_round.player1_move] += 1;
        }
    }
    if (this.adaptive_strategy == "bot_prev_move") {
        // Only upate if there was a previous round and bot chose in previous round and player chose this round
        if (prev_round &&
            prev_round.player2_move in this.memory_struct &&
            current_round.player1_move in this.memory_struct) {
                this.memory_struct[prev_round.player2_move][current_round.player1_move] += 1;
        }
    }
    if (this.adaptive_strategy == "opponent_bot_prev_move") {
        // Only update if there was a previous round and bot and player both chose in that round and player chose this round
        if (prev_round &&
            prev_round.player1_move in this.memory_struct &&
            prev_round.player2_move in this.memory_struct &&
            current_round.player1_move in this.memory_struct) {
                // Nested structure is (player previous move, bot previous move, player current move)
                this.memory_struct[prev_round.player1_move][prev_round.player2_move][current_round.player1_move] += 1;
        }
    }
    if (this.adaptive_strategy == "opponent_prev_two_moves") {
        // Only update if there was a player choice in the previous two rounds and in this one
        if (ante_prev_round && prev_round &&
            prev_round.player1_move in this.memory_struct &&
            ante_prev_round.player1_move in this.memory_struct &&
            current_round.player1_move in this.memory_struct) {
            // Nested structure is (player 2-back move, player previous move, player current move)
            this.memory_struct[ante_prev_round.player1_move][prev_round.player1_move][current_round.player1_move] += 1;
        }
    }
    if (this.adaptive_strategy == "bot_prev_two_moves") {
        // Only update if there was a bot choice in the previous two rounds and player choice this one
        if (ante_prev_round && prev_round &&
            prev_round.player2_move in this.memory_struct &&
            ante_prev_round.player2_move in this.memory_struct &&
            current_round.player1_move in this.memory_struct) {
            // Nested structure is (bot 2-back move, bot previous move, player current move)
            this.memory_struct[ante_prev_round.player2_move][prev_round.player2_move][current_round.player1_move] += 1;
        }
    }
    if (this.adaptive_strategy == "opponent_transitions") {
        // Only update if there was a player choice in the previous round and the current round
        if (prev_round && prev_round.player1_move && current_round.player1_move &&
            prev_round.player1_move != "none" && current_round.player1_move != "none") {
            this.memory_struct[TRANSITION_LOOKUP[prev_round.player1_move][current_round.player1_move]] += 1;
        }
    }
    if (this.adaptive_strategy == "opponent_courn_transitions") {
        // Only update if there was a bot choice in the previous round and a player choice in the current round
        if (prev_round && prev_round.player2_move && current_round.player1_move &&
            prev_round.player2_move != "none" && current_round.player1_move != "none") {
            this.memory_struct[TRANSITION_LOOKUP[prev_round.player2_move][current_round.player1_move]] += 1;
        }
    }
    if (this.adaptive_strategy == "opponent_outcome_transitions") {
        // Only update if there was a player choice in the previous round and the current round
        if (prev_round && prev_round.player1_move && current_round.player1_move &&
            prev_round.player1_move != "none" && current_round.player1_move != "none") {
            this.memory_struct[prev_round.player1_outcome][TRANSITION_LOOKUP[prev_round.player1_move][current_round.player1_move]] += 1;
        }
    }
    if (this.adaptive_strategy == "opponent_outcome_prev_transition_dual") {
        // Only update if there was a player choice in the previous two rounds and the current round
        if (ante_prev_round && prev_round &&
            ante_prev_round.player1_move && prev_round.player1_move && current_round.player1_move &&
            prev_round.player1_move != "none" && ante_prev_round.player1_move != "none" &&
            current_round.player1_move != "none") {
            var prev_transition = TRANSITION_LOOKUP[ante_prev_round.player1_move][prev_round.player1_move];
            var prev_outcome = prev_round.player1_outcome;
            var transition = TRANSITION_LOOKUP[prev_round.player1_move][current_round.player1_move];
            // Nested structure is (previous transition, previous outcome, most recent transition)
            this.memory_struct[prev_transition][prev_outcome][transition] += 1;
        }
    }
    console.log("game.js:\t updated memory struct: ", this.memory_struct);
    return this.memory_struct;
};




/*
 * Object class for keeping track of the RPS games being played
 * This is the work-horse of the server side game logic and the only thing visible to app.js
 */
GameServer = function() {
    this.active_games = {}; // dict: mapping from game_id to Game objects currently in play or awaiting more players
};


// Admin function to return active game state of this game server.
// Returns a dictionary with each active game and the current round index in that game.
// Used for /admin requests to ensure clean game state before running participants and to
// monitor state of each game while running participants
GameServer.prototype.getState = function() {
    var stateObj = {};
    for (elem in this.active_games) {
        stateObj[elem] = this.active_games[elem].current_round_index;
    }
    return stateObj;
};


// Util function to copy over relevant game attributes for sending to client
// NB: to avoid copying large(ish) amounts of data, we don't copy previous_rounds array here
GameServer.prototype.copyGameVals = function(game) {
    return {
        game_id: game.game_id,
        istest: game.istest,
        game_rounds: game.game_rounds,
        game_begin_ts: game.game_begin_ts,
        player1: game.player1,
        player2: game.player2,
        current_round_index: game.current_round_index,
        current_round: game.current_round,
        player1_points_total: game.player1_points_total,
        player2_points_total: game.player2_points_total
    };
};


// Util function to fetch the current game that a particular client belongs to
GameServer.prototype.getCurrentGame = function(client) {
    for (game_id in this.active_games) {
        current_game = this.active_games[game_id];
        if ((current_game.player1 && current_game.player1.client_id == client.userid) ||
            (current_game.player2 && current_game.player2.client_id == client.userid)) {
            return current_game;
        }
    }
};


// Util function to fetch the rps_player object that a particular client belongs to
GameServer.prototype.getCurrentPlayer = function(rps_game, client) {
    if (client.userid == rps_game.player1.client_id) {
        return rps_game.player1;
    } else if (client.userid == rps_game.player2.client_id) {
        return rps_game.player2;
    }
};


// Util function to fetch the rps_player object that a particular client *is matched up against*
GameServer.prototype.getOpponent = function(rps_game, client) {
    if (client.userid == rps_game.player1.client_id) {
        return rps_game.player2;
    } else if (client.userid == rps_game.player2.client_id) {
        return rps_game.player1;
    }
};


// Util function to fetch the socket connection for a given rps_player in the current Game
GameServer.prototype.getClient = function(rps_game, rps_player) {
    if (rps_game.player1_client && rps_player.client_id == rps_game.player1_client.userid) {
        return rps_game.player1_client;
    } else if (rps_game.player2_client && rps_player.client_id == rps_game.player2_client.userid) {
        return rps_game.player2_client;
    }
};


// Function to set a particular player's status within a game
GameServer.prototype.setPlayerStatus = function(rps_game, rps_player, status) {
    if (rps_player.client_id == rps_game.player1.client_id) {
        rps_game.player1.status = status;
        return rps_game;
    } else if (rps_player.client_id == rps_game.player2.client_id) {
        rps_game.player2.status = status;
        return rps_game;
    }
};


// Function to add new client to an existing game or create a new game with this client as the first player
GameServer.prototype.findGame = function(client, version, istest,
                                            // SONA online study params
                                            sona, experiment_id, credit_token, survey_code) {
    console.log("game.js:\t finding game for new client: ", client.userid, "\t version: ", version);
    // If playing paired version, first look for an existing game to add this client to
    if (version == "1" && Object.keys(this.active_games).length > 0) {
        for (game_id in this.active_games) {
            var current_game = this.active_games[game_id];
            if (!current_game.player1 || !current_game.player2) {
                // Add client to this game, update both clients accordingly
                this.addPlayerToGame(current_game.game_id, client, istest);
                return;
            }
        }
    }
    // If unable to find an existing game for the client (or if version is unpaired) create a new one
    this.createGame(client, version, istest, sona, experiment_id, credit_token, survey_code);
};


// Function to create a new game and add this client as the first player
GameServer.prototype.createGame = function(client, version, istest,
                                            // SONA online study params
                                            sona, experiment_id, credit_token, survey_code) {
    console.log("game.js:\t creating new game for client: ", client.userid, "\t version: ", version);
    // Create new player for this client
    newplayer = new HumanPlayer(client.userid);
    newplayer.status = "waiting_to_start"; // NB: setting status to "waiting_for_opponent" causes downstream issues in move processing
    // Create new game and add client
    newgame_id = UUID();
    var newgame = new Game(game_id = newgame_id, version = version, istest = istest,
                                sona = sona, experiment_id = experiment_id, credit_token = credit_token, survey_code = survey_code,
                                player1 = newplayer, player2 = null, total_rounds = GAME_ROUNDS);
    console.log("game.js:\t new game id: ", newgame.game_id);
    newgame.player1_client = client;
    // If this is a bot game, add bot as player 2
    if (version == "2" || version == "3") {
        var bot_opponent = this.createBotPlayer(version);
        newgame.player2 = bot_opponent;
    }

    // Add game to game server directory of games in play
    this.active_games[newgame_id] = newgame;
    // Update client telling them they're waiting and giving them the game id
    client.emit("newgame", {game_id: newgame_id});

    // If this is a bot game, proceed to game initialization with new player and paired bot
    if (version == "2" || version == "3") {
        this.beginBotGame(newgame.game_id);
    }
};


// Function to generate a bot player with a fixed or adaptive strategy (version 2 or 3)
GameServer.prototype.createBotPlayer = function(version) {
    if (version == "2") {
        var newbot = new BotPlayer();
        console.log("game.js:\t creating new bot player: ", newbot);
        return newbot;
    }
    if (version == "3") {
        var newbot = new AdaptiveBotPlayer();
        console.log("game.js:\t creating new bot player: ", newbot);
        return newbot;
    }
};


// Function to add client to an existing game that needs an opponent
GameServer.prototype.addPlayerToGame = function(game_id, client, istest) {
    console.log("game.js:\t adding client to existing game");
    // Create new player for this client
    var newplayer = new HumanPlayer(client.userid);
    // If this game was not a test game but the new client is a test, the game becomes a test game
    var current_game = this.active_games[game_id]
    if (current_game.istest == false && istest == true) {current_game.istest = istest;}
    current_game.player2 = newplayer;
    current_game.player2_client = client;
    // Modify relevant fields in existing game
    current_game.current_round_index = 1;
    current_game.game_begin_ts = new Date().getTime(); // unix timestamp
    // Create new round and add to game
    // NB: Round constructor relies on certain fields in game passed in
    var newround = new Round(current_game);
    newround.round_begin_ts = new Date().getTime(); // unix timetamp for when this round began
    current_game.current_round = newround;
    // Update player status values for this game
    current_game.player1.status = "in_play";
    current_game.player2.status = "in_play";
    // Update game server and update both clients
    this.active_games[game_id] = current_game;
    current_game.player1_client.emit("roundbegin", this.copyGameVals(current_game));
    current_game.player2_client.emit("roundbegin", this.copyGameVals(current_game));
};


// Function to finish initializing game with bot player
// NB: this has similar structure to `addPlayerToGame` but adjusted for second bot player
GameServer.prototype.beginBotGame = function(game_id, game_server) {
    var current_game = this.active_games[game_id];
    // Modify relevant fields in existing game
    current_game.current_round_index = 1;
    current_game.game_begin_ts = new Date().getTime(); // unix timestamp
    // Create new round and add to game
    // NB: Round constructor relies on certain fields in game passed in
    var newround = new Round(current_game);
    newround.round_begin_ts = new Date().getTime(); // unix timetamp for when this round began
    current_game.current_round = newround;
    // Update player status values for this game
    current_game.player1.status = "in_play";
    current_game.player2.status = "in_play";
    // Update individual client that round has begun and get move from bot
    var bot = current_game.player2;
    current_game = bot.makeMove(current_game);
    this.active_games[game_id] = current_game;
    current_game.player1_client.emit("roundbegin", this.copyGameVals(current_game));
};


// Find game that this client is playing and update state of the current round to reflect the new move.
// If waiting for other player's move, update status. If not, process winner and update both clients.
// NB: for adaptive bot, update stats on human opponent moves here when evaluating round outcome
GameServer.prototype.processMove = function(client, data) {
    console.log("game.js:\t received move: ", data.move, " from client: ", client.userid);
    // Find the game this client belongs to, update the game's move/rt fields with the values in `data`
    var current_game = this.getCurrentGame(client);
    var current_round = current_game.current_round;
    if (client.userid == current_game.player1.client_id) {
        current_round.player1_move = data.move;
        current_round.player1_rt = data.rt;
    } else if (client.userid == current_game.player2.client_id) {
        current_round.player2_move = data.move;
        current_round.player2_rt = data.rt;
    }
    // If this player is the first to choose a move, update the client that they're waiting for opponent
    if (!current_round.player1_move) {
        current_game.current_round = current_round;
        this.active_games[current_game.game_id] = current_game;
        current_game.player2_client.emit("roundwaiting_move");
    } else if (!current_round.player2_move) {
        current_game.current_round = current_round;
        this.active_games[current_game.game_id] = current_game;
        current_game.player1_client.emit("roundwaiting_move");
    }
    // If both players have chosen a move, determine winner and update players
    if (current_round.player1_move && current_round.player2_move) {
        current_round = this.evaluateRoundOutcome(current_round);
        current_game.current_round = current_round;
        current_game.player1_points_total += current_round.player1_points; // update game total points
        current_game.player2_points_total += current_round.player2_points; // update game total points
        // If this game is against an adaptive bot, update bot "memory"
        if (current_game.player2.client_id == "adaptive_bot") {
            current_game.player2.memory_struct = current_game.player2.updateMemory(current_game);
            current_game.current_round.player2_memory_struct = _.cloneDeep(current_game.player2.memory_struct);
        }
        // Update both clients (or single client if playing against a bot)
        this.active_games[current_game.game_id] = current_game;
        current_game.player1_client.emit("roundcomplete", this.copyGameVals(current_game));
        if (current_game.player2.client_id != "bot" && current_game.player2.client_id != "adaptive_bot") {
            current_game.player2_client.emit("roundcomplete", this.copyGameVals(current_game));
        }
    }
};


// Take in an rps_round object and determine the winner, fill in other relevant data.
// Returns the rps_round filled in
GameServer.prototype.evaluateRoundOutcome = function(rps_round) {
    console.log("game.js:\t evaluating round outcome");
    if (VALID_MOVES.indexOf(rps_round.player1_move) != -1 && VALID_MOVES.indexOf(rps_round.player2_move) != -1) {
        // All possible tie outcomes
        if (rps_round.player1_move == rps_round.player2_move) {
            player1_outcome = "tie";
            player2_outcome = "tie";
        // Player 1 wins
        } else if ((rps_round.player1_move == "rock" && rps_round.player2_move == "scissors") ||
            (rps_round.player1_move == "paper" && rps_round.player2_move == "rock") ||
            (rps_round.player1_move == "scissors" && rps_round.player2_move == "paper") ||
            (rps_round.player2_move == "none")) {
            player1_outcome = "win";
            player2_outcome = "loss";
        } else {
        // All other: player 2 wins
            player1_outcome = "loss";
            player2_outcome = "win";
        }
        rps_round.player1_outcome = player1_outcome;
        rps_round.player2_outcome = player2_outcome;
        rps_round.player1_points = POINTS[player1_outcome];
        rps_round.player2_points = POINTS[player2_outcome];
    }
    return rps_round;
};


// Received signal from client that we're ready for next round.
// Similar to receiving initial move, we tell the first client to wait for opponent.
// When both are ready, we update them accordingly.
GameServer.prototype.nextRound = function(client, data) {
    console.log("game.js:\t next round request from client: ", client.userid);
    // Identify game and round information based on the client
    var current_game = this.getCurrentGame(client);
    var current_round = current_game.current_round;
    var this_player = this.getCurrentPlayer(current_game, client);
    var this_opponent = this.getOpponent(current_game, client);
    // Update time player spent viewing round results
    if (client.userid == current_game.player1.client_id) {
        current_round.player1_results_viewtime = data.round_viewtime;
    } else if (client.userid == current_game.player2.client_id) {
        current_round.player2_results_viewtime = data.round_viewtime;
    }
    // Update player's status locally and for game object
    if (current_game.current_round_index == GAME_ROUNDS) {
        this_player.status = "exited"; // end of game, client should be taken to survey
        current_game = this.setPlayerStatus(current_game, this_player, "exited");
        this.active_games[current_game.game_id] = current_game;
        this.endGame(current_game.game_id, this_player);
    } else {
        this_player.status = "waiting_for_opponent"; // still in game
        current_game = this.setPlayerStatus(current_game, this_player, "waiting_for_opponent");
    }

    // Based on player status(es) set above, respond accordingly
    // If both players have now finished, write the results to a file and remove game from active game list
    if (this_player.status == "exited" && this_opponent.status == "exited") {
        current_game.previous_rounds.push(current_round);
        this.writeData(current_game);
        delete this.active_games[current_game.game_id];
        return;
    }
    // If only this client is ready, update them that they're waiting for opponent
    if (this_player.status == "waiting_for_opponent" &&
        this_opponent.status != "waiting_for_opponent") {
        this.active_games[current_game.game_id] = current_game;
        client.emit("roundwaiting_continue");
    // If both clients are ready, update them by starting the next round
    } else if (this_player.status == "waiting_for_opponent" &&
        this_opponent.status == "waiting_for_opponent") {
        current_game.current_round_index += 1;
        // NB: Round constructor relies on certain fields in game passed in (e.g. current_round_index updated above)
        var newround = new Round(current_game);
        newround.round_begin_ts = new Date().getTime(); // unix timetamp for when this round began
        current_game.current_round = newround;
        current_game.previous_rounds.push(current_round);
        // Update player status values for this game
        current_game = this.setPlayerStatus(current_game, this_player, "in_play");
        current_game = this.setPlayerStatus(current_game, this_opponent, "in_play");
        // Update both clients (or single client if playing against a bot)
        if (current_game.player2.client_id == "bot" || current_game.player2.client_id == "adaptive_bot") {
            current_game = this_opponent.makeMove(current_game);
        }
        this.active_games[current_game.game_id] = current_game;
        current_game.player1_client.emit("roundbegin", this.copyGameVals(current_game));
        if (current_game.player2.client_id != "bot" && current_game.player2.client_id != "adaptive_bot") {
            current_game.player2_client.emit("roundbegin", this.copyGameVals(current_game));
        }
    }
};


// One of the clients disconnected
// If this was unexpected, notify the other client and end this game
// If the game was already over, don't do anything
GameServer.prototype.clientDisconnect = function(client) {
    console.log("game.js:\t unexpected disconnect from client: ", client.userid);
    var current_game = this.getCurrentGame(client);
    // If there's still a game in progress, notify the other player
    if (current_game) {
        var this_player = this.getCurrentPlayer(current_game, client);
        var this_opponent = this.getOpponent(current_game, client);
        if (this_player.status != "exited") { // player has not yet completed all rounds
            if (this_opponent && this_opponent.status != "exited") {
                this.endGame(current_game.game_id, this_opponent);
            }
            this.writeData(current_game);
            delete this.active_games[current_game.game_id];
        }
    }
};


// Send game over signal to the client that matches rps_player in current_game (only if player is not a bot)
GameServer.prototype.endGame = function(game_id, rps_player) {
    current_game = this.active_games[game_id];
    if (current_game && rps_player && rps_player.client_id != "bot" && rps_player.client_id != "adaptive_bot") {
        console.log("game.js:\t sending gameover for client: ", rps_player.client_id);
        var player_socket = this.getClient(current_game, rps_player);
        player_socket.emit("gameover");
    }
};


// Write results of this game to json
// NB: conversion of this json to long format csv is handled by a separate python script outside this repo
GameServer.prototype.writeData = function(current_game) {
    var filename = __dirname + DATAPATH + "/";
    if (current_game.istest == true) {
        filename += "TEST_";
    }
    if (current_game.version == "2") {
        filename += "v2_";
    }
    if (current_game.version == "3") {
        filename += "v3_";
    }
    filename += current_game.game_id.toString() + ".json";
    // Make sure we have baseline data to write to file
    if (current_game.game_id && current_game.player1 && current_game.player2) {
        console.log("game.js:\t writing game results to file: ", filename);
        data_obj = {
            game_id: current_game.game_id,
            version: current_game.version,
            // SONA online study params
            sona: current_game.sona,
            experiment_id: current_game.experiment_id,
            credit_token: current_game.credit_token,
            survey_code: current_game.survey_code,
            player1_id: current_game.player1.client_id,
            player2_id: current_game.player2.client_id,
            rounds: []
        };
        // If this was an unpaired bot game, log additional info about the bot strategy
        if (current_game.version == "2" && current_game.player2.client_id == "bot") {
            data_obj["player2_botid"] = current_game.player2.uid;
            data_obj["player2_bot_strategy"] = current_game.player2.fixed_strategy;
            data_obj["player2_bot_move_probabilities"] = current_game.player2.fixed_strategy_transitions;
        }
        // If this was an unpaired *adaptive* bot game, log additional info about the bot strategy
        if (current_game.version == "3" && current_game.player2.client_id == "adaptive_bot") {
            data_obj["player2_botid"] = current_game.player2.uid;
            data_obj["player2_bot_strategy"] = current_game.player2.adaptive_strategy;
            data_obj["player2_final_memory"] = current_game.player2.memory_struct;
        }
        // Add data for each round
        for (round_idx in current_game.previous_rounds) {
            round = current_game.previous_rounds[round_idx];
            round_obj = {
                game_id: round.game_id, // this is the same as current_game.game_id,
                version: current_game.version, // this is redundant with above
                game_begin_ts: round.game_begin_ts, // this is the same as current_game.game_begin_ts
                round_index: round.round_index,
                player1_id: round.player1_id, // this is the same as current_game.player1.client_id
                player2_id: round.player2_id, // this is the same as current_game.player2.client_id
                round_begin_ts: round.round_begin_ts,
                player1_move: round.player1_move,
                player2_move: round.player2_move,
                player1_rt: round.player1_rt,
                player2_rt: round.player2_rt,
                player1_outcome: round.player1_outcome,
                player2_outcome: round.player2_outcome,
                player1_outcome_viewtime: round.player1_results_viewtime,
                player2_outcome_viewtime: round.player2_results_viewtime,
                player1_points: round.player1_points,
                player2_points: round.player2_points,
                player1_total: round.player1_points_total,
                player2_total: round.player2_points_total
            };
            if (current_game.version == "3" && current_game.player2.client_id == "adaptive_bot") {
                round_obj.player2_memory_struct = round.player2_memory_struct;
            }
            data_obj.rounds.push(round_obj);
        }
        data_str = JSON.stringify(data_obj, null, 2);
        console.log("game.js\t results string: ", data_str);
        fs.writeFile(filename, data_str, (err) => {
            if (err) throw err;
            console.log("game.js:\t game data successfully written to file.");
        });
    }
};

// Write free response exit survey results to json
GameServer.prototype.writeFreeRespData = function(data) {
    console.log("game.js:\t writing free response data.");
    if (data) {
        var filename = __dirname + DATAPATH + "/";
        filename += "freeResp_" + data.player_id.toString() + ".json";

        data_str = JSON.stringify(data, null, 2);
        console.log("game.js\t free response results string: ", data_str);
        fs.writeFile(filename, data_str, (err) => {
            if (err) throw err;
            console.log("game.js:\t free response data successfully written to file: ", filename);
        });
    }
};

// Write slider exit survey results to json
GameServer.prototype.writeSliderData = function(data) {
    if (data && data.length > 0) {
        console.log("game.js:\t writing slider data.");
        var filename = __dirname + DATAPATH + "/";
        filename += "sliderData_" + data[0].player_id.toString() + ".json";

        var data_obj = {
            game_id: data[0].game_id.toString(),
            player_id: data[0].player_id.toString(),
            slider_responses: data
        };
        data_str = JSON.stringify(data_obj, null, 2);
        console.log("game.js\t slider data string: ", data_str);
        fs.writeFile(filename, data_str, (err) => {
            if (err) throw err;
            console.log("game.js:\t slider data successfully written to file: ", filename);
        });
    }
};


// Declare game server and make it visible to node app
var game_server = new GameServer();
module.exports = game_server;
