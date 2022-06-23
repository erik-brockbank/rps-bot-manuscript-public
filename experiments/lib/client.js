/*
 * Core client-side functionality
 * Some of this is borrowed from https://github.com/hawkrobe/MWERT/blob/master/game.client.js
 */


// global for keeping track of the player object for this client
var THIS_PLAYER = {
    // Defaults for SONA online completion, the below are overriden if it's a SONA online study
    sona: 0,
    experiment_id: "",
    credit_token: "",
    survey_code: ""
};

// Start up: load consent page with callback to start instructions
$(window).ready(function() {
    $("body").load(HTML_LOOKUP["consent"], function() {
        $("#credit").text(STUDY_CREDITS);
        $("#duration").text(STUDY_DURATION);
        $("#consent-button").click(start_instructions);
    });
});



// Run through instructions with callback to begin socket.io connection for game play
start_instructions = function() {
    console.log("client.js:\t consent form agree");
    // get instructions and click through, then connect to rps game server to be paired with an opponent
    inst = new Instructions(HTML_LOOKUP["instructions"], INSTRUCTION_ARRAY, connect_to_server);
    inst.run();
};


// Callback after completion of instructions
connect_to_server = function() {
    var game = {};
    initialize_game(game); // sets game attributes, loads html
    // pass in relevant features on connection (this seems a bit hacky...)
    game.socket = io.connect("", {query: {istest: game.istest.toString(),
                                            version: game.version.toString(),
                                            sona: THIS_PLAYER.sona.toString(),
                                            experiment_id: THIS_PLAYER.experiment_id.toString(),
                                            credit_token: THIS_PLAYER.credit_token.toString(),
                                            survey_code: THIS_PLAYER.survey_code.toString()}});
    // Map out function bindings for various messages from the server
    game.socket.on("onconnected", client_onconnected.bind(THIS_PLAYER)); // message noting that we're connected
    game.socket.on("newgame", waitingroom_enter.bind(game)); // message that we're the first to join a new game
    game.socket.on("roundbegin", begin_round.bind(game)); // message that we're starting a new round (also joining an existing game)
    game.socket.on("roundwaiting_move", waiting_for_move); // message that we're waiting for the opponent to choose a move
    game.socket.on("roundwaiting_continue", waiting_next_round); // message that we're waiting for the opponent to continue to the next round
    game.socket.on("roundcomplete", display_results.bind(game)); // message that we've completed the current round
    game.socket.on("gameover", finish_game.bind(game)); // message that we've completed the game
};


// Initialization for game object: parses html, assigns attributes, and loads stub html for game
initialize_game = function(game) {
    // URL parsing
    // ex. http://localhost:3000/index.html?&mode=test&ver=2
    // Sample SONA link: http://lambdaserver.ucsd.edu/index.html?&ver=2&sona=1&survey_code=1859
    game.istest = false; // whether this is a test round (mostly used for debugging)
    game.version = 1; // whether this is a paired game (1) or against a bot (2): default = 1
    var urlParams = new URLSearchParams(window.location.href);
    if (urlParams.has("mode") && urlParams.get("mode").includes("test")) {
        game.istest = true;
    }
    if (urlParams.has("ver") && urlParams.get("ver").includes("2")) {
        game.version = 2;
    }
    if (urlParams.has("ver") && urlParams.get("ver").includes("3")) {
        game.version = 3;
    }
    if (urlParams.has("sona") && urlParams.get("sona").includes("1")) {
        // Note this should only occur in version 2 above
        THIS_PLAYER.sona = 1;
        THIS_PLAYER.experiment_id = EXPERIMENT_ID;
        THIS_PLAYER.credit_token = CREDIT_TOKEN;
        if (urlParams.has("survey_code")) {
            THIS_PLAYER.survey_code = urlParams.get("survey_code");
        }
    }
    console.log("client.js:\t initializing game. \n\tTEST: ", game.istest, "\n\tVERSION: ", game.version);
    console.log("client.js:\t Participant SONA info: \n\tSONA = ", THIS_PLAYER.sona,
                "; \n\texperiment id = ", THIS_PLAYER.experiment_id, "; \n\tcredit token = ", THIS_PLAYER.credit_token,
                "; \n\tsurvey code = ", THIS_PLAYER.survey_code);
    // Load game html
    $("body").load(HTML_LOOKUP["experiment"]);
};


// The server responded that we are now connected, this lets us store the information about ourselves
// We then load a page saying "waiting for server" while we're assigned to a game
client_onconnected = function(data) {
    this.client_id = data.id;
    display_message(SERVER_WAIT, hideall = true);
};


// The server told us we've been assigned to a game but are waiting for a partner.
// We update the state of our game to reflect that, and load a page saying "waiting for partner"
waitingroom_enter = function(data) {
    console.log("client.js:\t entered waiting room");
    this.game_id = data.game_id;
    THIS_PLAYER.game_id = data.game_id;
    display_message(OPPONENT_WAIT_JOIN, hideall = true);
};


// The server told us we can start the next round and sent along the current game state.
// Load a page showing all relevant game state and asking the user to choose a move, count down 10s
// data passed in here is a copy of the rps_game object
begin_round = function(data) {
    console.log("client.js:\t beginning round with game: ", data);
    if (data.current_round_index == 1) {
        console.log("client.js:\t first round of a new game");
        this.game_id = data.game_id;
        THIS_PLAYER.game_id = data.game_id;
    }

    if (THIS_PLAYER.client_id == data.player1.client_id) {
        client_total_points = data.player1_points_total;
        opponent_total_points = data.player2_points_total;
    } else if (THIS_PLAYER.client_id == data.player2.client_id) {
        client_total_points = data.player2_points_total;
        opponent_total_points = data.player1_points_total;
    }
    // Handle html transitions for beginning of round
    hide_points();
    hide_next_button();
    reset_move_elements();
    display_message(ROUND_BEGIN, hideall = false);
    initialize_banner_elements(data.current_round_index, data.game_rounds, client_total_points, opponent_total_points);

    // Add button interactivity and start move countdown
    that = this;
    start_countdown(ROUND_TIMEOUT, that);
};


// We've chosen a move, send it to the server and wait until we hear back
submit_move = function(move, rt, game) {
    console.log("client.js:\t move chosen: ", move);
    game.socket.emit("player_move", {"move": move, "rt": rt});
    display_message(SERVER_WAIT, hideall = false);
};


// The server told us we're waiting for our opponent's move.
// Display the "Waiting for opponent" page
waiting_for_move = function() {
    console.log("client.js:\t waiting for opponent to select a move");
    display_message(OPPONENT_WAIT_MOVE, hideall = false);
};


// We've heard back from the server that the round is complete: display the results
// data passed in here is a copy of the rps_game object
display_results = function(data) {
    // Determine outcome for this client based on round results
    console.log("client.js:\t displaying results of round with game: ", data);
    if (THIS_PLAYER.client_id == data.player1.client_id) {
        client_move = data.current_round.player1_move;
        opponent_move = data.current_round.player2_move;
        client_outcome = data.current_round.player1_outcome;
        client_round_points = data.current_round.player1_points;
        client_total_points = data.player1_points_total;
        opponent_round_points = data.current_round.player2_points;
        opponent_total_points = data.player2_points_total;
    } else if (THIS_PLAYER.client_id == data.player2.client_id) {
        client_move = data.current_round.player2_move;
        opponent_move = data.current_round.player1_move;
        client_outcome = data.current_round.player2_outcome;
        client_round_points = data.current_round.player2_points;
        client_total_points = data.player2_points_total;
        opponent_round_points = data.current_round.player1_points;
        opponent_total_points = data.player1_points_total;
    }
    if (client_outcome == "win") {
        outcome_text = "You won this round!";
    } else if (client_outcome == "loss") {
        outcome_text = "You lost this round.";
    } else if (client_outcome == "tie") {
        outcome_text = "This round was a tie.";
    }

    // Display html elements relevant to the results
    clear_countdown();
    display_message(outcome_text, hideall = false);
    display_points_update(client_total_points, opponent_total_points, client_round_points, opponent_round_points);
    highlight_opponent_move();
    // Track when participant is first shown results (for later analysis of how long participants viewed results)
    this.round_results_start_ts = new Date().getTime();
    show_next_button(finish_round.bind(this));
};


// Tell server that we're ready for the next round and wait to hear back
finish_round = function() {
    console.log("client.js:\t ready for next round.");
    var round_results_viewtime = new Date().getTime() - this.round_results_start_ts;
    this.socket.emit("player_round_complete", {"round_viewtime": round_results_viewtime});
    display_message(SERVER_WAIT, hideall = false);
};


// The server told us we're waiting for our opponent to continue to next round
// Display the "Waiting for opponent" page
waiting_next_round = function() {
    console.log("client.js:\t waiting for opponent to continue");
    hide_next_button();
    display_message(OPPONENT_WAIT_CONTINUE, hideall = false);
};


// Received message from server that game is complete (or opponent left unexpectedly):
// take participant to end of game survey page
finish_game = function() {
    console.log("client.js:\t received game over.");
    hide_game_elems();
    display_message(ENDGAME_MESSAGE, hideall = true);
    show_next_button(begin_survey.bind(this));
};


// Wrapper function for beginning post-game survey
// Administers free-response component with callback to execute Likert scale slider questions
begin_survey = function() {
    console.log("client.js:\t beginning exit survey.");
    survey_administer_free_resp(callback = survey_administer_sliders, game = this);
};


// Display and process free response question
// NB: currently only handles one question but can be easily extended for multiple like the slider function
survey_administer_free_resp = function(callback, game) {
    console.log("client.js:\t beginning exit survey free response.");
    $("#experiment-container").load(HTML_LOOKUP["survey_free_resp"], function() {
        hide_next_button();
        display_survey_message(SURVEY_FREE_RESPONSE);
        // Display "Continue" button with callback function to be executed on click
        show_next_button(function() {
            var resp = $("#input-response").val();
            if (resp === "") {
                alert("Please respond to the prompt!");
            } else {
                var free_resp_data = {
                    game_id: THIS_PLAYER.game_id,
                    player_id: THIS_PLAYER.client_id,
                    free_resp_prompt: SURVEY_FREE_RESPONSE,
                    free_resp_answer: resp
                };
                game.socket.emit("free_response_submit", free_resp_data);
                callback(game);
            }
        });
    });
};


// Wrapper function for recursively administering slider Likert scale questions
survey_administer_sliders = function(game) {
    console.log("client.js:\t beginning exit survey sliders.");
    $("#experiment-container").load(HTML_LOOKUP["survey_slider"], function() {
        hide_next_button();
        display_survey_message(SURVEY_SLIDER_MESSAGE);
        survey_slider_questions(game, slider_index = 0, slider_data = [], callback = end_experiment);
    });
};


// Recursive function for administering slider Likert scale questions and pushing the data to server when complete
survey_slider_questions = function(game, slider_index, slider_data, callback) {
    display_slider_message(SURVEY_ARRAY[slider_index]);
    // Display "Continue" button with callback function to be executed on click
    show_next_button(function() {
        slider_data.push({
            game_id: THIS_PLAYER.game_id,
            player_id: THIS_PLAYER.client_id,
            statement: SURVEY_ARRAY[slider_index],
            index: slider_index,
            resp: parseInt($("#eval-slider").val())
        });
        slider_index += 1;
        if (slider_index >= SURVEY_ARRAY.length) {
            game.socket.emit("slider_submit", slider_data);
            callback();
        } else {
            survey_slider_questions(game, slider_index, slider_data, callback);
        }
    });
};


// Replace all experiment html with end-of-experiment header message
end_experiment = function() {
    console.log("client.js:\t exit survey complete.");
    $("#experiment-container").html("<h2>All done. Thank you for playing!</h2>");
    post_completion();
};


// Utility function for handling SONA credit form posts
addHidden = function(id, value) {
	var input = document.createElement("input");
	input.setAttribute("type", "hidden");
	input.setAttribute("name", id);
	input.setAttribute("value", value);
	return(input);
};


// Send SONA completion
post_completion = function() {
    // URL for this experiment:
    // https://ucsd.sona-systems.com/webstudy_credit.aspx?experiment_id=1768&credit_token=19421bc286424246b6b1e873e7a55a8e&survey_code=XXXX
    if (THIS_PLAYER.sona) {
        var form = document.createElement('form')
        form.method = 'GET';
        form.action = 'https://ucsd.sona-systems.com/webstudy_credit.aspx';
        form.appendChild(addHidden('experiment_id', THIS_PLAYER.experiment_id));
        form.appendChild(addHidden('credit_token', THIS_PLAYER.credit_token));
        form.appendChild(addHidden('survey_code', THIS_PLAYER.survey_code));
        document.body.appendChild(form);
        console.log("Attempting completion post to SONA: ", form);
        form.submit();
    }
};
