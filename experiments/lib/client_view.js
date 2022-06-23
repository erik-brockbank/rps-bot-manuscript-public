/*
 * Client functions for interacting with html
 * These functions are pulled out of client.js for ease of interaction
 */


// Function to initialize banner (points, time remaining) at beginning of round
initialize_banner_elements = function(current_round_index, game_rounds, client_total_points, opponent_total_points) {
    $("#round-index").text(current_round_index + "/" + game_rounds);
    $("#client-points-total").text("Total: " + client_total_points);
    $("#opponent-points-total").text("Total: " + opponent_total_points);
    $("#client-points-update").height($("#client-points-total").height()); // Hacky, avoids empty div re-sizing
    $("#opponent-points-update").height($("#opponent-points-total").height()); // Hacky, avoids empty div re-sizing
    $("#game-container").css({visibility: "visible"});
};


// General function to display a message in the message container
display_message = function(msg, hideall) {
    if (hideall) {
        hide_points();
        hide_next_button();
        $("#game-container").css({visibility: "hidden"});
    }
    $("#message-container").text(msg);
};


// General function to display a message during the post-game survey component
display_survey_message = function(msg) {
    $("#survey-banner").text(msg);
};


// Function for displaying individual Likert scale slider questions
display_slider_message = function(msg) {
    $("#survey-eval").text(msg);
};


 // Function to hide point elements that we need to toggle visibility on and off for throughout the experiment
hide_points = function() {
    $("#client-points-update").css({visibility: "hidden"});
    $("#opponent-points-update").css({visibility: "hidden"});
};


// Function to show point elements that we need to toggle visibility on and off for throughout the experiment
show_points = function() {
    $("#client-points-update").css({visibility: "visible"});
    $("#opponent-points-update").css({visibility: "visible"});
};


// Function to display points at end of round
display_points_update = function(client_total_points, opponent_total_points, client_points, opponent_points) {
    $("#client-points-total").text("Total: " + client_total_points);
    $("#opponent-points-total").text("Total: " + opponent_total_points);
    if (client_points >= 0) {client_points = "+" + client_points;}
    if (opponent_points >= 0) {opponent_points = "+" + opponent_points;}
    $("#client-points-update").text(client_points);
    $("#opponent-points-update").text(opponent_points);
    show_points();
};


// Function to hide next button that we need to toggle visibility on and off for throughout the experiment
hide_next_button = function() {
    $("#exp-button-container").css({visibility: "hidden"});
};


// Function to show next button that we need to toggle visibility on and off for throughout the experiment
show_next_button = function(callback) {
    $("#exp-button-container").css({visibility: "visible"});
    $(".next-button").unbind().click(callback);
};


// Function to reset color decoration on move choice elements
reset_move_elements = function() {
    $(".move-button-container").css({border: "5px solid transparent"});
    $(".opponent-move").css({border: "5px solid transparent"});
    $(".opponent-move-img").css({opacity: "0.4"});
};


// Function to display opponent's move
highlight_opponent_move = function() {
    $(".opponent-move-img").css({opacity: "1.0"});
    if (opponent_move == "none") {
        $("#opponent-move-rock").css({border: "5px solid gray"});
        $("#opponent-move-paper").css({border: "5px solid gray"});
        $("#opponent-move-scissors").css({border: "5px solid gray"});
    } else {
        $("#opponent-move-" + opponent_move).css({border: "5px solid #008CBA"});
    }
};


// Function to add click response to move buttons and start countdown for selecting a move
start_countdown = function(timeout, game) {
    var move_clicked = false;
    // click response function for move buttons
    $(".move-button-container").click(function() {
        move_clicked = true;
        $(this).css({border:"5px solid #4CAF50"});
        $("#client-move-rock").unbind("click");
        $("#client-move-paper").unbind("click");
        $("#client-move-scissors").unbind("click");
        resp_time = new Date().getTime() - start_time_ms; // ms since start_time_ms
        move_chosen = $(this).attr("id").split("-")[2];
        submit_move(move_chosen, resp_time, game);
    });

    // Start the countdown clock for selecting a move
    start_time = new Date(); // timer for ROUND_TIMEOUT countdown (rounds to nearest second)
    start_time_ms = start_time.getTime(); // more accurate timer for participant responses
    end_time = Date.parse(start_time) + (1000 * timeout); // number of seconds for each round * 1000 since timestamp includes ms
    remaining = (end_time - (Date.parse(new Date()))) / 1000; // calculate seconds remaining (rounds to nearest second)
    $("#countdown").text(remaining);
    turn_countdown = setInterval(function() {
        remaining = (end_time - (Date.parse(new Date()))) / 1000; // calculate seconds remaining
        $("#countdown").text(remaining);
        if (remaining <= 0) {
            clear_countdown();
            if (!move_clicked) {
                $("#client-move-rock").unbind("click");
                $("#client-move-paper").unbind("click");
                $("#client-move-scissors").unbind("click");
                resp_time = timeout * 1000 // max response time (ms)
                submit_move("none", resp_time, game);
            }
        }
    }, 1000);
    $("#time-info").css({visibility: "visible"});
};


// Function to clear the countdown timer and hide the countdown
clear_countdown = function() {
    clearInterval(turn_countdown);
    $("#time-info").css({visibility: "hidden"});
};


// Function to hide all game-related elements before replacing them with post-game survey
hide_game_elems = function() {
    // hide_points();
    hide_next_button();
    clear_countdown();
    $("#game-banner").hide();
    $("#client-info").hide();
    $("#opponent-info").hide();
    $("#game-info").hide();
};
