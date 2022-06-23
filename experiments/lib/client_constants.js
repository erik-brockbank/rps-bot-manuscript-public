/*
 * constants library for rps client (this gets loaded in the browser)
 */
const STUDY_CREDITS = 0.5;
const STUDY_DURATION = 30;
// const EXPERIMENT_ID = 1768; // extracted from SONA for posting SONA credit (20EV07)
const EXPERIMENT_ID = 1996; // extracted from SONA for posting SONA credit (21EV01)
// const CREDIT_TOKEN = "19421bc286424246b6b1e873e7a55a8e"; // extracted from SONA for posting SONA credit (20EV07)
const CREDIT_TOKEN = "2a73382e8c16449f86f5b46dc02007e9"; // extracted from SONA for posting SONA credit (21EV01)

const HTML_LOOKUP = { // lookup table with human-understandable html file keys and the path to those files as vals
    "consent": "/static/consent.html",
    "instructions": "/static/instructions.html",
    "experiment": "/static/round_template.html",
    "survey_free_resp": "/static/survey_free_resp.html",
    "survey_slider": "/static/survey_slider.html"
};

const ROUND_TIMEOUT = 10; // number of seconds for players to make a decision each round (includes some buffer for loading)

// Messages for client display
const SERVER_WAIT = "Waiting for server..."; // message to display when waiting for server
const OPPONENT_WAIT_JOIN = "Waiting for an opponent to join..."; // message to display when waiting for opponent to join
const OPPONENT_WAIT_MOVE = "Waiting for opponent to select a move..."; // message to display when waiting for opponent's move
const OPPONENT_WAIT_CONTINUE = "Waiting for opponent to continue..."; // message to display when waiting for opponent to continue
const ROUND_BEGIN = "Choose a move!"; // message to display when beginning a round
const ENDGAME_MESSAGE = "Game over. Please click the button below to answer a few questions about your experience."; // message to display when game is over

// Array of instruction elements (with embedded html formatting) used to present instructions in instructions.js
const INSTRUCTION_ARRAY = [
    {
        // ORIGINAL TEXT
        // top_text: "<p>In today’s experiment, you’ll be playing the Rock, Paper, Scissors game " +
        //     "against another human player.</p>",
        // V2 TEXT
        top_text: "<p>In today’s experiment, you’ll be playing the Rock, Paper, Scissors game.</p>",
        canvas_img: "",
        bottom_text: "<p>Please do not refresh the browser at any point during the experiment or " +
            "the task will restart. Please complete the task in one sitting.</p>"
    },
    {
        top_text: "<p>If you’re unfamiliar with Rock, Paper, Scissors, here’s how to play:</p>",
        canvas_img: "",
        bottom_text: ""
    },
    {
        top_text: "<p>In each round, you will select one of the rock, paper, or scissors " +
            "cards to play against your opponent by clicking the appropriate card. They look " +
            "like the icons below.</p>",
        canvas_img: "../img/combined-standard.jpg",
        bottom_text: ""
    },
    {
        top_text: "<p>Your opponent is going to choose a card to play as well, but neither of " +
            "you can see what the other has selected until after you have both chosen.</p>",
        canvas_img: "",
        bottom_text: ""
    },
    {
        top_text: "<p>Once both you and your opponent have selected a card to play in the " +
            "current round, your chosen card and your opponent’s card will both be revealed.</p>",
        canvas_img: "",
        bottom_text: ""
    },
    {
        top_text: "<p>In each round, the rules for which card wins are simple:</p>" +
            "<p>- <b><i>Rock beats scissors</i></b> (to remember, imagine the rock breaking the scissors).</p>" +
            "<p>- <b><i>Scissors beats paper</i></b> (to remember, imagine the scissors cutting the paper).</p>" +
            "<p>- <b><i>Paper beats rock</i></b> (to remember, imagine the paper wrapping around the rock).</p>" +
            "<p>- If both players play the same card, the round is a tie.</p>",
        canvas_img: "",
        bottom_text: ""
    },
    {
        top_text: "<p>The rules for each card combination are illustrated below and will be " +
            "shown throughout the game as a reminder.</p>",
        canvas_img: "../img/schematic.jpg",
        bottom_text: ""
    },
    {
        top_text: "<p>In each round, the winner will receive 3 points, the loser will receive -1 " +
            "point, and when there’s a tie, both players will receive 0 points.</p>" +
            "<p>Your points and your opponent’s points will be visible throughout the game to " +
            "see who is winning.</p>",
        canvas_img: "",
        bottom_text: ""
    },
    {
        top_text: "<p>You’ll have 10 seconds to choose a card in each round. If you don’t choose " +
            "a card within the 10 seconds, your opponent will automatically win that round.</p>" +
            "<p>You and your opponent are going to play 300 rounds of the rock, paper, scissors game.</p>",
        canvas_img: "",
        bottom_text: ""
    },
    {
        top_text: "<p>Ready? Click the button below to get started!</p>",
        canvas_img: "",
        bottom_text: ""
    }
];


// Exit survey messages
const SURVEY_SLIDER_MESSAGE = "On the slider below, please indicate how much you agree with the following statement:";
const SURVEY_FREE_RESPONSE = "In the text box below, please describe any strategies you used to try and beat your opponent.";

// Exit survey slider questions
const SURVEY_ARRAY = [
    "My opponent was a real person and not a robot.",
    "I was trying to win each round against my opponent.",
    "I was focused on winning for the entire time I was playing.",
    "I paid attention to my opponent’s moves in order to try and predict their next move.",
    "There were noticeable patterns in my opponent’s moves that allowed me to predict their next move."
];
