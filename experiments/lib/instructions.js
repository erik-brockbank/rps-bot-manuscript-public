/*
 * Library for advancing through instructions
 */

Instructions = function(instructionPath, instructionSet, experimentCallback) {
    this.instructionPath = instructionPath; // Path to html page for instructions skeleton
    this.instructionSet = instructionSet; // Array of instruction text elements to display
    this.instructionsIndex = 0; // Index to keep track of how many instructions have been processed
    this.callback = experimentCallback; // Function to call when instructions complete
};


/*
 * Primary function for running instructions, called when new Instructions obj. is instantiated
 */
Instructions.prototype.run = function() {
    console.log("inst.js:\t starting instructions");
    // Load html for displaying instructions
    var that = this;
    $("body").load(that.instructionPath, function() {
        that.populateInstruction();
    });
};

/*
 * Function called by button click, used for moving through instruction flow
 */
Instructions.prototype.buttonNext = function() {
    if (this.instructionsIndex >= this.instructionSet.length) {
        console.log("inst.js:\t instructions complete");
        this.callback();
    } else {
        $("#next-inst").unbind("click");
        this.populateInstruction();
    }
};

/*
 * Function to populate instruction html elements with appropriate text/images
 * during each phase of instructions
*/
Instructions.prototype.populateInstruction = function() {
    instructionElem = this.instructionSet[this.instructionsIndex];
    // Remove any existing images in the canvas then add in necessary image
    $(".instruction-img").remove();
    if (instructionElem.canvas_img != "") {
        img_src = instructionElem.canvas_img;
        $("#canvas-mid").prepend("<img class='instruction-img' src='" + img_src + "'/>");
        $(".instruction-img").width($("#canvas-mid").width());
    }
    // Add text to top and bottom segments
    $("#text-top").html(instructionElem.top_text);
    $("#text-bottom").html(instructionElem.bottom_text);

    this.instructionsIndex++;
    // re-activate next button once the above has completed
    // (note this may execute before the above all terminate but unlikely to be an issue)
    var that = this;
    $("#next-inst").click(function() {that.buttonNext();});
};
