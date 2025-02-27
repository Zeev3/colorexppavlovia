/**
Functions in Sequential Task


**/



function getRandomInt(min, max) {
  return Math.floor(Math.random() * (max - min + 1) + min);
}

function getRandomElement (list){
  return list[Math.floor(Math.random()*list.length)];
}

function getRandom(arr, n) {
  var result = new Array(n),
      len = arr.length,
      taken = new Array(len);
  if (n > len)
      throw new RangeError("getRandom: more elements taken than available");
  while (n--) {
      var x = Math.floor(Math.random() * len);
      result[n] = arr[x in taken ? taken[x] : x];
      taken[x] = --len in taken ? taken[len] : len;
  }
  return result;
}

function shuffle(array) {
  var currentIndex = array.length, temporaryValue, randomIndex;

  // While there remain elements to shuffle...
  while (0 !== currentIndex) {

    // Pick a remaining element...
    randomIndex = Math.floor(Math.random() * currentIndex);
    currentIndex -= 1;

    // And swap it with the current element.
    temporaryValue = array[currentIndex];
    array[currentIndex] = array[randomIndex];
    array[randomIndex] = temporaryValue;
  }

  return array;
}


function loadccolorPool(start,end) { //the start and ending index of the images
  var pool = [];
  for(i = start; i < (end+1); i++){
     pool.push( 'img/red' + i + '.jpg'); pool.push( 'img/blue' + i + '.jpg');
    }
  return pool;
}


function check_consent (){
  if ($('#consent_checkbox').is(':checked')) {
    return true;
  }else {
    alert("If you wish to participate, you must check the box next to the statement 'I agree to participate in this study.'");
    return false;}
}

/**
 * IMPROVED DATA SAVING FUNCTION
 * This function saves all jsPsych data with the participant ID in the filename
 * It should be called only once at the end of the experiment
 */
function saveDataWithID() {
  // Get the participant ID (if available, otherwise use timestamp)
  var id = (typeof Face.subject_id !== 'undefined' && Face.subject_id) ? 
            Face.subject_id : 
            'participant_' + new Date().toISOString().replace(/[-:.]/g, "_");
  
  // Get all data
  var csv = jsPsych.data.get().csv();
  
  // Create filename with ID
  var filename = id + "_data.csv";
  
  // Create and trigger download
  var blob = new Blob([csv], { type: "text/csv" });
  var url = URL.createObjectURL(blob);
  var a = document.createElement("a");
  a.href = url;
  a.download = filename;
  document.body.appendChild(a);
  a.click();
  document.body.removeChild(a);
  URL.revokeObjectURL(url);
  
  console.log("Data saved as: " + filename);
}





function getNextSlide () {  //use to shift instruction slides
  var currentSlide = slideList.shift();
  return currentSlide;
}

function loadStimulus(end) { //the start and ending index of the images
  var list = [];
  for(i = 1; i < (end+1); i++){
    list.push( 'stimulus/' + i + '.jpg');}
  return list;
}

function checkAnswer (){
  var inputText = jsPsych.data.getLastTrialData().select('responses').values[0];
  var text = JSON.parse(inputText).Q0;
  var patt = new RegExp("[A-Za-z0-9 _.,!'/$]"); // this allows punctuations
  if (!patt.test(inputText  )){      //test if first/last character in response exist
    alert("Please describe the image just showed in a few words (this will be uses for validation purposes)");
    return true; }
  else{ return false;}
}

function getWord (){ //get a word for attention check from the word list
    Face.word = Face.wordList.shift();
    return Face.word;
}

function checkTyping(){  //test if type correctly
  var inputText = jsPsych.data.getLastTrialData().select('responses').values[0];
  var text = JSON.parse(inputText).Q0;
  var falseAllowance = 5;
  if (Face.word !== text){
    falseAnswer += 1;
    alert("Attention! Please type the word correctly. If the alert shows up for 4 times, the experiment will be terminated.");
    Face.wordList.unshift(Face.word);
    if (falseAnswer == falseAllowance){ //if participant gets alert this number of times
      alert("Hi! You've made too much errors in typing the word suggesting that you are not paying attention to the task. If you attempt to further proceed on the task, you will not be compensated.");
      window.close();
    }else{return true;} }
  else {falseAnswer = 0; return false;} //reset falseAnswer
}

function getTimeAndFace (){  //get randomized time of fixation by randomly choosing from 0.4, 0.5 and 0.6s
  Face.fixationTime = getRandomElement([400, 500, 600]);

    //choose face_itive or negative valence before displaying faces
    Face.emotionX = getRandomElement([25,50]); //1 is smallest 
    //choose the identity of the face
    

    return Face.fixationTime;
}

function make_stim(stim_array,face){
  var html_stim_array = []
  console.log(stim_array);
  for (p of stim_array){
    var random_margin = String(getRandomInt(20,50)); //jitter the margin
    html_stim_array.push("<div class='"+face+"' style='background-position:" + (p*100) + "% 0%;margin-top:"+random_margin+"px'></div>")
  }
  for (i = 0; i < 26 - html_stim_array.length; i++){
    html_stim_array.push("<div> </div>")
  }
  shuffle(html_stim_array);
  var xx = html_stim_array.join(" ")
  // return(xx)
  return ("<span class='grid'>"+xx+"</span>")
}

function getFaceSample(array_length, meanVal, personX) {
  console.log("Creating stimulus: length=" + array_length + ", mean=" + meanVal + ", color=" + personX);
  
  // Store these values in the Face object so they're accessible later
  Face.array_length = array_length;
  Face.meanVal = meanVal;
  Face.personX = personX;
  
  // Define the fixed arrays
  const arraySix = [-5, -3, -1, 1, 3, 5];
  const arrayTwo = [-3, 3];
  const arrayEight = [-6, -4, -3, -1, 1, 3, 4, 6];
  const arrayTwelve = [-7,-4,-6,-2,0,-1,1,1,2,4,5,7];

  // Choose array based on array_length and add meanVal to each element
  let emotion_value;
  if (array_length === 2){
     emotion_value = arrayTwo.map(x => x + meanVal);}
  else if (array_length === 6) {
    emotion_value = arraySix.map(x => x + meanVal);
  }
  else if (array_length === 8){
    emotion_value = arrayEight.map(x => x + meanVal);
  }
  else if (array_length === 12) {
    emotion_value = arrayTwelve.map(x => x + meanVal);
  }
  
  // Store the calculated array values
  Face.array_values = emotion_value;
  
  return make_stim(emotion_value, personX);
}


function getFaceSamplelearning(personX) {
  console.log("Creating learning stimulus for color: " + personX);
  
  // Store personX in Face object
  Face.personX = personX;
  
  var emotion_value;
  if (getRandomInt(1,2) == 1) {  
    emotion_value = [...Array(15).keys()].map(function(item) { 
      return item + 5; 
    });
    Face.array_length = getRandomInt(2,12);
    Face.array_values = getRandom(emotion_value, Face.array_length);
  } else {
    emotion_value = [...Array(15).keys()].map(function(item) {
      return item + 34;
    });
    Face.array_length = getRandomInt(2,12);
    Face.array_values = getRandom(emotion_value, Face.array_length);
  }
  
  // Calculate mean value
  Face.meanVal = Face.array_values.reduce((a, b) => a + b, 0) / Face.array_values.length;
  
  return make_stim_learning(Face.array_values, personX);
}

function make_stim_learning(stim_array,face){
  var html_stim_array = []
  for (p of stim_array){
    var random_margin = String(getRandomInt(20,50)); //jitter the margin
    html_stim_array.push("<div class='"+face+"' style='background-position:" + (p*100) + "% 0%;margin-top:"+random_margin+"px'></div>")
  }
  for (i = 0; i < 26 - html_stim_array.length; i++){
    html_stim_array.push("<div> </div>")
  }
  shuffle(html_stim_array);
  var xx = html_stim_array.join(" ")
  // return(xx)
  return ("<span class='grid'>"+xx+"</span>")
}
  function getScaleBlue (){ //generate the rating scale depending on the person and valence randomly chosen in faceArray
    var scale = [];
      Face.personX = "blue";
      for(i = 1; i < 51; i++){
        scale.push('img/'+Face.personX+(i) + '.jpg')}
    return scale;
  }

  // Modified morphedScaleBlue function with visual range indicator
function morphedScaleBlue() {
  // defining a few helper functions
  function nrange(size, startAt = 0) {
    return [...Array(size).keys()].map(i => i + startAt);
  };

  function fillArray(value, len) {
    var arr = [];
    for (var i = 0; i < len; i++) {
      arr.push(value);
    }
    return arr;
  };

  function generateSlices(vWidth, nSlices) {
    var step = vWidth*0.6/(nSlices-2);
    var stepArray = fillArray(step,nSlices-2)
    stepArray.unshift(0.2*vWidth)
    stepArray.push(0.2*vWidth)

    var bounds = [];
    
    for (var i = 0; i < nSlices; i++) {
      if (i==0) {
        bounds.push([(i*stepArray[i]), (i*stepArray[i])+stepArray[i]])
      } else if (i>0 && i!=nSlices-1) {
        bounds.push([(i*stepArray[i]+0.2*vWidth), (i*stepArray[i])+stepArray[i]+0.2*vWidth])
      } else {
        bounds.push([(vWidth-0.2*vWidth), vWidth])  
      }
    };
    return bounds;
  };
  
  // start trial timer
  var startTime = (new Date()).getTime();
  // get trial data
  var trialData = jsPsych.currentTrial();

  // remove the picture scales and the slider
  $('.jspsych-image-slider-response_noButton-container').css("visibility", "hidden");
  $('img').remove();
  var imgScale = getScaleBlue();
  
  // derive the letter of the image filenames
  var imgBase = imgScale[0].split('img/')[1].split('.jpg')[0].replace(/\d+/g, '');
  
  // split the number of the image filenames off from the rest of it
  var nScale = [];
  for (var i = 0; i < imgScale.length-1; i++) {
    var n = imgScale[i].split('img/')[1].split('.jpg')[0].split(imgBase)[1];
    nScale.push(n);
  };
  
  // calculate the element width, and slice it up into sections
  var vWidth = $(document).width();
  var nSlices = nScale.length;
  var slices = generateSlices(vWidth, nSlices);

  // setting up initial vertical line to start the mousemove functionality
  var vHeight = $(document).height()-8;
  var lineSlice = vWidth / 10;
  var vertLine = `<div style="border-left:black;border-style:solid;margin-left:${lineSlice}px; height:${vHeight}px;width:0px;position:absolute;" id="vertLine"></div>`;
  var linePrompt = `<div id="linePrompt"><div style="font-size:50px;position:absolute;margin-left:${lineSlice*1.3}px;margin-top:${vHeight/2}px"></div><div style="position:absolute;margin-left:${lineSlice*1.2}px;margin-top:${vHeight/2}px;z-index:5;">Move mouse left of the line to begin</div></div>`;
  $(".jspsych-content-wrapper").prepend(vertLine);
  $(".jspsych-content-wrapper").prepend(linePrompt);
  
  // hide prompt until the trial is begun
  $('#jspsych-content > p').css("visibility", "hidden");

  // initialize the central image with the most neutral one (i.e. from
  // the middle of the scale) and set the image to be blurred
  var initialImage = imgScale[0];
  $('#jspsych-image-slider-response_noButton-stimulus').append(`<img src="${initialImage}" style="filter:blur(4px);visibility:hidden;" id="changeable-image"/>`);

  // ADD VISUAL SCALE SHOWING MIN/MAX
  var scaleWidth = vWidth * 0.6;
  var scaleLeft = vWidth * 0.2;
  var scaleHtml = `
    <div id="scale-container" style="position:absolute; bottom:120px; left:${scaleLeft}px; width:${scaleWidth}px; height:30px; background-color:rgba(220,220,220,0.7); border-radius:5px; display:none; z-index:10;">
      <div style="position:absolute; left:0px; width:2px; height:30px; background-color:black;"></div>
      <div style="position:absolute; left:0px; top:32px; font-size:12px; color:black;">Min (1)</div>
      <div style="position:absolute; right:0px; width:2px; height:30px; background-color:black;"></div>
      <div style="position:absolute; right:0px; top:32px; font-size:12px; color:black;">Max (50)</div>
      <div id="current-marker" style="position:absolute; left:0px; width:2px; height:30px; background-color:blue; display:none;"></div>
    </div>
  `;
  $(".jspsych-content-wrapper").append(scaleHtml);

  // workaround with a global variable
  window.__imageMouseOver = {
    lineSlice: lineSlice,
    slices: slices,
    nScale: nScale,
    imgBase: imgBase,
    startTime: startTime,
    trialData: trialData,
    sliceSelected: 0,
  };
  var __listenerBools = {};

  // define mousemove event listener that changes image
  function changeImg(event) {
    var mouseX = Math.floor(event.pageX);
    
    // Show the scale after interaction begins
    $("#scale-container").css("display", "block");
    
    // Update current marker position based on mouse position
    var scaleWidth = $("#scale-container").width();
    var relativePos = Math.max(0, Math.min(mouseX - scaleLeft, scaleWidth));
    var percentPos = (relativePos / scaleWidth) * 100;
    
    $("#current-marker").css({
      "left": `${relativePos}px`,
      "display": "block"
    });
    
    for (var i = 0; i < __imageMouseOver.slices.length; i++) {
      // if mouse X position is within the bounds of the X axis slice,
      // change the image to one that is indexed to that slice
      if (mouseX >= __imageMouseOver.slices[i][0] && mouseX <= __imageMouseOver.slices[i][1]) {
        // capture which slice is selected
        __imageMouseOver.sliceSelected = i;
        // update img src to the picture that corresponds to that slice
        $('#changeable-image').attr('src', `img/${__imageMouseOver.imgBase}${__imageMouseOver.nScale[i]}.jpg`);
      }
    };
  };
  
  // define the click listener that ends trial
  function clickHandler(event) {
    if (__listenerBools['mousemove']) {
      // derive trial data
      var trialData = __imageMouseOver.trialData;
      var end_time = (new Date()).getTime();
      var rt = end_time - __imageMouseOver.startTime;
      trialData['rt'] = rt;
      trialData['stimulus_duration'] = rt;
      trialData['trial_duration'] = rt;
      trialData['imageSelected'] = `${__imageMouseOver.imgBase}${__imageMouseOver.nScale[__imageMouseOver.sliceSelected]}.jpg`
      trialData['indexSelected'] = __imageMouseOver.sliceSelected;
      // turn off listeners
      $(document).off('mousemove');
      $(document).off('click');
      // clean up variable namespaces
      delete window.__imageMouseOver
      delete __listenerBools;
      // clean up scale
      $("#scale-container").remove();
      // finish the trial with trial data
      jsPsych.finishTrial(trialData);
    };
  };

  function verticalLineInit(event) {
    var mouseX = Math.floor(event.pageX);
    if (mouseX <= __imageMouseOver.lineSlice) {
      $("#vertLine").remove();
      $("#linePrompt").remove();
      $("#jspsych-image-slider-response_noButton-stimulus > img").css({
        "filter":"blur(0px)",
        "visibility": "visible",
      });
      $('#jspsych-content > p').css("visibility", "visible");
      $('.jspsych-image-slider-response_noButton-').css("visibility", "visible");
      
      __listenerBools['mousemove'] = true;
      // turn off THIS mouse move listener
      $(document).off("mousemove");
      // turn on the mouse move listener that changes the image
      $(document).mousemove(changeImg);
      // add mouse click listener
      $(document).on('click', clickHandler);
    };
  };

  // turn on the vertical line mouse move listener
  $(document).mousemove(verticalLineInit);
}

  function getScaleRed (){ //generate the rating scale depending on the person and valence randomly chosen in faceArray
    var scale = [];
      Face.personX = "red";
      for(i = 1; i < 51; i++){
        scale.push('img/'+Face.personX+(i) + '.jpg')}
    return scale;
  }

  function morphedScaleRed() {
    // defining a few helper functions
    function nrange(size, startAt = 0) {
      return [...Array(size).keys()].map(i => i + startAt);
    };
  
    function fillArray(value, len) {
      var arr = [];
      for (var i = 0; i < len; i++) {
        arr.push(value);
      }
      return arr;
    };
  
    function generateSlices(vWidth, nSlices) {
      var step = vWidth*0.6/(nSlices-2);
      var stepArray = fillArray(step,nSlices-2)
      stepArray.unshift(0.2*vWidth)
      stepArray.push(0.2*vWidth)
  
      var bounds = [];
      
      for (var i = 0; i < nSlices; i++) {
        if (i==0) {
          bounds.push([(i*stepArray[i]), (i*stepArray[i])+stepArray[i]])
        } else if (i>0 && i!=nSlices-1) {
          bounds.push([(i*stepArray[i]+0.2*vWidth), (i*stepArray[i])+stepArray[i]+0.2*vWidth])
        } else {
          bounds.push([(vWidth-0.2*vWidth), vWidth])  
        }
      };
      return bounds;
    };
    
    // start trial timer
    var startTime = (new Date()).getTime();
    // get trial data
    var trialData = jsPsych.currentTrial();
  
    // remove the picture scales and the slider
    $('.jspsych-image-slider-response_noButton-container').css("visibility", "hidden");
    $('img').remove();
    var imgScale = getScaleRed();
    
    // derive the letter of the image filenames
    var imgBase = imgScale[0].split('img/')[1].split('.jpg')[0].replace(/\d+/g, '');
    
    // split the number of the image filenames off from the rest of it
    var nScale = [];
    for (var i = 0; i < imgScale.length-1; i++) {
      var n = imgScale[i].split('img/')[1].split('.jpg')[0].split(imgBase)[1];
      nScale.push(n);
    };
    
    // calculate the element width, and slice it up into sections
    var vWidth = $(document).width();
    var nSlices = nScale.length;
    var slices = generateSlices(vWidth, nSlices);
  
    // setting up initial vertical line to start the mousemove functionality
    var vHeight = $(document).height()-8;
    var lineSlice = vWidth / 10;
    var vertLine = `<div style="border-left:black;border-style:solid;margin-left:${lineSlice}px; height:${vHeight}px;width:0px;position:absolute;" id="vertLine"></div>`;
    var linePrompt = `<div id="linePrompt"><div style="font-size:50px;position:absolute;margin-left:${lineSlice*1.3}px;margin-top:${vHeight/2}px"></div><div style="position:absolute;margin-left:${lineSlice*1.2}px;margin-top:${vHeight/2}px;z-index:5;">Move mouse left of the line to begin</div></div>`;
    $(".jspsych-content-wrapper").prepend(vertLine);
    $(".jspsych-content-wrapper").prepend(linePrompt);
    
    // hide prompt until the trial is begun
    $('#jspsych-content > p').css("visibility", "hidden");
  
    // initialize the central image with the most neutral one (i.e. from
    // the middle of the scale) and set the image to be blurred
    var initialImage = imgScale[0];
    $('#jspsych-image-slider-response_noButton-stimulus').append(`<img src="${initialImage}" style="filter:blur(4px);visibility:hidden;" id="changeable-image"/>`);
  
    // ADD VISUAL SCALE SHOWING MIN/MAX
    var scaleWidth = vWidth * 0.6;
    var scaleLeft = vWidth * 0.2;
    var scaleHtml = `
      <div id="scale-container" style="position:absolute; bottom:120px; left:${scaleLeft}px; width:${scaleWidth}px; height:30px; background-color:rgba(220,220,220,0.7); border-radius:5px; display:none; z-index:10;">
        <div style="position:absolute; left:0px; width:2px; height:30px; background-color:black;"></div>
        <div style="position:absolute; left:0px; top:32px; font-size:12px; color:black;">Min (1)</div>
        <div style="position:absolute; right:0px; width:2px; height:30px; background-color:black;"></div>
        <div style="position:absolute; right:0px; top:32px; font-size:12px; color:black;">Max (50)</div>
        <div id="current-marker" style="position:absolute; left:0px; width:2px; height:30px; background-color:red; display:none;"></div>
      </div>
    `;
    $(".jspsych-content-wrapper").append(scaleHtml);
  
    // workaround with a global variable
    window.__imageMouseOver = {
      lineSlice: lineSlice,
      slices: slices,
      nScale: nScale,
      imgBase: imgBase,
      startTime: startTime,
      trialData: trialData,
      sliceSelected: 0,
    };
    var __listenerBools = {};
  
    // define mousemove event listener that changes image
    function changeImg(event) {
      var mouseX = Math.floor(event.pageX);
      
      // Show the scale after interaction begins
      $("#scale-container").css("display", "block");
      
      // Update current marker position based on mouse position
      var scaleWidth = $("#scale-container").width();
      var relativePos = Math.max(0, Math.min(mouseX - scaleLeft, scaleWidth));
      var percentPos = (relativePos / scaleWidth) * 100;
      
      $("#current-marker").css({
        "left": `${relativePos}px`,
        "display": "block"
      });
      
      for (var i = 0; i < __imageMouseOver.slices.length; i++) {
        // if mouse X position is within the bounds of the X axis slice,
        // change the image to one that is indexed to that slice
        if (mouseX >= __imageMouseOver.slices[i][0] && mouseX <= __imageMouseOver.slices[i][1]) {
          // capture which slice is selected
          __imageMouseOver.sliceSelected = i;
          // update img src to the picture that corresponds to that slice
          $('#changeable-image').attr('src', `img/${__imageMouseOver.imgBase}${__imageMouseOver.nScale[i]}.jpg`);
        }
      };
    };
    
    // define the click listener that ends trial
    function clickHandler(event) {
      if (__listenerBools['mousemove']) {
        // derive trial data
        var trialData = __imageMouseOver.trialData;
        var end_time = (new Date()).getTime();
        var rt = end_time - __imageMouseOver.startTime;
        trialData['rt'] = rt;
        trialData['stimulus_duration'] = rt;
        trialData['trial_duration'] = rt;
        trialData['imageSelected'] = `${__imageMouseOver.imgBase}${__imageMouseOver.nScale[__imageMouseOver.sliceSelected]}.jpg`
        trialData['indexSelected'] = __imageMouseOver.sliceSelected;
        // turn off listeners
        $(document).off('mousemove');
        $(document).off('click');
        // clean up variable namespaces
        delete window.__imageMouseOver
        delete __listenerBools;
        // clean up scale
        $("#scale-container").remove();
        // finish the trial with trial data
        jsPsych.finishTrial(trialData);
      };
    };
  
    function verticalLineInit(event) {
      var mouseX = Math.floor(event.pageX);
      if (mouseX <= __imageMouseOver.lineSlice) {
        $("#vertLine").remove();
        $("#linePrompt").remove();
        $("#jspsych-image-slider-response_noButton-stimulus > img").css({
          "filter":"blur(0px)",
          "visibility": "visible",
        });
        $('#jspsych-content > p').css("visibility", "visible");
        __listenerBools['mousemove'] = true;
        // turn off THIS mouse move listener
        $(document).off("mousemove");
        // turn on the mouse move listener that changes the image
        $(document).mousemove(changeImg);
        // add mouse click listener
        $(document).on('click', clickHandler);
      };
    };
  
    // turn on the vertical line mouse move listener
    $(document).mousemove(verticalLineInit);
  }
  
