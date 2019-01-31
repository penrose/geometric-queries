
//----------Global vars-----------
var P;
var G;

var elems = [];
var selections = [];
var recording = false;
var createMode = 0;
var currentFuncIndex = -1;
var args = [];

var mode = 1;

// graph
var graph = false;
var graphType = "";
var graphPos;
var dgraph = [];
var graphMax = 0;

// for testing individual queries
var funcs = [];

// for testing gradients
var gradfuncs = [];
var autostep = false;
var stepCounter = 0;
var terminated = true;
var args_orig = [];
var cumulative = [0];
var MAX_STEPS = 500;

//-------layout--------
var WIDTH = 800;
var LEFT_MARGIN = 8;
var BOTTOM_MARGIN = 16;
var FUNC_LIST_WIDTH = 260;
var TEXT_HEIGHT = 20;
var SELECTION_START_HEIGHT = 400;
var DIGITS = 2;
var ucirc = false;

var HIGHLIGHT;

//---------------------

window.onload = function () {
  // functions to be tested
  funcs = queryFuncsList;
  gradfuncs = gradFuncsList;

  document.getElementById('manual').onclick = manualAdd;
  
}

//--------- test grad -------------

function step() {
  [type, dim, orig] = func.type;
  if (terminated) { // first step, initialize cum and term
    terminated = false;
    cumulative = [];
    if(type==0||type==1) for(var i=0; i<dim; i++) cumulative.push(type);
    else cumulative = [0,0,0,1]; //type==2, combination of all
    if (type==3) cumulative = [0,0,0,1,0,0,0,1]; //type=3, move both polys

    args_orig = new Array(selections.length);
    for(var i=0; i<selections.length; i++) {
      args_orig[i] = copyElem(elems[selections[i]]);
    }
  } 
  // push elem arguments (originals, or transformed)
  if (orig==0)
    for (var i=0; i<selections.length; i++) {
      args.push(elems[selections[i]]);
    }
  else {
    for (var i=0; i<selections.length; i++) {
      args.push(args_orig[i]);
    }
  }
  // additional argument: cumulative change
  args.push([cumulative]);
  if (type>=2) args.push([[getInput('in1'),getInput('in2'),getInput('in3')]]); // combination: one additional arg (weight)
  var [move, state, cum, additional] = evaluate (func.f, args);
  cumulative = cum;
  func.action (move);
  if (additional) func.action2 (additional);
  console.log(state + ", " + cum);
  if (Math.abs(state) < func.epsilon || stepCounter >= MAX_STEPS) {
    console.log("stopped.");
    autostep = false; // need to generalize stoping condition
    stepCounter = 0;
    terminated = true;
    args = [];
    args_orig = [];
    return;
  }
  stepCounter++;
  args = [];
}

//--------- utility -----------

function copyElem (src) {
  var dest = new Array(src.length);
  for(var i=0; i<src.length; i++) {
    dest[i] = [src[i][0], src[i][1]];
  }
  return dest;
}

function evaluate (exp, args) {
  var result;
  var requestObj = {func: exp, args: args};
  $.ajax({
    type: 'POST',
    dataType: 'json',
    url: 'url',
    data: JSON.stringify(requestObj),
    success: (data)=>{
      result = data;
    },
    async: false
  })
  try {
    return eval(result.value);
  } catch (err) {
    return JSON.parse(result.value);
  }
}

function fix (n) {
  return n.toFixed(DIGITS);
}

function doHttpGet(url, handler) {
  var connection = new XMLHttpRequest();
  var myHandler = function() {
    if(connection.readyState === XMLHttpRequest.DONE) {
      handler(connection.responseText);
    }
  }
  if(handler !== undefined) {
    connection.onreadystatechange = myHandler;
  }
  connection.open('GET', url);
  connection.timeout = 10000;
  connection.ontimeout = function() {
    console.log('No response :/');
    connection.abort();
  }
  connection.send();
}

function manualAdd(){
  var txt = document.getElementById('input').value;
  elems.push(eval('['+txt+']'));
}

function getInput(id){
  var txt = document.getElementById(id).value;
  return eval(txt);
}

//--------- test max unsigned -----------

var rangeLo = 200;
var rangeHi = 600;

// returns a random polygon of n sides, w coordinates in specified range
function tester (n, lo, hi) {
  var randPolys = [];
  for (var i=0; i<n; i++) {
    var pts = [];
    var numV = Math.floor(lo + (hi-lo)*Math.random());
    for (var j=0; j<numV; j++) {
      var x = rangeLo + (rangeHi-rangeLo)*Math.random();
      var y = rangeLo + (rangeHi-rangeLo)*Math.random();
      pts.push([x,y]);
    }
    randPolys.push(pts);
  }
  // elems = elems.concat(randPolys);
  return randPolys;
}

// test max unsigned dist against sampling for n times
function maxU (n) {
  var numTests = n;
  var polys1 = tester (numTests, 3, 8);
  var polys2 = tester (numTests, 3, 8);
  for (var i=0; i<numTests; i++) {
    var res = evaluate ("maxUDistGG", [polys1[i], polys2[i]]);
    var ref = evaluate ("maxUDistGGtest", [polys1[i], polys2[i]]);
    var diff = Math.abs(res-ref);
    console.log(diff);
    if (diff > 0.5 || ref>res) {
      elems.push(polys1[i]);
      elems.push(polys2[i]);
      break;
    }
  }
  return 'good.'
}
