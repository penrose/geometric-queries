
//----------Global vars-----------
var P;
var G;

var elems = [];
var selections = [];
var recording = false;
var createMode = 0;
var currentFuncIndex;
var args = [];

var mode = 1;
// graph
var graph = false;
var dgraph = [];
var graphMax = 0;

// for testing individual queries
var funcs = [];

// for testing gradients
var gradfuncs = [];
var autostep = false;
var stepCounter = 0;
var MAX_STEPS = 500;

//-------layout--------
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
  funcs = [{
      f: "closestPointPS",
      str: 'closest point <pt> <seg>',
      render: (res)=>{
        console.log('added: closest point to p on given segment');
        elems.push([res]);
      }
    }, {
      f: "intersectionSS",
      str: 'intersection <seg> <seg>',
      render: (res)=>{
        if(!res.Just) {
          console.log('no segment-segment intersection');
        } else {
          console.log('added: segment-segment intersection');
          elems.push([res.value]);
        }
      }
    }, {
      f: "shortestSegmentSS",
      str: 'shortest segment <seg> <seg>',
      render: (res)=>{
        console.log('added: shortest segment');
        elems.push(res);
      }
    }, {
      f: "closestPointGP",
      str: 'closest point <poly> <pt>',
      render: (res)=>{
        console.log('added: closest point to p on polygon boundary');
        elems.push([res]);
      }
    }, {
      f: "segIsInside",
      str: 'segment inside polygon <poly> <seg>',
      render: (res)=>{
        console.log('segment is inside polygon? ' + res);
      }
    }, {
      f: "shortestSegmentGS",
      str: 'shortest segment <poly> <seg>',
      render: (res)=>{
        console.log('added: shortest segment');
        elems.push(res);
      }
    }, {
      f: "maxUDistSegGS",
      str: 'max unsigned dist <poly> <seg>',
      render: (res)=>{
        console.log('added: longest segment');
        elems.push(res);
      }
    }, {}, {
      f: "shortestSegmentGG",
      str: 'min unsigned dist <poly> <poly>',
      render: (res)=>{
        console.log('added: seg represents min unsigned distance');
        elems.push(res);
      }
    }, {
      f: "minSignedDistSegGG",
      str: 'min signed dist <poly> <poly>',
      render: (res)=>{
        console.log('added: seg represents min signed distance');
        elems.push(res);
      }
    }, {
      f: "maxUDistSegGG",
      str: 'max unsigned dist <poly> <poly>',
      render: (res)=>{
        console.log('added: seg reprensents max unsigned distance');
        console.log('calc seg: ' + res);
        elems.push(res);
      }
    }, {
      f: "maxSignedDistSegGG",
      str: 'max signed dist <poly> <poly>',
      render: (res)=>{
        console.log('added: seg represents max signed distance');
        elems.push(res);
      }
    }, {}, {
      f: "maxUDistGGtestSeg",
      str: 'max unsigned sampling ref',
      render: (res)=>{
        console.log('ref seg: ' + res);
        elems.push(res);
      }
    }, {
      f: "maxUDistSegGSaprx",
      str: 'max u sampling ref <poly> <seg>',
      render: (res)=>{
        console.log('added: seg represents max unsigned distance');
        elems.push(res);
      }
    }
  ];

  gradfuncs = [{
      f: "movepPS",
      epsilon: 1,
      str: 'move p to decrease dist <pt> <seg>',
      action: (res)=>{
        pInd = selections[0];
        elems[pInd][0][0] -= res[0];
        elems[pInd][0][1] -= res[1];
      }
    }, {
      f: "movexyPS",
      epsilon: 1,
      str: 'move xy to decrease dist <pt> <seg>',
      action: (res)=>{
        pInd = selections[1];
        elems[pInd][0][0] -= res[0];
        elems[pInd][0][1] -= res[1];
        elems[pInd][1][0] -= res[0];
        elems[pInd][1][1] -= res[1];
      }
    }, {
      f: "rotxyPSTout",
      epsilon: 10,
      str: 'rotate xy around midpt <pt> <seg>',
      action: (res)=>{
        pInd = selections[1];
        elems[pInd] = copyElem(res);
      }
    }, {
      f: "rotxyPSCout",
      epsilon: 0.1,
      str: 'rotate xy around C <pt> <seg> <pt>',
      action: (res)=>{
        pInd = selections[1];
        elems[pInd] = copyElem(res);
      }
    }, {}, {
      f: "movepPSS",
      epsilon: 1,
      str: 'move p to decrease dist <pt> <seg> <seg>',
      action: (res)=>{
        pInd = selections[0];
        elems[pInd][0][0] -= res[0];
        elems[pInd][0][1] -= res[1];
      }
    }, {
      f: "movexyPSS",
      epsilon: 1,
      str: 'move both segments <pt> <seg> <seg>',
      action: (res)=>{
        pInd1 = selections[1];
        pInd2 = selections[2];
        elems[pInd1][0][0] -= res[0];
        elems[pInd1][0][1] -= res[1];
        elems[pInd1][1][0] -= res[0];
        elems[pInd1][1][1] -= res[1];
        elems[pInd2][0][0] -= res[0];
        elems[pInd2][0][1] -= res[1];
        elems[pInd2][1][0] -= res[0];
        elems[pInd2][1][1] -= res[1];
      }
    }, {
      f: "rotxyPSSCout",
      epsilon: 1,
      str: 'rotate both segments <pt> <seg> <seg> <pt>',
      action: (res)=>{
        // console.log(res);
        pInd1 = selections[1];
        pInd2 = selections[2];
        elems[pInd1] = copyElem(res[0]);
        elems[pInd2] = copyElem(res[1]);
      }
    }, {}, {
      f: "rotbPGCout",
      epsilon: 1,
      str: 'rotate polygon <pt> <poly> <pt>',
      action: (res)=>{
        // console.log(res);
        pInd = selections[1];
        elems[pInd] = copyElem(res);
      }
    }, {
      f: "rotbSSCout",
      epsilon: 1,
      str: 'rotate segment B <seg> <seg> <pt>',
      action: (res)=>{
        // console.log(res);
        pInd = selections[1];
        elems[pInd] = copyElem(res);
      }
    }, {
      f: "rotbSGCout",
      epsilon: 1,
      str: 'rotate polygon <seg> <poly> <pt>',
      action: (res)=>{
        // console.log(res);
        pInd = selections[1];
        elems[pInd] = copyElem(res);
      }
    }, {
      f: "rotbGGCout",
      epsilon: 1,
      str: 'rotate polygon <poly> <poly> <pt>',
      action: (res)=>{
        // console.log(res);
        pInd = selections[1];
        elems[pInd] = copyElem(res);
      }
    }
  ];


  document.getElementById('manual').onclick = manualAdd;
  
}

//--------- test grad -------------

function step() {
  if (graph) P.updateGraph();
  for (var i=0; i<selections.length; i++) {
    args.push(elems[selections[i]]);
  }
  var [move, state] = evaluate (func.f, args);
  console.log(state);
  if (Math.abs(state) < func.epsilon || stepCounter >= MAX_STEPS) {
    autostep = false; // need to generalize stoping condition
    stepCounter = 0;
    args = [];
    return;
  }
  func.action (move);
  stepCounter++;
  args = [];
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

//--------- utility -----------

function copySeg (src, dest) {
  dest[0][0] = src[0][0];
  dest[0][1] = src[0][1];
  dest[1][0] = src[1][0];
  dest[1][1] = src[1][1];
}

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

//-------------- visualization with p5 -----------------

var visualization = function (p) {

  HIGHLIGHT = p.color(240, 100, 100);
  p.tmpElem = [];

  p.setup = function() {
    p.createCanvas(800, 600);
    p.frameRate(24);
    p.strokeWeight(2);
    p.fill(40);
  }

  p.draw = function(){
    p.background(240);

    if (ucirc) p.ellipse(p.frameCount % p.width, 50, 10, 10);
    // p.showGraph();

    //draw the polygon in recording process
    p.beginShape();
    p.fill(255,100,100,100);
    p.noStroke();
    for(var i=0; i<p.tmpElem.length; i++) 
      p.vertex(p.tmpElem[i][0], p.tmpElem[i][1]);
    p.endShape(p.CLOSE);

    //draw the red dot when recording
    if(recording) {
      p.fill(HIGHLIGHT);
      p.noStroke();
      p.ellipse(p.width-20, 20, 10, 10);
    }
    // list functions, when in query mode
    if (mode==0) for(var i=0; i<funcs.length; i++) {
      p.noStroke();
      p.fill(40);
      p.text(funcs[i].str, LEFT_MARGIN, 16+i*TEXT_HEIGHT);
      p.noFill();
      p.stroke(40);
      if(i==currentFuncIndex) p.rect(3,i*TEXT_HEIGHT+2,FUNC_LIST_WIDTH,20);
    }
    // list grad functions, when in grad mode
    if (mode==1) for(var i=0; i<gradfuncs.length; i++) {
      p.noStroke();
      p.fill(40);
      p.text(gradfuncs[i].str, LEFT_MARGIN, 16+i*TEXT_HEIGHT);
      p.noFill();
      p.stroke(40);
      if(i==currentFuncIndex) p.rect(3,i*TEXT_HEIGHT+2,FUNC_LIST_WIDTH,20);
    }
    // render elements
    for(var i=0; i<elems.length; i++) {
      var e = elems[i];
      var id = selections.indexOf(i);
      var c = p.findCenter(e);
      var label = String.fromCharCode(65 + id);
      if(e.length==1) {
        p.noStroke();
        if(id<0) p.fill(40); 
        else {
          p.noStroke();
          p.fill(HIGHLIGHT);
          p.text(label, c[0]+3, c[1]-5);
        }
        p.ellipse(e[0][0], e[0][1], 6, 6); // point
      } else if(e.length==2) {
        if(id<0) p.stroke(40); 
        else {
          p.fill(HIGHLIGHT);
          p.noStroke();
          p.text(label, c[0]+5, c[1]-5);
          p.stroke(HIGHLIGHT);
        }
        p.line(e[0][0],e[0][1],e[1][0],e[1][1]); //line
      } else {
        if(id>=0)
        {
          p.noStroke();
          p.fill(HIGHLIGHT);
          p.text(label, c[0]-6, c[1]+5);
        }
        p.beginShape();
        p.fill(0,50);
        if(id<0) p.stroke(40); 
        else {
          p.stroke(HIGHLIGHT);
        }
        for(var j=0; j<e.length; j++) 
          p.vertex(e[j][0], e[j][1])
        p.endShape(p.CLOSE);
        p.fill(40);
      }
    }

    // list the selected elements
    p.fill(40);
    p.noStroke();
    for(var i=0; i<selections.length; i++) {
      var e = elems[selections[i]];
      var x = LEFT_MARGIN;
      var y = SELECTION_START_HEIGHT+i*TEXT_HEIGHT;
      if(e.length==1) {
        p.text('point ('+fix(e[0][0])+', '+fix(e[0][1])+')', x, y);
      } else if(e.length==2) {
        p.text('segment ('+fix(e[0][0])+', '+fix(e[0][1])+') ('
          +fix(e[1][0])+', '+fix(e[1][1])+')', x, y);
      } else {
        p.text('polygon ('+e.length+' sides)', x, y);
      }
    }
    //show mouse coordinates
    p.text('mouse at: ('+p.mouseX+', '+p.mouseY+')', 
      LEFT_MARGIN, p.height-BOTTOM_MARGIN);

    // auto step
    if (autostep) step();
  }

  p.stopRecording = function() {
    elems.push(p.tmpElem);
    recording = false;
    p.tmpElem = [];
  }

  p.findCenter = function(e) {
    var sumx = 0; 
    var sumy = 0;
    for(var i=0; i<e.length; i++) {
      sumx += e[i][0];
      sumy += e[i][1];
    }
    return [sumx / e.length, sumy / e.length];
  }

  p.mouseClicked = function() {
    if(p.oncanvas()){
      // detect click on function
      if(mode==0 && p.mouseX < FUNC_LIST_WIDTH && p.mouseY < TEXT_HEIGHT*funcs.length) 
        currentFuncIndex = Math.floor((p.mouseY-2)/TEXT_HEIGHT);
      else if(mode==1 && p.mouseX < FUNC_LIST_WIDTH && p.mouseY < TEXT_HEIGHT*gradfuncs.length) 
        currentFuncIndex = Math.floor((p.mouseY-2)/TEXT_HEIGHT);
      // clicks for making elems
      else if (recording ) {
        if(createMode == 1){ // point
          p.tmpElem = [[p.mouseX, p.mouseY]];
          p.stopRecording();
        } else if(createMode == 2) { // line
          p.tmpElem.push([p.mouseX, p.mouseY]);
          if(p.tmpElem.length==2) {
            p.stopRecording();
          }
        } else { // polygon
          p.tmpElem.push([p.mouseX, p.mouseY]);
          // this one doesn't automatically stop recording
        }
      } else {
        for(var i=0; i<elems.length; i++) {
          if(p.isOnElem(elems[i])){
            var ind = selections.indexOf(i);
            if(ind>=0){
              selections.splice(ind,1);
            } else {
              selections.push(i);
            }
          }
        }
      }
    }
  }

  p.isOnElem = function(e) {
    var m = [[p.mouseX, p.mouseY]];
    if(e.length==1) {
      var d = evaluate("dist", [m, e]);
      return (d < 4);
    } else if(e.length==2) {
      var d = evaluate("shortestDistPS", [m, e]);
      return (d < 4);
    } else {
      var d = evaluate("outsidedness", [e, m]);
      return d==-1;
    }
  }

  p.updateGraph = function() {
    for (var i=0; i<selections.length; i++) {
      args.push(elems[selections[i]]);
    }
    if (args.length==3 && args[2].length==1) { // _, _, pt
      if (args[0].length==1) { // pt, _, pt
        if (args[1].length==2) [dgraph, graphMax] = evaluate("graphDistPsiPSC", args);
        else if (args[1].length>2)[dgraph, graphMax] = evaluate("graphDistPsiPGC", args);
      } else if (args[0].length==2) { // seg, _, pt
        if (args[1].length==2)
          [dgraph, graphMax] = evaluate("graphDistPsiSSC", args);
        else if (args[1].length>2)
          [dgraph, graphMax] = evaluate("graphDistPsiSGC", args);
      } else if (args[0].length>2) { // poly, _, pt
          [dgraph, graphMax] = evaluate("graphDistPsiGGC", args);
      }
    }
    else if (args.length==4 && 
        args[0].length==1 && args[1].length==2 &&
        args[2].length==2 && args[3].length==1) // pt, seg, seg, pt
          [dgraph, graphMax] = evaluate("graphDist2PsiPSC", args);
    args = [];
  }

  p.keyTyped = function() {
    if(p.oncanvas()){
      if(p.key=='c'){
        p.stopRecording();
      } else if(p.key=='1') {
        recording = true;
        createMode = 1;
      } else if(p.key=='2') {
        recording = true;
        createMode = 2;
      } else if(p.key=='3') {
        recording = true;
        createMode = 3;
      } else if(mode==0 && p.key=='0') {
        for (var i=0; i<selections.length; i++) {
          args.push(elems[selections[i]]);
        }
        func = funcs[currentFuncIndex];
        func.render(evaluate(func.f, args));
        args = [];
      } else if(mode==1 && p.key=='v') {
        func = gradfuncs[currentFuncIndex];
        step();
      } else if (mode==1 && p.key=='b'){
        func = gradfuncs[currentFuncIndex];
        autostep = true;
      } else if (mode==1 && p.key=='G') {
        graph = !graph;
      } else if(mode==1 && p.key=='g') {
        for (var i=0; i<selections.length; i++) {
          args.push(elems[selections[i]]);
        }
        if (args.length==3 && args[2].length==1) { // _, _, pt
          if (args[0].length==1) { // pt, _, pt
            if (args[1].length==2)[dgraph, graphMax] = evaluate("graphDistPsiPSC", args);
            else if (args[1].length>2)[dgraph, graphMax] = evaluate("graphDistPsiPGC", args);
          } else if (args[0].length==2) { // seg, _, pt
            if (args[1].length==2)
              [dgraph, graphMax] = evaluate("graphDistPsiSSC", args);
            else if (args[1].length>2)
              [dgraph, graphMax] = evaluate("graphDistPsiSGC", args);
          } else if (args[0].length>2) { // poly, _, pt
              [dgraph, graphMax] = evaluate("graphDistPsiGGC", args);
          }
        }
        else if (args.length==4 && 
            args[0].length==1 && args[1].length==2 &&
            args[2].length==2 && args[3].length==1) // pt, seg, seg, pt
              [dgraph, graphMax] = evaluate("graphDist2PsiPSC", args);
        args = [];
      } else if(p.key=='d') {
        selections.sort();
        for(var i=selections.length-1; i>=0; i--) {
          elems.splice(selections[i], 1);
        }
        selections = [];
      } else if (p.key=='z') {
        autostep = false;
        stepCounter = 0;
        args = [];
      }
    }
  }
  p.oncanvas = function() {
    return p.mouseX>=0 && p.mouseX<=p.width &&
      p.mouseY>=0 && p.mouseY<=p.height
  }

}

P = new p5(visualization, 'canvas-container');

var functionGraph = function (g) {
  
  HIGHLIGHT = g.color(240, 100, 100);

  g.setup = function() {
    g.createCanvas(800, 150);
    g.frameRate(24);
    g.noStroke();
    g.fill(40);
  }

  g.draw = function() {
    g.background(240);
    g.showGraph();
    if (graph) g.text("graphing..", LEFT_MARGIN, g.height-16);
    else g.text("paused.", LEFT_MARGIN, g.height-16);
    if (graphMax > 0) g.text ("max: " + fix(graphMax), LEFT_MARGIN, 16);
  }

  g.showGraph = function() {
    g.stroke(180);
    g.strokeWeight(2);
    for (var i=0; i<dgraph.length; i++) {
      var x = g.map(i,0,dgraph.length,0,g.width);
      var y = g.map(dgraph[i], 0, graphMax, 0, g.height-10);
      g.point(x, g.height-y);
    }
    g.line(g.width/2,g.height-5,g.width/2,g.height);
    g.noStroke();
  }

}

G = new p5(functionGraph, 'graph-container');
