//---- Global vars ----
var P, L, G;

var funcs = [];

var currentFuncIndex;
var args = [];

var elems = [];
var selections = [];

var recording = false;
var createMode = 0;

//-------layout--------
var LEFT_MARGIN = 8;
var FUNC_LIST_WIDTH = 200;
var TEXT_HEIGHT = 20;
var SELECTION_START_HEIGHT = 200;

//--------------

var sketch = function (p) {

  p.tmpElem = [];

  p.setup = function() {
    p.createCanvas(800, 600);
    p.frameRate(30);
    p.strokeWeight(1);
    p.fill(40);
  }

  p.draw = function(){
    p.background(240);

    //draw the half-done polygon
    p.beginShape();
    p.fill(255,100,100,100);
    p.noStroke();
    for(var i=0; i<p.tmpElem.length; i++) 
      p.vertex(p.tmpElem[i][0], p.tmpElem[i][1]);
    p.endShape(p.CLOSE);

    //draw the red dot when recording
    if(recording) {
      p.fill(240,0,0);
      p.noStroke();
      p.ellipse(p.width-20, 20, 10, 10);
    }
    // list functions
    for(var i=0; i<funcs.length; i++) {
      p.noStroke();
      p.fill(40);
      p.text(funcs[i].str, LEFT_MARGIN, 16+i*TEXT_HEIGHT);
      p.noFill();
      p.stroke(40);
      if(i==currentFuncIndex) p.rect(3,i*TEXT_HEIGHT,FUNC_LIST_WIDTH,20);
    }
    // render elements
    for(var i=0; i<elems.length; i++) {
      var e = elems[i];
      if(e.length==1) {
        p.noStroke();
        if(selections.indexOf(e)<0)p.fill(40); else p.fill(255,100,100);
        p.ellipse(e[0][0], e[0][1], 4, 4); // point
      } else if(e.length==2) {
        if(selections.indexOf(e)<0)p.stroke(40); else p.stroke(255,100,100);
        p.line(e[0][0],e[0][1],e[1][0],e[1][1]); //line
      } else {
        p.beginShape();
        p.fill(0,50);
        if(selections.indexOf(e)<0)p.stroke(40); else p.stroke(255,100,100);
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
      var e = selections[i];
      var x = LEFT_MARGIN;
      var y = SELECTION_START_HEIGHT+i*TEXT_HEIGHT;
      if(e.length==1) {
        p.text('point ('+e[0][0]+', '+e[0][1]+')', x, y);
      } else if(e.length==2) {
        p.text('segment ('+e[0][0]+', '+e[0][1]+') ('+e[1][0]+', '+e[1][1]+')', x, y);
      } else {
        p.text('polygon ('+e.length+') sides');
      }
    }
  }

  p.stopRecording = function() {
    elems.push(p.tmpElem);
    recording = false;
    p.tmpElem = [];
  }

  p.mouseClicked = function() {
    if(p.mouseX < FUNC_LIST_WIDTH && p.mouseY < TEXT_HEIGHT*funcs.length) 
      currentFuncIndex = Math.floor(p.mouseY/TEXT_HEIGHT);
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
          var ind = selections.indexOf(elems[i]);
          if(ind>=0){
            selections.splice(ind,1);
          } else {
            selections.push(elems[i]);
          }
        }
      }
    }
  }

  p.isOnElem = function(e) {
    var m = [p.mouseX, p.mouseY];
    if(e.length==1) {
      var p1 = e[0];
      var d = evaluate(L.dist, [m, p1]);
      return (d < 3);
    } else if(e.length==2) {
      var d = evaluate(L.shortestDistPS, [m, e]);
      return (d < 3);
    } else {
      var d = evaluate(G.outsidedness, [e, m]);
      return d==-1;
    }
  }

  p.keyTyped = function() {
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
    } else if(p.key==' ') {
      var f = function(e){
        if(e.length==1) return e[0];
        else return e;
      }
      args = selections.map(f);
      func = funcs[currentFuncIndex];
      func.render(evaluate(func.f, args));
    } else if(p.key=='d') {
      for(var i=0; i<selections.length; i++) {
        var ind = elems.indexOf(selections[i]);
        elems.splice(ind, 1);
      }
      selections = [];
    }
  }

}

P = new p5(sketch);
//--------------

window.onload = function () {
  L = Strict.PointsAndLines;
  G = Strict.Polygons;
  funcs = [{
      f: L.closestPointPS,
      str: 'closest point <pt> <seg>',
      render: (res)=>{
        console.log('closest point to p on given segment');
        elems.push([res]);
      }
    }, {
      f: L.intersectionSS,
      str: 'intersection <seg> <seg>',
      render: (res)=>{
        if(res.instance=='Nothing') {
          console.log('segment-segment intersection: nope');
        } else {
          console.log('segment-segment intersection: yup');
          elems.push([res.slot1]);
        }
      }
    }
  ];
}


function evaluate (exp, args) {
  if (args.length==0) return exp;
  else return evaluate(exp(args[0]), args.slice(1));
}
