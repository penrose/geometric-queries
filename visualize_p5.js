//-------------- visualization with p5 -----------------

var visualization = function (p) {

  HIGHLIGHT = p.color(240, 100, 100);
  p.tmpElem = [];

  p.setup = function() {
    p.createCanvas(WIDTH, 600);
    // p.frameRate(24);
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
    if (autostep) {
      step();
      graphPos = p.width/2+p.map(cumulative, 0, Math.PI, 0, p.width/2);
    }
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
      if (p.key=='g' || p.key=='G') {
          graphType = "rot";
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
      } else {
          graphType = "scale";
          if (args[0].length==1) { // pt, _, pt
            if (args[1].length==2) [dgraph, graphMax] = evaluate("scalePSgraph", args);
          }
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
        var posRaw = WIDTH/2+p.map(cumulative, 0, Math.PI, 0, WIDTH/2);
        graphPos = graphType=="rot" ? posRaw % WIDTH : posRaw;
      } else if (mode==1 && p.key=='b'){
        func = gradfuncs[currentFuncIndex];
        autostep = true;
      } /*else if (mode==1 && p.key=='G') {
        graph = !graph;
      }*/ else if(mode==1 && p.key=='g' && terminated) {
        p.updateGraph();
      } else if (mode==1 && p.key=='l' && terminated) {
        p.updateGraph(); 
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
    g.createCanvas(WIDTH, 150);
    g.frameRate(24);
    g.noStroke();
    g.fill(40);
  }

  g.draw = function() {
    g.background(240);
    g.strokeWeight(0.5);
    g.stroke(40);
    g.line(graphPos,0,graphPos,g.height);
    g.noStroke();
    g.text("ccw <--", g.width/2-52, 16);
    g.text("--> cw", g.width/2+10, 16);
    if (!terminated) g.text("optimizing..", LEFT_MARGIN, g.height-16);
    else g.text("stopped.", LEFT_MARGIN, g.height-16);
    if (graphMax > 0) g.text ("max: " + fix(graphMax), LEFT_MARGIN, 16);
    g.showGraph();
  }

  g.showGraph = function() {
    g.stroke(180);
    g.strokeWeight(2);
    for (var i=0; i<dgraph.length; i++) {
      var x = g.map(i,0,dgraph.length,0,g.width);
      var y = g.map(dgraph[i], 0, graphMax, 0, g.height-10);
      g.point(x, g.height-y);
    }
  }

}

G = new p5(functionGraph, 'graph-container');
