/*

creates ShapeNode from mouse recordings
pt: 1 click
seg: 2 clicks
poly: start from boundary, clicks until:
    h: close the current bound / hole, start the next hole.
    c: close the current boundary / hole, end recording.

Polygon be rendered as shape

TODO: click to select / deselect element. Can only select one at a time.
    Also allow dragging maybe?

At some point...
    want UI to select which constraint to add to which node.

*/

var recordMode = 0; // 1: point, 2: seg, 3: polygon boundary, 4: polygon hole, ...
var elems = [];
var selection;
// --- store tmp data when creating nodes.
var tmpElem = [];
var tmpPts = [];
var tmpHs = []

var HIGHLIGHT;

function setup() {
    createCanvas(600,600);
    noStroke();
    HIGHLIGHT= color(240,100,100);
    fill(HIGHLIGHT);
}

function draw() {
    background(240);
    // draw that red dot when recording elem
    if(recordMode > 0) {
        fill(HIGHLIGHT);
        noStroke();
        ellipse(width-20, 20, 10, 10);
    }
    // draw all elements
    for (var i=0; i<elems.length; i++) {
        elems[i].render(false, false);
    }
    // draw the polygon in-creation (if there's one)
    if( (recordMode==3 || recordMode==4) && (
        tmpHs.length==0 ||
        (tmpHs.length>0 && tmpHs[tmpHs.length-1].length!=0)
      )) {
        var tmpPoly = new Shape("Poly", [tmpPts, tmpHs]);
        tmpPoly.render(true, true);
    }
}

function mouseClicked() {
    if (recordMode==1) { // point
        var p = new Shape("Pt", [mouseX, mouseY]);
        stopRecording(new ShapeNode(p));
    } else if (recordMode == 2) { // line segment
        tmpElem.push([mouseX, mouseY]);
        if (tmpElem.length==2) {
            var s = new Shape("Seg", tmpElem);
            stopRecording(new ShapeNode(s));
        }
    } else if (recordMode==3) { // polygon boundary
        tmpPts.push([mouseX, mouseY]);
    } else if (recordMode==4) { // polygon hole
        tmpHs[tmpHs.length-1].push([mouseX, mouseY]);
    } else if (false) {
        for (var i=0; i<elems.length; i++) {
            var test = evaluate("isOn", 
                [elems[i].toShape(), new Shape("Pt",[mouseX, mouseY])]
            );
            if (test) selection = elems[i];
        }
    }
}

function stopRecording(node) {
    elems.push(node);
    recordMode = 0;
    tmpElem = [];
    tmpPts = [];
    tmpHs = [];
}

function keyTyped() {
    console.log("hey");
    if (key=='1') {
        recordMode = 1;
    } else if (key=='2') {
        recordMode = 2;
    } else if (key=='3') {
        recordMode = 3;
    } else if (key=='c') {
        if (recordMode==3 || recordMode==4) {
            var g = new Shape("Poly", [tmpPts, tmpHs]);
            stopRecording(new ShapeNode(g));
        }
    } else if (key=='h') {
        if (recordMode==3) recordMode = 4;
        if (recordMode==4) tmpHs.push([]);
    }
}


// create shape by clicking on canvas.
class Shape {
    constructor(type, shape) {
        this.type = type;
        this.shape = shape;
    }
    tostr() {
        var res = this.type;
        var s = this.shape;
        if (this.type=="Pt") {
            res += p2s(s);
        } else if (this.type=="Seg") {
            res += s2s(s);
        } else if (this.type=="Poly") {
            res += g2s(s);
        }
        return res;
    }
    render(tmp, hl) {
        var s = this.shape;
        if (this.type=="Pt") {
            if(hl) fill(HIGHLIGHT); else fill(40);
            noStroke();
            ellipse(s[0], s[1], 6, 6);
        } else if (this.type=="Seg") {
            if(hl) stroke(HIGHLIGHT); else stroke(40);
            strokeWeight(2);
            line(s[0][0], s[0][1], s[1][0], s[1][1]);
        } else if (this.type=="Poly") {
            beginShape();
            if (hl) fill(red(HIGHLIGHT), green(HIGHLIGHT), blue(HIGHLIGHT), 100); 
                else fill(0,50);
            if (hl) stroke(HIGHLIGHT); else stroke(40);
            if (tmp) noStroke();
            strokeWeight(2);
            for (var i=0; i<s[0].length; i++) {// draw outline. i: index of v
                var v = s[0][i];
                vertex(v[0], v[1]);
            }
            for (var i=0; i<s[1].length; i++) {// i: index of hole
                beginContour();
                for (var j=0; j<s[1][i].length; j++) { // j: index of v
                    var v = s[1][i][j];
                    vertex(v[0], v[1]);
                }
                endContour();
            }
            endShape(CLOSE);
        }
    }
}

class ShapeNode {
    // let input be a shape object, output a node object at 0 cumulative transformation.
    constructor(s) {
        this.trans = [0,0,0,1];
        if (s.type=="Pt") {
            this.shape = new Shape("Pt", [0,0]);
            this.origTrans = [s.shape[0], s.shape[1], 0, 1];
        } else if (s.type=="Seg") {
            var a = s.shape[0]; // endpoint 1
            var b = s.shape[1]; // endpoint 2
            var mid = mult(0.5, add(a,b)); // midpt
            this.origTrans = [mid[0], mid[1], 0, 1];
            this.shape = new Shape("Seg", [sub(a,mid), sub(b,mid)]);
        } else if (s.type=="Poly") {
            var [pts, hs] = s.shape;
            var c = centroid(pts);
            this.origTrans = [c[0], c[1], 0, 1];
            this.shape = new Shape("Poly", [
                pts.map(p => sub(p, c)),
                hs.map(h => 
                    h.map ( p => sub(p, c))
                )
            ]);
        }
    }
    toShape() {
        var tr = combTrans(this.trans, this.origTrans);
        var [mx,my,r,s] = tr;
        var s = this.shape;
        if (s.type=="Pt") {
            return new Shape("Pt", transformP(s.shape, tr));
        } else if (s.type=="Seg") {
            return new Shape("Seg", [transformP(s.shape[0],tr), transformP(s.shape[1],tr)]);
        } else if (s.type=="Poly") {
            var pts = transformG(s.shape[0], tr);
            var hs = s.shape[1].map(h=>transformG(h, tr));
            return new Shape("Poly", [pts, hs]);
        }
    }
    tostr() {
        var res = "Node{shape=";
        res += this.shape.tostr() + ", origTrans=";
        res += p2s(this.origTrans) + ", trans=";
        res += p2s(this.trans);
        return res += "}"
    }
    render(tmp, hl) {
        var s = this.toShape();
        s.render(tmp, hl);
    }
}

//--------- utils -----------

function add(v1, v2) {
    var res = new Array(v1.length);
    for(var i=0; i<v1.length; i++) res[i] = v1[i] + v2[i];
    return res;
}

function sub(v1, v2) {
    var res = new Array(v1.length);
    for(var i=0; i<v1.length; i++) res[i] = v1[i] - v2[i];
    return res;
}

function mult(k, v) {
    var res = new Array(v.length);
    for(var i=0; i<v.length; i++) res[i] = k * v[i];
    return res;
}

function centroid(pts) {
    var sum = [0,0];
    for(var i=0; i<pts.length; i++) sum = add(sum, pts[i]);
    return [sum[0]/pts.length, sum[1]/pts.length];
}

function combTrans([mx1,my1,r1,s1], [mx2,my2,r2,s2]) {
    return [mx1+mx2, my1+my2, r1+r2, s1*s2];
}

function transformP([x,y], [mx, my, t, s]) {
    var scost = s * cos(t);
    var ssint = s * sin(t);
    var x2 = x*scost - y*ssint + mx;
    var y2 = x*ssint + y*scost + my;
    return [x2, y2];
}

// a misnomer really. Transforms a blob (array of points).
function transformG(blob, amt) {
    return blob.map(p=>transformP(p,amt));
}

// TODO: generalize serialization of any array

// array (point) to hs string
function p2s(p) {
    // return "[" + p[0] + "," + p[1] + "]";
    var res = "[";
    for(var i=0; i<p.length; i++) 
        res += p[i] + (i==p.length-1 ? "":",");
    return res += "]";
}

// lineseg to hs string
function s2s(s) {
    return "(" + p2s(s[0]) + "," + p2s(s[1]) + ")";
}

// blog to hs string
function b2s(b) {
    var res = "[";
    for(var i=0; i<b.length; i++) 
        res += p2s(b[i]) + (i==b.length-1 ? "":",");
    return res += "]";
}

// polygon to hs string
function g2s([pts, hs]) {
    var res = "(";
    res += b2s(pts) + ",[";
    for(var i=0; i<hs.length; i++) 
        res += b2s(hs[i]) + (i==hs.length-1 ? "":",");
    return res += "])";
}

// a state is an array of nodes.
function state2s(state) {
    var res = "[";
    for(var i=0; i<state.length; i++) 
        res += state[i].tostr() + (i==state.length-1 ? "":",");
    return res += "]";
}

// strings -> bytestrings (char arrays, etc.) for performance
function evaluate (exp, args) {
  var result;
  var requestObj = {func: exp, args: state2s(args)};
  $.ajax({
    type: 'POST',
    dataType: 'json',
    url: 'eval',
    data: JSON.stringify(requestObj),
    success: (data)=>{
      result = data;
    },
    async: false
  })
  try {
    // console.log(result.value);
    return eval(result.value);
  } catch (err) {
    return JSON.parse(result.value);
  }
}

var g = new ShapeNode(new Shape("Poly", [[[0,0],[1,0],[0,1]], [[[0,0],[0,0.1],[0.1,0.1]]]]));
var s = new ShapeNode(new Shape("Seg", [[-1,-1],[0,2]]));
var p = new ShapeNode(new Shape("Pt", [0.4, 0.7]));
var st = [g, s, p]

