var EPS = Math.pow(0.1, 8);
var EPS4 = Math.pow(0.1, 4);

var gradFuncsList = [
    {
      f: "containB",
      type: [2,4,0],
      epsilon: EPS,
      str: 'A contain B <poly> <poly> <pt>',
      action: (res)=>{
        pInd = selections[1];
        elems[pInd] = copyElem(res);
      },
      action2: (res)=>{
        pInd = selections[2];
        elems[pInd] = copyElem(res);
      }
    },{
      f: "containAB",
      type: [3,4,0],
      epsilon: EPS,
      str: 'A contain B <poly> <pt> <poly> <pt>',
      action: (res)=>{
        pInd = selections[0];
        elems[pInd] = copyElem(res);
      },
      action2: (res)=>{
        pInd = selections[2];
        elems[pInd] = copyElem(res);
      }
    },{
      f: "containedB",
      type: [2,4,0],
      epsilon: EPS,
      str: 'B contain A <poly> <poly> <pt>',
      action: (res)=>{
        pInd = selections[1];
        elems[pInd] = copyElem(res);
      },
      action2: (res)=>{
        pInd = selections[2];
        elems[pInd] = copyElem(res);
      }
    },{
      f: "containedAB",
      type: [3,4,0],
      epsilon: EPS,
      str: 'B contain A <poly> <pt> <poly> <pt>',
      action: (res)=>{
        pInd = selections[0];
        elems[pInd] = copyElem(res);
      },
      action2: (res)=>{
        pInd = selections[2];
        elems[pInd] = copyElem(res);
      }
    },{
      f: "disjB",
      type: [2,4,0],
      epsilon: EPS,
      str: 'A, B disjoint <poly> <poly> <pt>',
      action: (res)=>{
        pInd = selections[1];
        elems[pInd] = copyElem(res);
      },
      action2: (res)=>{
        pInd = selections[2];
        elems[pInd] = copyElem(res);
      }
    },{
      f: "disjAB",
      type: [3,4,0],
      epsilon: EPS,
      str: 'A, B disjoint <poly> <pt> <poly> <pt>',
      action: (res)=>{
        pInd = selections[0];
        elems[pInd] = copyElem(res);
      },
      action2: (res)=>{
        pInd = selections[2];
        elems[pInd] = copyElem(res);
      }
    },{
      f: "inTangB",
      type: [2,4,0],
      epsilon: EPS,
      str: 'A contain B, inside tangent <poly> <poly> <pt>',
      action: (res)=>{
        pInd = selections[1];
        elems[pInd] = copyElem(res);
      },
      action2: (res)=>{
        pInd = selections[2];
        elems[pInd] = copyElem(res);
      }
    },{
      f: "outTangB",
      type: [2,4,0],
      epsilon: EPS,
      str: 'A,B disjoint, outside tangent <poly> <poly> <pt>',
      action: (res)=>{
        pInd = selections[1];
        elems[pInd] = copyElem(res);
      },
      action2: (res)=>{
        pInd = selections[2];
        elems[pInd] = copyElem(res);
      }
    },{
      f: "bdixAB",
      type: [3,4,0],
      epsilon: EPS,
      str: 'boundary intersect <poly> <pt> <poly> <pt>',
      action: (res)=>{
        pInd = selections[0];
        elems[pInd] = copyElem(res);
      },
      action2: (res)=>{
        pInd = selections[2];
        elems[pInd] = copyElem(res);
      }
    },{
      f: "bdixB",
      type: [2,4,0],
      epsilon: EPS,
      str: 'boundary intersect <poly> <poly> <pt>',
      action: (res)=>{
        pInd = selections[1];
        elems[pInd] = copyElem(res);
      }
    }
];

var queryFuncsList = [{
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
        //console.log('calc seg: ' + res);
        elems.push(res);
      }
    }, {
      f: "maxSignedDistSegGG",
      str: 'max signed dist <poly> <poly>',
      render: (res)=>{
        console.log('added: seg represents max signed distance');
        elems.push(res);
      }
    }
];
