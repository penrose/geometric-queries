var EPS = Math.pow(0.1, 8);
var EPS4 = Math.pow(0.1, 4);

var gradFuncsList = [
    {
      f: "containGGCout",
      type: [2,4,1],
      epsilon: EPS,
      str: 'encourage containment <poly> <poly> <pt>',
      action: (res)=>{
        pInd = selections[1];
        elems[pInd] = copyElem(res);
      }
    },{
      f: "combGCGCout",
      type: [3,4,1],
      epsilon: EPS,
      str: 'comb <poly> <pt> <poly> <pt>',
      action: (res)=>{
        pInd = selections[0];
        elems[pInd] = copyElem(res);
      },
      action2: ([res])=>{
        pInd = selections[2];
        elems[pInd] = copyElem(res);
      }
    },{
      f: "combGGCout",
      type: [2,4,1],
      epsilon: EPS,
      str: 'comb <poly> <poly> <pt>',
      action: (res)=>{
        pInd = selections[1];
        elems[pInd] = copyElem(res);
      },
      action2: (res)=>{
        pInd = selections[2];
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
