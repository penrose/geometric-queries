var EPS = Math.pow(0.1, 8);

var gradFuncsList = [{
      f: "movepPS",
      type: 0,
      epsilon: 0.01,
      str: 'move p to decrease dist <pt> <seg>',
      action: (res)=>{
        pInd = selections[0];
        elems[pInd][0][0] -= res[0];
        elems[pInd][0][1] -= res[1];
      }
    }, {
      f: "movexyPS",
      type: 0,
      epsilon: 0.01,
      str: 'move xy to decrease dist <pt> <seg>',
      action: (res)=>{
        pInd = selections[1];
        elems[pInd][0][0] -= res[0];
        elems[pInd][0][1] -= res[1];
        elems[pInd][1][0] -= res[0];
        elems[pInd][1][1] -= res[1];
      }
    }, /*{
      f: "rotxyPSTout",
      epsilon: 0.01,
      str: 'rotate xy around midpt <pt> <seg>',
      action: (res)=>{
        pInd = selections[1];
        elems[pInd] = copyElem(res);
      }
    },*/ {
      f: "rotxyPSCout",
      type: 0,
      epsilon: EPS,
      str: 'rotate xy around C <pt> <seg> <pt>',
      action: (res)=>{
        pInd = selections[1];
        elems[pInd] = copyElem(res);
      }
    }, {
      f: "scalexyPSCout",
      type: 1,
      epsilon: EPS,
      str: 'scale xy around C <pt> <seg> <pt>',
      action: (res)=>{
        pInd = selections[1];
        elems[pInd] = copyElem(res);
      }
    } ,/*{}, {
      f: "movepPSS",
      epsilon: 0.01,
      str: 'move p to decrease dist <pt> <seg> <seg>',
      action: (res)=>{
        pInd = selections[0];
        elems[pInd][0][0] -= res[0];
        elems[pInd][0][1] -= res[1];
      }
    }, {
      f: "movexyPSS",
      epsilon: 0.01,
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
      epsilon: 0.01,
      str: 'rotate both segments <pt> <seg> <seg> <pt>',
      action: (res)=>{
        // console.log(res);
        pInd1 = selections[1];
        pInd2 = selections[2];
        elems[pInd1] = copyElem(res[0]);
        elems[pInd2] = copyElem(res[1]);
      }
    },*/ {}, {
      f: "rotbPGCout",
      type: 0,
      epsilon: EPS,
      str: 'rotate polygon <pt> <poly> <pt>',
      action: (res)=>{
        // console.log(res);
        pInd = selections[1];
        elems[pInd] = copyElem(res);
      }
    }, {
      f: "rotbSSCout",
      type: 0,
      epsilon: EPS,
      str: 'rotate segment B <seg> <seg> <pt>',
      action: (res)=>{
        // console.log(res);
        pInd = selections[1];
        elems[pInd] = copyElem(res);
      }
    }, {
      f: "rotbSGCout",
      type: 0,
      epsilon: EPS,
      str: 'rotate polygon <seg> <poly> <pt>',
      action: (res)=>{
        // console.log(res);
        pInd = selections[1];
        elems[pInd] = copyElem(res);
      }
    }, {
      f: "rotbGGCout",
      type: 0,
      epsilon: EPS,
      str: 'rotate polygon <poly> <poly> <pt>',
      action: (res)=>{
        // console.log(res);
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