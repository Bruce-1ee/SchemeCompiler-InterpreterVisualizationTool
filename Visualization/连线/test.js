/**
 * 输入两个element的id进行连接
 * connect('box1','box2')
 */
function connect(s, t) {
    return jsPlumb.connect({
        source: s,  // from
        target: t,  // to
        endpoint: ['Dot', { radius: '0' }],
        overlays: [['Arrow', { width: 12, length: 12, location: 1 }]],
        connector: ['Flowchart'],
        anchor: ['Bottom', 'Right', 'Left']
    })
    //jsPlumb.draggable(s)
    //jsPlumb.draggable(t)
}

const conMap = new Map();

function makeLocalEnvConnection(envName, envFrame) {

    function connectEnvNameAndEnvFrame(envName, envFrame) {
        let con = jsPlumb.connect({
            source: envName,
            target: envFrame,
            endpoint: ['Dot', { radius: '0' }],
            overlays: [['Arrow', { width: 12, length: 12, location: 1 }]],
            connector: ['Flowchart'],
            anchor: ['Right', 'Left']
        });
        conMap.set(envName, con);
    }

    function connectEnvFrameAndGloEnv(envFrame) { //全局环境的名称为：globalEnvironmentFrame
        let con = jsPlumb.connect({
            source: envFrame,
            target: 'box1',
            endpoint: ['Dot', { radius: '0' }],
            overlays: [['Arrow', { width: 12, length: 12, location: 1 }]],
            connector: ['Flowchart'],
            anchor: ['Right', 'Bottom']
        });
        conMap.set(envFrame, con);
    }

    connectEnvNameAndEnvFrame(envName, envFrame);
    connectEnvFrameAndGloEnv(envFrame);
}


