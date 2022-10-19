/**
 * 这个文件里记载了用于绘图的函数
 */


/**
 * 新建一个标签
 * @param {String} content 标签的内容
 * @param {String} id id值
 * @param {String} style style值
 * @returns 
 */
function makeNewElement(content, id, style) {
    let newElement = document.createElement('div');
    let newContent = document.createTextNode(content);
    newElement.setAttribute('id', id);
    newElement.setAttribute('class', style);
    newElement.appendChild(newContent);
    return newElement;
}

/**
 * 用来删除stackFrame
 */
function popStackFrame(frameLength) {
    let parentNode = document.getElementById("stack");
    let currentNode = document.getElementById("stackFrame_" + frameLength);
    parentNode.removeChild(currentNode);
}

/**
 * 用来删除stackFrame
 */
function popStackElement(frameLength, stackLength) {
    let parentNode = document.getElementById("stackFrame_" + frameLength);
    let currentNode = document.getElementById("stackElementFrame_" + stackLength);
    parentNode.removeChild(currentNode);
}

/**
 * 通过两个element的id连接
 * makeLocalEnvConnection('localEnvrionmenName_' + frameCounter, 
 * 'localEnvrionmenFrame_' + frameCounter);
 */
const conMap = new Map(); //用于删除连接
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
            target: 'globalEnvironmentFrame',
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

var inteCounter = 1;
function drawInterpreterInfo(info) {
    let e = makeNewElement(inteCounter + ". " + info, "inteInfo" + inteCounter, "null");
    document.getElementById('interpreterInfo').appendChild(e);
    inteCounter++;
}

var VMCounter = 1;
function drawVMInfo(info) {
    let e = makeNewElement(VMCounter + ". " + info, "VMInfo" + VMCounter, "null");
    document.getElementById('VMInfo').appendChild(e);
    VMCounter++;
}

