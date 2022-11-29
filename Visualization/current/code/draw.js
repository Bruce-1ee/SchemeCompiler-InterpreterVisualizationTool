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



function drawClsure_bak(closureCounter, l) {
    //外侧的框
    let box = makeNewElement('', '', 'closureRightBox');
    //点对的绘制

    let cloBox = makeNewElement('', 'closure_' + closureCounter, 'doubleDotBox clearfix');

    let c1 = makeNewElement('', 'c' + closureCounter + 'l', 'circle');
    let c1p = makeNewElement('', 'closure_' + closureCounter + "_l_p", 'point');


    let c2 = makeNewElement('', 'c' + closureCounter + 'r', 'circle');
    let c2p = makeNewElement('', 'closure_' + closureCounter + "_r_p", 'point');

    cloBox.appendChild(c1);
    cloBox.appendChild(c2);
    c1.appendChild(c1p);
    c2.appendChild(c2p);

    box.appendChild(cloBox);
    //闭包本体

    let bodyBox = makeNewElement('', 'closureBodyBox_' + closureCounter, 'closureBodyBox');
    let cloFrm = makeNewElement(l[1], 'closureFrame_' + closureCounter, 'envClosureFrame');

    bodyBox.appendChild(cloFrm);
    box.appendChild(bodyBox);
    //return box;
    return [box, 'c' + closureCounter + 'l', 'closure_' + closureCounter + "_l_p", 'closure_' + closureCounter + "_r_p", 'closureFrame_' + closureCounter]
}

function drawClsure(closureCounter, l) {
    //外侧的框
    let box = makeNewElement('', 'envClosureBox' + closureCounter, 'envClosureBox');
    //点对的绘制


    let leftPoint = makeNewElement('', 'c' + closureCounter + 'r', 'circle');



    let cloBox = makeNewElement('', 'closure_' + closureCounter, 'doubleDotBox clearfix');

    let c1 = makeNewElement('', 'c' + closureCounter + 'l', 'circle');
    let c1p = makeNewElement('', 'closure_' + closureCounter + "_l_p", 'point');


    let c2 = makeNewElement('', 'c' + closureCounter + 'r', 'circle');
    let c2p = makeNewElement('', 'closure_' + closureCounter + "_r_p", 'point');

    cloBox.appendChild(c1);
    cloBox.appendChild(c2);
    c1.appendChild(c1p);
    c2.appendChild(c2p);

    box.appendChild(cloBox);
    //闭包本体

    let bodyBox = makeNewElement('', 'closureBodyBox_' + closureCounter, 'closureBodyBox');
    let cloFrm = makeNewElement(l[1], 'closureFrame_' + closureCounter, 'envClosureFrame');

    bodyBox.appendChild(cloFrm);
    box.appendChild(bodyBox);
    //return box;
    return [box, 'c' + closureCounter + 'l', 'closure_' + closureCounter + "_l_p", 'closure_' + closureCounter + "_r_p", 'closureFrame_' + closureCounter]
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
            connector: ['Bezier'],
            anchor: ['Right', 'Bottom']
        });
        conMap.set(envFrame, con);
    }

    connectEnvNameAndEnvFrame(envName, envFrame);
    connectEnvFrameAndGloEnv(envFrame);

    let lst = document.getElementsByTagName('path');
    lst[lst.length - 1].setAttribute('name', 'L' + callFrame.getVmFrame() + 'F' + callFrame.getVmCounter() + '_link');
    lst[lst.length - 1].setAttribute('syn', '1');
    lst[lst.length - 1].setAttribute('type', 'arrow');
    lst[lst.length - 2].setAttribute('name', 'L' + callFrame.getVmFrame() + 'F' + callFrame.getVmCounter() + '_link');
    lst[lst.length - 2].setAttribute('syn', '1');
    lst[lst.length - 2].setAttribute('type', 'line');

}

function makeCloConnection(cloName, cl, clp, crp, body) {

    function connectCloNameAndCloCircleL(cloName, cl) {
        jsPlumb.connect({
            source: cloName,
            target: cl,
            endpoint: ['Dot', { radius: '0' }],
            overlays: [['Arrow', { width: 12, length: 12, location: 1 }]],
            connector: ['Flowchart'],
            anchor: ['Right', 'Left']
        });
    }

    function connectclpToCloBody(clp, body) { //全局环境的名称为：globalEnvironmentFrame
        jsPlumb.connect({
            source: clp,
            target: body,
            endpoint: ['Dot', { radius: '0' }],
            overlays: [['Arrow', { width: 12, length: 12, location: 1 }]],
            connector: ['Flowchart'],
            anchor: ['Bottom', 'Top']
        });
    }

    function connectcrpToGlo(crp) { //全局环境的名称为：globalEnvironmentFrame
        jsPlumb.connect({
            source: crp,
            target: "globalEnvironmentFrame",
            endpoint: ['Dot', { radius: '0' }],
            overlays: [['Arrow', { width: 12, length: 12, location: 1 }]],
            connector: ['Flowchart'],
            anchor: ['Right', 'Bottom']
        });
    }

    connectCloNameAndCloCircleL(cloName, cl);
    connectclpToCloBody(clp, body);
    connectcrpToGlo(crp)

}

function connectAtoBRR(a, b) {

    function connectLinkAndStackTag(a, b) {
        jsPlumb.connect({
            source: a,
            target: b,
            endpoint: ['Dot', { radius: '0' }],
            overlays: [['Arrow', { width: 12, length: 12, location: 1 }]],
            // connector: ['Flowchart'],
            connector: ['Bezier'],
            anchor: ['Right', 'Right']
        });
    }
    connectLinkAndStackTag(a, b);
}

function connectAtoBLR(a, b) {

    function connectLinkAndStackTag(a, b) {
        jsPlumb.connect({
            source: a,
            target: b,
            endpoint: ['Dot', { radius: '0' }],
            overlays: [['Arrow', { width: 12, length: 12, location: 1 }]],
            // connector: ['Flowchart'],
            connector: ['Flowchart'],
            anchor: ['Left', 'Right']
        });
    }
    connectLinkAndStackTag(a, b);
}


function makeLinkConnection(link, stackTag) {

    function connectLinkAndStackTag(link, stackTag) {
        let con = jsPlumb.connect({
            source: link,
            target: stackTag,
            endpoint: ['Dot', { radius: '0' }],
            overlays: [['Arrow', { width: 12, length: 12, location: 1 }]],
            connector: ['Bezier'],
            anchor: ['Left', 'Left']
        });
        conMap.set(link, con);
    }
    connectLinkAndStackTag(link, stackTag);
}


function connectATOB(a, b, aanchor, banchor) {


    function connectLinkAndStackTag(a, b) {
        jsPlumb.connect({
            source: a,
            target: b,
            endpoint: ['Dot', { radius: '0' }],
            overlays: [['Arrow', { width: 12, length: 12, location: 1 }]],
            connector: ['Bezier'],
            anchor: [aanchor, banchor]
        });
    }
    connectLinkAndStackTag(a, b);
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


function drawexpression(exp) {
    let e = document.getElementById("expression");
    e.innerHTML = exp;
}

