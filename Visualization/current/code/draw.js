/**
 * 这个文件里记载了用于绘图的函数
 */


function makeNewSpanElement(content, id, style) {
    let newElement = document.createElement('span');
    let newContent = document.createTextNode(content);
    newElement.setAttribute('id', id);
    newElement.setAttribute('class', style);
    newElement.appendChild(newContent);
    return newElement;
}

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


function makeStaticLinkConnection(link, stackTag) {

    function connectLinkAndStackTag(link, stackTag) {
        jsPlumb.connect({
            source: link,
            target: stackTag,
            endpoint: ['Dot', { radius: '0' }],
            overlays: [['Arrow', { width: 12, length: 12, location: 1 }]],
            connector: ['Bezier', { curviness: 75 }],
            anchor: ['Left', 'Left']
        });
    }
    connectLinkAndStackTag(link, stackTag);
}

function makedynamicLinkConnection(link, stackTag) {

    function connectLinkAndStackTag(link, stackTag) {
        jsPlumb.connect({
            source: link,
            target: stackTag,
            endpoint: ['Dot', { radius: '0' }],
            overlays: [['Arrow', { width: 12, length: 12, location: 1 }]],
            connector: ['Bezier', { curviness: 75 }],
            anchor: ['Right', 'Right']
        });
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
var inteSubCounter = 1;
var inteIndent = 0;
var indentString = " \u00A0 \u00A0";

function addIndent() {
    inteIndent++;
}

function subIndent() {
    inteIndent--;
}

function drawInterpreterInfo(info) {
    var indentSpace = "";
    for (var i = 0; i < inteIndent; i++) {
        indentSpace += indentString;
    }
    info = info.toString();
    if (info.indexOf("'eval-") === -1) { //不包含
        var e = makeNewElement(indentSpace + info, "inteInfo" + inteSubCounter++, "null");
    } else { //包含
        var e = makeNewElement(indentSpace + inteCounter + ". " + info, "inteInfo" + inteCounter++, "null");
    }
    document.getElementById('interpreterInfo').appendChild(e);
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


function drawInterpreterExp(exp) {
    let e = document.getElementById("inteExp");
    exp = exp.toString();
    lst = exp.split('"');
    t = lst.join("");
    e.innerText = t;
}

function drawVMExp(exp) {
    let e = document.getElementById("VMExp");
    exp = exp.toString();
    lst = exp.split('"');
    t = lst.join("");
    e.innerText = t;
}





var str = `("L7" ('if ("L6" ("L5" 'a)) ("L4" ("L3" 'b)) ("L2" ("L1" 'c))))`;

var str2 = "('act-application 1 ('frame ('halt) ('act-args 1 ('act-constant 3 ('constant 33 ('argument ('act-constant 2 ('constant 22 ('argument ('act-constant 1 ('constant 11 ('argument ('act-fun-body 1 ('act-lambda 1 ('functional ('act-if 1 ('act-test 1 ('refer 0 0 ('test ('act-then 1 ('refer 0 1 ('return 4))) ('act-else 1 ('refer 0 2 ('return 4))))))) ('apply))))))))))))))))"
/**
 * 将代码中的L1 L2替换成dom的标签
 * @param {*} program 输入的程序
 * @returns 格式化好的html
 */
function parseCode(program) {

    //传入去掉标签之后的代码，然后获得需要替换的反括号的位置
    function getPosition(str) {
        var num = 1;
        for (var i = 0; i < str.length; i++) {
            if (str[i] === '(') {
                num++;
            } else if (str[i] === ')') {
                num--;
            } else {
                continue;
            }

            if (num === 0) {
                return i;
            }
        }
    }

    /**
     * 
     * @param {*} str 待修改的字符串
     * @param {*} index 修改的位置
     * @param {*} char 待替换的字符串
     * @returns 
     */
    function replace(str, index, char) {
        const strAry = str.split('');
        strAry[index] = char;
        return strAry.join('');
    }

    function main() {

        var lList = program.match(/L\d*/g);

        for (var i = 0; i < lList.length; i++) {
            var label = '"' + lList[i] + '"';
            var strList = program.split(label);
            var pos = getPosition(strList[1]);
            strList[0] = replace(strList[0], strList[0].length - 1, '<span id=' + label + '>')
            strList[1] = replace(strList[1], pos, '</span>');
            program = strList.join("");
        }
        return program;
    }
    return main();
}

/**
 * 将代码中的L1 L2替换成dom的标签
 * @param {*} program 输入的程序
 * @returns 格式化好的html
 */
function parseCodeVM(program) {

    //传入去掉标签之后的代码，然后获得需要替换的反括号的位置
    function getPosition(str) {
        var num = 1;
        for (var i = 0; i < str.length; i++) {
            if (str[i] === '(') {
                num++;
            } else if (str[i] === ')') {
                num--;
            } else {
                continue;
            }

            if (num === 0) {
                return i;
            }
        }
    }

    /**
     * 
     * @param {*} str 待修改的字符串
     * @param {*} index 修改的位置
     * @param {*} char 待替换的字符串
     * @returns 
     */
    function replace(str, index, char) {
        const strAry = str.split('');
        strAry[index] = char;
        return strAry.join('');
    }
    function makeNewLabel(label) {
        var lst = label.split(' ');
        return lst.join("_");
    }
    function main() {
        var lList = program.match(/'act[-[a-zA-Z]+]*[\s][0-9]+/g);
        for (var i = 0; i < lList.length; i++) {
            var label = lList[i];
            var strList = program.split(label);
            var pos = getPosition(strList[1]);
            strList[0] = replace(strList[0], strList[0].length - 1, '<span id="' + makeNewLabel(label) + '">')
            strList[1] = replace(strList[1], pos, '</span>');
            program = strList.join("");
        }
        return program;
    }
    return main();
}

var oldLabel = "";
function getInteLabel(label) {

    if (oldLabel !== "") {
        let old = document.getElementById(oldLabel);
        old.style.backgroundColor = "";
    }
    console.log(label);

    let newEle = document.getElementById(label);
    if (newEle === null) return;
    newEle.style.backgroundColor = "#bfa";
    oldLabel = label;

}
var oldLabelVM = "";
function getVMLabel(actName, number) {

    if (oldLabelVM !== "") {
        let old = document.getElementById(oldLabelVM);
        old.style.backgroundColor = "";
    }
    let newEle = document.getElementById(actName + "_" + number);
    if (newEle === null) return;
    newEle.style.backgroundColor = "#bfa";
    oldLabelVM = actName + "_" + number;

}

var code;
function getProgram(inteCode, VMCode) {
    code = VMCode.toString();
    var icode = inteCode.toString();
    // let e = document.getElementById("code");
    // let evm = document.getElementById("VMcode");
    let e = document.getElementById("inteExp");
    let evm = document.getElementById("VMExp");
    var codeEle = parseCode(icode);
    e.innerHTML = codeEle;
    evm.innerHTML = parseCodeVM(VMCode.toString());
}


