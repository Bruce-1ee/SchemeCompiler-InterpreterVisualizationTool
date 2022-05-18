/**
 * 新建一个标签
 * @param {String} tag 标签的类型
 * @param {String} content 标签的内容
 * @returns 
 */
function makeNewElement(tag, content) {
    let newElement = document.createElement(tag);
    let newContent = document.createTextNode(content);
    newElement.appendChild(newContent);
    return newElement;
}

/**
 * 为一个element设置属性
 * @param {Node} element 
 * @param {String} attribute 
 * @param {String} value 
 */
function setAttribute(element, attribute, value) {
    element.setAttribute(attribute, value);
}

/**
 * 在一个节点内部加入一个新的元素
 * @param {Node} currentNode 
 * @param {Node} element 
 */
function addElementInNode(currentNode, element) {
    //let node = document.getElementById(currentNodeId);
    currentNode.appendChild(element);
}

/**
 * 创建一个新的环境框架
 * @param {number} num 表示当前frame的编号，用于同步显示
 * @param {Array} args 装有这个frame中所有参数的表，需要将其绘制
 */
function makeNewEnvironmentFrame(num, args) {
    let currentNode = document.getElementById("environmentBox");

    let newFrame = makeNewElement("div", "");
    for (var i = 0; i < args.length; i++) {
        let newArgument = makeNewElement("div", args[i]);
        setAttribute(newArgument, "id", "localEnvironmentFrameArgument_" + i);
        setAttribute(newArgument, "class", "localEnvironmentFrameArgument");
        addElementInNode(newFrame, newArgument);
    }
    setAttribute(newFrame, "id", "localEnvironmentFrame_" + num);
    setAttribute(newFrame, "class", "localEnvironmentFrame");

    addElementInNode(currentNode, newFrame);
}

/**
 * 用来实现入栈的操作，新的碎片被插入到了第callFrameNumber位置的frame中
 * @param {number} callFrameNumber 
 * @param {number} stackCounter 
 * @param {String} content 
 */
function makeNewStackFrame(callFrameNumber, stackCounter, content) {
    let currentNode = document.getElementById("callFrame_" + callFrameNumber);

    let newStackFrame = makeNewElement("div", "");
    setAttribute(newStackFrame, "id", "stackFrame_" + stackCounter);
    setAttribute(newStackFrame, "class", "stackFrame");

    let newStack = makeNewElement("div", "");

    let newStackCounter = makeNewElement("div", stackCounter);
    setAttribute(newStackCounter, "id", "stackFrameConter_" + stackCounter);
    setAttribute(newStackCounter, "class", "stackFrameConter");
    let newStackContent = makeNewElement("div", content);
    setAttribute(newStackContent, "id", "stackFrameContent_" + stackCounter);
    setAttribute(newStackContent, "class", "stackFrameContent");

    addElementInNode(newStack, newStackCounter);
    addElementInNode(newStack, newStackContent);

    addElementInNode(newStackFrame, newStack);

    addElementInNode(currentNode, newStackFrame);
}

function makeNewCallFrame(callFrameNumber) {
    let currentNode = document.getElementById("stackBox");

    let newFrame = makeNewElement("div", "");
    setAttribute(newFrame, "id", "callFrame_" + callFrameNumber);
    setAttribute(newFrame, "class", "callFrame");

    addElementInNode(currentNode, newFrame);
}

function deleteStackFrame(callFrameNumber, stackCounter) {
    let parentNode = document.getElementById("callFrame_" + callFrameNumber);
    let currentNode = document.getElementById("stackFrame_" + stackCounter);
    parentNode.removeChild(currentNode);
}

function deleteCallFrame(callFrameNumber) {
    let parentNode = document.getElementById("stackBox");
    let currentNode = document.getElementById("callFrame_" + callFrameNumber);
    parentNode.removeChild(currentNode);
}