/**
 * 新建一个标签
 * @param {String} tag 标签的类型
 * @param {String} content 标签的内容
 * @param {String} attritute 属性
 * @param {String} value 属性值
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