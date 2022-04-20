var frameJson = {
    stack: ["first", "second", "third"],

}

/**
 * 全局变量定义
 */
//记录当前
var stackNumber = 0;



function myCreateContent(text) {
    return document.createTextNode(text);
}

function myCreateElement(ElementTag) {
    return document.createElement(ElementTag);
}

//生成一片stack ： 包括左侧的编号和右侧的stack 以及包裹这两者的元素
/**
 * @param {number} stackNumber 当前绘制stack的位置
 * @param {string} stackInfo 在stack中显示的内容
 */
function createStackElement(stackNumber, stackInfo, syn = 0) {
    /**
     * newStackNum:stack编号，左侧的元素
     * newStackInfo:stack的内容，右侧的元素
     * newStackNode:包裹一个stack的元素
     *  newStackNode
     *      -newStackNum
     *      -newStackInfo
     */
    var newStackNum = myCreateElement("div");
    var newStackInfo = myCreateElement("div");
    var newStackNode = myCreateElement("div");
    //设置stack计数
    newStackNum.id = "stackNum_" + stackNumber;
    newStackNum.className = "stackNum";
    newStackNum.appendChild(myCreateContent(stackNumber));
    //设置stack信息
    newStackInfo.id = "stackInfo_" + stackNumber;
    if (syn == 1) {  //当syn为1时，这个元素是可以同时高亮显示的
        newStackInfo.setAttribute("syn", "1");
    }
    newStackInfo.className = "stackInfo";
    newStackInfo.appendChild(myCreateContent(stackInfo));
    //设置包装元素
    newStackNode.id = "stack_" + stackNumber;
    newStackNode.setAttribute("name", newStackNode.id);
    //包装两个元素
    newStackNode.appendChild(newStackNum);
    newStackNode.appendChild(newStackInfo);
    return newStackNode;
}

/**
 * 参数未完成
 */
function createStackFrame() {
    var newStackFrame = myCreateElement("div");
    var newStackElement;

    newStackFrame.className = "stackFrameBackground";
    newStackFrame.setAttribute("name", "frame " + framenum);
    newStackFrame.setAttribute("syn", "1");

    for (var i = 0; i < frame.length; i++) {
        newStackElement = createStackElement(stackNumber, stackInfo, syn);
        newStackFrame.appendChild(newStackElement);
    }

    return newStackFrame;
}

/**
 * 参数未完成
 */
function appendStackLayout() {
    var wholeStack = document.getElementById("stackLayout");
    var newStackFrame;

    for (var i = 0; i < frames.length; i++) {
        newStackFrame = createStackFrame()
        wholeStack.appendChild(newStackFrame);
    }

}































function isSyn(name) {
    var list = ["frame", "frame1"];
    return list.includes(name)
}

var backtar = [];
var i = 0;
var framenum = 1;
var varnum = 1;



document.onmouseover = function (e) {
    var e = e ? e : window.event;
    var tar = e.srcElement || e.target;
    if (tar.getAttribute("syn") == "1") {
        var tarName = tar.getAttribute("name");
        // var tarClass = tar.className;
        var allObj = document.getElementsByName(tarName);
        var tarClass;
        for (var o of allObj) {
            tarClass = o.className;
            o.className = "selected" + tarClass;
            console.log(o.className);

        }
        backtar.push(tar);
    }
}
document.onmouseout = function (e) {
    if (backtar.length != 0) {
        var tar = backtar.shift();
        var tarName = tar.getAttribute("name");
        var allObj = document.getElementsByName(tarName);
        var tarClass;
        for (var o of allObj) {
            tarClass = o.className.slice(8, o.className.length);
            // var backClass = o.className.slice(8, tar.className.length);
            o.className = tarClass;
        }
    }
}





function fn() {
    var wholestack = document.getElementById("stackLayout");
    var wholeenv = document.getElementById("environmentGraphBackground");


    var newstackFrame = myCreateElement("div");

    function createStack(j) {

        var newstackNum = myCreateElement("div");
        var newstackInfo = myCreateElement("div");
        var newstackNode = myCreateElement("div");

        newstackNum.id = "stackNum_" + i;
        newstackNum.className = "stackNum";
        newstackNum.appendChild(myCreateContent(i));

        if (j == 2) {
            newstackInfo.id = "stackInfo_" + i;
            newstackInfo.className = "stackInfo";
            newstackInfo.appendChild(myCreateContent(" var " + "_ " + varnum));
            newstackInfo.setAttribute("name", "var " + varnum);
            newstackInfo.setAttribute("syn", "1");
        } else {
            newstackInfo.id = "stackInfo_" + i;
            newstackInfo.className = "stackInfo";
            newstackInfo.appendChild(myCreateContent(" frame " + framenum));
        }

        newstackNode.id = "stack_" + (i++);
        newstackNode.setAttribute("name", newstackNode.id);
        newstackNode.appendChild(newstackNum);
        newstackNode.appendChild(newstackInfo);

        newstackFrame.className = "stackFrameBackground"
        newstackFrame.setAttribute("name", "frame " + framenum);
        newstackFrame.setAttribute("syn", "1");

        newstackFrame.appendChild(newstackNode);
    }

    function createEnvironmentGraph() {
        var newenv = myCreateElement("div");
        var newvar = myCreateElement("div");

        newvar.className = "var";
        newvar.setAttribute("name", "var " + varnum);
        newvar.appendChild(myCreateContent("var_" + varnum));
        newvar.setAttribute("syn", "1");

        newenv.className = "environmentFrame";
        newenv.setAttribute("name", "frame " + framenum);
        newenv.setAttribute("syn", "1");

        newenv.appendChild(newvar);
        wholeenv.appendChild(newenv);

    }


    for (var j = 0; j < 4; j++) {
        createStack(j);
    }
    createEnvironmentGraph();
    varnum++;
    framenum++;



    wholestack.appendChild(newstackFrame);

}

