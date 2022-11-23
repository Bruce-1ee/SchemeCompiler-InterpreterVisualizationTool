/**
 * 定义了biwascheme的解释函数evl
 */
var onError = function (e) { console.error(e); }
var biwa = new BiwaScheme.Interpreter(onError);
var display = function (result) { console.log(result); }
function evl(exp) { biwa.evaluate(exp, display); }


/**
 * 获取文本框内的文字，用于临时获取scheme程序用
 */
function getInput() {
    return document.getElementById("programInput").value;
}

var txt;
function temp() {
    txt = getInput();
}


/**
 * 定义了编译、解释、VM下一步、解释器下一步
 * 用于控制程序执行
 */
function interpreter() {
    evl("(eval '" + getInput() + ")");
}

function compiler() {
    evl("(run '" + getInput() + ")");
}

function nextInte() {
    evl("(exec-k 'ok)");
}

function start() {
    interpreter();
    compiler();
}

function nextComp() {
    evl("(vm-k 'ok)");
    // drawEnv([])

    drawStack(stack, pointer, frameCounter);
}

function next() {
    nextInte();
    nextComp();
}




/**
 * 用biwascheme执行文本框内的程序
 */
var biwac = function () { evl(getInput()); }


/////////////////////////////////////////////
////////////////////分界线////////////////////
/////////////////////////////////////////////


/**
 * 以下是sheme和js通信的程序
 */

var impName = ''    //正在执行的操作名   eg:constant
var impExpr = ''    //正在执行的操作内容 eg:1

var stack;          //堆栈 scheme:stack
var pointer = 0;    //栈的指针 scheme:s
var old_pointer = 0;

var frameCounter = 0; //frame计数器
var old_frameCounter = 0;

var columnCounter = 0; //存放函数frame的柱子的编号
var columnSize = 0; //当前column中元素的数量
const maxSize = 5; //最大可存放数量

var argument = new Array(2); //用于存放函数调用的参数


var evalFlag = false;   //接收完毕标志
var vmFlag = false;     //接收完毕标志

/**
 * 更新解释器执行数据
 * @param {String} Name 
 * @param {String} Expr 
 */
function updateEvalInfo(Name, Expr) {
    impExpr = Expr;
    impName = Name;
    // document.getElementById('impName').innerHTML = Name;
    // document.getElementById('impExpr').innerHTML = Expr;
    evalFlag = true;
}

/**
 * 更新VM的执行数据
 * @param {String} schemeStack 
 * @param {String} s 
 */
function updateVmInfo(schemeStack, s) {
    stack = schemeStack;
    pointer = s;
    vmFlag = true;
}

/**
 * 通过scheme程序的“frame”指令来判断
 * 新函数的调用，此时frameCounter需要自增
 */
function addFrameCounter() {
    frameCounter++;
}

/**
 * 通过scheme程序的
 * “apply” -- primitive函数的执行
 * “return” -- 组合函数的执行
 * 来判断弹出堆栈的时机
 * 此时frameCounter需要自减
 */
function subFrameCounter() {
    frameCounter--;
}

function getArgumentsFromScheme(args, vals, body, type) {
    var t = new Array(3);
    if (type === 0) {//type == 0 : primitive
        t[0] = null;
    } else { //type == 1 : funciton
        var argList = [];
        while (args !== undefined) {
            argList.push(args.car);
            args = args.cdr;
        }
        t[0] = [];
        for (var i = 0; i < argList.length - 1; i++) {
            t[0].push(argList[i].name);
        }
    }
    var valList = [];
    while (vals !== undefined) {
        valList.push(vals.car);
        vals = vals.cdr;
    }
    valList.pop();
    t[1] = valList;

    t[2] = body;
    argument[0] = [];
    for (var i = 0; i < t[0].length; i++) {
        argument[0].push(t[0][i] + " : " + t[1][i])
    }
    argument[1] = t[2];

    console.log(argument);

}

function makeEnvrionmentFrame() {
    makeEnvNameTag(frameCounter)
    let s = ("localEnvironmentName_" + frameCounter);
    makeNewEnvironmentFrame(frameCounter, argument[0]);
    let t = ("localEnvironmentFrame_" + frameCounter++);
    makeEnvConnection(s, t)
    let g = "globalEnvironmentFrame"
    makeGloEnvConnection(t, g)
}


function updatePage() {

    while (vmFlag !== true);

    vmFlag = false;


}

























function removeAllChildren(element) {
    while (element.firstChild) {
        element.removeChild(element.firstChild);
    }
}

function updateStack(s, p) {
    stack = s;
    pointer = p;

    // document.getElementById('stack').innerHTML = stack;
    // document.getElementById('pointer').innerHTML = pointer;

    // var stackframe = document.getElementById("stack-frame");

    // function createStack(num) {
    //     var newstackFrame = document.createElement("div");

    //     var newstackNum = document.createElement("div");
    //     var newstackInfo = document.createElement("div");
    //     var newstackNode = document.createElement("div");

    //     newstackNum.id = "stackNum_" + num;
    //     newstackNum.className = "stackNum";
    //     newstackNum.appendChild(document.createTextNode(num));


    //     newstackInfo.id = "stackInfo_" + num;
    //     newstackInfo.className = "stackInfo";
    //     newstackInfo.appendChild(document.createTextNode(stack[num]));


    //     newstackNode.id = "stack_" + (num);
    //     newstackNode.setAttribute("name", newstackNode.id);
    //     newstackNode.appendChild(newstackNum);
    //     newstackNode.appendChild(newstackInfo);

    //     newstackFrame.className = "stackFrameBackground"
    //     newstackFrame.appendChild(newstackNode);

    //     stackframe.appendChild(newstackFrame);
    // }
    // removeAllChildren(stackframe);
    // for (var i = 0; i < p; i++) {
    //     createStack(i);
    // }
}












function test(size) {


}
