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

    //drawStack(stack, pointer, frameCounter);
}

function next() {
    nextInte();
    nextComp();
}

/**
 * 用biwascheme执行文本框内的程序
 */
var biwac = function () { evl(getInput()); }