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
    let p = "(next 'ok)"
    evl(p);
}

/**
 * 用biwascheme执行文本框内的程序
 */
var biwac = function () { evl(getInput()); }

var breakpointOn = function () { evl('(breakpoint-on)'); }
var breakpointOff = function () { evl('(breakpoint-off)'); }





/**
 * 同步闪烁程序
 */


var backtar = [];

document.onmouseover = function (e) {
    // console.log("in");
    var e = e ? e : window.event;
    var tar = e.target;

    eleList = document.getElementsByName(tar.getAttribute("name"));

    if (eleList.length > 1) {
        console.log('in');
        var tarClass;
        for (var o of eleList) {
            tarClass = o.className;
            o.className = tarClass + 'BESELECTED';
            console.log(o.className);
        }
        backtar.push(tar)

    }
}

document.onmouseout = function (e) {
    // console.log("onmouseout!");
    if (backtar.length != 0) {
        console.log('out')
        var tar = backtar.shift();
        var tarName = tar.getAttribute("name");
        var allObj = document.getElementsByName(tarName);
        var tarClass;
        for (var o of allObj) {
            c = o.className.split(" ")[0];
            tarClass = c.slice(0, o.className.length - 10);
            // var backClass = o.className.slice(8, tar.className.length);
            o.className = tarClass;
        }
    }
}
