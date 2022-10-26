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
var g;
document.onmouseover = function (e) {
    // console.log("in");
    var e = e ? e : window.event;
    var tar = e.target;

    if (tar.getAttribute('syn') === '1') {
        tagname = tar.getAttribute("name");
        //console.log(tagname.slice(tagname.length - 4))
        if (tagname.slice(tagname.length - 4) === 'link') {
            //link处理
            //箭头变色
            eleList = document.getElementsByName(tagname);
            eleList[1].setAttribute('stroke', '#ea213a');
            eleList[2].setAttribute('stroke', '#ea213a');
            eleList[2].setAttribute('fill', '#ea213a');

            // stackElementFrameNumber_0
            var tarClass = eleList[0].className;
            eleList[0].className = tarClass + 'BESELECTED';
            var n = document.getElementById('stackElementFrameNumber_' + eleList[0].textContent);

            var nClass = n.className;
            n.className = nClass + 'BESELECTED';



        } else {
            eleList = document.getElementsByName(tagname);
            var tarClass;
            for (var o of eleList) {
                tarClass = o.className;
                o.className = tarClass + 'BESELECTED';
                //console.log(o.className);
            }
        }
        backtar.push(tar)


    }
}

document.onmouseout = function (e) {
    // console.log("onmouseout!");
    if (backtar.length != 0) {
        var tar = backtar.shift();
        var tarName = tar.getAttribute("name");
        if (tarName.slice(tarName.length - 4) === 'link') {
            //link处理
            //箭头变色
            arrowList = document.getElementsByName(tagname);
            arrowList[1].setAttribute('stroke', '#456');
            arrowList[2].setAttribute('stroke', '#456');
            arrowList[2].setAttribute('fill', '#456');

            let tarClass = arrowList[0].className.split(" ")[0];
            tarClass = tarClass.slice(0, arrowList[0].className.length - 10);
            arrowList[0].className = tarClass;

            let n = document.getElementById('stackElementFrameNumber_' + arrowList[0].textContent);
            g = n;
            let nClass = n.className;
            nClass = nClass.slice(0, nClass.length - 10);
            n.className = nClass;

        } else {
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
}
