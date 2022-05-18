var onError = function (e) { console.error(e); }
var biwa = new BiwaScheme.Interpreter(onError);
var display = function (result) { console.log(result); }


function evl(exp) { biwa.evaluate(exp, display); }



function getInput() {
    return document.getElementById("programInput").value;
}


function interpreter() {
    evl("(eval '" + getInput() + ")");
}

function compiler() {
    evl("(run '" + getInput() + ")");
}


function nextInte() {
    evl("(exec-k 'ok)");
}

function nextComp() {
    evl("(vm-k 'ok)");
}

var biwac = function () {
    evl(getInput());
}

var impName = ''
var impExpr = ''

var stack;
var pointer = 0;


function removeAllChildren(element) {
    while (element.firstChild) {
        element.removeChild(element.firstChild);
    }
}

function updateStack(s, p) {
    stack = s;
    pointer = p;

    document.getElementById('stack').innerHTML = stack;
    document.getElementById('pointer').innerHTML = pointer;

    var stackframe = document.getElementById("stack-frame");

    function createStack(num) {
        var newstackFrame = document.createElement("div");

        var newstackNum = document.createElement("div");
        var newstackInfo = document.createElement("div");
        var newstackNode = document.createElement("div");

        newstackNum.id = "stackNum_" + num;
        newstackNum.className = "stackNum";
        newstackNum.appendChild(document.createTextNode(num));


        newstackInfo.id = "stackInfo_" + num;
        newstackInfo.className = "stackInfo";
        newstackInfo.appendChild(document.createTextNode(stack[num]));


        newstackNode.id = "stack_" + (num);
        newstackNode.setAttribute("name", newstackNode.id);
        newstackNode.appendChild(newstackNum);
        newstackNode.appendChild(newstackInfo);

        newstackFrame.className = "stackFrameBackground"
        newstackFrame.appendChild(newstackNode);

        stackframe.appendChild(newstackFrame);
    }
    removeAllChildren(stackframe);
    for (var i = 0; i < p; i++) {
        createStack(i);
    }
}

function updateEvalInfo(Name, Expr) {
    impExpr = Expr;
    impName = Name;
    document.getElementById('impName').innerHTML = Name;
    document.getElementById('impExpr').innerHTML = Expr;
}










function test(size) {


}
