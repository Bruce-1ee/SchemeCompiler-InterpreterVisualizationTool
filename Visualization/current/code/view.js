class CallFrame {
    /**
     * frame是一个存储调用层级的计数器，例如[1,2,3]的意思是说，1级的函数调用
     * 了1次，嵌套等级二级的函数调用了2次，嵌套等级三级的调用了三次
     * 
     * 这个类是用来帮助同步程序标记相互一致的frame和参数的
     */
    evalFrame = [];
    evalLevel = [];
    evalPointer = 0;
    vmFrame = [];
    vmLevel = [];
    vmPointer = 0;

    evalCallFunc(num) { //解释器调用新的函数的时候等级上升
        this.evalPointer = num;
        this.evalLevel.push(num);
        if (num >= this.evalFrame.length) {
            this.evalFrame.push([]);
        }
        this.evalFrame[num - 1].push(1);  //1代表活着的函数
    }

    evalReturn(num) { //return的时候等级下降
        let l = this.evalFrame[num - 1].length;
        for (var i = 0; i < l; i++) {
            if (this.evalFrame[num - 1][l - 1 - i] === 1) {
                this.evalFrame[num - 1][l - 1 - i] = 0;
                break;
            }
        }
        this.evalPointer = this.evalLevel.pop();
    }

    getEvalFrame() {
        return this.evalPointer;
    }


    getEvalCounter(mode = 1) {
        let num;
        if (mode === 1) {
            num = this.evalPointer - 1;
        } else if (mode === 2) {
            num = this.evalPointer - 2;
        } else {
            error("bad mode");
        }
        let l = this.evalFrame[num].length;
        for (var i = 0; i < l; i++) {
            if (this.evalFrame[num][l - 1 - i] === 1) {
                return l - i;
            }
        }
    }

    vmCallFunc(num) { //同上
        this.vmPointer = num;
        this.vmLevel.push(num);
        if (num >= this.vmFrame.length) {
            this.vmFrame.push([]);
        }
        this.vmFrame[num - 1].push(1);
    }

    vmReturn() {
        let num = this.vmLevel.pop();
        let l = this.vmFrame[num - 1].length;
        for (var i = 0; i < l; i++) {
            if (this.vmFrame[num - 1][l - 1 - i] === 1) {
                this.vmFrame[num - 1][l - 1 - i] = 0;
                break;
            }
        }
        this.vmPointer = num;
    }

    getVmFrame() {
        return this.vmPointer;
    }

    getVmCounter(mode = 1) {
        let num;
        if (mode === 1) {
            num = this.vmPointer - 1;
        } else if (mode === 2) {
            num = this.vmPointer - 2;
        } else {
            error("bad mode");
        }
        let l = this.vmFrame[num].length;
        for (var i = 0; i < l; i++) {
            if (this.vmFrame[num][l - 1 - i] === 1) {
                return l - i;
            }
        }
    }

}

var callFrame = new CallFrame();


var stackFrameCounter = 0;
var stackArgumentCounter = [];

var envFrameCounter = 0;
var envArgumentCounter = 0;

/**
 * View类是整个画面的类，其中包含了环境、栈、和闭包的表示
 */
class View {
    frameLength = 0;

    /**
     * 构造方法中建立了环境、栈、和闭包的对象
     */
    constructor() {
        this.environment = new Environment();
        this.stack = new Stack();
        this.closure = new Closure();
    }
}

/**
 * 环境
 */
class Environment {

    currentEnv = [];
    constructor() {
        let environment = makeNewElement('environment', 'environment', 'environment');
        document.getElementById('view').appendChild(environment); //控制其生成的位置
        this.frameCounter = 1;
        this.closureCounter = 1;
        this.box = new EnvironmentFrame();

    }

    changeGlobalVariable(frameNumber, varName, val) {
        function makeStr(str) {
            let varname = varName + " : ";
            let start = str.indexOf(varname);
            let end = str.indexOf("</div>", start + varname.length);
            let oldVal = str.substring(start + varname.length, end);
            let lst = str.split(oldVal);
            lst = lst.join(val);
            return lst;
        }

        if (frameNumber === 0) { //全局变量
            let ele = document.getElementById("Variable:" + varName + "_varval");
            ele.innerText = val;
        } else {
            let env = document.querySelector("div[framenumber = " + "'" + frameNumber + "'" + " ]");
            let str = env.innerHTML;
            env.innerHTML = makeStr(str)
        }


    }

    addGlobalVariable(varname, varval) {
        let box = makeNewElement("", 'Variable:' + varname, '');
        let varnameBox = makeNewSpanElement(varname, 'Variable:' + varname + '_varname', '');
        let con = makeNewSpanElement(" : ", '', '');
        let varvalBox = makeNewSpanElement(varval, 'Variable:' + varname + '_varval', '');
        //let varnameBox = makeNewElement(varname + " : " + varval, 'Variable:' + varname + '_varname', '');
        box.appendChild(varnameBox);
        box.appendChild(con);
        box.appendChild(varvalBox);
        let glo = document.getElementById("globalEnvironmentFrame");
        glo.appendChild(box);
    }
    addFrame(varList = [], frameNum, targetNum, syn) {
        this.box.newFrame(this.frameCounter++, varList, frameNum, targetNum, syn);
    }

    /**
     *  
     * @param {list} l 包含参数和函数体的列表
     *                 vars
     *                 body  
     */
    addClosure(l, targetNum) {
        this.box.newClosure(this.closureCounter++, l, targetNum);
    }

    highlightFrame(frameNumber) {
        if (this.currentEnv.length > 0) {
            let old = this.currentEnv.pop();
            let oldClassName = old.className;
            oldClassName = oldClassName.split(" ");
            for (let i = 0; i < oldClassName.length; i++) {
                if (oldClassName[i] === "localEnvironmentFrameCURRENT") {
                    oldClassName.splice(i, 1);
                    oldClassName = oldClassName.join(" ");
                    old.className = oldClassName;
                    break;
                }
            }

        }
        let cur = document.querySelector("div[framenumber = " + "'" + frameNumber + "'" + " ]");
        this.currentEnv.push(cur);
        cur.className += " localEnvironmentFrameCURRENT";
    }
}


class EnvironmentFrame {
    colCounter = 0;
    closureCounter = 0;

    constructor() {

        let globalEnvironmentFrame = makeNewElement('GLOBALENV', 'globalEnvironmentFrame', 'globalEnvironmentFrame');
        globalEnvironmentFrame.setAttribute('frameNumber', 0);
        document.getElementById('environment').appendChild(globalEnvironmentFrame);
        // let box1 = makeNewElement('', "testbox", '');
        // document.getElementById('environment').appendChild(box1);

    }

    newFrame(frameCounter, varList, frameNum, targetNum, syn) {

        if (targetNum === 0) {
            var col = makeNewElement('', "col" + this.colCounter++, 'localEnvironmentColumn');
            document.getElementById('environment').appendChild(col);
        } else {
            var col = document.querySelector("div[framenumber = " + "'" + targetNum + "'" + " ]").parentNode

        }



        //外侧的框
        let box = makeNewElement('', 'localEnvrionmenBox_' + frameCounter, 'localEnvrionmenBox');

        col.appendChild(box)


        // document.getElementById('environment').appendChild(box);



        //document.getElementById('environment').appendChild(box1);

        //环境的名字
        let envName = makeNewElement('env' + frameCounter, 'localEnvrionmenName_' + frameCounter, 'localEnvironmentName');
        // document.getElementById('localEnvrionmenBox_' + frameCounter).appendChild(envName);
        box.appendChild(envName);
        //环境本体的框
        let evnFrm = makeNewElement('', 'localEnvrionmenFrame_' + frameCounter, 'localEnvironmentFrame');
        //同步用
        // L: level F:frame
        evnFrm.setAttribute('name', 'L' + callFrame.getEvalFrame() + 'F' + callFrame.getEvalCounter());
        evnFrm.setAttribute('EnvironmentFrame', 'L' + callFrame.getEvalFrame() + 'F' + callFrame.getEvalCounter());
        evnFrm.setAttribute('syn', '1');
        evnFrm.setAttribute('synframenumber', syn);

        evnFrm.setAttribute('frameNumber', frameNum);
        // document.getElementById('localEnvrionmenBox_' + frameCounter).appendChild(evnFrm);
        box.appendChild(evnFrm);


        connectAtoBLR(envName, evnFrm);
        let myself = document.querySelector("div[framenumber = " + "'" + frameNum + "'" + " ]").getAttribute("id");
        let target = document.querySelector("div[framenumber = " + "'" + targetNum + "'" + " ]").getAttribute("id");


        if (targetNum === 0) {
            // connectATOB(myself, target, 'Right', 'Bottom')
            connectATOB(myself, target, 'Right', 'BottomRight')
        } else
        //  if (frameNum - 1 === targetNum) {
        //     connectATOB(myself, target, 'Top', 'Bottom')
        // } else 
        {
            connectATOB(myself, target, 'Right', 'Right')
        }

        // if (callFrame.evalPointer == 1) {
        //     makeLocalEnvConnection('localEnvrionmenName_' + frameCounter, 'localEnvrionmenFrame_' + frameCounter);

        // } else {
        //     let lst = document.getElementsByName('L' + (callFrame.evalPointer - 1) + 'F' + callFrame.getEvalCounter(2));
        //     console.log(lst);
        //     for (var i = 0; i < lst.length; i++) {
        //         if (lst[i].getAttribute("environmentframe") != null) {
        //             parent = lst[i].getAttribute("id");
        //             connectAtoBLR(envName, evnFrm)
        //             connectAtoBRR(evnFrm, parent)
        //             break;
        //         }
        //     }
        // }





        //将所有参数插入
        for (var i = 0; i < varList.length; i++) {
            let v = makeNewElement(varList[i], 'frame' + frameCounter + ' variable' + i, 'envVirable');
            v.setAttribute('syn', '1');
            // v.setAttribute('name', 'L' + callFrame.getEvalFrame() + 'F' + callFrame.getEvalCounter() + 'A' + (varList.length - 1 - i));
            v.setAttribute('name', 'F' + syn + 'A' + (varList.length - 1 - i));
            // document.getElementById('localEnvrionmenFrame_' + frameCounter).appendChild(v);
            evnFrm.appendChild(v);
        }
    }

    newClosure(closureCounter, l, targetNum) {

        if (targetNum === 0) {
            var col = makeNewElement('', "col" + this.colCounter++, 'localEnvironmentColumn');
            document.getElementById('environment').appendChild(col);
        } else {

            var col = document.querySelector("div[framenumber = " + "'" + targetNum + "'" + " ]").parentNode

        }




        //外侧的框
        let box = makeNewElement('', 'closureBox_' + closureCounter, 'localEnvrionmenBox');
        col.appendChild(box)
        // document.getElementById('environment').appendChild(box);
        //闭包的名字
        let cloName = makeNewElement('clo' + closureCounter, 'closureName_' + closureCounter, 'localEnvironmentName');
        //document.getElementById('closureBox_' + closureCounter).appendChild(cloName);
        box.appendChild(cloName);

        // let closureBox = makeNewElement('', 'c' + closureCounter + 'box', 'envClosureFrame');

        // let doubleDotBox = makeNewElement('', 'c' + closureCounter + 'db', 'doubleDotBox');
        // let leftPoint = makeNewElement('', 'c' + closureCounter + 'lp', 'point');
        // let leftCircle = makeNewElement('', 'c' + closureCounter + 'lc', 'circle');

        // leftCircle.appendChild(leftPoint);
        // doubleDotBox.appendChild(leftCircle);

        // let rightPoint = makeNewElement('', 'c' + closureCounter + 'rp', 'point');
        // let rightCircle = makeNewElement('', 'c' + closureCounter + 'rc', 'circle');

        // rightCircle.appendChild(rightPoint);
        // doubleDotBox.appendChild(rightCircle);

        // let closureBody = makeNewElement(l[1], 'c' + closureCounter + 'bd', 'envClosureBody');

        // let doubleDotBoxBox = makeNewElement('', '', 'clearfix');

        // doubleDotBoxBox.appendChild(doubleDotBox)

        // closureBox.appendChild(doubleDotBoxBox);
        // closureBox.appendChild(closureBody);

        //let lst = drawClsure(closureCounter, l);
        //box.appendChild(lst[0]);

        let closureFrame = makeNewElement('', 'closureFrame_' + closureCounter, 'envClosureFrame');
        let code = parseCode(l[1].toString())
        //let closureElement = makeNewElement(l[1], 'closureElement', 'envClosureBody');
        let closureElement = makeNewElement("", 'closureElement', 'envClosureBody');
        closureElement.innerHTML = code;
        closureFrame.appendChild(closureElement);

        box.appendChild(closureFrame);

        connectAtoBLR(cloName, closureFrame);
        let myself = closureFrame.getAttribute("id");
        let target = document.querySelector("div[framenumber = " + "'" + targetNum + "'" + " ]").getAttribute("id");

        if (targetNum === 0) {
            // connectATOB(myself, target, 'Right', 'Bottom')
            connectATOB(myself, target, 'Right', 'BottomRight')
        } else {
            connectATOB(myself, target, 'Right', 'Right')
        }

        //makeCloConnection('closureName_' + closureCounter, lst[1], lst[2], lst[3], lst[4]);


    }
}




/**
 * 栈
 */
class Stack {

    /**
     * stackLength 是存放栈长度的变量，
     * frameLength 是存放frame长度的变量，用于设定新生成的frame的属性
     * frameList 中存放着StackFrame类的对象
     */
    stackLength = 0;
    frameLength = 0;
    frameList = [];

    stackCounter = 1;

    //实例化View的是会调用一次这个构造方法
    //其作用是创建一个div并将其id、style、和name都设置为stack
    //然后将其插入到view节点之中
    constructor() {
        let stack = makeNewElement('stack', 'stack', 'stack');
        document.getElementById('view').appendChild(stack);
        //document.getElementsByTagName("body")[0].appendChild(stack); //已经弃用，当前通过寻找view元素来确定插入点
    }

    /**
     * 插入stack元素之前需要先生成一个frame去容纳它们
     * 其中frame是另一个名叫StackFrame的类
     * frame的编号是自动维护的
     */
    createFrame() {
        let newFrame = new StackFrame(this.frameLength, this.stackCounter++);
        this.frameLength += 1;
        this.frameList.push(newFrame);
        //为了和环境模型生成相同编号所引入的变量
        stackArgumentCounter.push(0);
    }

    /**
     * 当函数return的时候这个方法将会被调用
     * frame的编号是自动维护的
     */
    deleteFrame() {
        this.frameList.pop().delete(this.frameLength - 1);
        this.frameLength--;
        stackArgumentCounter.pop();
    }

    push(ele, type) {
        this.frameList[this.frameLength - 1].pushElement(ele, this.stackLength, this.frameLength - 1, type, this.stackCounter);
        this.stackLength += 1;
    }

    pop() {
        this.frameList[this.frameLength - 1].popElement(this.frameLength - 1, this.stackLength - 1);
        this.stackLength -= 1;
    }
}

class StackFrame {

    elementList = [];

    linkNodeList = [];

    frameNodeList = [];

    constructor(number, stackCounter) {
        let stackFrame = makeNewElement('', 'stackFrame_' + number, 'stackFrame');
        //为了同步显示新加入的全局变量
        stackFrame.setAttribute('syn', '1');
        stackFrame.setAttribute('name', 'L' + callFrame.getVmFrame() + 'F' + callFrame.getVmCounter());

        stackFrame.setAttribute('synframenumber', stackCounter);

        document.getElementById('stack').appendChild(stackFrame);

    }

    delete(frameLength) {
        popStackFrame(frameLength)
    }

    pushElement(ele, number, frameNumber, type, stackCounter) {
        let newElement = new StackElementFrame(ele, number, frameNumber, type);
        this.elementList.push(newElement);
        this.frameLength += 1;
        if (type === 'argument') {
            let t = document.getElementById('StackElementFrameContent_' + number);
            t.setAttribute('syn', '1');
            t.setAttribute("name", 'F' + (stackCounter - 1) + "A" + stackArgumentCounter[stackArgumentCounter.length - 1]);
            stackArgumentCounter[stackArgumentCounter.length - 1]++;
        }
        if (type === 'link') {
            let t = document.getElementById('StackElementFrameContent_' + number);
            t.setAttribute('syn', '1');
            t.setAttribute('name', 'L' + callFrame.getVmFrame() + 'F' + callFrame.getVmCounter() + '_link')

            makeStaticLinkConnection(t.id, 'stackElementFrameNumber_' + t.textContent)

            let self = document.getElementsByTagName("path");
            self = self[self.length - 1];
            let p = self.parentElement
            self = p;
            p = self.parentElement;
            this.linkNodeList.push([p, self])
        }
        if (type === 'frame') {
            let t = document.getElementById('StackElementFrameContent_' + number);
            t.setAttribute('syn', '1');
            t.setAttribute('name', 'L' + callFrame.getVmFrame() + 'F' + callFrame.getVmCounter() + '_link')

            makedynamicLinkConnection(t.id, 'StackElementFrameContent_' + t.textContent)

            let self = document.getElementsByTagName("path");
            self = self[self.length - 1];
            let p = self.parentElement
            self = p;
            p = self.parentElement;
            this.frameNodeList.push([p, self])
        }
    }

    popElement(frameLength, stackLength) {

        let currentNode = document.getElementById("stackElementFrame_" + stackLength);
        let contentNode = currentNode.children[1];

        let type = contentNode.getAttribute('type');
        if (type === 'link') {

            var l = this.linkNodeList.pop();
            console.log(l)
            l[0].removeChild(l[1])
        }
        if (type === 'frame') {

            var l = this.frameNodeList.pop();
            console.log(l)
            l[0].removeChild(l[1])
        }
        this.elementList.pop().delete(frameLength, stackLength);
    }
}

class StackElementFrame {
    stackElementFrameNumber = null;
    stackElementFrameContent = null;
    constructor(ele, number, frameNumber, type) {
        let newStackElementFrame = new makeNewElement('', 'stackElementFrame_' + number, 'stackElementFrame');
        document.getElementById('stackFrame_' + frameNumber).appendChild(newStackElementFrame);
        self.stackElementFrameNumber = new StackElementFrameNumber(number)
        self.stackElementFrameContent = new StackElementFrameContent(ele, number, type);
    }

    delete(frameLength, stackLength) {

        popStackElement(frameLength, stackLength);
    }

}

class StackElementFrameNumber {

    constructor(satckNumber) {
        let elementNumber = makeNewElement(satckNumber, 'stackElementFrameNumber_' + satckNumber, 'stackElementFrameNumber');
        document.getElementById('stackElementFrame_' + satckNumber).appendChild(elementNumber);
    }

}

class StackElementFrameContent {
    constructor(ele, number, type) {
        //console.log(type)
        let elementContent = makeNewElement(ele, 'StackElementFrameContent_' + number, 'StackElementFrameContent');
        elementContent.setAttribute('oncontextmenu', 'menuFun()');

        elementContent.setAttribute('type', type);
        document.getElementById('stackElementFrame_' + number).appendChild(elementContent);
    }
    delete(frameLength, stackLength) {
        popStackElement(frameLength, stackLength)
    }
}


class Closure {

    closureCounter = 0;

    constructor() {
        let closure = makeNewElement('closure', 'closure', 'closure');
        document.getElementById('view').appendChild(closure);
        //document.getElementsByTagName("body")[0].appendChild(closure);

    }

    createClosure(eleList) {
        let eleCoutner = 0;
        //let body = eleList[0];
        let closureFrame = makeNewElement("", "C" + this.closureCounter++, 'closureFrame clearfix');
        let cloName = makeNewElement("clo" + this.closureCounter + ":", "C" + this.closureCounter + "E" + eleCoutner++, 'closureElement');
        closureFrame.appendChild(cloName);
        for (let i = 0; i < eleList.length; i++) {
            let closureElement = makeNewElement(eleList[i], "C" + this.closureCounter + "E" + eleCoutner++, 'closureElement');
            closureFrame.appendChild(closureElement);
        }
        document.getElementById('closure').appendChild(closureFrame);
    }

}



// let view = new View();
// let s = view.stack;
// s.createFrame();
// s.push("test")
// s.push("test")
// s.push("test")
// s.push("test")
// s.createFrame();
// s.push("test")
// s.push("test")
// s.push("test")
// s.push("test")

let view = new View();



