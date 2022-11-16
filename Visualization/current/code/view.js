/**
 * 这个文件里记载了关于画面类的定义及其方法
 */
class AST {
    tree = [];
    pointer = 0;

    levelUp() {
        this.pointer++;
        if (this.pointer >= this.tree.length) {
            this.tree.push(0);
        }
        this.tree[this.pointer - 1]++;
    }

    levelDown() {
        this.pointer--;
    }

    //// L: level F:frame [L,F]
    current() {
        return [this.pointer - 1, this.tree[this.pointer - 1]];
    }
}

var ast = new AST();
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

    constructor() {
        let environment = makeNewElement('environment', 'environment', 'environment');
        document.getElementById('view').appendChild(environment); //控制其生成的位置
        //document.getElementsByTagName("body")[0].appendChild(environment); 
        this.frameCounter = 1;
        this.closureCounter = 1;
        this.box = new EnvironmentFrame();

    }

    addFrame(varList = []) {
        this.box.newFrame(this.frameCounter, varList);
        this.frameCounter++;
    }

    /**
     *  
     * @param {list} l 包含参数和函数体的列表
     *                 vars
     *                 body  
     */
    addClosure(l) {
        this.box.newClosure(this.closureCounter++, l);
    }
}


class EnvironmentFrame {

    closureCounter = 0;

    constructor() {
        let globalEnvironmentFrame = makeNewElement('', 'globalEnvironmentFrame', 'globalEnvironmentFrame');
        document.getElementById('environment').appendChild(globalEnvironmentFrame);
    }
    newFrame(frameCounter, varList) {
        //外侧的框
        let box = makeNewElement('', 'localEnvrionmenBox_' + frameCounter, 'localEnvrionmenBox');
        document.getElementById('environment').appendChild(box);
        //环境的名字
        let envName = makeNewElement('env' + frameCounter, 'localEnvrionmenName_' + frameCounter, 'localEnvironmentName');
        document.getElementById('localEnvrionmenBox_' + frameCounter).appendChild(envName);
        //环境本体的框
        let evnFrm = makeNewElement('', 'localEnvrionmenFrame_' + frameCounter, 'localEnvironmentFrame');
        //同步用
        // L: level F:frame
        evnFrm.setAttribute('name', 'L' + ast.current()[0] + 'F' + ast.current()[1]);
        evnFrm.setAttribute('syn', '1');
        document.getElementById('localEnvrionmenBox_' + frameCounter).appendChild(evnFrm);

        makeLocalEnvConnection('localEnvrionmenName_' + frameCounter, 'localEnvrionmenFrame_' + frameCounter);


        //将所有变量插入
        for (var i = 0; i < varList.length; i++) {
            let v = makeNewElement(varList[i], 'frame' + frameCounter + ' variable' + i, 'envVirable');
            v.setAttribute('syn', '1');
            v.setAttribute('name', 'L' + ast.current()[0] + 'F' + ast.current()[1] + 'A' + (varList.length - 1 - i));
            document.getElementById('localEnvrionmenFrame_' + frameCounter).appendChild(v);
        }
    }

    newClosure(closureCounter, l) {
        //外侧的框
        let box = makeNewElement('', 'closureBox_' + closureCounter, 'localEnvrionmenBox');
        document.getElementById('environment').appendChild(box);
        //闭包的名字
        let cloName = makeNewElement('clo' + closureCounter, 'closureName_' + closureCounter, 'localEnvironmentName');
        //document.getElementById('closureBox_' + closureCounter).appendChild(cloName);
        box.appendChild(cloName);
        //     //闭包本体的框
        //     let cloFrm = makeNewElement(l[1], 'closureFrame_' + closureCounter, 'localEnvironmentFrame');

        //     // //同步用 待修改
        //     // // L: level F:frame
        //     // evnFrm.setAttribute('name', 'L' + ast.current()[0] + 'F' + ast.current()[1]);
        //     // evnFrm.setAttribute('syn', '1');
        //     document.getElementById('closureBox_' + closureCounter).appendChild(cloFrm);

        //     makeCloConnection('closureName_' + closureCounter, 'closureFrame_' + closureCounter);

        //     //将所有变量插入
        //     console.log(l);
        //     for (var i = 0; i < l[0].length; i++) {
        //         let v = makeNewElement(l[0][i], 'C' + closureCounter + ' A' + i, 'envVirable');
        //         // v.setAttribute('syn', '1');
        //         // v.setAttribute('name', 'L' + ast.current()[0] + 'F' + ast.current()[1] + 'A' + (varList.length - 1 - i));
        //         document.getElementById('closureFrame_' + closureCounter).appendChild(v);
        //     }
        let lst = drawClsure(closureCounter, l);
        box.appendChild(lst[0]);

        makeCloConnection('closureName_' + closureCounter, lst[1], lst[2], lst[3], lst[4]);


    }
}




/**
 * 栈
 */
class Stack {
    stackLength = 0;
    frameLength = 0;
    frameList = [];

    constructor() {
        let stack = makeNewElement('stack', 'stack', 'stack');
        document.getElementById('view').appendChild(stack);
        //document.getElementsByTagName("body")[0].appendChild(stack);
    }

    createFrame() {
        //新加入
        ast.levelUp();
        let newFrame = new StackFrame(this.frameLength);
        this.frameLength += 1;
        this.frameList.push(newFrame);
        stackArgumentCounter.push(0);
    }

    deleteFrame() {
        //新加入
        ast.levelDown();
        this.frameList.pop().delete(this.frameLength - 1);
        this.frameLength--;
        stackArgumentCounter.pop();
    }

    pushArgument(ele) {
        this.frameList[this.frameLength - 1].pushArgument(ele, this.stackLength, this.frameLength - 1);
        this.stackLength += 1;
    }

    pushStaticLink(ele) {
        this.frameList[this.frameLength - 1].pushStaticLink(ele, this.stackLength, this.frameLength - 1);
        this.stackLength += 1;
    }

    push(ele) {
        this.frameList[this.frameLength - 1].pushElement(ele, this.stackLength, this.frameLength - 1);
        this.stackLength += 1;
    }

    pop() {
        this.frameList[this.frameLength - 1].popElement(this.frameLength - 1, this.stackLength - 1);
        this.stackLength -= 1;
    }
}

class StackFrame {

    elementList = [];

    constructor(number) {
        let stackFrame = makeNewElement('', 'stackFrame_' + number, 'stackFrame');
        //为了同步显示新加入的全局变量
        stackFrame.setAttribute('syn', '1');
        stackFrame.setAttribute('name', 'L' + ast.current()[0] + 'F' + ast.current()[1]);
        document.getElementById('stack').appendChild(stackFrame);
    }

    delete(frameLength) {
        popStackFrame(frameLength)
    }

    //为了同步显示新加入的方法，可以和pushElement合并成同一个，待修改
    pushArgument(ele, number, frameNumber) {
        let newElement = new StackElementFrame(ele, number, frameNumber);
        this.elementList.push(newElement);
        this.frameLength += 1;
        let t = document.getElementById('StackElementFrameContent_' + number);
        t.setAttribute('syn', '1');
        t.setAttribute("name", 'L' + ast.current()[0] + 'F' + ast.current()[1] + "A" + stackArgumentCounter[stackArgumentCounter.length - 1]);
        stackArgumentCounter[stackArgumentCounter.length - 1]++;
    }

    pushStaticLink(ele, number, frameNumber) {
        let newElement = new StackElementFrame(ele, number, frameNumber);
        this.elementList.push(newElement);
        this.frameLength += 1;

        let t = document.getElementById('StackElementFrameContent_' + number);
        t.setAttribute('syn', '1');
        t.setAttribute('name', 'L' + ast.current()[0] + 'F' + ast.current()[1] + '_link')



    }

    pushElement(ele, number, frameNumber) {
        let newElement = new StackElementFrame(ele, number, frameNumber);
        this.elementList.push(newElement);
        this.frameLength += 1;
    }

    popElement(frameLength, stackLength) {
        this.elementList.pop().delete(frameLength, stackLength);
    }
}

class StackElementFrame {
    stackElementFrameNumber = null;
    stackElementFrameContent = null;
    constructor(ele, number, frameNumber) {
        let newStackElementFrame = new makeNewElement('', 'stackElementFrame_' + number, 'stackElementFrame');
        document.getElementById('stackFrame_' + frameNumber).appendChild(newStackElementFrame);
        self.stackElementFrameNumber = new StackElementFrameNumber(number)
        self.stackElementFrameContent = new StackElementFrameContent(ele, number, frameNumber);
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
    elementName = '';
    elementValue = 0;

    constructor(ele, number) {
        let elementContent = makeNewElement(ele, 'StackElementFrameContent_' + number, 'StackElementFrameContent');
        document.getElementById('stackElementFrame_' + number).appendChild(elementContent);
    }

    delete(frameLength, stackLength) {
        popStackElement(frameLength, stackLength)
    }

    blinking() {
        console.log("blinking");
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
        let closureFrame = makeNewElement("", "C" + this.closureCounter++, 'closureFrame');
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

