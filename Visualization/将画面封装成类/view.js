class View {
    frameLength = 0;


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
    globalEnvironmentFrame = null;
    constructor() {
        let environment = makeNewElement('environment', 'environment', 'environment');
        document.getElementsByTagName("body")[0].appendChild(environment);
        this.globalEnvironmentFrame = new EnvironmentFrame(-1, true);
    }

    createFrame() {

    }
}

class EnvironmentFrame {

    constructor(frameNumber, global = false) {
        if (global === true) {
            let globalEnvironmentFrame = makeNewElement('', 'globalEnvironmentFrame', 'globalEnvironmentFrame');
            document.getElementById('environment').appendChild(globalEnvironmentFrame);
        } else {
            let newglobalEnvironmentFrame = makeNewElement('', 'environmentFrame_' + frameNumber, 'localEnvironmentFrame');
            document.getElementById('environment').appendChild(newglobalEnvironmentFrame);
        }

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
        document.getElementsByTagName("body")[0].appendChild(stack);
    }

    createFrame() {
        let newFrame = new StackFrame(this.frameLength);
        this.frameLength += 1;
        this.frameList.push(newFrame);
    }

    deleteFrame() {
        this.frameList.pop().delete(this.frameLength - 1);
        this.frameLength--;
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
        document.getElementById('stack').appendChild(stackFrame);
    }

    delete(frameLength) {
        popStackFrame(frameLength)
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
        console.log(frameLength); console.log(stackLength)
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
    constructor() {
        let closure = makeNewElement('closure', 'closure', 'closure');
        document.getElementsByTagName("body")[0].appendChild(closure);

    }
}



let view = new View();
let s = view.stack;
s.createFrame();
s.push("test")
s.push("test")
s.push("test")
s.push("test")
s.createFrame();
s.push("test")
s.push("test")
s.push("test")
s.push("test")
