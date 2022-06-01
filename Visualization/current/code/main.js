
/**
 * 绘制每一帧的stack，目前由interpreterlib.js/nextComp()函数控制
 * 之后需改成控制系统控制
 * @param {stack} s Stack 
 * @param {number} pointer 
 * @param {number} frameCounter 
 */
function drawStack(s, pointer, frameCounter) {

    if (old_frameCounter < frameCounter) { //需要产生新的callframe了
        // console.log("makenew");
        makeNewCallFrame(frameCounter);
        old_frameCounter = frameCounter;
    }

    if (old_frameCounter > frameCounter) { //需要移除旧的callframe了
        var n = old_frameCounter - frameCounter;
        for (var i = 0; i < n; i++) {
            // console.log("remove old");
            var tmp = document.getElementById("callFrame_" + (old_frameCounter - i));
            tmp.remove();
        }
        old_frameCounter = frameCounter;
    }
    //只有当栈的指针发生变化的时候才绘制栈的图像
    //因为有些情况并没有入栈的操作
    if (pointer > old_pointer) {
        for (var i = old_pointer; i < pointer; i++) {
            makeNewStackFrame(frameCounter, i, s[i]);
        }
    }
    old_pointer = pointer;
}








var c = 1
function drawEnv(args) {
    if (old_frameCounter < frameCounter) {
        makeEnv(c++, args, '(lambda () ...)');
    }
}

class controler {

    constructor() {
        this.impName = ''    //正在执行的操作名   eg:constant
        this.impExpr = ''    //正在执行的操作内容 eg:1

        this.stack;          //堆栈 scheme:stack
        this.pointer = 0;    //栈的指针 scheme:s
        this.old_pointer = 0;

        this.frameCounter = 0; //frame计数器
        this.old_frameCounter = 0;

        this.evalFlag = false;   //接收完毕标志
        this.vmFlag = false;     //接收完毕标志
    }


    drawStack(Stack, pointer, frameCounter) {

        if (old_frameCounter < frameCounter) { //需要产生新的callframe了
            // console.log("makenew");
            makeNewCallFrame(frameCounter);
            old_frameCounter = frameCounter;
        }

        if (old_frameCounter > frameCounter) { //需要移除旧的callframe了
            var n = old_frameCounter - frameCounter;
            for (var i = 0; i < n; i++) {
                // console.log("remove old");
                var tmp = document.getElementById("callFrame_" + (old_frameCounter - i));
                tmp.remove();
            }
            old_frameCounter = frameCounter;
        }
        //只有当栈的指针发生变化的时候才绘制栈的图像
        //因为有些情况并没有入栈的操作
        if (pointer > old_pointer) {
            for (var i = old_pointer; i < pointer; i++) {
                makeNewStackFrame(frameCounter, i, Stack[i]);
            }
        }
        old_pointer = pointer;
    }

    makeEnv(count, args) {
        makeEnvNameTag(count)
        let s = ("localEnvironmentName_" + count);
        makeNewEnvironmentFrame(count, args)
        let t = ("localEnvironmentFrame_" + count);
        makeEnvConnection(s, t)
        let g = "globalEnvironmentFrame"
        makeGloEnvConnection(t, g)

    }



}


