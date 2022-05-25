
/**
 * 绘制每一帧的stack，目前由interpreterlib.js/nextComp()函数控制
 * 之后需改成控制系统控制
 * @param {Pair} Stack 
 * @param {number} pointer 
 * @param {number} frameCounter 
 */
function drawStack(Stack, pointer, frameCounter) {
    console.log("old:" + old_frameCounter);

    console.log("cur:" + frameCounter);

    if (old_frameCounter < frameCounter) { //需要产生新的callframe了
        console.log("makenew");
        makeNewCallFrame(frameCounter);
        old_frameCounter = frameCounter;
    }

    if (old_frameCounter > frameCounter) { //需要移除旧的callframe了
        var n = old_frameCounter - frameCounter;
        for (var i = 0; i < n; i++) {
            console.log("remove old");
            var tmp = document.getElementById("callFrame_" + (old_frameCounter - i));
            tmp.remove();
        }
        old_frameCounter = frameCounter;
    }
    // console.log("old_pointer: " + old_pointer);
    // console.log("pointer: " + pointer);
    if (pointer > old_pointer) {
        for (var i = old_pointer; i < pointer; i++) {
            makeNewStackFrame(frameCounter, i, Stack[i]);
        }
    }
    old_pointer = pointer;
}




var c = 2
function drawEnv(args) {
    if (old_frameCounter < frameCounter) {
        makeEnv(c++, args)
    }
}