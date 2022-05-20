var frameCounter = 0;


function drawStack(stack, pointer) {
    removeAllChildren(document.getElementById("callFrame_1"));
    if (pointer === 0) return;
    for (var i = 0; i < pointer; i++) {
        makeNewStackFrame(1, i, stack[i]);
    }
}