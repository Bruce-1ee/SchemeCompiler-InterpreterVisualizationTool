

// function drawStack(stack, pointer) {
//     removeAllChildren(document.getElementById("callFrame_1"));
//     if (pointer === 0) return;
//     for (var i = 0; i < pointer; i++) {
//         makeNewStackFrame(1, i, stack[i]);
//     }
// }

function drawStack(Stack, pointer) {

    console.log("old_pointer: " + old_pointer);
    console.log("pointer: " + pointer);
    if(pointer > old_pointer){
        for (var i = old_pointer; i < pointer; i++) {
            makeNewStackFrame(1, i, Stack[i]);
        }
    }
    old_pointer = pointer;
}