var i = 0;

function fn() {
    var wholestack = document.getElementById("stackFrame");
    var newstackNode = document.createElement("div");
    var newstackNum = document.createElement("div");
    var newstack = document.createElement("div");

    newstackNum.id = "stack" + i;
    newstackNum.className = "stackNum";
    newstackNum.appendChild(document.createTextNode("stack" + i));

    newstack.id = i;
    newstack.className = "stack";
    newstack.appendChild(document.createTextNode("(halt)"));

    newstackNode.id = i++;
    newstackNode.appendChild(newstackNum);
    newstackNode.appendChild(newstack);

    wholestack.appendChild(newstackNode);

}