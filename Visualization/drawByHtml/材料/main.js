function main(){
    var cot = 0;
    addElement ();
}

var StackSize = 0;

function drawStack () {
    var stackFrame = document.getElementById("StackFrame");
    if(stackFrame == null){
        stackFrame = document.createElement("div");
        stackFrame.id ='StackFrame';  
        stackFrame.className = 'StackFrame'; 
        $(stackFrame).insertAfter(document.getElementById("title"));
    }
    
    var newDiv = document.createElement("div");
    newDiv.id = "Stack_" + StackSize;
    if(StackSize == 0)
        newDiv.className = 'BaseStack';
    else
        newDiv.className = 'Stack';
    let newContent = document.createTextNode("StackInfomation" + StackSize);
    var t = newDiv;
    // 添加文本节点 到这个新的 div 元素
    newDiv.appendChild(newContent);
    var t = document.getElementById(`Stack_${StackSize - 1}`)
    console.log(t);
    if(StackSize == 0){
        stackFrame.appendChild(newDiv);
    }else{
        stackFrame.appendChild(newDiv);
    }
   
    StackSize++;
  }

function test1(){
    var Div1 = document.createElement("div");
    Div1.id = "nested";
    var newContent = document.createTextNode("this is content");
    Div1.appendChild(newContent);
    var Div2 = document.createElement("div");
    Div2.id = "top";
    var newContent = document.createTextNode("this is content");
    //Div2.appendChild(newContent);
    Div2.appendChild(Div1);
    var currentDiv = document.getElementById("title");
    $(Div2).insertAfter(currentDiv);
    currentDiv = document.getElementById("nested");
    $(Div1).insertAfter(currentDiv);
  
}

function test(){
    var Div = document.createElement("div");
    Div.id = "new";
    var currentDiv = document.getElementById("nested");
    //$(Div).insertAfter(currentDiv);
    currentDiv.appendChild(Div);
}