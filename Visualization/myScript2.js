//var str = "(((x) (5)) ((square + - * /) (function)))"
//var str = "(((square ) (function)))"
function strtoarr(str){
    var strStack = [];

    var x;
    var isname = true;
    var tmp = [],   //临时存放当前变量或值的变量
        l = [];     //存放结果的数组
    for(x in str){
    //   console.log(strStack.print());
        if(str[x] == '('){
            strStack.push('(');
            continue;} 
        
        if(str[x] == ')'){
            strStack.pop();
            if(strStack.length == 1 || strStack.length == 0){
                continue;
            }
            if(isname == true){
                isname = false;
                if(tmp.length != 0){
                    l.push( tmp.join("") );
                    tmp = []; 
                }
                l.push(",");
            }else{
                isname = true;
                if(tmp.length != 0){
                    l.push( tmp.join("") );
                    tmp = [];
                }
                l.push(".");
            }
            continue;
        }
        if(str[x] == ' '){
            if(str[x-1] == ')'){
                continue;
            }
            l.push( tmp.join("") );
            tmp = [];
            continue;
        }

        tmp.push(str[x]);  
    }
    console.log(l);
    return l;
}
