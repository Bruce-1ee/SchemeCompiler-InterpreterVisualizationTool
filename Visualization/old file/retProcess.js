function stackProcess(orgStack){  //输入原始栈信息

    var str = orgStack.slice(2,orgStack.length-1);
    var Pflag = 0; //括号匹配
    var Pcounter = 0; //括号数量

    for(i = 0; i < str.length; i++){
        if(str[i] == '('){
            Pflag = 1;
            Pcounter++;
            continue;
        }

        if(str[i] == ')'){
            Pcounter--;
            if(Pcounter == 0){ Pflag = 0; }
            continue;
        }


        if(str[i] == " " && Pflag == 0){
            str = setCharAt(str,i,';');
        }
    }


    return str.split(";"); //数组形式
}

function setCharAt(str,index,chr) {
    if(index > str.length-1) return str;
    return str.substr(0,index) + chr + str.substr(index+1);
}


// ( ((a b) 1 2) ((+ - =) (primitive . #<procedure:+>) (primitive . #<procedure:->) (primitive . #<procedure:=>)) )

// (  ( ()  )    )
function envProcess(orgEnv){  //输入原始信息
    var str = orgEnv.split("");
    var Pcounter = 0;
    var temp = "";
    var env = [];
    var frame = [];
    var vara = [];
    var val = [];
    var inpro = false;
    var flag = 1; //1: vara  2:val
    var down = false;
    var f = false;
    for(var i = 0; i < str.length; i++){
        console.log("str" + i + ":" +str[i]);
        console.log("temp:" + temp);
        console.log("vara:" + val);
        console.log("frame:" + frame);
        console.log(" ");
        if(str[i] == "(" && down == false) {
            Pcounter++; 
            continue;
        }else if(str[i] == "(" && down == true){
            for(var t = i+1; t < str.length; t++){
                if(str[t] == ")"){

                    val.push((str.slice(i+12,t)).join(""));
                    i = t+1;
                    break;
                }
                   
            }
        }


        if(inpro == true && str[i] == " " || str[i] == ")") {
            if(flag == 1) {
                vara.push(temp);
            }else{
                val.push(temp);
            }
            temp = "";
            inpro = false;
        }

        if(str[i] == ")" && inpro == false && Pcounter == 2) {
            
            Pcounter--; 
            for(var j = 0 ; j<val.length ; j++){
                frame.push(vara[j] + ":" + val[j]);
                console.log("aaaaaaaaaaa" + frame)
            }
            env.push(frame);
            vara = [];
            val = [];
            frame = []; 
            down = false;
            continue;
        }

        if(str[i] == ")" && inpro == false && Pcounter == 3) {
            down = true;
            Pcounter--; 
            continue;
        }

        if(str[i] == " " && inpro == false){
            continue;
        }



        if(Pcounter == 3){
            flag = 1;
        }
        if(Pcounter == 2){
            flag = 2;
        }
        inpro = true;
        temp += str[i];

    }
    return env;
}

var t = "(((a b) 1 2) ((+ - =) (primitive . #<procedure:+>) (primitive . #<procedure:->) (primitive . #<procedure:=>)) )";
var ret = envProcess(t);
