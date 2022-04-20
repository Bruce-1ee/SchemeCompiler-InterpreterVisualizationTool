var RIGHT = 0;              //定义了线段或者箭头的方向，其中的数字是角度角度
var UP = 90;                //
var LEFT = 180;
var DOWN = 270;

var RADIUS = 15;            //圆的半径

var LENGTH = 1920;          //画面大小
var WIDTH= 1080;

var Margin = 10;            //缩进

var ArrowLength = 30;       //箭头长度

var usedX = 0;      //已经使用的画面大小
var usedY = 0;

var inirectlength = 600;    //全局环境矩形的长和宽
var inirectwidth = 100;

var inil = 170;             //起始点坐标
var iniw = 30;

var counter = 1; //环境用框架计数器，用来生成E1 E2 等标签

var stackFrameCounter = 0; //栈图像用计数器，用来确认当然框架层数来绘制颜色缩进等信息；
var stackFramePoint = new Array();
stackFramePoint.push(0);
stackFramePoint.push(5);
stackFramePoint.push(9);

var font = "Monaco";
var fontsize = 20;
var fontwidth = 0.6*fontsize;

//颜色用变量
var rgb = ["#FF0000","#0000FF","#00FFFF","#00FF00","#FFFF00","#FF00FF","#0000FF"];
var loop = 0;


var c=document.getElementById("Canvas");   //画布
var ctx=c.getContext("2d");
ctx.font = fontsize + "px " + font;
//ctx.font = "20px Monaco";
//var fontsize = parseInt(ctx.font.slice(0,2));


function stackInfo(frameCreationFlag,instruction,stackPointer,stack){
    this.frameCreationFlag = frameCreationFlag;
    this.instruction = instruction;
    this.stackPointer = stackPointer;
    this.stack = stack;

}







function pointchange(point,length,angle){   //类似极坐标
    switch(angle){
        case 0:
            point.x = point.x + length;
            break;
        case 90:
            point.y = point.y - length;
            break;
        case 180:
            point.x = point.x - length;
            break;
        case 270:
            point.y = point.y + length;
            break;
        default:
            console.log("angle err -------- pointchange");
    }
    return point;
}

function pointchangebyxy(point,x,y){
    point.x = point.x + x;
    point.y = point.y + y;
    return point;
}

function maxlength(strarr){
    var maxlen = ctx.measureText(strarr[0]).width;
    for(n in strarr){
        if (maxlen < ctx.measureText(strarr[n]).width){
            maxlen = ctx.measureText(strarr[n]).width
        }
    }
    return maxlen;
}
function max(a,b){
    if (a>b){
        return a;
    }
    return b;
}
function min(a,b){
    if (a>b){
        return b;
    }
    return a;
}


function resize(w,h)
{
	var nc = document.createElement("canvas");
	nc.width = canvas.width;
	nc.height = canvas.height;
	nc.getContext("2d").drawImage(canvas,0,0);
	canvas.width = w;
	canvas.height = h;
	cxt.drawImage(nc,0,0);
}




/*
var p = {                   //绘图的基准点
    x:beginPointX,
    y:beginPointY
};

function savexy(point){     //用于存储坐标
    x = point.x;
    y = point.y;
}
function restorexy(point){  //用于恢复坐标
    point.x = x;
    point.y = y;
}


var tx = 0;                 //用于存储中间的坐标
var ty = 0;
var nx = 0;                 //下一个frame的坐标
var ny = 0;

*/