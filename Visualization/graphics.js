class Point{
    constructor(x, y){
        this.x = x;
        this.y = y;
    }
    drawLine(length,angle){    //绘制线段
        ctx.beginPath();                            
        ctx.moveTo(this.x,this.y);
        this.pointchange(length,angle);
        ctx.lineTo(this.x,this.y);
        ctx.stroke();
        //return this;   //返回的是线段结束的位置
    }

    drawrectangle(length,width){ //绘制矩形，输入一个点对象和长与宽
        ctx.beginPath();
        ctx.moveTo(this.x,this.y);

        this.drawLine(length,RIGHT);
        this.drawLine(width,DOWN);
        this.drawLine(length,LEFT);
        this.drawLine(width,UP);
        //return point;   //返回的位置是矩形左上角的位置

    }

    drawFrame(length,width){
        this.de();
        this.drawrectangle(length + 2 * Margin, width + 2 * Margin);
        this.in();

    }

    draw2circle(){      //这个函数绘制了一个中心涂黑的点对
        var r = RADIUS;     
        this.y = this.y + 0.5 * RADIUS;   //输入的坐标为两个圆相切的地方
        this.x = this.x - r;              //所以x坐标需要减少一个半径r（向左移动r）

        ctx.beginPath();            //绘制第一个空心圆
        ctx.arc(this.x , this.y , r , 0 , 2*Math.PI);
        ctx.stroke();
        ctx.beginPath();            //与小实心圆
        ctx.arc(this.x , this.y , 0.3*r , 0 , 2*Math.PI);
        ctx.fill();
        ctx.beginPath();            //同理空心圆二号
        ctx.arc(this.x + 2*r , this.y , r ,0,2*Math.PI);
        ctx.stroke();
        ctx.beginPath();            //实心圆二号
        ctx.arc(this.x + 2*r ,this.y, 0.3*r,0,2*Math.PI);
        ctx.fill();
        //return point;    //返回左侧圆心的坐标
    }

    drawArrowwithoutline(angle) {  //能用，待修改
        var headlen = 17;//自定义箭头线的长度
        var theta = 20;//自定义箭头线与直线的夹角
        var arrowX, arrowY;//箭头线终点坐标
        var toX,toY;
        //console.log(angle) //显示角度
        switch(angle){
            case 0:
                toX = this.x + 10;
                toY = this.y;
                break;
            case 90:
                toX = this.x;
                toY = this.y - 10;
                break;
            case 180:
                toX = this.x - 10;
                toY = this.y;
                break;
            case 270:
                toX = this.x;
                toY = this.y + 10;
                break;
            default:
                console.log("angle error -------- drawArrow" + angle);
        }
        // 计算各角度和对应的箭头终点坐标
    
    
        var Angle = Math.atan2(this.y - toY, this.x - toX) * 180 / Math.PI;
        var angle1 = (Angle + theta) * Math.PI / 180;
        var angle2 = (Angle - theta) * Math.PI / 180;
        var topX = headlen * Math.cos(angle1);
        var topY = headlen * Math.sin(angle1);
        var botX = headlen * Math.cos(angle2);
        var botY = headlen * Math.sin(angle2);
        ctx.beginPath();
        //画直线
        ctx.moveTo(this.x, this.y);
        ctx.lineTo(toX, toY);
        ctx.stroke();
        arrowX = toX + topX;
        arrowY = toY + topY;
    
        ctx.beginPath();
        //画上边箭头线
        ctx.moveTo(arrowX, arrowY);
        ctx.lineTo(toX, toY);
    
        arrowX = toX + botX;
        arrowY = toY + botY;
        //画下边箭头线
        ctx.lineTo(arrowX, arrowY);
        ctx.closePath();
        //ctx.strokeStyle = color;
        ctx.fill();
    
        this.pointchange(20,angle);
    }
    drawArrow(angle,length){
        this.drawLine(length,angle);
        this.drawArrowwithoutline(angle);
    }





    /*
    drawText(text,w){
        var num = ctx.measureText(w).width;

        var textArr = text.split("");
        var temp = "";				
        var row = [];
        // ctx.font = "20px Arial";
        // ctx.fillStyle = "black";
        // ctx.textBaseline = "middle";
        for(var a = 0; a < textArr.length; a++){
            if( ctx.measureText(temp).width < num ){
                ;
            }
            else{
                row.push(temp);
                temp = "";
            }
            temp += textArr[a];
        }
        
        row.push(temp);
        
        for(var b = 0; b < row.length; b++){
            //ctx.fillText(row[b],this.x,this.y+(b+1)*20);
            ctx.fillText(row[b],this.x,this.y+(b+0)*20);
        }
        //this.x = this.x + ctx.measureText(text).width;
        //this.y = this.y + fontsize;
        //return point;
    }
    */



    /*
     * count用来表示一行中字符的上限
     * makenewline用来确认是否换行
     * pointPostion用来确认结束之后坐标的位置
     */

    drawText(text,count,makenewline,pointPostion){

        var width;
        if(count == 0){
            width = 10000000;
        }else{
            width = ctx.measureText(" ").width * count;
        }
        
        var textArr = text.split("");
        var temp = "";				
        var row = [];

        for(var a = 0; a < textArr.length; a++){
            if( ctx.measureText(temp).width < width ){
                ;
            }
            else{
                row.push(temp);
                temp = "";
            }
            temp += textArr[a];
        }
        
        row.push(temp);

        if(makenewline){
            for(var b = 0; b < row.length; b++){
                ctx.fillText(row[b],this.x,this.y+(b+1)*20);
            }
        }else{
            ctx.fillText(row[0],this.x,this.y+20);
        }

        switch(pointPostion){
            case 0: //返回初始点
                return;
            case 1: //换行
                this.y = this.y + fontsize + Margin/2;
                break;
        }
        
        


            
        
        //this.x = this.x + ctx.measureText(text).width;
        //this.y = this.y + fontsize;
        //return point;
    }



    drawStack(stackArr,size){

        var t = new Point(this.x,this.y);

        for(var i = 0; i < size; i++){
            this.drawrectangle(100,40);
            t.x = this.x + 20;
            t.y = this.y + 33;
            t.drawText(stackArr[i], 5);

            //this.drawText(stackArr[i],"PPPPPPPPPP");
            this.pointchange(40,UP);
        }
        this.drawrectangle(100,40);  
    }





    pointchange(length,angle){   //类似极坐标
        switch(angle){
            case 0:
                this.x = this.x + length;
                break;
            case 90:
                this.y = this.y - length;
                break;
            case 180:
                this.x = this.x - length;
                break;
            case 270:
                this.y = this.y + length;
                break;
            default:
                console.log("angle err -------- pointchange");
        }
        //return point;
    }

    de(times){
        this.x = this.x - times * Margin;
        this.y = this.y - times * Margin;
    }
    in(times){
        this.x = this.x + times * Margin;
        this.y = this.y + times * Margin;
    }




    test(){
        ctx.beginPath();
        ctx.arc(this.x,this.y,5,0,2*Math.PI);
        ctx.stroke();
    }
}






function clearCanvas()
{  
    var c=document.getElementById("Canvas");  
    var cxt=c.getContext("2d");  
    c.height=c.height;  
} 





/*
function drawrectangle(point,length,width){ //绘制矩形，输入一个点对象和长与宽
    ctx.beginPath();
    ctx.moveTo(point.x,point.y);
    point = drawLine(point,length,RIGHT);
    point = drawLine(point,width,DOWN);
    point = drawLine(point,length,LEFT);
    point = drawLine(point,width,UP);
    return point;   //返回的位置是矩形左上角的位置
}


function draw2circle(point){      //这个函数绘制了一个中心涂黑的点对
    var r = RADIUS;     
    point.y = point.y + 0.5 * RADIUS;   //输入的坐标为两个圆相切的地方
    point.x = point.x - r;              //所以x坐标需要减少一个半径r（向左移动r）
    ctx.beginPath();            //绘制第一个空心圆
    ctx.arc(point.x , point.y , r , 0 , 2*Math.PI);
    ctx.stroke();
    ctx.beginPath();            //与小实心圆
    ctx.arc(point.x , point.y , 0.3*r , 0 , 2*Math.PI);
    ctx.fill();
    ctx.beginPath();            //同理空心圆二号
    ctx.arc(point.x + 2*r , point.y , r ,0,2*Math.PI);
    ctx.stroke();
    ctx.beginPath();            //实心圆二号
    ctx.arc(point.x + 2*r ,point.y, 0.3*r,0,2*Math.PI);
    ctx.fill();
    return point;    //返回左侧圆心的坐标
}



/////
function drawArrow(point, angle) {  //能用，待修改
    var headlen = 17;//自定义箭头线的长度
    var theta = 20;//自定义箭头线与直线的夹角，个人觉得45°刚刚好
    var arrowX, arrowY;//箭头线终点坐标
    var toX,toY;
    //console.log(angle) //显示角度
    switch(angle){
        case 0:
            toX = point.x + 10;
            toY = point.y;
            break;
        case 90:
            toX = point.x;
            toY = point.y - 10;
            break;
        case 180:
            toX = point.x - 10;
            toY = point.y;
            break;
        case 270:
            toX = point.x;
            toY = point.y + 10;
            break;
        default:
            console.log("angle err -------- drawArrow");
    }
    // 计算各角度和对应的箭头终点坐标


    var Angle = Math.atan2(point.y - toY, point.x - toX) * 180 / Math.PI;
    var angle1 = (Angle + theta) * Math.PI / 180;
    var angle2 = (Angle - theta) * Math.PI / 180;
    var topX = headlen * Math.cos(angle1);
    var topY = headlen * Math.sin(angle1);
    var botX = headlen * Math.cos(angle2);
    var botY = headlen * Math.sin(angle2);
    ctx.beginPath();
    //画直线
    ctx.moveTo(point.x, point.y);
    ctx.lineTo(toX, toY);
    ctx.stroke();
    arrowX = toX + topX;
    arrowY = toY + topY;

    ctx.beginPath();
    //画上边箭头线
    ctx.moveTo(arrowX, arrowY);
    ctx.lineTo(toX, toY);

    arrowX = toX + botX;
    arrowY = toY + botY;
    //画下边箭头线
    ctx.lineTo(arrowX, arrowY);
    ctx.closePath();
    //ctx.strokeStyle = color;
    ctx.fill();

    return point = pointchange(point,20,angle);
}

function drawText(t,point,w){
	var chr = t.split("");
	var temp = "";				
	var row = [];
	// ctx.font = "20px Arial";
	// ctx.fillStyle = "black";
	// ctx.textBaseline = "middle";
	for(var a = 0; a < chr.length; a++){
		if( ctx.measureText(temp).width < w ){
			;
		}
		else{
			row.push(temp);
			temp = "";
		}
		temp += chr[a];
	}
	
	row.push(temp);
	
	for(var b = 0; b < row.length; b++){
		ctx.fillText(row[b],point.x,point.y+(b+1)*20);
	}
    point.x = point.x + ctx.measureText(t).width;
    point.y = point.y + fontsize;
    return point;
}




function drawDefexp(envlist,point){
    var funcname = envlist[0];
    var funcval = envlist [1];
    drawtxt(funcname.toString(),point.x,point.y)
}

*/





// function test(){       
//     //ctx.measureText()
//     drawtxt("AB",document.getElementById("x").value,document.getElementById("y").value)
// }








 

