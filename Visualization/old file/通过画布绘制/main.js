


function ini(){
    var envPoint = new Point(inil,iniw);
    makeframe(envPoint,["Global env"],null);
/**
    usedY = envPoint.y + inirectwidth/2 + 3 * Margin;
    usedX = envPoint.x;

    var p = new Point(usedX,usedY);

    makeframe(p,"E1",6*fontwidth,3*fontsize);

    p.test();

    var stackPoint = new Point(1920 / 4 * 3,1080 /5 * 4);
 */


}

function makeglobalenv(point){
    var x = point.x;
    usedX += x;
    usedY += iniw + 3 * fontsize + 2.5*Margin + 3*Margin;
    var y = point.y;

    var length = LENGTH/3;
    var width = 3 * fontsize;

    point.de(1);
    point.drawrectangle(length + 2.5*Margin, width + 2.5*Margin);

    point.y = y - Margin + ((width + 2.5*Margin) / 2);
    var y = point.y;
    
    point.x = x - "Global env".length * fontwidth - 20;
    point.y = point.y - fontsize;
    
    //写入Global env
    point.drawText("Global ",0,0,1);
    point.drawText("env",0,0,0);
    point.y = y;
    point.x = point.x + "Global ".length * fontwidth;
    //画箭头
    point.drawArrow(RIGHT,30);

    

}

/**
 * 
 * @param {*} point 起始点坐标
 * @param {*} strarr frame内的字符串，数组表示
 *            strarr: [ “a:1”,“b:2” ]
 * @param {*} body 函数体内容 ， 数组表示
 *            body: [ "(if" , "(< a b)" , "a" , "b)" ]
 * @param {*} subframe 子函数，0：假的 1：真的
 * @returns 
 */
function makeframe(point,strarr,body,subframe){ 
    var x = point.x;
    var y = point.y;
    //寻找最大的边长
    var length = 0;
    var width = strarr.length;
    for(var i = 0; i < strarr.length; i++){
        if(length < strarr[i].length){
            length = strarr[i].length;
        }
    }
    //如果过短将定义为默认值
    if(length < 5) length = 5;
    if(width < 1) width = 1;

    //画外边框，留出一倍边距，绘制完毕之后回到初始绘制点
    point.de(1);
    point.drawrectangle(length*fontwidth + 2.5*Margin, width*(fontsize+3) + 2.5*Margin);
    point.x += length*fontwidth + 1.5 * Margin;
    point.y += Margin;

    //绘制对映标识点
    changeColor(counter-1);
    ctx.lineWidth = 2;
    point.drawCircle(3);
    ctx.lineWidth = 1;
    ctx.strokeStyle = "#000000"
    point.x = x;
    point.y = y;
    
    //写入内部文字
    if(strarr != null){
        for(var i = 0; i < strarr.length; i++){
            point.drawText(strarr[i],0,0,1);
        }
    }
    //换行留出空余，准备写入函数体内容
    point.y = point.y + 2 * Margin;

    //写入函数体内容
    var bodylength = 0;
    if(body != null){
        bodylength = body.length;
        for(var i = 0; i<body.length; i++){
            point.drawText(body[i],0,0,1);
        }
    }
    //准备绘制左侧箭头与文字
    point.y = y + (width*(fontsize+3) / 2);
    point.x = x - 100;
    point.y = point.y - fontsize/2;

    //写入环境编号
    point.drawText("E"+counter++,0,0,0);

    point.y = point.y + fontsize/2;;
    point.x = point.x + 2*fontwidth + Margin;
    //画箭头
    point.drawArrow(RIGHT,40);
    
    //画指向环境的线
    point.x = x + (length*fontwidth) + 1.5*Margin;
    point.drawLine(100,RIGHT);
    if(subframe){
        point.drawLine(usedY-iniw - 3 * fontsize - 9* Margin , UP);
        point.drawArrow(LEFT,90);
    }else{
        point.drawArrow(UP,usedY - iniw - 3 * fontsize - Margin - width/2);
    }

    
    

    usedY +=  width*(fontsize+3) + 6.5*Margin + bodylength * fontsize;

    

    //point.x = 

    //这里写入变量，arr未定义
}

function makeStack(point,stackinfo){
    var x,y; 

    var length = 15 * fontwidth; //长度为15个字符的宽度
    var width = fontsize;

    for(var i = 0; i<=stackinfo[2]; i++){//stackinfo[2]：栈顶指针
        //画矩形和内部文字
        point.de(1);
        point.drawrectangle(length+2*Margin, width+2*Margin);
        point.in(1);
        point.drawText(stackinfo[3][i],0,0,0)
        point.x += length + 1.2 *Margin;
        point.drawText(String(i) ,0,0,0);
        point.x -= length + 1.2 *Margin;
        point.y -= width+2*Margin;
    }
    //画框架指示器

    point.y += width+2*Margin;
    if(stackinfo[0] == 1){
        stackFramePoint.push(stackinfo[2]);
    }
    
    for(var i = 0; i<stackFramePoint.length ; i++){
        x = point.x;
        y = point.y;
        point.y += (stackinfo[2] - stackFramePoint[i]) * (width+2*Margin) + Margin + width;
        point.x -= 1.5 * Margin;
        //point.drawLine(10*(1+stackFrameCounter++),LEFT);
        changeColor(i);
        point.drawLine(10*(stackFramePoint.length-i),LEFT);
        point.drawLine(100,UP);
        ctx.strokeStyle = "#000000"
        point.x = x;
        point.y = y;
    }

    if(stackinfo[0] == 2){
        point.y = point.y - Margin;
        point.x = point.x - 1.5 * Margin;
        changeColor(stackFramePoint.length - 1);
        point.drawLine(10,LEFT);
        point.drawLine(100,DOWN);
        ctx.strokeStyle = "#000000"
        stackFramePoint.pop();
    }

    

}



function drawdef(point,type,parameters,name,val){
    point.x = point.x+2*fontsize;
    point.y = point.y+fontsize
    point = drawText(name+":",point,99999);
    point = drawLine(point,10,RIGHT);
    point = drawLine(point,inirectwidth,DOWN);
    point = drawArrow(point,DOWN);
    point = draw2circle(point);
   
    savexy(point);
    
    point = drawLine(point , 40 , DOWN);
    point = drawArrow(point, DOWN);
    point.x = point.x - ctx.measureText("parameters: "+parameters).width/2;
    point = drawText("parameters: "+ parameters , point,99999);
    point.x = point.x - ctx.measureText("parameters: "+parameters).width;
    point.y = point.y+0.3*fontsize
    point = drawText("body: (* x x)" ,point ,99999);

    restorexy(point);
    point.x = point.x + 2*RADIUS;
    point = drawLine(point , 30 , RIGHT);
    point = drawLine(point , 3*RADIUS, UP);
    point = drawArrow(point, UP);


}

function freshpoint(point){
    point.x = usedX;
    point.y = usedY;
}

function drawStack(stackInfo){


}

function drawFunction(impInfo,point){
    
}

