2021年12月14日
动画数据定义：
    stack：
        [frameCreationFlag:int, // 1:开始生成新框架 2:结束框架生成 3:otherwise
         instruction:string, //当前指令
         stackPointer:int, //指向栈顶元素 
         stack:string, //栈
         flagStack:int //用来存储框架开始时的指针，结束时弹出一个元素
          ]





2021年6月23日
疑问：
    js中函数传递的到底是什么？ pointchangebyxy() 需要修改

    今天完成了绘制函数调用的函数，还需要修改的地方有，body被传入较长数据时，将会出现换行问题，例如传入lambda表达式等
    



2021年6月16日
definition.js
建立了一个存放变量和函数定义的脚本文件

main.js
完成了基本的点对绘制，将其备份至main_copy.js
日后可以通过对其中的步骤进行封装实现绘制点对的函数

graphics.js
修改了全部的draw函数，使其的输入参数为一个点，返回值为返回修改后的点。

待修改函数：    
    drawArrow(point, angle)
    drawText(t,point,w)



2021年6月15日
graphics.js
目前完成了基本图形的绘制，其中包括：
    点对（两个圆圈） draw2circle
    文字 drawtxt   
        目前接收的是文本框内的文字
    矩形 drawrectangle
    箭头 drawArrow
        待完成 ！！！ 还需要另一个绘制尾巴额函数与之配合 drawArrowTail
    
目标：
    从文本框输入简化环境列表，然后绘制出简化环境模型，其中可以省略参数和
    具体函数内容。最低限完成箭头的指示和元素的排列。


drawLine(point,length,angle)        //返回的是线段结束的坐标
drawrectangle(point,length,width)   //返回的位置是矩形左上角的坐标
draw2circle(point)                  //返回左侧圆心的坐标

{"env":[
    {"frame":[
        {"name":["name1","name2"],
         "val":["val1","val2"]
        }
    ]}
]}