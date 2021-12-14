var count = 0;
var impinfo = "function$$(if (= n 0) 1 (* n (self self (- n 1))))$$(((self n) (function (self n) (if (= n 0) 1 (* n (self self (- n 1)))) (((+ - * =) (primitive . #<procedure:+>) (primitive . #<procedure:->) (primitive . #<procedure:*>) (primitive . #<procedure:=>)))) 1) ((+ - * =) (primitive . #<procedure:+>) (primitive . #<procedure:->) (primitive . #<procedure:*>) (primitive . #<procedure:=>)))!"
var stackinfo = "#(() (halt) 1 ((frame (test (constant 1 (return 3)) (frame (return 3) (frame (argument (refer 0 1 (argument (refer-free * (apply 2))))) (frame (argument (refer 0 0 (argument (refer 0 0 (apply 2))))) (constant 1 (argument (refer 0 1 (argument (refer-free - (apply 2)))))))))) (constant 0 (argument (refer 0 1 (argument (refer-free = (apply 2))))))) ()) () 4 (return 3) 1 ((frame (test (constant 1 (return 3)) (frame (return 3) (frame (argument (refer 0 1 (argument (refer-free * (apply 2))))) (frame (argument (refer 0 0 (argument (refer 0 0 (apply 2))))) (constant 1 (argument (refer 0 1 (argument (refer-free - (apply 2)))))))))) (constant 0 (argument (refer 0 1 (argument (refer-free = (apply 2))))))) ()) () 9 (test (constant 1 (return 3)) (frame (return 3) (frame (argument (refer 0 1 (argument (refer-free * (apply 2))))) (frame (argument (refer 0 0 (argument (refer 0 0 (apply 2))))) (constant 1 (argument (refer 0 1 (argument (refer-free - (apply 2)))))))))) 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)";

var imp = impinfo.split("$$");
var stack = stackProcess(stackinfo);
function demo(){
    //ini();

    var envPoint = new Point(50,50);

    var stackPoint = new Point(1920 / 4 * 3,1080 /5 * 4);


    var t = new Point(envPoint.x,envPoint.y);
    t.y = t.y + inirectwidth/2;
    t.drawText("Global",99);
    t.pointchange(ctx.measureText("Global ").width,RIGHT);
    t.drawText("variable",99);
    t.pointchange(ctx.measureText("variable ").width,RIGHT);
    t.y = t.y - fontsize/3;
    t.drawLine(20,RIGHT);
    t.drawArrow(RIGHT);
    t.y = envPoint.y;
    t.drawrectangle(inirectlength,inirectwidth);


    stackPoint.drawStack(stack,14);

    t.y = envPoint.y + inirectwidth * 2;
    t.x = t.x - 40;
    t.drawText("E1",99);
    t.pointchange(ctx.measureText("E1 ").width,RIGHT);
    t.y = t.y - fontsize/3;
    t.drawLine(20,RIGHT);
    t.drawArrow(RIGHT);
    t.y = t.y - 50;
    t.drawrectangle(100,70);
    t.x = t.x + 30;
    t.y = t.y + 30;
    t.drawText("f:... ",1);
    t.y = t.y + fontsize;
    t.drawText("x:1",99);
    t.y =  t.y - 47;
    t.x = t.x + 25;
    t.drawLine(30,UP);
    t.drawArrow(UP);

    t.x = t.x -30;
    t.y = t.y + 140;
    t.drawText("(f f x)",99);


    t.x = envPoint.x + 100;
    t.y = envPoint.y *8;

    t.drawText("E2",99);
    t.pointchange(ctx.measureText("E1 ").width,RIGHT);
    t.y = t.y - fontsize/3;
    t.drawLine(20,RIGHT);
    t.drawArrow(RIGHT);
    t.y = t.y - 50;
    t.drawrectangle(100,70);
    t.x = t.x + 30;
    t.y = t.y + 30;
    t.drawText("self:...",99);
    t.y = t.y + fontsize;
    t.drawText("n:1",99);
    t.y =  t.y;
    t.x = t.x + 72;
    t.drawLine(30,RIGHT)
    t.drawLine(220,UP);
    t.drawArrow(UP);

    t.x = t.x -130;
    t.y = t.y + 280;
    t.drawText("(if (= n 0) 1...",99);



 
}

//(
//    ( (self n) (function (self n) (if (= n 0) 1 (* n (self self (- n 1)))) (((+ - * =) (primitive . #<procedure:+>) (primitive . #<procedure:->) (primitive . #<procedure:*>) (primitive . #<procedure:=>)))) 1) 
//    ((+ - * =) (primitive . #<procedure:+>) (primitive . #<procedure:->) (primitive . #<procedure:*>) (primitive . #<procedure:=>)))

