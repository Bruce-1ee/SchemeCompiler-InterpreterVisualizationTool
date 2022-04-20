var teststr = ["((lambda (c) ","(+ b c)) (+ a b))"];
var teststrarr = ["a:1","b:2"];
var testbody = [ "((lambda (c) ","(+ b c)) (+ a b))" ];
var testbody2 = ["(+ b c)"] ;
var teststrarr2 = ["c:3"];

var teststackinfo = [2,"finish",12,stackProcess("#(() (halt) 2 1 () 4 (return 3) 3 4 8 (return 2) 3 2 0)")];

function test(){
    var p1 = new Point(inil,iniw); 
    var p2 = new Point(LENGTH * 5/6, WIDTH * 8/9);

    makeglobalenv(p1);
    freshpoint(p1);
    makeframe(p1,teststrarr,testbody,0);
    freshpoint(p1);
    makeframe(p1,teststrarr2,testbody2,1);
    freshpoint(p1);

    makeStack(p2,teststackinfo);





   


}