var teststr = ["(((square ) (function)))","(((square ) (function)))"];
var teststrarr = ["a:1","b:2"];
var testbody = [ "(if" , "(< a b)" , "a" , "b)" ];

var teststackinfo = [0,"argument",4,stackProcess("#(() (halt) 2 1 0 0 0"),null];

function test(){
    var p1 = new Point(inil,iniw); 
    var p2 = new Point(LENGTH * 5/6, WIDTH * 8/9);

    makeglobalenv(p1);
    freshpoint(p1);
    makeframe(p1,teststrarr,testbody);
    freshpoint(p1);
    makeframe(p1,teststrarr,testbody);
    freshpoint(p1);
    makeStack(p2,teststackinfo);



   
    

}