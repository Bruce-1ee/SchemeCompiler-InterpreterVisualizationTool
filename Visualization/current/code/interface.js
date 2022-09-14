/**
 * 这个文件里记载了用于被scheme调用的函数
 */


lst = [];
function testVal(val) {
    lst.push(val);
};

function interNewFrame(vals, args) {

    var values = vals;
    var retVals = [];
    while (values.car != null) {
        retVals.push(values.car)
        values = values.cdr;
    }
    var argument = args;
    if (argument.car.name == "function") {
        argument = argument.cdr.car;
        retArgs = [];
        for (var i = 0; i < retVals.length; i++) {
            retArgs.push(argument.car.name);
            argument = argument.cdr;
        }
        for (var i = 0; i < retVals.length; i++) {
            retVals[i] = `${retArgs[i]} : ${retVals[i]}`;
        }
    } else if (argument.car.name == "primitive") {
        //tbd
    }
    view.environment.addFrame(retVals);
}



