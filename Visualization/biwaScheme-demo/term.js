var onError = function (e) { console.error(e); }
var biwa = new BiwaScheme.Interpreter(onError);
var display = function (result) { console.log(result); }


function evl(exp) { biwa.evaluate(exp, display); }


