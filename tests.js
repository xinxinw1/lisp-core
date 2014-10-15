title("Lisp Core Testing");

load("lib/tools.js");
load("lib/ajax.js");
load("lib/prec-math.js");
load("lib/lisp-tools.js");
load("lib/lisp-parse.js");
load("lib/lisp-exec.js");
load("lib/lisp-core.js");

//// Evaluator ////

function testevl(a, x, f){
  return test('L.dsj(L.evl(L.prs("' + a + '")))', x, f);
}

testevl("`1", "1");
testevl("`a", "a");
testevl("`(a b c)", "(a b c)");
testevl("`(a b ,(+ 2 3))", "(a b 5)");
testevl("`,(+ 2 3)", "5");
testevl("`(a b `(c ,d ,,(+ 2 3)))", "(a b `(c ,d 5))");
testevl("``,,(+ 2 3)", "`5");
testevl("``,(+ 2 3)", "`,(+ 2 3)");
testevl("``(a b ,(c d ,(+ 2 3)))", "`(a b ,(c d 5))");
// ...
