QUnit.test('Core Functions', function (assert){
  assert.testevl("(typ \"test\")", "str");
  assert.testevl("(typ 'test)", "sym");
  assert.testevl("(typ 253)", "num");
  assert.testevl("(typ has)", "jn");
  
  assert.testevl("(+ 2 3)", "5");
  
  assert.testevl("((tfn 'lin) 'lin)", "t");
  assert.testevl("((tfn 'lin) 'lns)", "nil");
  assert.testevl("((tfn [> _ 3]) 4)", "t");
  assert.testevl("((tfn [> _ 3]) 3)", "nil");
  assert.testevl("((tfn [> _ 3]) 2)", "nil");
  
  assert.testevl("(ohas {a 3} 'a)", "t");
  assert.testevl("(ohas {a 3} 'b)", "nil");
  
  assert.testevl("(let a {a 3} (oput a 'a 5) a)", "{a 5}");
  assert.testevl("(let a {a 3} (oput a 'b 5) a)", "{a 3 b 5}");
  
  assert.testevl("(let a {a 3} (owith a 'a 5))", "{a 5}");
  assert.testevl("(let a {a 3} (owith a 'a 5) a)", "{a 3}");
  
  assert.testevl("((fn ((o a 3)) a))", "3")
});
