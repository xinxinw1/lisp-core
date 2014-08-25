/***** Lisp Core Devel *****/

/* require tools >= 3.1 */
/* require ajax >= 4.1 */
/* require lisp-tools */
/* require lisp-parse */
/* require lisp-exec */

(function (win, udf){
  ////// Import //////
  
  var exe = L.exe;
  
  var jn = L.jn;
  var bol = L.bol;
  
  ////// JS functions //////
  
  jn({
    ou: L.ou,
    out: L.out,
    pr: L.pr,
    prn: L.prn,
    
    add: L.add,
    sub: L.sub,
    mul: L.mul,
    div: L.div
  });
  
  bol({
    gt: L.gt,
    lt: L.lt,
    ge: L.ge,
    le: L.le
  });
  
  ////// Import lisp //////
  
  function fil(a){
    return exe($.get(a));
  }
  
  fil("/codes/libjs/lisp-core/devel/lisp-core.lisp");
  
  ////// Object exposure //////
  
  $.att({fil: fil}, L);
  
  ////// Testing //////
  
  
  
})(window);
