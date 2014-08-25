/***** Lisp Core Devel *****/

/* require tools >= 3.1 */
/* require ajax >= 4.1 */
/* require lisp-tools */
/* require lisp-parse */
/* require lisp-exec */

(function (win, udf){
  ////// Import //////
  
  var evls = L.evls;
  
  var jn = L.jn;
  var bol = L.bol;
  
  ////// JS functions //////
  
  jn({
    ou: L.ou,
    out: L.out,
    pr: L.pr,
    prn: L.prn,
    
    map: L.map,
    
    grp: L.grp,
    
    rdc: L.rdc,
    rdc1: L.rdc1,
    
    "+": L.add,
    "-": L.sub,
    "*": L.mul,
    "/": L.div
  });
  
  bol({
    ">": L.gt,
    "<": L.lt,
    ">=": L.ge,
    "<=": L.le
  });
  
  ////// Import lisp //////
  
  function evlf(a){
    return evls($.get(a));
  }
  
  evlf("/codes/libjs/lisp-core/devel/lisp-core.lisp");
  
  ////// Object exposure //////
  
  $.att({evlf: evlf}, L);
  
  ////// Testing //////
  
  
  
})(window);
