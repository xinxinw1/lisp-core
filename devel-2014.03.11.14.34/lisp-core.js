/***** Lisp Core Devel *****/

/* require tools >= 3.1 */
/* require ajax >= 4.1 */
/* require lisp-tools */
/* require lisp-parse */
/* require lisp-exec */

(function (win, udef){
  ////// Import //////
  
  var ou = Lisp.ou;
  var out = Lisp.out;
  var pr = Lisp.pr;
  var prn = Lisp.prn;
  
  var exec = Lisp.exec;
  
  var jsfn = Lisp.jsfn;
  
  ////// JS functions //////
  
  jsfn({
    ou: ou,
    out: out,
    pr: pr,
    prn: prn
  });
  
  ////// Import lisp //////
  
  function file(url){
    return exec($.get(url));
  }
  
  file("/codes/libjs/lisp-core/devel/lisp-core.lisp");
  
  ////// Object exposure //////
  
  $.apd({
    file: file
  }, Lisp);
  
  ////// Testing //////
  
  
  
})(window);
