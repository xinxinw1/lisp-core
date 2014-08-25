/***** Lisp Core Devel *****/

/* require tools >= 3.1 */
/* require ajax >= 4.1 */
/* require lisp-tools */
/* require lisp-parse */
/* require lisp-exec */

(function (win, udf){
  ////// Import //////
  
  var evls = L.evls;
  
  var r = L.r;
  
  var jn = L.jn;
  var bol = L.bol;
  var chrb = L.chrb;
  
  ////// JS functions //////
  
  function arr(){
    return r($.cpy(arguments));
  }
  
  jn({
    dsp: L.dsp,
    
    ou: L.ou,
    out: L.out,
    pr: L.pr,
    prn: L.prn,
    al: L.al,
    
    sym: L.sym,
    str: L.str,
    num: L.num,
    tfn: L.tfn,
    tarr: L.tarr,
    tlis: L.tlis,
    tobj: L.tobj,
    
    map: L.map,
    dmap: L.dmap,
    pos: L.pos,
    has: chrb(L.has),
    all: chrb(L.all),
    rem: L.rem,
    rpl: L.rpl,
    
    len: L.len,
    emp: chrb(L.emp),
    cpy: L.cpy,
    cln: L.cln,
    rev: L.rev,
    
    sli: L.sli,
    fstn: L.fstn,
    rstn: L.rstn,
    rst: L.rst,
    mid: L.mid,
    
    spl: L.spl,
    grp: L.grp,
    par: L.par,
    tup: L.tup,
    
    joi: L.joi,
    fla: L.fla,
    app: L.app,
    
    rdc: L.rdc,
    rdc1: L.rdc1,
    
    hea: L.hea,
    tai: L.tai,
    
    beg: chrb(L.beg),
    end: chrb(L.end),
    bnd: chrb(L.bnd),
    
    psh: L.psh,
    pop: L.pop,
    ush: L.ush,
    shf: L.shf,
    
    arr: L.arr,
    
    stf: L.stf,
    
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
