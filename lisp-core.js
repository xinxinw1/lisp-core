/***** Lisp Core 0.1 *****/

/* require tools 4.6.1 */
/* require ajax 4.5.1 */
/* require prec-math 4.3.2 */
/* require lisp-tools 0.1 */
/* require lisp-parse 0.1 */
/* require lisp-exec 0.1.1 */

(function (win, udf){
  ////// Import //////
  
  var udfp = $.udfp;
  
  var dsj = L.dsj;
  var prs = L.prs;
  var evl = L.evl;
  
  var chrb = L.chrb;
  var chkb = L.chkb;
  
  var djn = L.djn;
  var bol = L.bol;
  
  var ofn = L.ofn;
  var cal = L.cal;
  var get = L.xget;
  
  ////// JS functions //////
  
  djn({
    typ: function ltyp(a){
           return L.sy(L.typ(a));
         },
    tag: L.tag,
    rep: L.rep,
    det: L.det,
    dat: L.dat,
    sdat: L.sdat,
    
    mk: function lmk(t, o){
          return L.mk(L.jstr(t), udfp(o)?o:L.dat(o));
        },
    
    mkdat: function lmkdat(t, d, o){
             return L.mkdat(L.jstr(t), d, udfp(o)?o:L.dat(o));
           },
    mkbui: function lmkbui(t){
             return L.jn(L.mkbui(t));
           },
           
    sy: L.sy,
    nu: L.nu,
    st: L.st,
    ar: L.ar,
    ob: L.ob,
    rx: L.rx,
    jn: L.jn,
    ma: L.ma,
    sm: L.sm,
           
    car: L.car,
    cdr: L.cdr,
    gcar: L.gcar,
    gcdr: L.gcdr,
    cons: L.cons,
    scar: L.scar,
    scdr: L.scdr,
    lis: L.lis,
    lisd: L.lisd,
    arr: L.arr,
    
    caar: L.caar,
    cdar: L.cdar,
    cadr: L.cadr,
    cddr: L.cddr
  });
  
  bol({
    isa: function lisa(t, a){
           return L.chkb(L.isa(L.jstr(t), a));
         },
    isany: function lisany(t){
             var r = $.cpy(arguments);
             r[0] = L.jstr(t);
             return $.apl(L.isany, r);
           },
    typin: function ltypin(a){
             var tp = L.typ(a);
             var t = arguments;
             for (var i = 1; i < t.length; i++){
               if (tp === L.jstr(t[i]))return true;
             }
             return false;
           }
  });
  
  djn({
    mkpre: function lmkpre(t){
             return L.jn(L.mkpre(L.jstr(t)));
           }
  });
  
  bol({
    "tag?": L.tagp,
    "list?": L.listp,
    "atm?": L.atmp,
    "nil?": L.nilp,
    "lis?": L.lisp,
    "syn?": L.synp,
    "sym?": L.symp,
    "num?": L.nump,
    "obj?": L.objp,
    "rgx?": L.rgxp,
    "str?": L.strp,
    "arr?": L.arrp,
    "fn?": L.fnp,
    "jn?": L.jnp,
    "mac?": L.macp,
    "spec?": L.specp,
    "proc?": L.procp,
    
    is: L.is,
    isn: L.isn,
    iso: L.iso,
    "in": L.inp
  });
  
  djn({
    //sta: L.sta,  stack should be done by macro
    
    dsp: L.dsp,
    
    ou: L.ou,
    out: L.out,
    pr: L.pr,
    prn: L.prn,
    al: L.al,
    logobj: L.logobj,
    log: L.log,
    
    sym: L.sym,
    str: L.str,
    str1: L.str1,
    num: L.num,
    tfn: L.tfn,
    tarr: L.tarr,
    tlis: L.tlis,
    tobj: L.tobj,
    
    ref: L.ref,
    ref1: L.ref1,
    set: L.set,
    fst: L.fst,
    las: L.las,
    
    apl: L.apl,
    cal: ["(f . a)", L.apl],
    //cal: L.cal,   would convert args to js arr then back to lisp list
    map: L.map,
    //dmap: L.dmap,
    pos: function lpos(x, a, n){
      var o = L.pos(x, a, n);
      if (L.is(o, L.nu("-1")))return L.nil();
      return o;
    },
    has: chrb(L.has),
    //all: chrb(L.all),
    //keep: L.keep,
    rem: L.rem,
    rpl: L.rpl,
    //mat: L.mat,
    //mats: L.mats,
    
    len: L.len,
    emp: chrb(L.emp),
    copy: L.cpy,
    //cln: L.cln,
    rev: L.rev,
    nrev: L.nrev,
    
    sli: L.sli,
    fstn: L.fstn,
    rstn: L.rstn,
    rst: L.rst,
    mid: L.mid,
    
    spl: L.spl,
    grp: L.grp,
    //par: L.par,
    //tup: L.tup,
    
    join: L.joi,
    flat: L.fla,
    app: L.app,
    afta: L.afta,
    
    //evry: L.evry,
    
    fold: L.fold,
    foldi: L.foldi,
    foldr: L.foldr,
    foldri: L.foldri,
    
    head: L.head,
    tail: L.tail,
    
    beg: chrb(L.beg),
    //end: chrb(L.end),
    //bnd: chrb(L.bnd),
    
    eachfn: L.each,
    //oeachfn: L.oeach,
    
    push: L.psh,
    pop: L.pop,
    //ush: L.ush,
    //shf: L.shf,
    
    nth: L.nth,
    ncdr: L.ncdr,
    nrev: L.nrev,
    //napp: L.napp,
    
    low: L.low,
    upp: L.upp,
    stf: L.stf,
    
    odd: chrb(L.odd),
    evn: chrb(L.evn),
    
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
  
  djn({
    rnd: L.rnd,
    
    ohas: function lohas(a, x){
            return chkb(L.ohas(L.dat(a), x));
          },
    oput: function loput(a, x, y){
            return L.oput(L.dat(a), x, y);
          },
    orem: function lorem(a, x){
            return L.orem(L.dat(a), x);
          },
    oref: function loref(a, x){
            return L.oref(L.dat(a), x);
          },
    oset: function loset(a, x, y){
            return L.oset(L.dat(a), x, y);
          },
    "oset?": function losetp(a, x){
               return chkb(L.osetp(L.dat(a), x));
             },
    odel: function lodel(a, x){
            return L.ohas(L.dat(a), x);
          },
    oren: function loren(a, x, y){
            return L.oren(L.dat(a), x, y);
          },
    owith: function lowith(a, x, y){
             return L.ob(L.owith(L.dat(a), x, y));
           },
           
    currtim: L.currtim,
    
    self: $.self,
    
    err: function lerr(f, a){
           throw new $.Err(lerr, L.dsj(f), L.dat($.apl(L.stf, $.sli(arguments, 1))));
         },
    
    do: L.dol,
    //do1: do1,  convert list to arr then get first item?
    gs: L.gs,
    gsn: function lgsn(){
           return L.nu($.str(L.gsn()));
         },
    
    prs: function lprs(a){
           return L.prs(L.jstr(a));
         },
    name: L.name,
    "*out*": function (a){return L.nil();},
    allglobs: L.allglbs,
    "get-apply-stack": L.getApplyStack
  });
  
  ofn(function (a){
    return cal(get("*out*"), a);
  });
  
  ////// Import lisp //////
  
  function prsf(a){
    return prs($.get(a));
  }
  
  // eval string
  function evls(a){
    return dsj(evl(prs(a)));
  }
  
  function evlf(a){
    return evls($.get(a));
  }
  
  djn({
    load: function load(a){
      return evl(prs($.get(L.jstr(a))));
    }
  });
  
  if (!$.udfp($.libdir))evlf($.libdir + "/lisp-core/lisp-core.lisp");
  else evlf("lib/lisp-core/lisp-core.lisp");
  
  ////// Object exposure //////
  
  $.att(L, {
    prsf: prsf,
    evls: evls,
    evlf: evlf
  });
  
  ////// Testing //////
  
  
  
})(window);
