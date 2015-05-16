;;;; Lisp Core Functions ;;;;

;;; mac, def, syntax ;;;

(var mac (mc (nm ag . bd)
           `(do (var ,nm (mc ,ag ,@bd))
                (=nm ,nm ',nm))))
(=nm mac 'mac)

(mac def (nm ag . bd)
  `(do (var ,nm (fn ,ag ,@bd))
       (=nm ,nm ',nm)))

(mac fn1 (a) `(fn (_) ,a))

;;; aqt, auq ;;;

(def aqt (a) (lis 'qt a))
(def auq (a) (lis 'uq a))

;;; Basic Conditional ;;;

(mac no (a) `(nil? ,a))
(mac not (a) `(no ,a))

(def isn (a b)
  (not (is a b)))

(mac when (ts . bd)
  `(if ,ts (do ,@bd)))

; = unless
(mac ifnot (ts . bd)
  `(when (not ,ts)
     ,@bd))

(mac assert (a)
  `(ifnot ,a (err assert "Assertion a = $1 failed" ',a)))

;;; zap ;;;

(mac zap (f a . rst)
  `(= ,a (,f ,a ,@rst)))

;(mac += (a x) `(= ,a (+ ,a ,x)))
(mac += (a . x) `(zap + ,a ,@x))
(mac -= (a . x) `(zap - ,a ,@x))
(mac app= (a . x) `(zap app ,a ,@x))
(mac zappop (a) `(zap cdr ,a))

(mac ++ (a) `(+= ,a 1))
(mac -- (a) `(-= ,a 1))

;;; mapn ;;;

; (mapn `(,_ hey) '(1 2 3)) -> ((1 hey) (2 hey) (3 hey))
(mac mapn (bd a)
  `(map (fn1 ,bd) ,a))

; (mapf (a b) #[a b 30] '((1 2) (3 4))) -> (#[1 2 30] #[3 4 30])
(mac mapf (ag bd a)
  `(map (fn (,ag) ,bd) ,a))

(def mapapp (f a)
  (apl app (map f a)))

; (mapnapp `(,_ hey) '(1 2 3)) -> (1 hey 2 hey 3 hey)
(mac mapnapp (bd a)
  `(apl app (mapn ,bd ,a)))

; (mapfapp (a b) `(,a ,b hey) '((1 2) (3 4))) -> (1 2 hey 3 4 hey)
(mac mapfapp (ag bd a)
  `(apl app (mapf ,ag ,bd ,a)))

;;; by ;;;

#|
(mac test1 (a) `(prn ,a))
(by 1 test test1)
(test 1 2 3)
->
1
2
3
nil
|#

(mac by (n nm op)
  `(mac ,nm #g
     `(do ,@(mapn `(,,op ,@_) (grp #g ,n)))))

#|
(macby hey (a b) `(prn (+ ,a ,b)))
(hey 1 2  3 4  5 6)
->
3
7
11
nil
|#

(mac macby (nm ag . bd)
  `(do (mac #g ,ag ,@bd)
       (by ,(len ag) ,nm #g)))

;;; alias ;;;

(macby alias (new old)
  `(mac ,new #args `(,,old ,@#args)))

(alias defn def)

;;; Object ;;;

(def haskey (a x)
  (ohas a x))

;;; let, with ;;;

; simple block
(mac sblock a
  `((fn () ,@a)))

(mac let (a x . bd)
  `((fn (,a) ,@bd) ,x))

; simple each
(mac seach (x a . bd)
  `(eachfn ,a (fn (,x) ,@bd)))

; (splevery 3 '(1 2 3 4 5 6 7 8)) -> #[(1 4 7) (2 5 8) (3 6)]
; like (buckets (let i 0 [do1 (% i n) (++ i)]) a)
(def splevery (n a (o m 0))
  (let r #[]
    (seach x a
      ; (insure (r m) nil (push x (r m)))
      (if (no (haskey r m)) (= (r m) nil))
      (push x (r m))
      (++ m)
      (if (is m n) (= m 0)))
    (map nrev r)))

(def splevery2 (a)
  (splevery 2 a))
  
#|(mac with (vs . bd)
  (let r (splevery2 vs)
    `((fn ,(r 0) ,@bd) ,@(r 1))))|#

(mac with (vs . bd)
  (let g (grp vs 2)
    `((fn ,(map car g) ,@bd) ,@(map cadr g))))

; (remdup '(a a b c c d e e)) -> (b c c d)
(def remdup (a)
  (if (no a) nil
      (no (cdr a)) (lis (car a))
      (is (car a) (cadr a)) (remdup (cddr a))
      (lisd (car a) (cadr a) (remdup (cddr a)))))

(mac withi (vs . bd)
  (let r (remdup vs)
    (if (no r) `(do ,@bd)
        `(with ,r ,@bd))))

(mac withs (vs . bd)
  (if (no vs) `(do ,@bd)
      `(let ,(car vs) ,(cadr vs)
         (withs ,(cddr vs) ,@bd))))

(mac iflet (i a . rst)
  `(let ,i ,a
     (if ,i ,@rst)))

;;; runon ;;;

#|
(runoni x '(1 2 3 4 5)
  (= (car x) 10))
->
(10 2 3 4 5)
|#

(mac runoni (i a . bd)
  `(let ,i ,a
     ,@bd
     ,i))

#|
(runon '(1 2 3 4 5)
  (= (car _) 10))
->
(10 2 3 4 5)
|#

(mac runon (a . bd)
  `(runoni _ ,a ,@bd))

;;; named functions ;;;

; named function
; (nfn hey (a) (prn a)) -> <fn hey (a)>
(mac nfn (nm ag . bd)
  `(runoni #_ (fn ,ag ,@bd)
     (=nm #_ ',nm)))

(mac nmc (nm ag . bd)
  `(runoni #_ (mc ,ag ,@bd)
     (=nm #_ ',nm)))

;;; Macro Writing Helpers ;;;

(def afta (a x)
  (if (no a) nil
      (lisd (car a) x (afta (cdr a) x))))

(def befa (a x)
  (if (no a) nil
      (lisd x (car a) (befa (cdr a) x))))

(def btwa (a x)
  (if (no a) nil
      (cons (car a) (befa (cdr a) x))))

; with list
; (let a 3 (w/lis a a)) -> (3)
; (let a '(3) (w/lis a a)) -> (3)
(mac w/lis (a . bd)
  `(if (atm? ,a) (let ,a (lis ,a) ,@bd)
       (do ,@bd)))

; (w/gs test (lis test)) -> (gs3)
; (w/gs (a b c) (lis a b c)) -> (gs1 gs2 gs3)
(mac w/gs (nm . bd)
  (w/lis nm
    `(with ,(afta nm '(gs)) ,@bd)))

;;; rfn ;;;

(mac rfn (nm ag . bd)
  `(let ,nm nil
     (= ,nm (nfn ,nm ,ag ,@bd))))

(mac afn (ag . bd)
  `(rfn self ,ag ,@bd))

#|
(rwith fact (n 4)
  (if (is n 0) 1
      (* n (fact (- n 1)))))
-> 24
|#

(mac rwith (nm vs . bd)
  (let g (grp vs 2)
    `((rfn ,nm ,(map car g) ,@bd) ,@(map cadr g))))

; with recursive
(mac w/rec (vs . bd)
  (let g (grp vs 2)
    `(with ,(mapnapp `((car ,_) nil) g)
        ,@(mapn `(= ,(car _) ,(cadr _)) g)
        ,@bd)))

;;; flet ;;;

; function let
(mac flet ((nm . fbd) . bd)
  `(let ,nm (nfn ,nm ,@fbd) ,@bd))

(mac mlet ((nm . mbd) . bd)
  `(let ,nm (nmc ,nm ,@mbd) ,@bd))

(mac mwith (as . bd)
  `(with ,(mapfapp (nm . bd) `(,nm (nmc ,nm ,@bd)) as) ,@bd))

#|
(smlet a '(lis 1 2 3)
  a)
|#
(mac smlet (a x . bd)
  `(let ,a (smc ,x) ,@bd))

(mac smwith (vs . bd)
  `(with ,(mapfapp (fr to) `(,fr (smc ,to)) (grp vs 2))
     ,@bd))

;;; defover ;;;

#|
(def test (a) (prn a))
(defover test (a b)
  (sup a)
  (prn b))
(test 3 4)
->
3
4
nil
|#

(mac defover (nm ag . bd)
  `(let sup ,nm
     (= ,nm (nfn ,nm ,ag ,@bd))))

(mac macover (nm ag . bd)
  `(let sup ,nm
     (= ,nm (nmc ,nm ,ag ,@bd))))

(mac fletover ((nm . mbd) . bd)
  `(let ,nm (let sup ,nm (nmc ,nm ,@mbd))
     ,@bd))

#|
(def test (a) (prn a))
(defbef test (a b)
  (prn b))
(test 3 4)
->
4
3
nil
|#

(mac defbef (nm ag . bd)
  `(defover ,nm #a
     (apl (fn ,ag ,@bd) #a)
     (apl sup #a)))

(mac defaft (nm ag . bd)
  `(defover ,nm #a
     (apl sup #a)
     (apl (fn ,ag ,@bd) #a)))

;;; once ;;;

(def mkngs (n)
  (if (is n 0) nil
      (cons (gs) (mkngs (- n 1)))))

; n gensyms
; can't use repcoll because of ngs -> repcoll -> rep -> down -> once -> ngs
(mac ngs (n v . bd)
  `(let ,v (mkngs ,n) ,@bd))

#|(mac ngs (n v . bd)
  `(let ,v (repcoll ,n (gs)) ,@bd))|#

#|
(with (a 1 b 2 c 3)
  (once (a b c)
    `(do (prn ,a)
         (prn ,b)
         (prn ,c))))
        
(mcx '(once (a b c)
        (prn a)
        (prn b)
        (prn c)))

(with (gs4 (gs) gs5 (gs) gs6 (gs))
  `(with (gs4 a gs5 b gs6 c)
     ,(with (a gs4 b gs5 c gs6)
        (prn a)
        (prn b)
        (prn c))))

(LET ((#:G3081 (GENSYM)) (#:G3082 (GENSYM)) (#:G3083 (GENSYM)))
  (LIST 'LET (LIST (LIST #:G3081 A) (LIST #:G3082 B) (LIST #:G3083 C))
    (LET ((A #:G3081) (B #:G3082) (C #:G3083))
      (PRN A))))

(with (#:G3081 (gs) #:G3082 (gs) #:G3083 (gs))
  `(with (,#:G3081 ,A ,#:G3082 ,B ,#:G3083 ,C)
     ,(with (A #:G3081 B #:G3082 C #:G3083)
        (prn A))))

(once (a b c)
 `(do (prn ,a)
      (prn ,b)
      (prn ,c))))

(with (gs4 (gs) gs5 (gs) gs6 (gs))
  `(with (,gs4 ,a ,gs5 ,b ,gs6 ,c)
     ,(with (a gs4 b gs5 c gs6)
        (prn a)
        (prn b)
        (prn c))))

(with (gs4 (if (sym? a) a (gs)) gs5 (gs) gs6 (gs))
  `(with ,(gslis (lis gs4 gs5 gs6) (lis a b c))
     ,(with (a gs4 b gs5 c gs6)
        (prn a)
        (prn b)
        (prn c))))

(with (gs4 (if (sym? a) a (gs)) gs5 (gs) gs6 (gs))
  (gswith (lis gs4 gs5 gs6) (lis a b c)
     (with (a gs4 b gs5 c gs6)
        (prn a)
        (prn b)
        (prn c))))


|#

#|(mac once (vs . bd)
  (if (sym? vs) `(once (,vs) ,@bd)
      (ngs (len vs) gens
        `(w/gs ,gens
          `(with ,,(map auq (flat (pair gens vs)))
             ,(with ,(flat (pair vs gens)) ,@bd))))))|#

#|(mac once (vs . bd)
  (if (sym? vs) `(once (,vs) ,@bd)
      (ngs (len vs) gens
        `(with ,(flat (pair gens (map [qq (if (sym? ,_) ,_ (gs))] vs)))
          `(with ,(gslis (lis ,@gens) (lis ,@vs))
             ,(with ,(flat (pair vs gens)) ,@bd))))))|#

(def pair (a b)
  (if (no a) nil
      (atm? a) (lis (lis a b))
      (app (pair (car a) (car b)) (pair (cdr a) (cdr b)))))

(def gswith (gens vs rst)
  `(withi ,(gslis gens vs) ,rst))

(def gslis (gens vs)
  (if (or (no gens) (no vs)) nil
      (sym? (car vs)) (gslis (cdr gens) (cdr vs))
      (app (lis (car gens) (car vs)) (gslis (cdr gens) (cdr vs)))))

(mac once (vs . bd)
  (w/lis vs
    (ngs (len vs) gens
      `(with ,(flat (pair gens (mapn `(if (sym? ,_) ,_ (gs)) vs)))
         (gswith (lis ,@gens) (lis ,@vs)
           (with ,(flat (pair vs gens)) ,@bd))))))

;;; prepif ;;;

#|
(prepif (no (set? 'a)) (let a 3)
  (al a))
->
(if (no (set? 'a)) (let a 3 (al a))
    (do (al a)))
|#

; prepend if
(mac prepif (c p . bd)
  `(if ,c ,(app p bd) (do ,@bd)))

; if v is not set, set it to d before running bd
(mac insure (v d . bd)
  `(prepif (no (set? ',v)) (let ,v ,d)
     ,@bd))

(mac globinsure (v d . bd)
  `(prepif (no (set? ',v)) (do (glob ,v ,d))
     ,@bd))

;;; dyn ;;;

(mac dyn (a x . bd)
  `(globinsure ,a nil
     (let #ori ,a
       (= ,a ,x)
       (prot (do ,@bd)
         (= ,a #ori)))))

(mac dyn+= (a x . bd)
  `(dyn ,a (+ ,a ,x) ,@bd))

(mac dynzap (f a . bd)
  `(dyn ,a (,f ,a) ,@bd))

(mac sta (a x . bd)
  `(do (push ,x ,a)
       (prot (do ,@bd)
         (pop ,a))))

(mac dynwith (vs . bd)
  (if (no vs) `(do ,@bd)
      `(dyn ,(car vs) ,(cadr vs)
         (dynwith ,(cddr vs) ,@bd))))

(mac dynmlet ((nm . mbd) . bd)
  `(dyn ,nm (nmc ,nm ,@mbd) ,@bd))

(mac dynmwith (as . bd)
  `(dynwith ,(mapfapp (nm . bd) `(,nm (nmc ,nm ,@bd)) as) ,@bd))

(mac tostr bd
  `(runoni #s ""
     (dyn *out* [app= #s _] ,@bd)))

;;; deferr ;;;

(mac deferr (nm ag . errag)
  `(mac ,nm ,ag
     (err ,nm ,@errag)))

;;; block ;;;

(deferr retfrom (s r) "Unknown block $1" s)

#|
(block a
  (prn " Entering BLOCK")
  (bar [retfrom a])
  (prn " Leaving BLOCK"))

->

(with (gs1 (lis nil) gs2 retfrom)
  (let retfrom (mc (s r)
               (if (is s 'a) `(throw gs1 ,r)
                   `(gs2 ,s ,r)))
    (catch gs1
      (prn " Entering BLOCK")
      (bar [retfrom a])
      (prn " Leaving BLOCK"))
|#

(mac block (v . bd)
  `(with (#g (lis nil) #retfrom retfrom)
     (mlet (retfrom (s r)
             (if (is s ',v) `(throw #g ,r)
                 `(#retfrom ,s ,r)))
       (catch #g ,@bd))))

(mac dynblock (v . bd)
  `(dynwith (#g (lis nil) #retfrom retfrom)
     (dynmlet (retfrom (s r)
                (if (is s ',v) `(throw #g ,r)
                    `(#retfrom ,s ,r)))
       (catch #g ,@bd))))

; block def
(mac bdef (nm ag . bd)
  `(def ,nm ,ag (block ,nm ,@bd)))

(mac brfn (nm ag . bd)
  `(rfn ,nm ,ag (block ,nm ,@bd)))

(mac w/exiter (nm fr . bd)
  `(flet (,nm (a) (retfrom ,fr a))
     ,@bd))

#|(mac w/exit (nm . bd)
  `(block #g
     (mlet (,nm (a) `(retfrom #g ,a))
       ,@bd)))|#
       
#|
(w/exit hey
  (prn 3)
  (hey "nice!")
  (prn 5))
->
3
"nice!"
|#

(mac w/exit (nm . bd)
  `(block #g
     (w/exiter ,nm #g
       ,@bd)))

(mac w/mexiter (nm fr . bd)
  `(mlet (,nm (a) `(retfrom ,,fr ,a))
     ,@bd))

;;; loop ;;;

#|
(loop (var i 0) (< i 10) (++ i)
  (if (is i 3) (cont))
  (prn i))
->
(sblock
  (mlet (cont () `(retfrom #g))
    (var i 0)
    (while (< i 10)
      (block #g
        (if (is i 3) (cont))
        (prn i))
      (++ i))))
|#

#|(mac loop (st p up . bd)
  `(sblock
     (mlet (cont () `(retfrom #g))
       ,st
       (while ,p
         (block #g ,@bd)
         ,up))))|#

#|(mac loop (vs p up . bd)
  `(withs ,vs
     (while ,p
       (w/exit cont ,@bd)
       ,up)))|#

(mac loop (vs p up . bd)
  `(withs ,vs
     (w/mexiter cont #g
       (while ,p
         (block #g ,@bd)
         ,up))))

(mac up (i n m . bd)
  (once (n m)
    `(loop (,i ,n) (<= ,i ,m) (++ ,i) ,@bd)))

(alias for up)

(mac down (i n m . bd)
  (once (n m)
    `(loop (,i ,n) (>= ,i ,m) (-- ,i) ,@bd)))

(mac to (i n . bd)
  (once n
    `(loop (,i 0) (< ,i ,n) (++ ,i) ,@bd)))

(mac from (i n . bd)
  `(down ,i ,n 0 ,@bd))
  
(mac index (i a . bd)
  `(to ,i (len ,a) ,@bd))

(mac indexr (i a . bd)
  `(from ,i (- (len ,a) 1) ,@bd))

(mac indexval (i x a . bd)
  (once a
    `(let ,x nil
        (index ,i ,a
          (= ,x (,a ,i))
          ,@bd))))

(mac rep (n . bd)
  `(down #i ,n 1 ,@bd))

#|(mac eachlis (x a . bd)
  `(loop (#a ,a ,x nil) #a (zappop #a)
     (= ,x (car #a))
     ,@bd))

(mac eacharr (x a . bd)
  `(indexval #i ,x ,a ,@bd))

(mac each (x a . bd)
  (once a
    `(casetyp ,a
       nil nil
       lis (eachlis ,x ,a ,@bd)
       arr (eacharr ,x ,a ,@bd)
       (err each "Can't loop through each in a = $1" a))))|#

(mac each (x a . bd)
  `(w/mexiter cont #g
     (eachfn ,a
       (brfn #g (,x) ,@bd))))

(mac eachexist (x a . bd)
  `(each ,x ,a
     (if (no ,x) (cont))
     ,@bd))

(mac oeach (i x a . bd)
  `(mlet (cont () `(retfrom #g))
     (oeachfn ,a
       (brfn #g (,i ,x) ,@bd))))

(mac forever a `(while t ,@a))

;;; Collectors ;;;

(mac w/collr (nm . bd)
  `(let #g nil
     (flet (,nm (a) (push a #g))
       ,@bd)
     (nrev #g)))

; (w/coll (for i 1 10 (coll i))) -> (1 2 3 4 5 6 7 8 9 10)
(mac w/coll bd
  `(w/collr coll ,@bd))

; (let a 0 (nofcoll 5 (++ a))) -> (1 2 3 4 5)
; nof collect
(mac repcoll (n . bd)
  `(w/collr #coll
     (rep ,n (#coll (do ,@bd)))))

(mac forcoll (i n m . bd)
  `(w/collr #coll
     (for ,i ,n ,m (#coll (do ,@bd)))))

; (lisfor 2 5) -> (2 3 4 5)
(def lisfor (n m)
  (forcoll i n m i))

;;; First (Loop Modifiers) ;;;

#|
(w/fst
  (each x '(1 2 3 4 5)
    (if fst (prn "first"))
    (prn x)))
->
first
1
2
3
4
5
nil
|#

(mac w/fsti (i a)
  `(let ,i t
     ,(tail a `(if ,i (= ,i nil)))))

(mac w/fst (a)
  `(w/fsti fst ,a))

;;; run ;;;

#|
(run prn '(1 2 () 3)
  btw (prn "hey!")
  skip-nil)
->
1
hey!
2
hey!
3
nil
|#

(mac run (f a . opt)
  (w/gs (g fst)
    `(w/fsti ,fst
       (each ,g ,a
         ,(if (has 'skip-nil opt) `(if (no ,g) (cont)))
         ,(iflet r (has 'btw opt) `(ifnot ,fst ,(cadr r)))
         (,f ,g)))))

(mac runn (bd a)
  `(run (fn1 ,bd) ,a))

(mac runf (ag bd a)
  `(run (fn (,ag) ,bd) ,a))

(mac runexist (f a . opt)
  `(run ,f ,a skip-nil ,@opt))

;;; and, or, in ;;;

(mac and a
  (if (no a) t
      (no (cdr a)) (car a)
      `(if ,(car a) (and ,@(cdr a)))))

(mac or a
  (w/gs g
    `(let ,g nil
        (if ,@(afta (mapn `(= ,g ,_) a) g)))))

#|
(or a b c)
-> (let gs nil
     (if (= gs a) gs
         (= gs b) gs
         (= gs c) gs))
|#

(mac in (x . a)
  (once x
    `(or ,@(mapn `(is ,x ,_) a))))

;;; (dot) ;;;

(mac . (x . a)
  (if (no a) x
      `(. ,(let y (car a)
               (if (atm? y) `(,x ',y)
                   `((. ,x ,(car y)) ,@(cdr y))))
          ,@(cdr a))))

(mac mmac (name arg1 arg2 . body)
  `(mac ,name ,arg1
     (mc ,arg2 ,@body)))

; ((dtfn a b c) x 1 2 3)
; -> ((. x a b c) 1 2 3)
(mmac dtfn a (x . args)
  `((. ,x ,@a) ,@args))

;;; mcx ;;;

(var mcxp nil)
(def mcx1 (a)
  (if mcxp (= mcxp nil))
  (if (atm? a) a
      (and (sym? (car a)) (set? (car a)))
        (let m (eval (car a))
          (if (mac? m) (do (= mcxp t)
                           (apl (dat m) (cdr a)))
              (mcx1l a)))
      (mcx1l a)))

(def mcx1l (a)
  (if (no a) nil
      (let e (mcx1 (car a))
        (if mcxp (cons e (cdr a))
            (cons (car a) (mcx1l (cdr a)))))))

(def mcx (a)
  (if (atm? a) a
      (and (sym? (car a)) (set? (car a)))
        (let m (eval (car a))
          (if (mac? m) (mcx (apl (dat m) (cdr a)))
              (map mcx a)))
      (map mcx a)))

;;; tailrec, inline ;;;

(mac tailrec (nm vs . bd)
  (let g (grp vs 2)
    `(mlet (,nm ,(map car g)
             `(nrt (do ,,@(mapn `(= ,_ ,(auq _)) (map car g)))))
       (with ,vs
         (forever
           (ret ,@bd))))))

(def pnms (a)
  (if (no a) nil
      (atm? a) (lis a)
      (is (car a) 'o) (lis (cadr a))
      (app (pnms (car a)) (pnms (cdr a)))))

(mac inline (nm ag . bd)
  `(mac ,nm ,ag
     `(do ,,@(let p (pnms ag)
               (dmap [if (has _ p) (auq _) _] bd)))))

;;; case ;;;

#|
(casef > 3
  1 'hey
  2 'what
  3 'whoa
  4 'yay
  5 'huh
  'no)
->
yay
|#

#|(mac casef (f x . a)
  (once x
    `(if ,@(mkcasef f x a))))

(def mkcasef (f x a)
  (if (no a) nil
      (no (cdr a)) (lis (car a))
      (lisd `(,f ,(car a) ,x) (cadr a)
             (mkcasef f x (cddr a)))))|#

#|
(mac case (x . a)
  `(casef (fn (a x) ((tfn a) x)) ,x
     ,@a))
|#

(mac case (x . a)
  (once x
    `(if ,@(mkcase x a))))

(def mkcase (x a)
  (if (no a) nil
      (no (cdr a)) (lis (car a))
      (lisd `((tfn ,(car a)) ,x) (cadr a)
             (mkcase x (cddr a)))))

; same as case but using is for comparison instead of tfn
#|(mac caseis (x . a)
  `(casef is ,x ,@a))|#
#|

(casesym 'test
  a 'hey
  b 'what
  (c d e) 'you
  'else)
  
->

(case 'test
  'a 'hey
  'b 'what
  (infn 'c 'd 'e) 'you
  'else)
  
|#

(def infn a [has _ a])

(def mkcasesym (a)
  (if (no a) nil
      (no (cdr a)) (lis (car a))
      (lisd (if (atm? (car a))
                `',(car a)
                `(infn ,@(mapn `',_ (car a))))
            (cadr a)
            (mkcasesym (cddr a)))))

(mac casesym (x . a)
  `(case ,x ,@(mkcasesym a)))

(mac casetyp (x . a)
  `(casesym (typ ,x) ,@a))

;;; Default ;;;

#|
doesn't work:
(macgs *defs*
  (var *defs* {})
  (def setdef (a x)
    (= (*defs* a) x)))
->
(var gs1 {})
(def setdef (a x)
  (= (gs1 a) x))
|#

#|(mac macgs (v . bd)
  `(smlet ,v #g ,@bd))

(macgs *defs*
  (var *defs* {})
  (def setdef (a x)
    (= (*defs* a) x)))

(mac defvar1 (a x)
  `(do (var ,a ,x)
       (setdef ',a ,x)))

(bytwo defvar defvar1)|#

(var *defaults* {})
(var *defaultsf* {})

(def setdef (a x)
  (= (*defaults* a) x))

(def setdeff (a x)
  (= (*defaultsf* a) x))

(macby defvar (a x)
  `(do (var ,a ,x)
       (setdef ',a ,x)))

(def dcopy (a)
  (casetyp a
    (lis obj arr) (map dcopy a)
    (copy a)))

; defvar form
(macby defvarf (a x)
  (setdeff a (dcopy x))
  `(var ,a ,x))

(macby reset (a)
  (if (haskey *defaults* a) `(= ,a (*defaults* ',a))
      (haskey *defaultsf* a)  `(= ,a ,(*defaultsf* a))
      (err reset "a = $1 not in defaults list" a)))

;;; defargs ;;;

#|
(defargs test (a) (prn a) (prn args))
(test 1 2 3)
->
1
(1 2 3)
nil
|#

(mac defargsi (i nm ag . bd)
  `(def ,nm ,i
     ((fn ,ag ,@bd) @,i)))

(mac defargs (nm ag . bd)
  `(defargsi args ,nm ,ag ,@bd))

;;; tim ;;;

; (tim (rep 5 (prn 3))) -> 256
(mac tim bd
  `(let #t1 (currtim)
     ,@bd
     (- (currtim) #t1)))

; (timval (map [+ _ 3] '(1 2 3 4 5))) -> (0 (4 5 6 7 8))
(mac timval bd
  `(with (#t1 (currtim) #val (do ,@bd))
     (lis (- (currtim) #t1) #val)))

#|
(w/tim 'hey (+ 2 3))
->
Time hey: 1 ms
5
|#

(defvarf *times* nil)

(def prntra ()
  (runf (note tim) (prn "Time $1: $2 ms" note tim) *times*))

(def cleartra ()
  (reset *times*))

(mac w/tim (note . bd)
  `(let (#tim #val) (timval ,@bd)
     (prn "Time $1: $2 ms" ,note #tim)
     #val))

(mac w/tra (note . bd)
  `(let (#tim #val) (timval ,@bd)
     (push (lis ,note #tim) *times*)
     #val))

#|
(w/timfn test '(1 2 3 4 5) (+ 2 3))
->
Time (test 1 2 3 4 5): 0 ms
5
|#

(mac w/timfn (nm args . bd)
  `(w/tim `(,,nm ,@,args)
     ,@bd))

(mac w/trafn (nm args . bd)
  `(w/tra `(,,nm ,@,args)
     ,@bd))

#|
(def fact (n)
  (if (is n 0) 1
      (* n (fact (- n 1)))))
(timover fact)
(fact 5)
->
Time (fact 0): 0 ms
Time (fact 1): 5 ms
Time (fact 2): 9 ms
Time (fact 3): 65 ms
Time (fact 4): 68 ms
Time (fact 5): 73 ms
120
|#

(macby timover (nm)
  `(defover ,nm #args
     (w/timfn ,nm #args
       (sup @#args))))

(macby traover (nm)
  `(defover ,nm #args
     (w/trafn ,nm #args
       (sup @#args))))

#|
(deftim fact (n)
  (if (is n 0) 1
      (* n (fact (- n 1)))))
(fact 5)
->
Time (fact 0): 0 ms
Time (fact 1): 5 ms
Time (fact 2): 9 ms
Time (fact 3): 65 ms
Time (fact 4): 68 ms
Time (fact 5): 73 ms
120
|#

#|(mac deftim (nm ag . bd)
  `(defargsi #args ,nm ,ag
     (w/timfn ,nm #args
       ,@bd)))|#
       
(mac deftim (nm ag . bd)
  `(do (def ,nm ,ag ,@bd)
       (timover ,nm)))

(mac deftra (nm ag . bd)
  `(do (def ,nm ,ag ,@bd)
       (traover ,nm)))

;;; tagbody ;;;

(def splbef (a x (o l))
  (if (no a) (lis (nrev l))
      (x (car a)) (cons (nrev l) (splbef (cdr a) x (lis (car a))))
      (splbef (cdr a) x (cons (car a) l))))

; tagbody
(mac tags a
  (let (beftag . s) (splbef a sym?)
    `(mlet (go (a) `(ret (,a)))
        (sblock ,@beftag
              ,@(let g (maplis [lisd (caar _) (caadr _) (cdar _)] s)
                  (map mktag1 g))
              (,(caar s))))))

(def maplis (f a)
  (if (no a) nil
      (cons (f a) (maplis f (cdr a)))))

(def mktag1 ((nm tg . bd))
  `(def ,nm ()
     ,@bd
     ,(if (no tg) nil `(,tg))))

;;; nof ;;;

(def nof (n a)
  (runon (casetyp a
           lis nil
           arr #[]
           str ""
           sym '||
           num (sli 0 1)
           (err nof "Can't make n = $1 of a = $2" n a))
    (rep n (app= _ a))))

;;; do1 ;;;

(mac do1 a
  (if (no a) nil
      `(let #r ,(car a)
         ,@(cdr a)
         #r)))

;;; Object ;;;

(mac olay (a)
  `(= ,a {0 ,a}))

(mac oulay (a)
  `(= ,a (,a 0)))

; make object accessors
(mac mkoacc (nm pre)
  (let g (app '* nm '*)
    `(do (var ,g {})
         (def ,(app pre 'ref) (a) (oref ,g a))
         (def ,(app pre 'put) (a x) (oput ,g a x))
         (def ,(app pre 'set) (a x) (oset ,g a x))
         (def ,(app pre 'set?) (a) (oset? ,g a))
         (def ,(app pre 'del) (a) (odel ,g a))
         (def ,(app pre 'ren) (a b) (oren ,g a b))
         (def ,(app pre 'lay) () (olay ,g))
         (def ,(app pre 'ulay) () (oulay ,g)))))

;;; bug ;;;

; (mapi1 (fn (_ i) `(,_ ,i)) '(a b c)) -> ((a 0) (b 1) (c 2))
(def mapi1 (f a (o i 0))
  (if (no a) nil
      (cons (f (car a) i) (mapi1 f (cdr a) (+ i 1)))))

; (mapi `(,_ ,i) '(a b c)) -> ((a 0) (b 1) (c 2))
(mac mapi (fbd a (o i 0))
  `(mapi1 (fn (_ i) ,fbd) ,a ,i))

(mac bug a
  `(let #g (lis ,@a)
     (al ,(join (mapi (str _ " = $" i) a 1) " | ") @#g)
     (las #g)))

(mac bugs a
  `(do ,@(mapn `(bug ,_) a)))

(mac bugnm (nm . a)
  `(let #g (lis ,@a)
     (al (str ,nm " | "
              ,(join (mapi (str _ " = $" i) a 1) " | "))
         @#g)
     (las #g)))

(mac bugsnm (nm . a)
  `(do ,@(mapn `(bugnm ,nm ,_) a)))

;;; Type ;;;

; applies f to dat property of a
; (calldat [+ _ 3] (mkdat 'test 3 {a 5})) -> <test {a 5 data 6}>
(def calldat (f a)
  (mkdat (typ a) (f (dat a)) (ob a)))

(macby defbui (tp)
  `(def ,tp (a) (mkdat ',tp a)))

(macby defbuil (tp)
  `(def ,tp a (mkdat ',tp a)))

(mac defbuild (tp ag opt)
  `(def ,tp ,ag
     (mk ',tp ,opt)))

(mac defbuidat (tp ag a opt)
  `(def ,tp ,ag
     (mkdat ',tp ,a ,opt)))

(macby defpred (tp)
  `(def ,(app tp '?) (a) (isa ',tp a)))

;;; package ;;;

(mac package (nm . bd)
  `(do (var ,nm {})
       (sblock
         (macby export (a)
           `(= (,,nm ',a) ,a))
         ,@bd)))

(mac import (nm . vs)
  `(do ,@(mapn `(var ,_ (,nm ',_)) vs)))

;;; Random Functions ;;;

; group overlap
; (grpovr '(1 2 3 4 5 6 7 8) 3 0) -> ((1 2 3) (4 5 6) (7 8))
; (grpovr '(1 2 3 4 5 6 7 8) 3 1) -> ((1 2 3) (3 4 5) (5 6 7) (7 8))
(def grpovr (a n m)
  (if (>= m n) (err grpovr "m = $1 must be < n = $2" m n)
      (no a) nil
      (no (ncdr n a)) (lis (fstn n a))
      (cons (fstn n a) (grpovr (ncdr (- n m) a) n m))))

(def fold3 (a zer one two)
  (if (no a) (zer)
      (no (cdr a)) (one (car a))
      (fold two a)))

(def foldr3 (a zer one two)
  (if (no a) (zer)
      (no (cdr a)) (one (car a))
      (foldr two a)))

#|(def las (a)
  (if (no a) nil
      (no (cdr a)) (car a)
      (las (cdr a))))|#

(def but (a)
  (if (no a) nil
      (no (cdr a)) nil
      (cons (car a) (but (cdr a)))))

; (flatall '((ts g e (ge ef)) (e f e) (f e))) -> (ts g e ge ef e f e f e)
(def flatall (a)
  (if (no a) nil
      (atm? (car a)) (cons (car a) (flatall (cdr a)))
      (app (flatall (car a)) (flatall (cdr a)))))


