(var mac (mc (nm ag . bd)
           `(var ,nm (mc ,ag ,@bd))))

(mac def (nm ag . bd)
  `(var ,nm (fn ,ag ,@bd)))

(mac dot () `|.|)

(def aqt (a) (lis 'qt a))
(def auq (a) (lis 'uq a))

(mac byone (nm a)
  ``(do ,@(map [qq (,,nm ,_)] ,a)))

(mac bytwo (nm a)
  ``(do ,@(map [qq (,,nm ,(car _) ,(cadr _))] (grp ,a 2))))

(mac nfn (a) `(fn (_) ,a))

(mac blk a
  `((fn () ,@a)))

(mac let (a x . bd)
  `((fn (,a) ,@bd) ,x))

(mac with (vs . bd)
  (let g (grp vs 2)
    `((fn ,(map car g) ,@bd) ,@(map cadr g))))

(mac withi (vs . bd)
  (let r (remdup vs)
    (if (no r) `(do ,@bd)
        `(with ,r ,@bd))))

(def remdup (a)
  (if (no a) nil
      (no (cdr a)) (lis (car a))
      (is (car a) (cadr a)) (remdup (cddr a))
      (lisd (car a) (cadr a) (remdup (cddr a)))))

(mac withs (vs . bd)
  (if (no vs) `(do ,@bd)
      `(let ,(car vs) ,(cadr vs)
         (withs ,(cddr vs) ,@bd))))

(mac symlis (a . bd)
  `(if (sym? ,a) (let ,a (lis ,a) ,@bd)
       (do ,@bd)))

(mac wgs (nm . bd)
  (symlis nm
    `(with ,(afta nm '(gs)) ,@bd)))

(mac wrec (vs . bd)
  (let g (grp vs 2)
    `(with ,(fla (map [lis (car _) nil] g))
        ,@(map [qq (= ,(car _) ,(cadr _))] g)
        ,@bd)))

(def afta (a x)
  (if (no a) nil
      (cons (car a)
            (cons x (afta (cdr a) x)))))

(mac rfn (nm ag . bd)
  `(let ,nm nil
     (= ,nm (fn ,ag ,@bd))))

(mac afn (ag . bd)
  `(rfn self ,ag ,@bd))

(mac mlet (a . bd)
  `(let ,(car a) (mc ,@(cdr a)) ,@bd))

(mac rwith (nm vs . bd)
    (let g (grp vs 2)
      `((rfn ,nm ,(map car g) ,@bd) ,@(map cadr g))))

(mac infloop a `(whi true ,@a))

(mac tailrec (nm vs . bd)
    (let g (grp vs 2)
      `(mlet (,nm ,(map car g)
               `(nrt (do ,,@(map [qq (= ,_ ,(auq _))] (map car g)))))
         (with ,vs
           (infloop
             (ret ,@bd))))))

(mac ngs (n v . bd)
  `(let ,v (mkngs ,n) ,@bd))

(def mkngs (n)
  (if (is n 0) nil
      (cons (gs) (mkngs (- n 1)))))

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
        `(wgs ,gens
          `(with ,,(map auq (fla (par gens vs)))
             ,(with ,(fla (par vs gens)) ,@bd))))))|#

#|(mac once (vs . bd)
  (if (sym? vs) `(once (,vs) ,@bd)
      (ngs (len vs) gens
        `(with ,(fla (par gens (map [qq (if (sym? ,_) ,_ (gs))] vs)))
          `(with ,(gslis (lis ,@gens) (lis ,@vs))
             ,(with ,(fla (par vs gens)) ,@bd))))))|#

(mac once (vs . bd)
  (symlis vs
    (ngs (len vs) gens
      `(with ,(fla (par gens (map [qq (if (sym? ,_) ,_ (gs))] vs)))
         (gswith (lis ,@gens) (lis ,@vs)
           (with ,(fla (par vs gens)) ,@bd))))))

(def gswith (gens vs rst)
  `(withi ,(gslis gens vs) ,rst))

(def gslis (gens vs)
  (if (or (no gens) (no vs)) nil
      (sym? (car vs)) (gslis (cdr gens) (cdr vs))
      (app (lis (car gens) (car vs)) (gslis (cdr gens) (cdr vs)))))

(mac loop (st p up . bd)
  `(blk ,st (while ,p ,@bd ,up)))

(mac for (i n m . bd)
  (once (n m)
    `(loop (var ,i ,n) (<= ,i ,m) (++ ,i) ,@bd)))

(mac down (i n m . bd)
  (once (n m)
    `(loop (var ,i ,n) (>= ,i ,m) (-- ,i) ,@bd)))

(mac to (i n . bd)
  (once n
    `(loop (var ,i 0) (< ,i ,n) (++ ,i) ,@bd)))

(mac fr (i n . bd)
  `(down ,i ,n 0 ,@bd))
  
(mac ind (i a . bd)
  `(to ,i (len ,a) ,@bd))

(mac inr (i a . bd)
  `(fr ,i (- (len ,a) 1) ,@bd))

(mac rep (n . bd)
  `(down #i ,n 1 ,@bd))

(mac nofcol (n a)
  `(let #g nil
     (rep ,n (psh ,a #g))
     (rev #g)))

(mac and a
  (if (no a) t
      (no (cdr a)) (car a)
      `(if ,(car a) (and ,@(cdr a)))))

(mac or a
  (wgs g
    `(let ,g nil
        (if ,@(afta (map [qq (= ,g ,_)] a) g)))))

#|
(or a b c)
-> (let gs nil
     (if (= gs a) gs
         (= gs b) gs
         (= gs c) gs))
|#
(mac no (a) `(nil? ,a))
(mac not (a) `(no ,a))

(mac when (ts . bd)
  `(if ,ts (do ,@bd)))

(mac mmc (name arg1 arg2 . body)
  `(mac ,name ,arg1
     (mc ,arg2 ,@body)))

; ((dtfn a b c) x 1 2 3)
; -> ((. x a b c) 1 2 3)
(mmc dtfn a (x . args)
  `((. ,x ,@a) ,@args))

(mac zap (f a . rst)
  `(= ,a (,f ,a ,@rst)))

(mac += (a x) `(= ,a (+ ,a ,x)))
(mac -= (a x) `(= ,a (- ,a ,x)))

(mac ++ (a) `(+= ,a 1))
(mac -- (a) `(-= ,a 1))

(var mcxp nil)
(def mcx1 (a)
  (if mcxp (= mcxp nil))
  (if (lis? a)
    (if (and (sym? (car a)) (set? (car a)))
      (let m (evl (car a))
        (if (mac? m)
          (do (= mcxp t)
              (apl (rp m) (cdr a)))
          (mcx1l a)))
      (mcx1l a))
    a))

(def mcx1l (a)
  (if (no a) nil
      (let e (mcx1 (car a))
        (if mcxp (cons e (cdr a))
            (cons (car a) (mcx1l (cdr a)))))))

(def mcx (a)
  (if (lis? a)
    (if (and (sym? (car a)) (set? (car a)))
      (let m (evl (car a))
        (if (mac? m)
          (mcx (apl (rp m) (cdr a)))
          (map mcx a)))
      (map mcx a))
    a))

(mac in (x . a)
  (once x
    `(or ,@(map [qq (is ,x ,_)] a))))

(mac inl (nm ag . bd)
  `(mac ,nm ,ag
     `(do ,,@(let p (pnms ag)
               (dmap [if (has _ p) (auq _) _] bd)))))

(def pnms (a)
  (if (no a) nil
      (atm? a) (lis a)
      (is (car a) 'o) (lis (cadr a))
      (app (pnms (car a)) (pnms (cdr a)))))

(mac case (x . a)
  (once x
    `(if ,@(mkcase x a))))

(def mkcase (g a)
  (if (no a) nil
      (no (cdr a)) (lis (car a))
      (lisd `((tfn ,(car a)) ,g) (cadr a)
             (mkcase g (cddr a)))))

(def splbef (a x (o l))
  (if (no a) (lis (nrev l))
      (x (car a)) (cons (nrev l) (splbef (cdr a) x (lis (car a))))
      (splbef (cdr a) x (cons (car a) l))))

(mac tags a
  (let (f . s) (splbef a sym?)
    `(mlet (go (a) `(ret (,a)))
        (blk ,@f
          ,@(let g (maplis [lisd (caar _) (caadr _) (cdar _)] s)
              (map mktag1 g))
         (,(caar s))))))

(def maplis (f a)
  (if (no a) nil
  (cons (f a) (maplis f (cdr a)))))

(def mktag1 (a)
  `(def ,(car a) ()
     ,@(cddr a)
     ,(if (no (cadr a)) nil (lis (cadr a)))))
   