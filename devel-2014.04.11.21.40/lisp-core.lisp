(var mac (mc (nm ag . bd)
           `(var ,nm (mc ,ag ,@bd))))

(mac def (nm ag . bd)
  `(var ,nm (fn ,ag ,@bd)))

(def aq (a) (lis 'uq a))

(mac blk a
  `((fn () ,@a)))

(mac let (a x . bd)
  `((fn (,a) ,@bd) ,x))

(mac with (vs . bd)
  (let g (grp vs 2)
    `((fn ,(map car g) ,@bd) ,@(map cadr g))))

(mac wgs (nm . bd)
  (if (lis? nm)
        `(with ,(afta nm '(gs)) ,@bd)
      `(let ,nm (gs) ,@bd)))

(def afta (a x)
  (if (no a) nil
      (cons (car a)
            (cons x (afta (cdr a) x)))))

(mac rfn (nm ag . bd)
  `(let ,nm nil (= ,nm (fn ,ag ,@bd))))

(mac afn (ag . bd)
  `(rfn self ,ag ,@bd))

(mac loop (st p up . bd)
  `(blk ,st (whi ,p ,@bd ,up)))

(mac for (i n m . bd)
  `(let #m ,m
     (loop (var ,i ,n) (<= ,i #m) (++ ,i) ,@bd)))

(mac to (i n . bd)
  `(let #m ,n
     (loop (var ,i 0) (< ,i #m) (++ ,i) ,@bd)))

(mac fr (i n . bd)
  `(loop (var ,i ,n) (>= ,i 0) (-- ,i) ,@bd))

(mac rep (n . bd)
  `(loop (var #i ,n) (>= #i 1) (-- #i) ,@bd))

(mac and a
  (if (no a) t
      (no (cdr a)) (car a)
      `(if ,(car a) (and ,@(cdr a)))))

(mac or a
  (wgs g
    `(let ,g nil
        (if ,@(afta (map #[qq (= ,g ,%)] a) g)))))

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
(def mcx (a)
  (if mcxp (= mcxp nil))
  (if (lis? a)
    (if (and (sym? (car a)) (set? (car a)))
      (let m (evl (car a))
        (if (mac? m)
          (do (= mcxp t)
              (apl (rp m) (cdr a)))
          (mcxl a)))
      (mcxl a))
    a))

(def mcxl (a)
  (if (no a) nil
      (let e (mcx (car a))
        (if mcxp
          e
          (cons (car a) (mcxl (cdr a)))))))

(def mcxa (a)
  (if (lis? a)
    (if (and (sym? (car a)) (set? (car a)))
      (let m (evl (car a))
        (if (mac? m)
          (mcxa (apl (rp m) (cdr a)))
          (mcxal a)))
      (mcxal a))
    a))

(def mcxal (a)
  (if (no a) nil
      (cons (mcxa (car a)) (mcxal (cdr a)))))

(mac in (x . a)
  `(or ,@(map #[qq (is ,x ,%)] a)))

(mac inl (nm ag . bd)
  `(mac ,nm ,ag
     `(do ,,@(let p (pnms ag)
               (dmap #[if (has % p) (aq %) %] bd)))))

(def pnms (a)
  (if (no a) nil
      (atm? a) (lis a)
      (is (car a) 'o) (lis (cadr a))
      (app (pnms (car a)) (pnms (cdr a)))))


