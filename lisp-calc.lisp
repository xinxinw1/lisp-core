(def mapcdr (f a)
  (cons (car a) (map f (cdr a))))

(def der (a)
  (prune (der1 a)))

(def rpltree (x y a)
  (if (no a) nil
      (atm? a) (if (is a x) y a)
      (cons (rpltree x y (car a)) (rpltree x y (cdr a)))))

(defbuild x ((o n 1) (o p 1)) {n n p p})
(defpred x)

(def grpby (f a)
  (
  (mapn `(,_ ,(f _)) a)

(def prep (a)
  (rpltree 'x (x)))

(def der1 (a)
  (if (atm? a) (if (x? a) 1 0)
      (case (car a)
        '+ (mapcdr der1 a)
        '* `(+ (* ,(der1 (a 1)) ,(a 2)) (* ,(a 1) ,(der1 (a 2))))
        (err der1 "Unknown a = $1" a))))

(def prune (a)
  (if (atm? a) a
      (case (car a)
        '+ (prune+ (map prune (cdr a)))
        '* (prune* (map prune (cdr a)))
         (err prune "Unknown a = $1" a))))

(def single (a)
  (no (cdr a)))

(def sumf (f a)
  (sum (map f a)))

(def prune+ (a)
  (zap [rem 0 _] a)
  (if (single a) (car a)
      (sum [_ 'n] (keep x? a))
      `(+ ,@a)))

(def prune* (a)
  (if (has 0 a) 0
      (do (zap [rem 1 _] a)
          (if (no a) 1
              (single a) (car a)
              `(* ,@a)))))