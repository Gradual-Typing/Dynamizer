#lang racket

(require syntax/parse)
(provide annotize type-of)

;; Types: 'Int | 'Bool | (cons t1 t2) | number?
(define (type-of id)
  (let ([t (syntax-property id 'type)])
    (if t t (error "untyped syntax object" id))))
(define (annotize e)
  (define tenv (make-hash))
  (define subs (make-hash))
  (define fresh -1)
  (define (look-type v)
    (hash-ref tenv v))
  (define (add-type! v t)
    (hash-set! tenv v t))
  (define (look-sub tv res)
    (hash-ref subs tv res))
  (define (add-sub tv t)
    (hash-set! subs tv t))
  ;;  Create a fresh type variable
  (define gen-tvar (lambda () (set! fresh (+ fresh 1)) fresh))
  ;; hypothetical reasoning
  (define (tvsub t)
    (match t
      [(cons t1 t2) (cons (tvsub t1) (tvsub t2))]
      [_ (if (integer? t)
             (let ([t_ (look-sub t #f)])
               (if t_
                   (tvsub t_)
                   t))
             t)]))
  ;; shallow substitution
  (define (tvchase t)
    (if (integer? t)
        (let ([t_ (look-sub t #f)])
          (if t_
              (tvchase t_)
              t))
        t))
  ;; if tv appears free in t
  (define (is-free tv t)
    (match t
      ['Int #f]
      ['Bool #f]
      [(cons t1 t2) (or (is-free tv t1) (is-free tv t2))]
      [(? number?) (let ([t_ (look-sub t (= tv t))])
                     (if (boolean? t_)
                         t_
                         (is-free tv t_)))]))
  (define (unifyv tv t)
    (match t
      [(? number?) (unless (= tv t) (add-sub tv t))]
      [_ (if (is-free tv t)
             (error "if-free check failed")
             (add-sub tv t))]))
  (define (unify_ t1 t2)
    (match* (t1 t2)
      [('Int 'Int) void]
      [('Bool 'Bool) void]
      [((cons t1a t1r) (cons t2a t2r)) (unify t1a t2a) (unify t1r t2r)]
      [((? number?) _) (unifyv t1 t2)]
      [(_ (? number?)) (unifyv t2 t1)]
      [(_ _) (error "constant mismatch")]))
  (define (unify t1 t2)
    (unify_ (tvchase t1) (tvchase t2)))
  (define (add-type-vars! v stx t)
    (syntax-parse stx
      #:literals (if lambda)
      [(lambda (x) e) (if (eq? (syntax-e #'x) v)
                          #'(lambda (x) e)
                          (with-syntax ([e_ (add-type-vars! v #'e t)])
                            #'(lambda (x) e_)))]
      [(e1 e2) (with-syntax ([e2_ (add-type-vars! v #'e2 t)]
                             [e1_ (add-type-vars! v #'e1 t)])
                 #'(e1_ e2_))]
      [(if e1 e2 e3) (with-syntax ([e1_ (add-type-vars! v #'e1 t)]
                                   [e2_ (add-type-vars! v #'e2 t)]
                                   [e3_ (add-type-vars! v #'e3 t)])
                       #'(if e1_ e2_ e3_))]
      [v_:identifier (if (eq? (syntax-e #'v_) v)
                         (with-syntax ([v__ (syntax-property #'v_ 'type t)])
                           (add-type! #'v__ t)
                           #'v__)
                         #'v_)]
      [e #'e]))
  (define (infer e)
    (syntax-parse e
      #:literals (if lambda)
      [v:identifier (let ([t_ (look-type #'v)])
                      (syntax-property #'v 'type t_))]
      [n:number (syntax-property #'n 'type 'Int)]
      [b:boolean (syntax-property #'b 'type 'Bool)]
      [(if e1 e2 e3) (with-syntax ([e1_ (infer #'e1)])
                       (unify (type-of #'e1_) 'Bool)
                       (with-syntax ([e2_ (infer #'e2)]
                                     [e3_ (infer #'e3)])
                         (let ([t2 (type-of #'e2_)])
                           (unify t2 (type-of #'e3_))
                           (syntax-property #'(if e1_ e2_ e3_) 'type t2))))]
      [(lambda (x) e) (let ([tv (gen-tvar)])
                        (add-type! #'x tv)
                        (with-syntax ([e (add-type-vars! (syntax-e #'x) #'e tv)]
                                      [x (syntax-property #'x 'type tv)])
                          (with-syntax ([e_ (infer #'e)])
                            (syntax-property #'(lambda (x) e_) 'type (cons tv (type-of #'e_))))))]
      [(e1 e2) (with-syntax ([e1_ (infer #'e1)]
                             [e2_ (infer #'e2)])
                 (let ([tv (gen-tvar)])
                   (unify (type-of #'e1_) (cons (type-of #'e2_) tv))
                   (syntax-property #'(e1_ e2_) 'type tv)))]
      [(op x y) (with-syntax ([x_ (infer #'x)]
                              [y_ (infer #'y)])
                  (unify (type-of #'x_) 'Int)
                  (unify (type-of #'y_) 'Int)
                  (syntax-property #'(op x_ y_) 'type 'Int))]))
  (define (loctvsub e)
    (let ([t (tvsub (type-of e))])
      (syntax-parse e
        #:literals (if lambda)
        [v:identifier (syntax-property #'v 'type t)]
        [n:number #'n]
        [b:boolean #'b]
        [(if e1 e2 e3) (with-syntax ([e1_ (loctvsub #'e1)] [e2_ (loctvsub #'e2)] [e3_ (loctvsub #'e3)])
                         (syntax-property #'(if e1_ e2_ e3_) 'type t))]
        [(lambda (x) e) (with-syntax ([x_ (loctvsub #'x)]
                                      [e_ (loctvsub #'e)])
                          (syntax-property #'(lambda (x_) e_) 'type t))]
        [(e1 e2) (with-syntax ([e1_ (loctvsub #'e1)]
                               [e2_ (loctvsub #'e2)])
                   (syntax-property #'(e1_ e2_) 'type t))]
        [(op x y) (with-syntax ([x_ (loctvsub #'x)]
                                [y_ (loctvsub #'y)])
                    (syntax-property #'(op x_ y_) 'type t))])))
  (loctvsub (infer e)))
