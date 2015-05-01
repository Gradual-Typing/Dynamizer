(module annotizer racket
  (require "./trecon.rkt" syntax/parse)
  (provide nAnnotize)
  (define (nAnnotizeTy t)
    (match t
      [(? number?) (list #f 'Int)]
      [(cons t1 t2) (cons #f (map (lambda (x) (apply x '()))
                                  (flatten (for/list ([t1_ (nAnnotizeTy t1)])
                                             (for/list ([t2_ (nAnnotizeTy t2)])
                                               (lambda () (cons t1_ t2_)))))))]
      [_ (list #f t)]))
  (define (nAnnotize_ ee)
    (syntax-parse ee
      #:literals (if lambda)
      [(if e1 e2 e3) (flatten (for/list ([e1_ (nAnnotize_ #'e1)])
                                (for/list ([e2_ (nAnnotize_ #'e2)])
                                  (for/list ([e3_ (nAnnotize_ #'e3)])
                                    (with-syntax ([x e1_]
                                                  [y e2_]
                                                  [z e3_])
                                      (syntax-property #'(if x y z) 'type (type-of e2_)))))))]
      [(lambda (x) e) (let ([t1 (type-of #'x)]
                            [t2 (type-of #'e)])
                        (flatten (for/list ([t1_ (nAnnotizeTy t1)])
                                   (for/list ([t2_ (nAnnotizeTy t2)])
                                     (for/list ([e_ (nAnnotize_ #'e)])
                                       (with-syntax ([e_ e_])
                                         (with-syntax ([e_ (syntax-property #'e_ 'type t2_)]
                                                       [x_ (syntax-property #'x 'type t1_)])
                                           (syntax-property #'(lambda (x_) e_) 'type (cons t1_ t2_)))))))))]
      [(e1 e2) (flatten (for/list ([e1_ (nAnnotize_ #'e1)])
                          (for/list ([e2_ (nAnnotize_ #'e2)])
                            (with-syntax ([x e1_]
                                          [y e2_])
                              #'(x y)))))]
      [(op e1 e2) (flatten (for/list ([e1_ (nAnnotize_ #'e1)])
                             (for/list ([e2_ (nAnnotize_ #'e2)])
                               (with-syntax ([x e1_]
                                             [y e2_])
                                 #'(op y z)))))]
      [e (list #'e)]))
  (define (syntax->string e)
    (format "~a" (syntax-e e)))
  (define (schml-tygen t)
    (match t
      ['#f "Dyn"]
      ['Int "Int"]
      ['Bool "Bool"]
      [(cons t1 t2) (string-append "-> " (schml-tygen t1) " " (schml-tygen t2) ")")]
      [(? number?) "Int"]))
  (define (schml-codegen e)
    (syntax-parse e
      #:literals (if lambda)
      [v:identifier (syntax->string #'v)]
      [n:number (syntax->string #'n)]
      [b:boolean (syntax->string #'b)]
      [(if e1 e2 e3) (let ([e1s (schml-codegen #'e1)]
                           [e2s (schml-codegen #'e2)]
                           [e3s (schml-codegen #'e3)])
                       (string-append "(if " e1s "\n" e2s "\n" e3s ")"))]
      [(lambda (x) e) (let ([t1 (syntax-property #'x 'type)]
                            [t2 (syntax-property #'e 'type)])
                        (string-append "(lambda ("
                                       (if t1
                                           (string-append"[" (syntax->string #'x) " : " (schml-tygen t1) "]")
                                           (syntax->string #'x))
                                       ") : " (schml-tygen t2) "\n" (schml-codegen #'e) ")"))]
      [(e1 e2) (string-append "(" (schml-codegen #'e1) " " (schml-codegen #'e2) ")")]
      [(op e1 e2) (syntax-parse #'op
                    ['+ (string-append "(+ " (schml-codegen #'e1) " " (schml-codegen #'e2) ")")]
                    ['* (string-append "(* " (schml-codegen #'e1) " " (schml-codegen #'e2) ")")])]))
  (define (nAnnotize e)
    (letrec ([dir "test/"]
             [map-write (lambda (n l)
                          (unless (null? l)
                            (begin (display-to-file (string-append (car l) "\n") (string-append dir (number->string n) ".schml"))
                                   (map-write (+ n 1) (cdr l)))))])
      (make-directory dir)
      (map-write 0 (map schml-codegen (nAnnotize_ (annotize e))))))
  )
