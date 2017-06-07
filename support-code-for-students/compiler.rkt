#lang racket
(require racket/fixnum)
(require racket/trace)
(require "interp.rkt")
(require "utilities.rkt")
;; This exports r0-passes, defined below, to users of this file.
(provide r0-passes r1-passes)

;; The following pass is just a silly pass that doesn't change anything important,
;; but is nevertheless an example of a pass. It flips the arguments of +. -Jeremy
(define (flipper e)
  (match e
    [(? fixnum?) e]
    [`(read) `(read)]
    [`(- ,e1) `(- ,(flipper e1))]
    [`(+ ,e1 ,e2) `(+ ,(flipper e2) ,(flipper e1))]
    [`(program ,e) `(program ,(flipper e))]
    ))


;; Next we have the partial evaluation pass described in the book.
(define (pe-neg r)
  (cond [(fixnum? r) (fx- 0 r)]
	[else `(- ,r)]))

(define (pe-add r1 r2)
  (cond [(and (fixnum? r1) (fixnum? r2)) (fx+ r1 r2)]
	[else `(+ ,r1 ,r2)]))

(define (pe-arith e)
  (match e
    [(? fixnum?) e]
    [`(read) `(read)]
    [`(- ,e1) (pe-neg (pe-arith e1))]
    [`(+ ,e1 ,e2) (pe-add (pe-arith e1) (pe-arith e2))]
    [`(program ,e) `(program ,(pe-arith e))]
    ))   

;; Define the passes to be used by interp-tests and the grader
;; Note that your compiler file (or whatever file provides your passes)
;; should be named "compiler.rkt"
(define r0-passes
  `( ("flipper" ,flipper ,interp-scheme)
     ("partial evaluator" ,pe-arith ,interp-scheme)
     ))

(define (mkassign v exp)
  (list 'assign v exp))

(define (mkreturn v)
  (list 'return v))

;; uniquify
(define uniquify
  (letrec ([helper
            (lambda (symbol-table e)
              (match e
                [(? symbol?)
                 (lookup e symbol-table)]
                [(? integer?)
                 e]
                [`(let ([,x ,exp]) ,body)
                 (let ([newvar (gensym x)])
                   `(let ([,newvar ,(helper symbol-table exp)])
                      ,(helper (add-table `(,x . ,newvar) symbol-table) body)))]
                [`(,op ,es ...)
                 `(,op ,@(map (lambda (ne)
                                (helper symbol-table ne)) es))]))])
    (lambda (p)
      (match p
        [`(program ,body)
         `(program ,(helper '() body))]))))

;; (trace uniquify)

;; flatten
;; (let ([x 10]) body)
;; ==> (assign x 10)
;;     (body')
;; (+ (+ 1 2) (+ 3 4))
;; ==> (assign x (+ 1 2))
;;     (assign y (+ 3 4))
;;     (+ x y)

(define (remove-duplicate-var lst)
  lst)

(define flatten-r0
  (letrec ([helper
            (lambda (e)
              (match e
                [(? symbol?)
                 (values e '() `(,e))]
                [(? integer?)
                 (values e '() '())]
                [`(let ([,x ,exp]) ,body)
                 (let-values ([(id1 statements1 vlst1) (helper exp)]
                              [(id2 statements2 vlst2) (helper body)])
                   (values id2
                           `(,@statements1 ,(mkassign x id1) ,@statements2)
                           `(,@vlst1 ,x ,@vlst2)))]
                ['(read)
                 (let ([newv (gensym 'tmp)])
                   (values newv `(,(mkassign newv '(read))) `(,newv)
                           ))]
                [`(,op ,es ...)
                 (let-values ([(idlst statements vlst) (map3 helper es)]
                              [(newid) (gensym 'tmp)])
                   (values newid `(,@(apply append statements) ,(mkassign newid `(,op ,@idlst))) `(,@(apply append vlst) ,newid)))]))])
    (lambda (e)
      ;; (printf "~s\n" e)
      (match e
        [`(program ,body)
         (let-values ([(id statements vlst) (helper body)])
           `(program ,(remove-duplicate-var vlst) ,@statements ,(mkreturn id)))]))))

;; (trace flatten-r0)
;; (flatten-r0 (read-program "tests/r0_3.rkt"))
;; (flatten-r0 (read-program "tests/r0_1.rkt"))
;; (flatten-r0 (read-program "tests/r0_2.rkt"))

;; (uniquify '() '(+ 1 2))
;; (uniquify '() '(+ (let ([x 1])
;;                     (+ x 1))
;;                   2))
;; (uniquify '() '(let ([x 10]) (+ x 2)))
;; (uniquify '() '(let ([x 10])
;;                  (let ([y 20])
;;                    (let ([x 30])
;;                      (+ x y)))))

;; definition of x86

;;
(define (is-int? t)
  (match t
    [`(int ,_) #t]
    [_ #f]))

(define (is-reg? t)
  (match t
    [`(reg ,_) #t]
    [_ #f]))

(define (is-deref? t)
  (match t
    [`(deref ,_ ,_) #t]
    [_ #f]))

(define (is-movq? t)
  (match t
    [`(movq ,_ ,_) #t]
    [_ #f]))

(define (is-addq? t)
  (match t
    [`(addq ,_ ,_) #t]
    [_ #f]))

(define (is-subq? t)
  (match t
    [`(subq ,_ ,_) #t]
    [_ #f]))

(define (is-negq? t)
  (match t
    [`(negq ,_) #t]
    [_ #f]))

(define (is-pushq? t)
  (match t
    [`(pushq ,_) #t]
    [_ #f]))

(define (is-popq? t)
  (match t
    [`(popq ,_) #t]
    [_ #f]))

(define (is-callq? t)
  (match t
    [`(callq ,_) #t]
    [_ #f]))

(define (is-retq? t)
  (match t
    ['(retq) #t]
    [_ #f]))

;; constructor
;; singular operator
(define (mk-movq f t)
  (list 'movq f t))

(define (mk-addq f t)
  (list 'addq f t))

(define (mk-subq f t)
  (list 'subq f t))

(define (mk-deref r offset)
  (list 'deref r offset))

;; binary operator
(define (mk-pushq r)
  (list 'pushq r))

(define (mk-popq r)
  (list 'popq r))

(define (mk-negq r)
  (list 'negq r))

(define (mk-callq f)
  (list 'callq f))

(define (mk-retq)
  (list 'retq))


;; data
(define (mk-var v)
  (list 'var v))

(define (mk-int v)
  (list 'int v))

(define (mk-reg r)
  (list 'reg r))

(define (mk-arg v-or-c)
  (match v-or-c
    [(? symbol?)
     (mk-var v-or-c)]
    [(? integer?)
     (mk-int v-or-c)]))

;; accessor
(define (get-int-1 r)
  (list-ref r 1))

(define (get-reg-1 r)
  (list-ref r 1))

(define (get-movq-1 r)
  (list-ref r 1))

(define (get-movq-2 r)
  (list-ref r 2))

(define (get-addq-1 r)
  (list-ref r 1))

(define (get-addq-2 r)
  (list-ref r 2))

(define (get-subq-1 r)
  (list-ref r 1))

(define (get-subq-2 r)
  (list-ref r 2))

(define (get-negq-1 r)
  (list-ref r 1))

(define (get-pushq-1 r)
  (list-ref r 1))

(define (get-popq-1 r)
  (list-ref r 1))

(define (get-callq-1 r)
  (list-ref r 1))

(define (get-deref-1 r)
  (list-ref r 1))

(define (get-deref-2 r)
  (list-ref r 2))

;; end of x86 definition

(define (str-x86-term t)
  (cond
    [(is-int? t)
     (string-append "$" (number->string (get-int-1 t)))]        
    [(is-reg? t)
     (string-append "%" (symbol->string (get-reg-1 t)))]
    [(is-deref? t)
     (string-append (number->string (get-deref-2 t)) "(%" (symbol->string (get-deref-1 t)) ")")]
    [else (error 'str-x86-term "Invalid input ~s" t)]))

(define (str-x86 e)
  (cond 
    [(is-movq? e)
     (string-append "movq " (str-x86-term (get-movq-1 e)) "," (str-x86-term (get-movq-2 e)))]
    [(is-addq? e)
     (string-append "addq " (str-x86-term (get-addq-1 e)) "," (str-x86-term (get-addq-2 e)))]
    [(is-subq? e)
     (string-append "subq " (str-x86-term (get-subq-1 e)) "," (str-x86-term (get-subq-2 e)))]
    [(is-negq? e)
     (string-append "negq " (str-x86-term (get-negq-1 e)))]
    [(is-pushq? e)
     (string-append "pushq " (str-x86-term (get-pushq-1 e)))]
    [(is-popq? e)
     (string-append "popq " (str-x86-term (get-popq-1 e)))]
    [(is-callq? e)
     (string-append "callq _" (symbol->string (get-callq-1 e)))]
    [(is-retq? e)
     (string-append "retq")]
    [else (error 'str-x86 "Invalid input ~s" e)]))

(define (select-intruction p)
  (letrec ([helper
            (lambda (e)
              (match e
                [`(assign ,v ,exp)
                 (match exp
                   [(? symbol?)
                    (list
                     (mk-movq (mk-arg exp) (mk-arg v)))]
                   [(? integer?)
                    (list
                     (mk-movq (mk-arg exp) (mk-arg v)))]
                   [`(,op ,e1 ,e2)
                    (list
                     (mk-movq (mk-arg e1) (mk-arg v))
                     (mk-addq (mk-arg e2) (mk-arg v)))]
                   [`(,op ,e1)
                    (list                     
                     (mk-movq (mk-arg e1) (mk-arg v))
                     (mk-negq (mk-arg v)))]
                   [`(read)
                    (list
                     (mk-callq 'read_int)
                     (mk-movq (mk-reg 'rax) (mk-arg v)))]
                    )]
                [`(return ,v)
                 (list
                  (mk-movq (mk-arg v) (mk-reg 'rax)))]))])
    (match p
      [`(program ,vlst ,body ...)
       `(program ,vlst ,@(apply append (map helper body)))])))

;; (trace select-intruction)

;; (select-intruction (flatten-r0 (read-program "tests/r0_3.rkt")))

(define (assign-home p)
  ;; (printf "~s\n" p)
  (letrec ([assign-stack
            (lambda (vlst)
              (foldl
               (lambda (item r)
                 (match r
                   [`(,start . ,slst)
                    (let ([offset (- start 8)])
                      `(,offset . ,(cons (cons item (mk-deref 'rbp offset)) slst)))]))
               (cons 0 '())
               vlst))]
           [replace
            (lambda (s slst)
              (match s
                [`(var ,v)
                 (lookup v slst)]
                [_ s]))]
           [rename-var
            (lambda (slst)
              (lambda (e)
                (match e
                  [`(movq ,s ,d)
                   `(movq ,(replace s slst) ,(replace d slst))]
                  [`(addq ,s ,d)
                   `(addq ,(replace s slst) ,(replace d slst))]
                  [`(negq ,s)
                   `(negq ,(replace s slst))]
                  [_ e])))])
    (match p
      [`(program ,info ,body ...)
       (let* ([stackinfo (assign-stack info)]
              [slst (cdr stackinfo)]
              [size (- (car stackinfo))])
         ;; (printf "~s\n" slst)
         `(program ,size ,@(map (rename-var slst) body)))])))

(define (print-x86 p)
  (match p
    [`(program ,info ,body ...)
     (let ([str-body
            (lambda (p)
              (foldl
               (lambda (e r)
                 (string-append r  (str-x86 e) "\n"))
               ".globl _main\n_main:\n"
               p))]
           [add-pre-conclude
            (lambda (n p)
              (let* ([r (remainder n 16)]
                     [frame-size (+ n r)])
                `(,(mk-pushq (mk-reg 'rbp))
                  ,(mk-movq (mk-reg 'rsp) (mk-reg 'rbp))
                  ,(mk-subq (mk-int frame-size) (mk-reg 'rsp))
                  ,@p
                  ,(mk-movq (mk-reg 'rax) (mk-reg 'rdi))
                  ,(mk-callq 'print_int)
                  ,(mk-addq (mk-int frame-size) (mk-reg 'rsp))
                  ,(mk-popq (mk-reg 'rbp))
                  ,(mk-retq))))])
       (str-body (add-pre-conclude info body)))]
     [_ (error 'print-x86 "Invalid input ~s" p)]))


(define (patch-instruction p)
  (let ([helper
         (lambda (e)
           (match e
             [`(movq ,s ,d)
              (match s
                [`(deref ,reg1 ,offset1)
                 (match d
                   [`(deref ,reg2 ,offset2)
                    (list
                     (mk-movq s (mk-reg 'rax))
                     (mk-movq (mk-reg 'rax) d))]
                   [_ (list e)])]
                [_ (list e)])]
             [`(addq ,s ,d)
              (match s
                [`(deref ,reg1 ,offset1)
                 (match d
                   [`(deref ,reg2 ,offset2)
                    (list
                     (mk-movq s (mk-reg 'rax))
                     (mk-addq (mk-reg 'rax) d))]
                   [_ (list e)])]
                [_ (list e)])]
             [_ (list e)]))])
    (match p
      [`(program ,info ,body ...)
       (let* ([np `(program ,info ,@(apply append (map helper body)))]
              [x86 (print-x86 np)])
         ;; (printf "\n")
         ;; (printf x86)
         np)])))
;; (trace patch-instruction)



;; (trace assign-home)
(define r1-passes
  `( ("uniquify",uniquify,interp-scheme)
     ("flatten",flatten-r0,interp-C)
     ("select-intruction", select-intruction,interp-x86)
     ("assign-home", assign-home, interp-x86)
     ("patch-instruction", patch-instruction, interp-x86)
     ("print-x86",print-x86,#f)
     ))
(define (compile-fname fname)
  ((compile-file #f r1-passes) fname))

     
