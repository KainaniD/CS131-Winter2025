#lang racket

(provide (all-defined-out))

;; expr-compare

; helper functions
(define (lambda? x) (or (equal? x 'lambda) (equal? x 'λ)))

(define (unify-terms x y) (string->symbol (string-append (symbol->string x) "!" (symbol->string y))))

(define (create-map x y key) 
    ( cond
    
    ; x is empty
    [(equal? x '()) (hash)]

    ; x and y are equal
    [(equal? (car x) (car y)) (create-map (cdr x) (cdr y) (cdr key))]

    ; x and y are different - add entry to map
    [else (hash-set (create-map (cdr x) (cdr y) (cdr key)) (car key) (unify-terms (car x) (car y)))]

))

(define (is-mapped? maps key)
    ( cond 
    ; no maps left
    [(equal? (length maps) 0) '()]
    
    ; key is found
    [(not (equal? (hash-ref (car maps) key '()) '())) (hash-ref (car maps) key)]
    
    ; key not found
    [else (is-mapped? (cdr maps) key)]

    )
)


(define (compare-single-value x y) 
    (cond
    [(equal? x y) x] ; equal
    [(and (boolean? x) (boolean? y)) (if x '% '(not %))] ; boolean
    [else `(if % ,x ,y)] ; constant
))

; end helper functions

(define (expr-compare x y)
    (cond
        ; constants
        
        ; equal
        [(equal? x y) x]

        ; both are single values
        [(and (not (pair? x)) (not (pair? y))) (compare-single-value x y)]

        ; one s-expression one single value - so we can't merge
        [(or 
            (and (pair? x) (not (pair? y)))
            (and (not (pair? x)) (pair? y)))
        `(if % ,x ,y)  
        ]

        ; s-expressions of unequal length - so we can't merge
        [(not (equal? 
            (length x) 
            (length y)))
        `(if % ,x ,y)
        ] 

        ; both s-expressions of equal length - so we try to merge
        [else (merge-s-expressions x y)] 
    )
)

(define (merge-s-expressions x y) 

    (cond 
        ;types of s-expressions: lambda, quote, if
    
        ; merge any amount of quotes
        [(or (equal? 'quote (car x)) (equal? 'quote (car y))) `(if % ,x ,y)] 

        ; merge lambda and lambda
        [(and (lambda? (car x)) (lambda? (car x))) (merge-lambda-expression x y)]
        
        ; merge if and non-if 
        [(and (not (equal? (car x) (car y))) (or (equal? (car x) 'if) (equal? (car x) 'if))) `(if % ,x ,y)]

        ; merge default things
        [else (merge-default x y)]
    )
)

(define (merge-lambda-expression x y) 
    (cond 

    ; different arg sizes
    [(not (equal? (length (car (cdr x))) (length (car (cdr y))))) `(if % ,x ,y)]

    ; contains λ expression
    [(or (equal? (car x) 'λ) (equal? (car y) 'λ))  (merge-lambda 'λ (cdr x) (cdr y) '() '())]

    ; only lambda expressions
    [else (merge-lambda 'lambda (cdr x) (cdr y) '() '())]
    )
)

(define (merge-lambda lambda-val x y x-maps y-maps) 
    (list lambda-val 
        (merge-lambda-args (car x) (car y)) 
        (merge-lambda-expr (car (cdr x)) (car (cdr y)) (cons (create-map (car x) (car y) (car x)) x-maps) (cons (create-map (car x) (car y) (car y)) y-maps))
    )
)

(define (merge-lambda-args x y) 
    (cond 

    ; both args are empty
    [(equal? x '()) '()]

    ; args are the same
    [(equal? (car x) (car y)) (cons (car x) (merge-lambda-args (cdr x) (cdr y)))]

    ; both args are different
    [else (cons (unify-terms (car x) (car y)) (merge-lambda-args (cdr x) (cdr y)))])
)

(define (merge-lambda-expr x y x-maps y-maps) 
    ;(display "merging lambda body\n")
    (let 
        ([mapped-x (if (equal? (is-mapped? x-maps x) '()) x (is-mapped? x-maps x))]
        [mapped-y (if (equal? (is-mapped? y-maps y) '()) y (is-mapped? y-maps y))])
        ;(display "mapped x:")
        ;(display mapped-x)
        ;(display "\nmapped y:")
        ;(display mapped-y)
        ;(display "\n")
        ( cond 
            ; mapped/unmapped versions are equal
            [(equal? mapped-x mapped-y) mapped-x]

            ; x and y are lists
            [(and (list? x) (list? y)) (expr-compare (map-apply x-maps x #t) (map-apply y-maps y #t))]

            ; x is list and y is val
            [(and (list? x) (not (list? y))) (expr-compare (map-apply x-maps x #t) mapped-y)]

            ; y is list and x is val
            [(and (not (list? x)) (list? y)) (expr-compare mapped-x (map-apply y-maps y #t))]

            ; only x or only y is mapped
            [else (expr-compare mapped-x mapped-y)]
            
        )
    )
)

(define (map-apply maps keys is-keyword?) 

        ( cond
        
        ; no maps
        [(equal? maps '()) keys]

        ; nothing to apply if keys are empty
        [(equal? keys '()) '()]

        ; don't map to special chars

        ; quote
        [(and is-keyword? (equal? (car keys) 'quote))  keys]
            
        ; if
        [(and is-keyword? (equal? (car keys) 'if))  (cons 'if (map-apply maps (cdr keys) #f))]

        ; lambda
        [(and is-keyword? (lambda? (car keys)))
            (cons (car keys) ;lambda
            (cons (car (cdr keys)) ;args
            (map-apply '() (cdr (cdr keys)) #t)))]
            
        ; recurse for lists
        [(list? (car keys)) (cons (map-apply maps (car keys) #t) (map-apply maps (cdr keys) #f))]

        ; replace with mapped copy
        [ else  (cons (if (equal? (is-mapped? maps (car keys)) '()) (car keys) (is-mapped? maps (car keys))) (map-apply maps (cdr keys) #f))]
        
    )
)

(define (merge-default x y) 
    (cond 

    ; both terms are empty
    [(equal? x '()) '()]

    ; both terms are equal
    [(equal? (car x) (car y)) (cons (car x) (merge-default (cdr x) (cdr y)))]

    ; both terms are different
    [else (cons (expr-compare (car x) (car y)) (merge-default (cdr x) (cdr y)))]
    )    
)


;; test-expr-compare

(define (test-expr-compare x y) 
    (and 
    (equal? (eval x) (eval `(let ([% #t]) ,(expr-compare x y))))
    (equal? (eval y) (eval `(let ([% #f]) ,(expr-compare x y))))
    )
)


;; test-expr-compare-x test-expr-compare-y

(define test-expr-x 
    '(lambda (a b c) 
        (if #t 
            (/ (- c (+ a b)) 0) 
            (- a (+ b c))
        )
    )
)

(define test-expr-y 
    '(λ (c b if) 
        (if #f 
            (/ (- if (+ b c) 1)) 
            (/ c (+ if b))
        )
    )
)