;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |Forming an intuition|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; local definitions

(local [(define a 1)
        (define b 2)]
  (+ a b))

(define p "incendio ")

(local [(define p "accio ")
        (define (fetch n) (string-append p n))]
  (fetch "portkey"))

; top level definition

;(define p "accio ")
;(define (fetch n) (string-append p n))
;(fetch "portkey")

;(define a 1)
;(define b 2)
;(+ a
;   (local [(define b 3)]
;     (+ a b))
;   b)

(define b 1)

(+ b
   (local [(define b 2)]
     (* b b))
   b)

(+ 1
   (local [(define b 2)]
     (* b b))
   b)

;; Renaming
(+ 1
   (local [(define b_0 2)]
     (* b_0 b_0 ))
   b)

;; Lifting
(define b_0 2)
(+ 1
   (local []
     (* b_0 b_0 ))
   b)

;; Replacing
;(define b_0 2)
(+ 1
   (* b_0 b_0 )
   b)

;; Step by step evaluation
(define (foo x)
  (local [(define (bar y) (+ x y))]
    (+ x (bar (* 2 x)))))

(list (foo 2) (foo 3))

(list 8 (foo 3))

(list 8 (local [(define (bar y) (+ 3 y))]
    (+ 3 (bar (* 2 3)))))

(define (bar_0 y) (+ 3 y))
(list 8 
    (+ 3 (bar_0 (* 2 3))))

(list 8 
    (+ 3 (bar_0 6)))

(list 8 
    (+ 3 (+ 3 6)))

(list 8 12)

