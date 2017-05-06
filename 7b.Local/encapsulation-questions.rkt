;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname encapsulation-questions) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Person -> ListOfString
;; ListOfPerson -> ListOfString
;; produce a list of the names of the persons under 20

;(check-expect (names-under-20 P1) (list "N1"))
;(check-expect (names-under-20 P2) (list "N1"))
;(check-expect (names-under-20 P4) (list "N3" "N1"))
#;
(define (names-under-20 p)
  (local [(define (names-under-20--person p)
            (if (< (person-age p) 20)
                (cons (person-name p)
                      (names-under-20--lop (person-children p)))
                (names-under-20--lop (person-children p))))
      
          (define (names-under-20--lop lop)
            (cond [(empty? lop) empty]
                  [else
                   (append (names-under-20--person (first lop))
                           (names-under-20--lop (rest lop)))]))]
    (names-under-20--person p)))


;; ListOfNumber -> ListOfNumber
;; sort the numbers in lon in increasing order
(check-expect (sort-lon empty) empty)
(check-expect (sort-lon (list 1)) (list 1))
(check-expect (sort-lon (list 1 2 3)) (list 1 2 3))
(check-expect (sort-lon (list 2 1 3)) (list 1 2 3))
(check-expect (sort-lon (list 3 2 1)) (list 1 2 3))

(define (sort-lon lon)
  (local [(define (insert n lon)
            (cond [(empty? lon) (cons n empty)]
                  [else
                   (if (> (first lon) n)
                       (cons n lon)
                       (cons (first lon) (insert n (rest lon))))]))
          
          (define (sort-lon lon)
            (cond [(empty? lon) empty]
                  [else
                   (insert (first lon)
                           (sort-lon (rest lon)))]))]
    (sort-lon lon)))
                 
;; Number ListOfNumber -> ListOfNumber
;; insert n in proper position in lon
;; ASSUME: lon is sorted in increasing order
(check-expect (insert 2 empty) (list 2))
(check-expect (insert 2 (list 1 3)) (list 1 2 3))

(define (insert n lon)
  (cond [(empty? lon) (cons n empty)]
        [else
         (if (> (first lon) n)
             (cons n lon)
             (cons (first lon) (insert n (rest lon))))]))