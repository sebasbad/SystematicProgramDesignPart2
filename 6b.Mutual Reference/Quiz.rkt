;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Quiz) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(define-struct node (key val l r))

;; BST is one of:
;; - false
;; - (make-node Integer String BST BST)
;; interp. false means no BST, or empty BST
;;         key is the node key
;;         val is the node val
;;         l and r are left and right subtrees
;; INVARIANT: for a given node:
;;     key is > all keys in its l(eft)  child
;;     key is < all keys in its r(ight) child
;;     the same key never appears twice in the tree

(define BSTA (make-node 6 "a" false false))

;; BST -> ListOfInteger
;; present the keys in the binary search tree in descending order as a list
(check-expect (tree-to-list false) empty)
(check-expect (tree-to-list (make-node 4 "e" 
                                       (make-node 1 "o" false false)
                                       (make-node 7 "p" false false)))
              (list 7 4 1))
(check-expect (tree-to-list (make-node 6 "a"
                                       (make-node 4 "b"
                                                  (make-node 1 "v" false false)
                                                  (make-node 5 "s" false false))
                                       (make-node 9 "q" 
                                                  false
                                                  (make-node 10 "x" false false))))
              (list 10 9 6 5 4 1))

;(define (tree-to-list bt) empty)   ;stub

(define (tree-to-list bt)
  (cond [(false? bt) empty]
        [else
         (append (tree-to-list (node-r bt))
                 (list (node-key bt))
                 (tree-to-list (node-l bt)))]))

(define (fn-for-title t)
  (cond [(string=? t "President") (...)]
        [(string=? t "Vice President") (...)]
        [(string=? t "Director") (...)]
        [(string=? t "Supervisor") (...)]
        [(string=? t "Employee") (...)]))


(define (fn-for-position p)
  (... (position-name p)
       (fn-for-title (position-title p))
       (fn-for-lop (position-subs p))))
       
(define (fn-for-lop lop)
  (cond [(empty? lop) (...)]
        [else 
         (... (fn-for-position (first p))
              (fn-for-lop (rest p)))]))

(define-struct position (name title subs))
;; Position is (make-position String Title ListOfPosition)

(define (find--position n p)
  (if (string=? n (position-name p))
      p
      (find--lop n (position-subs p))))

(define (find--lop n lop)
  (cond [(empty? lop) false]
        [else
         (if (not (false? (find--position n (first lop))))
             (find--position n (first lop))
             (find--lop n (rest lop)))]))