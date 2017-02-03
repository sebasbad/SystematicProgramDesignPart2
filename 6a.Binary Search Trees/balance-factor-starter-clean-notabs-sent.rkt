BST, AVL, insert, insertavl, tree rotation and the rest ...

It's been fun (and also time consuming) implementing all the autobalanced binary search tree logic. I'll post here a clean version of my implementation (removing the analysis draft work) for if it helps someone and to be able to improve the algorithm using the feedback.


;; balance-factor-starter.rkt

; PROBLEM:
; 
; As discussed in lecture, for optimal lookup time we want a BST to be balanced. 
; The oldest approach to this is called AVL self-balancing trees and was invented in 1962. 
; The remainder of this problem set is based on AVL trees. (https://en.wikipedia.org/wiki/AVL_tree)
; 
; An individual node is balanced when the height of its left and right branches differ 
; by no more than 1. A tree is balanced when all its nodes are balanced.
; 
; a) Design the function balance-factor that consumes a node and produces its balance factor,
; which is defined as the height of its left child minus the height of its right child.
; 
; b) Use your function in part a) to design the function balanced?, which consumes a BST and 
; produces true if the tree is balanced.
; 
; Once you have the function, use it to compare what happens with the following two sequences
; of insertions:
; 
; 
; (insert 4 "a" 
;         (insert 5 "a"
;                 (insert 6 "a" 
;                         (insert 7 "a" 
;                                 (insert 8 "a" false)))))
;         
;         
; (insert 4 "a" 
;         (insert 5 "a"
;                 (insert 8 "a" 
;                         (insert 7 "a" 
;                                 (insert 6 "a" false))))) 
; 


;; Data definitions:

(define-struct node (key val l r))
;; A BST (Binary Search Tree) is one of:
;;  - false
;;  - (make-node Integer String BST BST)
;; interp. false means no BST, or empty BST
;;         key is the node key
;;         val is the node val
;;         l and r are left and right subtrees
;; INVARIANT: for a given node:
;;     key is > all keys in its l(eft)  child
;;     key is < all keys in its r(ight) child
;;     the same key never appears twice in the tree

(define BST0 false)
(define BST1 (make-node 1 "abc" false false))
(define BST4 (make-node 4 "dcj" false (make-node 7 "ruf" false false)))
(define BST3 (make-node 3 "ilk" BST1 BST4))
(define BST42 
  (make-node 42 "ily"
             (make-node 27 "wit" (make-node 14 "olp" false false) false)
             false))
(define BST10 (make-node 10 "why" BST3 BST42))

; .

#;
(define (fn-for-bst t)
  (cond [(false? t) (...)]
        [else
         (... (node-key t)    ;Integer
              (node-val t)    ;String
              (fn-for-bst (node-l t))
              (fn-for-bst (node-r t)))]))

;; Template rules used:
;;  - one of: 2 cases
;;  - atomic-distinct: false
;;  - compound: (make-node Integer String BST BST)
;;  - self reference: (node-l t) has type BST
;;  - self reference: (node-r t) has type BST

(define-struct avlnode (key val l r))
;; An AVLBST (autobalanced Binary Search Tree) is one of:
;;  - false
;;  - (make-avlnode Integer String AVLBST AVLBST)
;; interp. false means no AVLBST, or empty AVLBST
;;         key is the node key
;;         val is the node val
;;         l and r are left and right subtrees
;; INVARIANT: for a given node:
;;     key is > all keys in its l(eft)  child
;;     key is < all keys in its r(ight) child
;;     the same key never appears twice in the tree
;;     all nodes of the tree are balanced, i.e. an
;;      individual node is balanced when the height
;;      of its left and right branches differ by no
;;      more than 1

(define AVLBST0 false)
(define AVLBST1 (make-node 1 "abc" false false))
(define AVLBST4 (make-node 4 "dcj" false (make-node 7 "ruf" false false)))
(define AVLBST3 (make-node 3 "ilk" AVLBST1 AVLBST4))
(define AVLBST42 
  (make-node 42 "ily"
             (make-node 27 "wit" (make-node 14 "olp" false false) false)
             (make-node 114 "kjs" false false)))
(define AVLBST10 (make-node 10 "why" AVLBST3 AVLBST42))

#;
(define (fn-for-avlbst bt)
  (cond [(false? bt) (...)]
        [else
         (... (node-key bt)    ;Integer
              (node-val bt)    ;String
              (fn-for-avlbst (node-l bt))
              (fn-for-avlbst (node-r bt)))]))

;; Template rules used:
;;  - one of: 2 cases
;;  - atomic-distinct: false
;;  - compound: (make-node Integer String AVLBST AVLBST)
;;  - self reference: (node-l t) has type AVLBST
;;  - self reference: (node-r t) has type AVLBST

;; Functions

;; BST -> Integer
;; consumes a node and produces its balance factor, which is defined as the height of
;; its left child minus the height of its right child, where height is the maximum number
;; of nodes of the left and right branches of a node
(check-expect (balance-factor BST0) (- 0 0))
(check-expect (balance-factor BST1) (- 0 0))
(check-expect (balance-factor BST4) (- 0 1))
(check-expect (balance-factor BST3) (- 0 1))
(check-expect (balance-factor BST42) (- 2 0))
(check-expect (balance-factor BST10) (- 3 3))

;(define (balance-factor t) 0) ;stub

(define (balance-factor t)
  (cond [(false? t) 0]
        [else (- (height (node-l t)) (height (node-r t)))]))

;; BST -> Natural
;; returns the height of the node which is the maximum number
;; of nodes of the left and right branches of given a node

;(define (height t) 0) ;stub

(define (height t)
  (cond [(false? t) 0]
        [else (+ 1 (max (height (node-l t))
                        (height (node-r t))))]))

;; BST -> Boolean
;; consumes a BST and  produces true if the tree is balanced. An individual node is balanced
;; when the height of its left and right branches differ by no more than 1. A tree is balanced
;; when all its nodes are balanced.
(check-expect (balanced? BST0) true)
(check-expect (balanced? BST1) true)
(check-expect (balanced? BST4) true)
(check-expect (balanced? BST3) true)
(check-expect (balanced? BST42) false)
(check-expect (balanced? BST10) false)

;(define (balanced? b) false) ;stub

(define (balanced? b)
  (and (>= (balance-factor b) -1) (<= (balance-factor b) 1)
       (if (false? b) true (balanced? (node-l b)))
       (if (false? b) true (balanced? (node-r b)))))

;; Analysis - insert node into Binary Search Tree
#;
(insert 2 "a"
        (insert 4 "a"
                (insert 5 "a"
                        (insert 8 "a"
                                (insert 7 "a"
                                        (insert 6 "a"
                                                (insert 1 "a"
                                                        (insert 3 "a" false))))))))

; Insert node into BST (recursive version)
; 
;  3    3    3       3         3             3               3                     3
; / \  / \  / \     / \       / \          /   \           /   \                 /   \
;     1    1   6   1   6     1   6        1     6         1     6               1     6
;    / \  / \ / \ / \ / \   / \ / \      / \   / \       / \   / \             / \   / \
;                        7         7          5   7           5   7               2 5   7
;                       / \       / \        / \ / \         / \ / \               / \ / \
;                                    8              8       4       8             4       8
;                                   / \            / \     / \     / \           / \     / \
;                                                           


;; Integer String BST -> BST
;; consumes a key a value and a BST and generates a BST using the following rules
;;  - if new node > t root (so new node also > t root left)
;;    - insert new node into t root right
;;  - if new node < t root (so new node also < t root right)
;;    - insert new node into t root left
(check-expect (insert 1 "a" false) (make-node 1 "a" false false))
(check-expect (insert 1 "a" false) (make-node 1 "a" false false))

(check-expect (insert 4 "a" (insert 5 "a" (insert 6 "a" (insert 7 "a" (insert 8 "a" false)))))
              (make-node 8 "a"
                         (make-node 7 "a"
                                    (make-node 6 "a"
                                               (make-node 5 "a"
                                                          (make-node 4 "a"
                                                                     false
                                                                     false)
                                                          false)
                                               false)
                                    false)
                         false))

(check-expect (insert 4 "a" (insert 5 "a" (insert 8 "a" (insert 7 "a" (insert 6 "a" false)))))
              (make-node 6 "a"
                         (make-node 5 "a"
                                    (make-node 4 "a"
                                               false
                                               false)
                                    false)
                         (make-node 7 "a"
                                    false
                                    (make-node 8 "a"
                                               false
                                               false))))

(check-expect (insert 2 "a"
                      (insert 4 "a"
                              (insert 5 "a"
                                      (insert 8 "a"
                                              (insert 7 "a"
                                                      (insert 6 "a"
                                                              (insert 1 "a"
                                                                      (insert 3 "a" false))))))))
              (make-node 3 "a"
                         (make-node 1 "a"
                                    false
                                    (make-node 2 "a"
                                               false
                                               false))
                         (make-node 6 "a"
                                    (make-node 5 "a"
                                               (make-node 4 "a"
                                                          false
                                                          false)
                                               false)
                                    (make-node 7 "a"
                                               false
                                               (make-node 8 "a"
                                                          false
                                                          false)))))

;(define (insert i s t) false) ;stub

(define (insert i s t)
  (cond [(false? t) (make-node i s false false)]
        [else
         (if (< i (node-key t))    ;Integer
             (make-node (node-key t) (node-val t) (insert i s (node-l t)) (node-r t))
             (make-node (node-key t) (node-val t) (node-l t) (insert i s (node-r t))))]))

;; Analysis: tree rotation (https://en.wikipedia.org/wiki/Tree_rotation)

; Rotate-Left:
; ============================================
; 
; L1 - R2 = -1             R2 - L1 = 1
; 
;    5                   7
;   / \                 / \
;  4   7               5   8 
;     / \             / \    
;    6   8           4   6    
; 
; (4, 5, (6, 7, 8))     ((4, 5, 6), 7, 8)
; 
; Example1: 
; ---------
; Root-key          = 5
; Root.Left         = 4
; Root.Right-key    = 7      = Pivot-key
; Pivot.Left        = 6
; Pivot.Right       = 8
; --
; NewRoot-key      = Pivot-key = 7
; NewRoot.Left-key = 5       = Root-key
; NewRoot.Right    = 8       = Pivot.Right
; NewPivot-key     = Root-key  = 5
; NewPivot.Left    = 4       = Root.Left
; NewPivot.Right   = 6       = Pivot.Left
; --
; NewPivot-key  = Root-key
; NewPivot.Left  = Root.Left (Full Tree)
; NewPivot.Right  = Root.Right.Left (Full Tree)
; NewRoot-key  = Root.Right-key
; NewRoot.Left  = NewPivot (Full Tree)
; NewRoot.Right  = Root.Right.Right (Full Tree)
; 
; ============================================
; 
; L1 - R3 = -2                    R2 - L2 = 0
; 
;    3                                  5
;   / \                               /   \
;  1   5                             3     7 
;     / \                           / \   / \
;    4   7                         1   4 6   8
;       / \
;      6   8
; 
; (1, 3, (4, 5, (6, 7, 8))) ((1, 3, 4), 5, (6, 7, 8))) 
; 
; Example2:
; ---------
; Root-key  = 3
; Root.Left  = 1
; Root.Right-key  = 5      = Pivot-key
; Pivot.Left  = 4
; Pivot.Right  = (6, 7, 8) 
; --
; NewRoot-key  = Pivot-key = 5
; NewRoot.Left-key = 3       = Root-key
; NewRoot.Right   = (6, 7, 8) = Pivot.Right
; NewPivot-key     = Root-key  = 3
; NewPivot.Left   = 4       = Root.Left
; NewPivot.Right   = 6       = Pivot.Left
; --
; NewPivot-key  = Root-key
; NewPivot.Left  = Root.Left (Full Tree)
; NewPivot.Right  = Root.Right.Left (Full Tree)
; NewRoot-key  = Root.Right-key
; NewRoot.Left  = NewPivot (Full Tree)
; NewRoot.Right  = Root.Right.Right (Full Tree)


; Rotate-Right:
; ============================================
; R2 - L1 = 1             L1 - R2 = -1             
;         7                      5               
;        / \                    / \                
;       5   8                  4   7            
;      / \                        / \        
;     4   6                      6   8      
; 
; ((4, 5, 6), 7, 8)    (4, 5, (6, 7, 8))
; 
; Example1: 
; ---------
; Root-key        = 7
; Root.Left-key  = 5         = Pivot-key
; Root.Right  = 8 
; Pivot.Left  = 4
; Pivot.Right  = 6
; --
; NewRoot-key    = Pivot-key = 5
; NewRoot.Left   = 4         = Pivot.Left
; NewRoot.Right  = 7         = Root-key
; NewPivot-key   = 7         = Root-key
; NewPivot.Left  = 6         = Pivot.Right
; NewPivot.Right = 8         = Root.Right
; --
; NewPivot-key   = Root-key
; NewPivot.Left  = Root.Left.Right (Full Tree)
; NewPivot.Right = Root.Right (Full Tree)
; NewRoot-key    = Root.Left-key
; NewRoot.Left   = Root.Left.Left (Full Tree)
; NewRoot.Right  = NewPivot (Full Tree)
; 
; ============================================
; 
; L1 - R3 = -2                R2 - L2 = 0
;         6                         4
;        / \                      /   \
;       4   7                    2     6 
;      / \                      / \   / \
;     2   5                    1   3 5   7
;    / \
;   1   3
; 
; (((1, 2, 3), 4, 5), 6, 7) ((1, 2, 3), 4, (5, 6, 7))
; 
; Example2:
; ---------
; Root-key      = 6
; Root.Left-key = 4         = Pivot
; Root.Right    = 7
; Pivot.Left    = (1, 2, 3)
; Pivot.Right   = 5
; --
; NewRoot-key       = Pivot-key = 4
; NewRoot.Left      = (1, 2, 3) = Pivot.Left
; NewRoot.Right-key = 6         = Root-key
; NewPivot-key      = Root-key  = 6
; NewPivot.Left     = 5         = Pivot.Right
; NewPivot.Right    = 7         = Root.Right
; --
; NewPivot-key   = Root-key
; NewPivot.Left  = Root.Left.Right (Full Tree)
; NewPivot.Right = Root.Right (Full Tree)
; NewRoot-key    = Root.Left-key
; NewRoot.Left   = Root.Left.Left (Full Tree)
; NewRoot.Right  = NewPivot (Full Tree)


; Initial tree, balance factor = -4 
; 4
;  \
;   5
;    \
;     6
;      \
;       7
;        \
;         8

(define T1 (make-node 4 "a"
                      false
                      (make-node 5 "a"
                                 false
                                 (make-node 6 "a"
                                            false
                                            (make-node 7 "a"
                                                       false
                                                       (make-node 8 "a" false false))))))
;; balance-factor = -4
;(balance-factor T1)

; Tree rotated left once, balance-factor = -2
;      5
;     / \
;    4   6
;         \
;          7
;           \
;            8


#;
(define T1RL1 (make-node 5 "a"
                         (make-node 4 "a" false false)
                         (make-node 6 "a"
                                    false
                                    (make-node 7 "a"
                                               false
                                               (make-node 8 "a" false false)))))

;; Rotate left
;; - NewPivot.Left  = Root.Left (Full Tree)        =
;(node-l T1)
;; - NewPivot.Right = Root.Right.Left (Full Tree)  =
;(node-l (node-r T11))
;; - NewPivot      = Root                         =
;(make-node (node-key T1) (node-val T1) (node-l T1) (node-l (node-r T1)))
;; - NewRoot.Left   = NewPivot (Full Tree)         =
;(make-node (node-key T1) (node-val T1) (node-l T1) (node-l (node-r T1)))
;; - NewRoot.Right  = Root.Right.Right (Full Tree) =
;(node-r (node-r T1))
;; - NewRoot      = Root.Right                   =
(define T1RL1
  (make-node (node-key (node-r T1)) (node-val (node-r T1))
             (make-node (node-key T1) (node-val T1)
                        (node-l T1)
                        (node-l (node-r T1)))
             (node-r (node-r T1))))

;; balance-factor = -2
;(balance-factor T1RL1)

; Tree rotated left twice, balance-factor = 0
;      6
;     / \
;    5   7
;   /     \
;  4       8


(define T1RL2
  (make-node (node-key (node-r T1RL1)) (node-val (node-r T1RL1))
             (make-node (node-key T1RL1) (node-val T1RL1)
                        (node-l T1RL1)
                        (node-l (node-r T1RL1)))
             (node-r (node-r T1RL1))))


; Initial tree, balance factor = 4 
;          8
;         /
;        7
;       /
;      6
;     /
;    5
;   /
;  4


(define T2 (make-node 8 "a"
                      (make-node 7 "a"
                                 (make-node 6 "a"
                                            (make-node 5 "a"
                                                       (make-node 4 "a" false false)
                                                       false)
                                            false)
                                 false)
                      false))

;; balance-factor = 4
;(balance-factor T2)

; Tree rotated right once, balance-factor = 2
;          7
;         / \
;        6   8
;       /
;      5
;     /
;    4


(define T2RR1 (make-node 7 "a"
                           (make-node 6 "a"
                                      (make-node 5 "a"
                                                 (make-node 4 "a"
                                                            false
                                                            false)
                                                 false)
                                      false)
                           (make-node 8 "a" false false)))

;; balance-factor = 2
;(balance-factor T2RR1)

;; Rotate right
;; - NewPivot.Left  = Root.Left.Right (Full Tree) =
;(node-r (node-l T2))
;; - NewPivot.Right = Root.Right (Full Tree)      =
;(node-r T2)
;; - NewPivot       = Root                        =
;(make-node (node-key T2) (node-val T2)
;           (node-r (node-l T2)) (node-r T2))
;; - NewRoot.Left   = Root.Left.Left (Full Tree)  =
;(node-l (node-l T2))
;; - NewRoot.Right  = NewPivot (Full Tree)        =
;(make-node (node-key T2) (node-val T2)
;           (node-r (node-l T2)) (node-r T2))
;; - NewRoot        = Root.Left                   =
;(make-node (node-key (node-l T2)) (node-val (node-l T2))
;           (node-l (node-l T2))
;           (make-node (node-key T2) (node-val T2)
;                      (node-r (node-l T2)) (node-r T2)))


; Tree rotated right twice, balance-factor = 0
;          6
;         / \
;        5   7
;       /     \
;      4       8


(define T2RR2 (make-node (node-key (node-l T2RR1)) (node-val (node-l T2RR1))
                           (node-l (node-l T2RR1))
                           (make-node (node-key T2RR1) (node-val T2RR1)
                                      (node-r (node-l T2RR1)) (node-r T2RR1))))

;; balance-factor = 0
;(balance-factor AVL2RR2)


;; BST -> BST
;; Rotates a binary search tree to the left
(check-expect (rotate-left false) false)
(check-expect (rotate-left (make-node 1 "a" false false)) (make-node 1 "a" false false))
(check-expect (rotate-left T1) T1RL1)
(check-expect (rotate-left T1RL1) T1RL2)

;(define (rotate-left t) false) ;stub

(define (rotate-left t)
  (cond [(false? t) false]
        [else
         (if (can-rotate-left t)
             (make-node (node-key (node-r t)) (node-val (node-r t))
                        (make-node (node-key t) (node-val t)
                                   (node-l t)
                                   (node-l (node-r t)))
                        (node-r (node-r t)))
             t)
         ]))

;; BST -> Boolean
;; consumes a binary search tree and produces true if the tree can be rotated to left,
;; i.e. the right node is not false or empty
(check-expect (can-rotate-left false) false)
(check-expect (can-rotate-left (make-node 1 "a" false false)) false)
(check-expect (can-rotate-left (make-node 2 "a" (make-node 1 "a" false false) false)) false)
(check-expect (can-rotate-left (make-node 1 "a" false (make-node 2 "a" false false))) true)

;(define (can-rotate-left t) false) ;stub

(define (can-rotate-left t)
  (cond [(false? t) false]
        [else
         (if (false? (node-r t))
             false
             true)]))

;; BST -> BST
;; Rotates a binary search tree to the right
(check-expect (rotate-right false) false)
(check-expect (rotate-right (make-node 1 "a" false false)) (make-node 1 "a" false false))
(check-expect (rotate-right T2) T2RR1)
(check-expect (rotate-right T2RR1) T2RR2)

;(define (rotate-right t) false) ;stub

(define (rotate-right t)
  (cond [(false? t) false]
        [else
         (if (can-rotate-right t)
             (make-node (node-key (node-l t)) (node-val (node-l t))
                        (node-l (node-l t))
                        (make-node (node-key t) (node-val t)
                                   (node-r (node-l t)) (node-r t)))
             t)
         ]))

;; BST -> Boolean
;; consumes a binary search tree and produces true if the tree can be rotated to right,
;; i.e. the left node is not false or empty
(check-expect (can-rotate-right false) false)
(check-expect (can-rotate-right (make-node 1 "a" false false)) false)
(check-expect (can-rotate-right (make-node 1 "a" false (make-node 2 "a" false false))) false)
(check-expect (can-rotate-right (make-node 2 "a" (make-node 2 "a" false false) false)) true)

;(define (can-rotate-right t) false) ;stub

(define (can-rotate-right t)
  (cond [(false? t) false]
        [else
         (if (false? (node-l t))
             false
             true)]))

;; Integer String AVLBST -> AVLBST
;; consumes a key a value and a BST and generates a BST where the heights of the two child
;; subtrees of any node differ by at most one
(check-expect (insertavl 1 "a" false) (make-node 1 "a" false false))
(check-expect (insertavl 1 "a" (insertavl 2 "a" false))
              (make-node 2 "a" (make-node 1 "a" false false) false))
(check-expect (insertavl 2 "a" (make-node 1 "a" false false))
              (make-node 1 "a" false (make-node 2 "a" false false)))

(check-expect (insertavl 4 "a" (insertavl 5 "a" (insertavl 6 "a" (insertavl 7 "a" (insertavl 8 "a" false)))))
              (make-node 6 "a"
                         (make-node 5 "a"
                                    (make-node 4 "a"
                                               false
                                               false)
                                    false)
                         (make-node 7 "a"
                                    false
                                    (make-node 8 "a"
                                               false
                                               false))))

(check-expect (insertavl 4 "a" (insertavl 5 "a" (insertavl 8 "a" (insertavl 7 "a" (insertavl 6 "a" false)))))
              (make-node 6 "a"
                         (make-node 5 "a"
                                    (make-node 4 "a"
                                               false
                                               false)
                                    false)
                         (make-node 7 "a"
                                    false
                                    (make-node 8 "a"
                                               false
                                               false))))

(check-expect (insert 2 "a"
                      (insert 4 "a"
                              (insert 5 "a"
                                      (insert 8 "a"
                                              (insert 7 "a"
                                                      (insert 6 "a"
                                                              (insert 1 "a"
                                                                      (insert 3 "a" false))))))))
              (make-node
               3
               "a"
               (make-node 1 "a" false (make-node 2 "a" false false))
               (make-node
                6
                "a"
                (make-node 5 "a" (make-node 4 "a" false false) false)
                (make-node 7 "a" false (make-node 8 "a" false false)))))


;(define (insertavl i s t) false) ;stub

(define (insertavl i s bt)
  (balance (insert i s bt)))

;; BST -> BST
;; consumes a binary search tree and produces a balanced binary tree
(check-expect (balance false) false)
(check-expect (balance (insert 1 "a" false)) (make-node 1 "a" false false))
(check-expect (balance (insert 1 "a" (make-node 2 "a" false false)))
              (make-node 2 "a" (make-node 1 "a" false false) false))
(check-expect (balance (insert 2 "a" (make-node 1 "a" false false)))
              (make-node 1 "a" false (make-node 2 "a" false false)))

(check-expect (balance (insert 4 "a" (insert 5 "a" (insert 6 "a" (insert 7 "a" (insert 8 "a" false))))))
              (make-node 6 "a"
                         (make-node 5 "a"
                                    (make-node 4 "a"
                                               false
                                               false)
                                    false)
                         (make-node 7 "a"
                                    false
                                    (make-node 8 "a"
                                               false
                                               false))))

(check-expect (balance (insert 4 "a" (insert 5 "a" (insert 8 "a" (insert 7 "a" (insert 6 "a" false))))))
              (make-node 6 "a"
                         (make-node 5 "a"
                                    (make-node 4 "a"
                                               false
                                               false)
                                    false)
                         (make-node 7 "a"
                                    false
                                    (make-node 8 "a"
                                               false
                                               false))))

;(define (balance t) false) ;stub

(define (balance bt)
  (cond [(false? bt) false]
        [(balanced? bt) bt]
        [(< (balance-factor bt) -1)
         (if (>= (+ (balance-factor (rotate-left bt)) (balance-factor bt)) 0)
             (balance (make-node (node-key bt) (node-val bt) (balance (node-l bt)) (balance (node-r bt))))
             (balance (rotate-left bt)))]
        [else
         (if (<= (+ (balance-factor (rotate-right bt)) (balance-factor bt)) 0)
             (balance (make-node (node-key bt) (node-val bt) (balance (node-l bt)) (balance (node-r bt))))
             (balance (rotate-right bt)))]))