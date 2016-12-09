;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname | Multiple Choice Quiz|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define L1 (cons "Apple" (cons "Banana" (cons "Orange" (cons "Pear" empty)))))

(define-struct concert (artist venue))
;; Concert is (make-concert String String)
;; interp. a concert with the band playing, and the venue they're playing at
(define C1 (make-concert "Shakey Graves" "Commodore Ballroom"))
(define C2 (make-concert "Tallest Man On Earth" "Orpheum Theatre")) 
#;
(define (fn-for-concert c)
  (... (concert-artist c)
       (fn-for-venue (concert-venue c))))

;; ListOfConcert is one of:
;; - empty
;; - (cons Concert ListOfConcert)
;; interp. a list of concerts
(define LOC1 empty)
(define LOC2 (cons C1 (cons C2 empty)))
#;
(define (fn-for-loc loc)
  (cond [(empty? loc)(...)] 
        [else
         (... (fn-for-concert (first loc))
              (fn-for-loc (rest loc)))]))

;; Template rules used:
;;  - one of: 2 cases
;;  - atomic distinct: empty
;;  - compound: 2 fields, (cons Concert ListOfConcert)
;;  - reference: (first loc) is Concert
;;  - self-reference: (rest loc) is ListOfConcert


(define-struct afestival (name headliner shows))
;; Festival is (make-festival String Concert ListOfConcert)
;; interp. a festival with a name, a headliner concert and a list of other concerts that are
;; part of the festival, where:
;;              - name the name of the festival
;;              - headliner is the headliner concert
;;              - shows is a list of concerts included in the festival
(define CANCELLED-AFESTIVAL (make-afestival "" (make-concert "" "")  empty))
(define AVFMF (make-afestival "Vancouver Folk Music Festival"
                            (make-concert "Headliner Group" "Main Stage")
                            (cons (make-concert "Hawksley Workman" "Stage 2")
                                  (cons (make-concert "Grace Petrie" "Stage 1")
                                        (cons (make-concert "Mary Gauthier" "Stage 5") empty)))))


(define (fn-for-afestival af)
  (... (afestival-name af)      ;String
       (fn-for-concert (afestival-headliner af)) ;Concert
       (fn-for-loc (afestival-shows af))     ;ListOfConcert
       ))

;; Template rules used:
;;  - compound: 3 fields
;;  - atomic non-distinct: String
;;  - reference: (afestival-headliner) is a Concert
;;  - reference: (afestival-shows) is a ListOfConcert

(define-struct festival (name shows))
;; Festival is (make-festival String ListOfConcert)
;; interp. a festival with name, and list of shows that are part of the festival
(define CANCELLED-FESTIVAL (make-festival "" empty))
(define VFMF (make-festival "Vancouver Folk Music Festival" 
                            (cons (make-concert "Hawksley Workman" "Main Stage")
                                  (cons (make-concert "Grace Petrie" "Stage 1")
                                        (cons (make-concert "Mary Gauthier" "Stage 5") empty)))))
#;
(define (fn-for-festival f)
  (... (festival-name f))
  (cond [(empty? (festival-shows f))(...)]
        [else
         (... (fn-for-concert (first (festival-shows f)))
              (fn-for-festival (rest (festival-shows f))))]))

;; Festival -> ListOfString
;; produces a list of each band paired with where they are performing
(check-expect (festival-schedule (make-festival "" empty)) empty)
(check-expect (festival-schedule
               (make-festival "CFMF" 
                              (cons (make-concert "Father John Misty" "Main Stage") empty)))
              (cons "Father John Misty: Main Stage" empty))

;(define (festival-schedule f) f)   ; stub

(define (festival-schedule f)
    (schedule-loc (festival-shows f)))

;; Concert -> String
;; produce the name and venue of a concert, separated with a colon, as a string
(define (display-concert c) "")  ;stub

;; ListOfConcert -> ListOfString
;; given a list of concerts, produce a list of each artist paired with where they are performing.
;(define (schedule-loc loc) empty)  ;stub

(define (schedule-loc loc)
  (cond [(empty? loc) empty]
        [else 
         (cons (display-concert (first loc))
               (schedule-loc (rest loc)))]))

