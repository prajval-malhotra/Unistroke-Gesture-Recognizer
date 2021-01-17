(require "templates.rkt")

;; "templates.rkt" provides templates, a TemplateLibrary (see data definition)
;; It also provides the following test gestures for your recognizer: 
;;    testd testk tests testy testa testt


;; A Point is a (list Num Num)

;; A Gesture is a (listof (list Num Num))

;; A BoundingBox (BB) is a (list Point Point)
;; requires: the coordinate values in the first point are less than the
;;             respective values in the second point

;; A TemplateLibrary (TL) is a (listof (list Sym Gesture))
;; requires: the list is non-empty
;;           each Sym key is unqiue
;;           each Gesture value is not both vertical and horizontal
       

;; (get-x lst)consumes a point(list) and produces the corresponding
;;   x-coordinate
;; Example:
(check-expect (get-x (list 1 0)) 1)
(check-expect (get-x (list 12 3)) 12)
(check-expect (get-x (list 34 100)) 34)
;; get-x: Point -> Num
(define (get-x lst)
  (first lst))


;; (get-y lst) consumes a point(list) and produces the corresponding
;;   y-coordinate
;; Example:
(check-expect (get-y (list 1 0)) 0)
;; get-x: Point -> Num
(define (get-y lst)
  (first (rest lst)))

;; (tranlate-gesture gesture-list x-offset y-offset) consumes a gesture
;;   an x and y offset and translates the gesture accoridingly
;; Example:
(check-expect (translate-gesture (list (list 2 2) (list 100 10)) -5 5)
              (list (list -3 7) (list 95 15)))
(check-expect (translate-gesture (list(list 0 0)) 100 100)
              (list(list 100 100)))

;; tranlate-gesture: Gesture Num Num -> Gesture
(define (translate-gesture gesture-list x-offset y-offset)
  (cond[(empty? gesture-list) empty]
       [else (cons (list (+ (first (first gesture-list)) x-offset)
                         (+ (first (rest (first gesture-list))) y-offset))
                   (translate-gesture (rest gesture-list)
                                      x-offset y-offset))]))


;; (scale-gesture gesture-list x-scale y-scale) produces a scaled
;    version of given gesture
;; Example:
(check-expect (scale-gesture (list (list 2 2) (list 100 10)) 100 2)
              (list (list 200 4) (list 10000 20)))
(check-expect (scale-gesture (list(list 1 0)) 212 782) (list(list 212 0)))
(check-expect (scale-gesture (list(list 0 0)) 212 782) (list(list 0 0)))

;; scale-gesture: Gesture Num Num -> Gesture
(define (scale-gesture gesture-list x-scale y-scale)
  (cond[(empty? gesture-list) empty]
       [else (cons (list (* (first (first gesture-list)) x-scale)
                         (* (first (rest (first gesture-list))) y-scale))
                   (scale-gesture (rest gesture-list) x-scale y-scale))]))   



;; (get-b-box gesture-list) produces a gesture's bounding box values
;; Example:
(check-expect (get-b-box (list (list 1 20) (list 20 30) (list 5 500)
                               (list 50 5) (list 10 20))) (list (list 1 5)
                                                                (list 50 500)))

;; get-b-box: Gesture -> Gesture 
(define (get-b-box gesture-list)
  (list (list (smallest-x gesture-list) (smallest-y gesture-list))
        (list (greatest-x gesture-list) (greatest-y gesture-list))))


;; (greatest-x gesture-list) helper function to get the greatest x-value
;;   from a gesture 
;; Example:
(check-expect (greatest-x (list (list 1 2) (list 5 1) (list 0 0))) 5)

;; greatest-x: Gesture -> Num
(define (greatest-x gesture-list)
  (cond[(empty? (rest gesture-list)) (first (first gesture-list))]
       [(> (first (first gesture-list)) (first (first (rest gesture-list))))
        (greatest-x (cons (first gesture-list) (rest (rest gesture-list))))]
       [else (greatest-x (cons (first (rest gesture-list))
                               (rest (rest gesture-list))))]))


;; (greatest-y gesture-list) helper function to get the greatest y-value
;;   from a gesture 
;; Example:
(check-expect (greatest-y (list (list 1 2) (list 5 1) (list 0 0))) 2)
(check-expect (greatest-y (list (list 10 10) (list 50 150) (list 20 120)
                                (list 15 15))) 150)

;; greatest-y: Gesture -> Num
(define (greatest-y gesture-list)
  (cond[(empty? (rest gesture-list)) (first (rest (first gesture-list)))]
       [(> (first (rest (first gesture-list)))
           (first (rest (first (rest gesture-list)))))
        (greatest-y (cons (first gesture-list) (rest (rest gesture-list))))]
       [else (greatest-y (cons (first (rest gesture-list))
                               (rest (rest gesture-list))))])) 

 

;; (smallest-x gesture-list) produces the smallest x-coordinate in a gesture
;; Example:
(check-expect (smallest-x (list (list 100 4) (list 100 2) (list 100 4)
                                (list 100 3))) 100)

;; smallest-x: Gesture -> Num
(define (smallest-x gesture-list)
  (cond[(empty? (rest gesture-list)) (first (first gesture-list))]
       [(< (first (first gesture-list)) (first (first (rest gesture-list))))
        (smallest-x (cons (first gesture-list) (rest (rest gesture-list))))]
       [else (smallest-x (cons (first (rest gesture-list))
                               (rest (rest gesture-list))))]))

;; (smallest-y gesture-list) produces the smallest y-coordinate in a gesture
;; Example:
(check-expect (smallest-y (list (list 100 4) (list 100 2) (list 100 4)
                                (list 100 3))) 2)

;; smallest-y: Gesture -> Num
(define (smallest-y gesture-list)
  (cond[(empty? (rest gesture-list)) (first (rest (first gesture-list)))]
       [(< (first (rest (first gesture-list)))
           (first (rest (first (rest gesture-list)))))
        (smallest-y (cons (first gesture-list) (rest (rest gesture-list))))]
       [else (smallest-y (cons (first (rest gesture-list))
                               (rest (rest gesture-list))))]))



;; (gesture-length gesture-list) calculates the length of a gesture
;; Example:
(check-expect (gesture-length empty) 0)
(check-expect (gesture-length (list (list 0 0) (list 0 0)
                                    (list 0 0))) 0)
(check-within (gesture-length (list (list 4 4) (list 20 25) 
                                    (list 1 35) (list 100 56))) 0.1 149)

;; gesture-length: gesture -> Num
(define (gesture-length gesture-list)
  (cond[(empty? gesture-list) 0]
       [(empty? (rest gesture-list)) 0]
       [else (+ (calculate-length  (first gesture-list)
                                   (first (rest gesture-list)))
                (gesture-length (rest gesture-list)))]))

;; (calculate-length point1 point2) helper function which produces
;;    the distance between two points
;; Example:
(check-within (calculate-length (list 4 3) (list 3 -2)) 0.1 5)
(check-within (calculate-length (list 1 3) (list 3 5)) 1 2)
(check-within (calculate-length (list 0 310) (list 310 0)) 1 438)

;; calculate-length: (list Num Num) (list Num Num) -> Num 
(define (calculate-length point1 point2)
  (sqrt (+ (sqr (- (first point2) (first point1)))
                      (sqr (- (first (rest point2)) (first (rest point1)))))))


;; Tests:
(check-expect (gesture-length (list(list 0 0) (list 0 0))) 0)
(check-within (gesture-length (list(list -16 10) (list 93 30))) 110.8 0.1)
(check-within (gesture-length (list(list 10 -61) (list 20 -62))) 10 0.1)
(check-within (gesture-length (list(list -31 -24) (list -13 -24))) 18 0.1)
(check-within (gesture-length (list(list -51 -22) (list 36 33))) 102.9 0.1)



; (get-points gesture index) (wrapper function) consumes a gesture and a list
;;    and produces a new listthe points at the index in the given list 
; Example:
(check-expect (get-points (list (list 4 4) (list 20 25)
                          (list 1 35)(list 100 56)) (list 1 1 1))
              (list (list 20 25) (list 20 25) (list 20 25)))
 
;; get-points: (list Nat) Gesture -> Gesture 
(define (get-points gesture index)
  (temp-get-points 0 gesture index))

;; (temp-get-points n gesture index) consumes a gesture and a list and produces
;;     a new list the points at the index in the given list
(define (temp-get-points n gesture index)
  (cond[(empty? gesture) empty]
       [(empty? index) empty]
       [(= n (first index)) (cons (first gesture)
                                  (temp-get-points n gesture (rest index)))]
       [else (temp-get-points (add1 n) (rest gesture) index)]))

;; Tests:
(check-expect (get-points (list empty)  (list 0)) (list empty))
(check-expect (get-points (list (list 0 0) (list 1 1) (list 2 2)
                                               (list 3 3))  (list 0 1 2 3))
              (list (list 0 0) (list 1 1) (list 2 2) (list 3 3)))
(check-expect (get-points (list (list 0 0) (list 1 1) (list 2 2)
                                               (list 3 3)) (list 0 0 0))
              (list (list 0 0) (list 0 0) (list 0 0)))



;;(five-sample gesture) produces a sampling of gesture 5 points
;;  the first, n/4th, n/2th, 3n/4th, and last point.
;; Examples:
(check-expect (five-sample (list (list 1 1) (list 2 2)))
              (list (list 1 1) (list 1 1) (list 2 2) (list 2 2) (list 2 2)))
(check-expect (five-sample (list (list 1 1) (list 2 2) (list 3 3) (list 4 4)
                                (list 5 5) (list 6 6) (list 7 7) (list 8 8)))
              (list (list 1 1) (list 3 3) (list 5 5) (list 7 7) (list 8 8)))


;; five-sample: Gesture -> Gesture
;; requires: gesture is non-empty
(define (five-sample gesture)
  (get-points gesture (create-index-list -1 gesture)))

;; (create-index-list n gesture) creates a list of all the
;;   required indexes from which the gesture points need to be produced
;; Example:
(check-expect (create-index-list -1 (list (list 10 0) (list 50 24)
                                          (list 1 91))) (list 0 0 1 2 2))
;; Nat Gesture -> Gesture
(define (create-index-list n gesture)
  (cond[(= n 3) (cons (- (length gesture) 1) empty)]
       [(= n -1) (cons 0 (create-index-list (add1 n) gesture) )]
       [(< n 3) (cons  (floor (* (* 0.25 (add1 n))  (length gesture)))
                       (create-index-list (add1 n) gesture))]))

 
;; Tests:
(check-expect (five-sample (list (list 1 1) (list 2 2) (list 3 3) (list 4 4)))
              (list (list 1 1) (list 2 2) (list 3 3) (list 4 4) (list 4 4)))
(check-expect (five-sample (list (list 1 1)))
              (list (list 1 1) (list 1 1) (list 1 1) (list 1 1) (list 1 1)))
(check-expect (five-sample (list (list 1 1) (list 2 2) (list 3 3) (list 4 4)
                                 (list 5 5)))
              (list (list 1 1) (list 2 2) (list 3 3) (list 4 4) (list 5 5)))


;;(move-and-scale gesture x-scale y-scale) moves gesture to (0, 0) and
;;  scales it by (x-scale)x(y-scale)
;; Examples:
(check-expect (move-and-scale (list (list 1 1)) 1 1) (list (list 0 0)))
(check-expect (move-and-scale (list (list 1 5) (list 3 4)) 1 2)
              (list (list 0 2) (list 2 0)))

;; move-and-scale: Gesture Num Num -> Gesture
;; requires: gesture is non-empty
;;           x-scale > 0
;;           y-scale > 0
(define (move-and-scale gesture scale-x scale-y)
  (scale-gesture (translated-gesture gesture) scale-x scale-y))

;; (translated-gesture gesture) helper function to return the translated 
;;   gesture to move-and-scale
;; Example:
(check-expect (translated-gesture (list (list 5 5) (list 2 2)))
(list (list 3 3) (list 0 0)))

;; translated-gesture: Gesture -> Gesture
(define (translated-gesture gesture)
  (translate-gesture gesture (* -1 (smallest-x gesture))
                     (* -1 (smallest-y gesture))))


;; Tests:
(check-expect (move-and-scale (list (list 5 5) (list 2 2)) 3 0.5)
              (list (list 9 1.5) (list 0 0)))
(check-expect (move-and-scale (list (list 1 5) (list 3 4)) 1 2)
              (list (list 0 2) (list 2 0)))
(check-expect (move-and-scale (list (list 5 5) (list 1 2)) 3 0.5)
              (list (list 12 1.5) (list 0 0)))


(define min-width 30)
(define min-height 30)
(define norm-size 200)

;;(normalize-gesture gesture) normalizes gesture to (0,0) and a standard size
;; Examples:
(check-within (normalize-gesture (list (list 0 0) (list 100 100)))
              (list (list 0 0) (list 200 200)) 0.01)
(check-within (normalize-gesture (list (list 100 0) (list 100 50)
                                       (list 200 50)))
              (list (list 0 0) (list 0 200) (list 200 200)) 0.01)
 
;; normalize-gesture: Gesture -> Gesture
;; requires: gesture is not both vertical and horizontal
;;           gesture is non-empty
(define (normalize-gesture gesture)
  (cond[(horizontal? gesture) (move-and-scale gesture (find-scale-x gesture) 1)]
       [(vertical? gesture) (move-and-scale gesture 1 (find-scale-y gesture))]
       [else (move-and-scale gesture (find-scale-x gesture)
                             (find-scale-y gesture))]))

;; (vertical? gesture) helper function to determine whether a gesture
;;   is vertical
;; Example:
(check-expect (vertical? (list (list 35 5) (list 1 2))) false)

;; vertical? Gesture -> Bool
(define (vertical? gesture)
  (cond[(>= (- (greatest-x gesture) (smallest-x gesture)) min-width) false]
       [else true]))



;; (vertical? gesture) helper function to determine whether a gesture is
;;    horizontal
;; Example:
(check-expect (horizontal? (list (list 35 5) (list 1 2))) true)

;; horizontal? Gesture -> Bool
(define (horizontal? gesture)
  (cond[(>= (- (greatest-y gesture) (smallest-y gesture)) min-height) false]
       [else true]))

;; (find-scale-x gesture) helper function to find the x-offset for passing to
;;    move-and-scale
;; Example:
(check-within (find-scale-x (list (list 0 30) (list 10 0) (list 40 110))) 0.1 5)

;; find-scale-x: Gesture -> Num
(define (find-scale-x gesture)
  (/ norm-size (greatest-x (translated-gesture gesture))))

;; (find-scale-y gesture) helper function to find the y-offset for passing to
;;    move-and-scale
;; Example:
(check-within (find-scale-y (list (list 0 30) (list 10 0) (list 40 110)))
              0.1 1.8)

;; find-scale-x: Gesture -> Num
(define (find-scale-y gesture) 
  (/ norm-size (greatest-y (translated-gesture gesture)))) 
 

;; Tests:
(check-within (normalize-gesture (list (list 0 0) (list 100 30)))
              (list (list 0 0) (list 200 200)) 0.01)
(check-within (normalize-gesture (list (list 0 0) (list 100 29)))
              (list (list 0 0) (list 200 29)) 0.01)
(check-within (normalize-gesture (list (list 0 0) (list 30 100)))
              (list (list 0 0) (list 200 200)) 0.01)
(check-within (normalize-gesture (list (list 0 0) (list 29 100)))
              (list (list 0 0) (list 29 200)) 0.01)
(check-within (normalize-gesture (list (list 0 0) (list 400 400)))
              (list (list 0 0) (list 200 200)) 0.01)
(check-within (get-b-box
               (normalize-gesture (list (list 43 59) (list 166 937)
                                        (list 223 937))))
              (list (list 0 0) (list 200 200)) 0.01)
(check-within (get-b-box
               (normalize-gesture (list (list 821 920) (list 626 927)
                                        (list 231 197))))
              (list (list 0 0) (list 200 200)) 0.1)
(check-within (get-b-box
               (normalize-gesture (list (list 13 19) (list 26 27)
                                        (list 213 47))))
              (list (list 0 0) (list 200 28)) 0.1)


;;(geometric-5match gesture1 gesture2) produces the average distance between
;;  points in sub-sampled gesture1 and gesture2 after sub-sampling
;;   them with k points
;; Examples:
(check-within (geometric-5match
               (list (list 10 10) (list 30 30) (list 50 50) (list 70 70)
                     (list 80 80))
               (list (list 10 10) (list 20 20) (list 30 30) (list 40 40)
                     (list 40 40)))
               16.16 0.01)   
 
;; geometric-5match: Gesture Gesture -> Num
;; requires: gesture1 and gesture2 are each not both vertical and horizontal
(define (geometric-5match gesture1 gesture2)
  (/ (avg-distance (sub-gesture gesture1) (sub-gesture gesture2) 0) 5))


(define (sub-gesture gesture)
  (normalize-gesture (five-sample gesture)))
  
(define (avg-distance gesture1 gesture2 n)
  (cond[(= n 5) 0]
       [else (+ (calculate-length (first gesture1) (first gesture2))
                (avg-distance (rest gesture1) (rest gesture2) (add1 n)))]))

(check-within (geometric-5match (second (fourth templates))
                                (second (fourth templates))) 0 0.1)



;; Tests:


(define mystrokec (list (list 211 127.66665649414062)
                        (list 211 127.66665649414062)
                        (list 211 125.66665649414062)
                        (list 209 125.66665649414062)
                        (list 207 125.66665649414062)
                        (list 204 123.66665649414062)
                        (list 202 123.66665649414062)
                        (list 199 122.66665649414062)
                        (list 196 122.66665649414062)
                        (list 192 122.66665649414062)
                        (list 189 122.66665649414062)
                        (list 187 122.66665649414062)
                        (list 185 122.66665649414062)
                        (list 180 124.66665649414062)
                        (list 176 125.66665649414062)
                        (list 171 129.66665649414062)
                        (list 169 133.66665649414062)
                        (list 166 137.66665649414062)
                        (list 163 142.66665649414062)
                        (list 161 147.66665649414062)
                        (list 160 154.66665649414062)
                        (list 158 156.66665649414062)
                        (list 158 158.66665649414062)
                        (list 158 161.66665649414062)
                        (list 158 166.66665649414062)
                        (list 158 170.66665649414062)
                        (list 158 174.66665649414062)
                        (list 158 181.66665649414062)
                        (list 159 183.66665649414062)
                        (list 162 188.66665649414062)
                        (list 163 190.66665649414062)
                        (list 165 194.66665649414062)
                        (list 166 197.66665649414062)
                        (list 169 199.66665649414062)
                        (list 171 200.66665649414062)
                        (list 177 205.66665649414062)
                        (list 181 207.66665649414062)
                        (list 185 208.66665649414062)
                        (list 187 210.66665649414062)
                        (list 193 211.66665649414062)
                        (list 197 211.66665649414062)
                        (list 199 211.66665649414062)
                        (list 206 211.66665649414062)
                        (list 211 211.66665649414062)
                        (list 214 211.66665649414062)
                        (list 215 211.66665649414062)
                        (list 218 210.66665649414062)
                        (list 219 210.66665649414062)
                        (list 220 210.66665649414062)))


  
;; 3cv)
;(five-point-rec mystrokeg templates)
;;(five-point-rec candidate template-library) produces the symbol in
;;  template-library closest to candidate
;; Examples:
(check-expect (five-point-rec testd templates) 'd)
(check-expect (five-point-rec testk templates) 'k)

;; five-point-rec Gesture TL -> Sym
;; requires: candidate is not both vertical and horizontal
(define (five-point-rec gesture template-gesture) 
(smallest-diff (difference-list template-gesture gesture)))


;; (geometric-difference template-gesture gesture) produces the absolute 
;;   value of the the value returned from geometric-5match 
;; Example:
;(check-within (geometric-difference mystrokec mystrokeg) 0.1 107.4)

;; geometric-difference: Gesture Gesture -> Num
(define (geometric-difference template-gesture gesture)
  (cond[(< (geometric-5match template-gesture gesture) 0)
        (* -1(geometric-5match template-gesture gesture))]
       [else (geometric-5match template-gesture gesture)]))
  

;; (difference-list template-gesture gesture) helper function to produce a
;;   key-value pair list of alphabets and their geometric-5match values with
;;   the given gesture

;; difference-list: Gesture Gesture -> (list Num)
(define (difference-list template-gesture gesture)
  (cond[(empty? template-gesture) empty] 
       [else (cons (list (first (first template-gesture))
                         (geometric-difference (second
                                         (first template-gesture)) gesture))
                   (difference-list (rest template-gesture) gesture))]))



;; (smallest-diff difference-list) parses the difference-list and
;;    produces the alphabet correspondding to the minimum diff value
;; Example:
(check-expect (smallest-diff (list (list 'a 10)
                                   (list 'b 35) (list 'c 1))) 'c)
(check-expect (smallest-diff empty) empty)
(define (smallest-diff difference-list)
  (cond[(empty? difference-list) empty]
       [(empty? (rest difference-list)) (first (first difference-list))]
       [(< (first (rest (first difference-list)))
           (first (rest (first (rest difference-list)))))
        (smallest-diff (cons (first difference-list)
                             (rest (rest difference-list))))]
       [else (smallest-diff (cons (first (rest difference-list))
                               (rest (rest difference-list))))]))
 


;; Tests
(check-expect (five-point-rec tests templates) 's)
(check-expect (five-point-rec testy templates) 'y)
(check-expect (five-point-rec testd templates) 'd)
(check-expect (five-point-rec testk templates) 'k)
(check-expect (five-point-rec testt templates) 't)
(check-expect (five-point-rec testa templates) 'a)


;(five-point-rec mystrokeg templates)
;;(five-point-rec candidate template-library) produces the symbol in
;;  template-library closest to candidate
;; Examples:
(check-expect (k-point-rec testd templates 300) 'd)
(check-expect (k-point-rec testk templates 100) 'k)
(check-expect (k-point-rec tests templates 500) 's)
(check-expect (k-point-rec testy templates 500) 'y)
(check-expect (k-point-rec testa templates 500) 'a)
(check-expect (k-point-rec testt templates 500) 't)

;; k-point-rec Gesture TL Nat -> Sym 
;; requires:
;;    Input is not both vertical and horizontal
(define (k-point-rec gesture template-gesture k)
(smallest-diff (difference-list-k template-gesture gesture k)))

;; (sub-sample gesture k) produces a list of gesture of length k points
;;    according to given formula

;; sub-sample: Gesture Nat -> Gesture
(define (sub-sample gesture k)
  (get-points gesture (create-index-list-k -1 gesture k)))
;; Examples:
(check-expect (sub-sample (list (list 10 10) (list 20 20)) 5)
              (list (list 10 10)(list 10 10)(list 20 20)
                    (list 20 20)(list 20 20)))
 
;; Tests:
(check-expect (sub-sample (list (list 10 10) (list 21 25)
                                (list 23 53) (list 4 43)) 7) (list
                                                              (list 10 10)

                                                              (list 21 25)

                                                              (list 23 53)

                                                              (list 4 43)))
(check-expect (sub-sample (list (list 13 41)) 6)
              (list (list 13 41) (list 13 41) (list 13 41) (list 13 41)))
(check-expect (sub-sample (list (list 15 11) (list 221 22) (list 13 33)
                                (list 34 44)) 45) (list (list 15 11)
                                                        (list 221 22)
                                                        (list 13 33)
                                                        (list 34 44)))
(check-expect (sub-sample (list (list 132 131) (list 201 122)) 60)
(list (list 132 131) (list 132 131) (list 201 122) (list 201 122)))

;; requires: gesture is non-empty

(define (sub-gesture-k gesture k)
  (normalize-gesture (sub-sample gesture k))) 

(define (create-index-list-k n gesture k)
  (cond[(= n (- k 2)) (cons (- (length gesture) 1) empty)]
       [(= n -1) (cons 0 (create-index-list-k (add1 n) gesture k) )]
       [(< n (- k 2)) (cons  (floor (* (* 0.25 (add1 n)) (length gesture)))
                             (create-index-list-k (add1 n) gesture k))]))

;; Tests:
(check-expect (five-sample (list (list 1 1) (list 2 2) (list 3 3) (list 4 4)))
              (list (list 1 1) (list 2 2) (list 3 3) (list 4 4) (list 4 4)))
(check-expect (five-sample (list (list 1 1)))
              (list (list 1 1) (list 1 1) (list 1 1) (list 1 1) (list 1 1)))
(check-expect (five-sample (list (list 1 1) (list 2 2) (list 3 3) (list 4 4)
                                 (list 5 5)))
              (list (list 1 1) (list 2 2) (list 3 3) (list 4 4) (list 5 5)))





;;(geometric-5match gesture1 gesture2) produces the average distance between
;;  points in sub-sampled gesture1 and gesture2 after sub-sampling them with
;;   k points
;; Examples:   

;; geometric-match: Gesture Gesture -> Num
;; requires: gesture1 and gesture2 are each not both vertical and horizontal
(define (geometric-match gesture1 gesture2 k)
  (/ (avg-distance-k (sub-gesture-k gesture1 k)
                     (sub-gesture-k gesture2 k) 0 k) k))


;; (avg-distance-k gesture1 gesture2 n k) produces average distance between
;;   two gestures
;; avg-distance-k Gesture gesture Nat Nat -> Num
(define (avg-distance-k gesture1 gesture2 n k)
  (cond[(empty? (rest gesture1)) 0] 
       [else (+ (calculate-length (first gesture1) (first gesture2))
                (avg-distance-k (rest gesture1) (rest gesture2)
                                (add1 n) k))]))


;; (geometric-difference-k template-gesture gesture k) produces the
;;     absolute value of the the value returned from geometric-5match
;; geometric-difference-k Gesture Gesture Nat -> Num
(define (geometric-difference-k template-gesture gesture k)
  (cond[(< (geometric-match template-gesture gesture k) 0)
        (* -1(geometric-match template-gesture gesture k))]
       [else (geometric-match template-gesture gesture k)]))


 
;; (difference-list-k template-gesture gesture k) produces a list of key value
;;    pairs of alphabets and values from geometric-match

;; difference-list-k: Gesture Gesture Nat -> (list Sym Num)
(define (difference-list-k template-gesture gesture k)
  (cond[(empty? template-gesture) empty]
       [else
        (cons (list (first (first template-gesture))
                    (geometric-difference-k (second (first template-gesture))  
                                            gesture k))
              (difference-list-k (rest template-gesture) gesture k))]))

