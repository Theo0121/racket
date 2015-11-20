
(require picturing-programs)
(define maze-size 15)

; A 'Cell' contains two numbers: 'x' and 'y' coordinates.
; The intended usage is:
; make-Cell : number number -> Cell
; Cell-x : Cell -> number
; Cell-y : Cell -> number

(define-struct Cell (x y))

; adjacents : Cell -> list-of-Cells
; Produce a list of the four Cells above, below, left, and right of 'cell'.

(define (adjacents cell)
  (list
   (make-Cell (Cell-x cell) (+ (Cell-y cell) 1))
   (make-Cell (Cell-x cell) (- (Cell-y cell) 1))
   (make-Cell (- (Cell-x cell) 1) (Cell-y cell))
   (make-Cell (+ (Cell-x cell) 1) (Cell-y cell))))

; random-adjacent : list-of-Cells -> Cell
; Produce a random Cell adjacent to a random Cell from the non-empty list 'cells'.

(define (random-adjacent cells)
  (let ((neighbors (adjacents cells)))
    (list-ref neighbors (random (length neighbors)))))



; count-in : Cell list-of-Cells -> number
; Produce 1 if 'cell' is in 'cells', otherwise produce 0.

(define (count-in cell cells)
  (cond
    [(member? cell cells) 1]
    [else 0]))




; touches : Cell list-of-Cells -> number
; Produce the number of Cells adjacent to 'cell' that are in 'cells'.

(define (touches cell cells)
  (+
   (count-in
    (make-Cell (Cell-x cell) (+ (Cell-y cell) 1))
    cells)
   (count-in
    (make-Cell (Cell-x cell) (+ (Cell-y cell) -1))
    cells)
   (count-in
    (make-Cell (+ (Cell-x cell) -1) (Cell-y cell))
    cells)
   (count-in
    (make-Cell (+ (Cell-x cell) 1) (Cell-y cell))
    cells)))



; try : Cell list-of-Cells -> list-of-Cells

(define (try cell cells)
  (and (not (member? cell cells))
       (<= 0 (Cell-x cell) maze-size)
       (<= 0 (Cell-y cell) maze-size)
       (= 1 (touches cell cells))))

; extend : list-of-Cells -> list-of-Cells
; Produce the version of 'cells' produced from trying to include a random Cell adjacent to one of
; the Cells in 'cells'.

(define (flatten L)
  (cond [(list? L) (apply append (map flatten L))]
        [else (list L)]))

(define (extend cells) (map random-adjacent cells))

(define small-maze (extend (list (make-Cell 0 0))))

; repeat : any unary-function number -> list
(define (repeat start transformer times)
  (cond [(zero? times) (list)]
        [else (flatten(list* start (repeat (transformer start) transformer (sub1 times))))]))
; A list of the intermediate mazes created by extending a single cell maze ten times.
(repeat (list(make-Cell 0 0)) extend 10)
