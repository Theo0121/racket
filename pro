
    (require picturing-programs)
    ; maze size
    (define maze-size 15)
    
    ; A 'Cell' contains two numbers: 'x' and 'y' coordinates.
    ; The intended usage is:
    ; make-Cell : number number -> Cell
    ; Cell-x : Cell -> number
    ; Cell-y : Cell -> number
    
    (define-struct Cell (x y))
    
    ; list-4 : any -> list
    ; Produce a list containing 'element' four times
    
    (define (list-4 element)
          (list element element element element))
        
    
    ; random-element : list -> any
    ; Produce a random element from the non-empty list 'a-list'.
    
    (define (random-element a-list)
          (list-ref a-list (random (length a-list))))
    
    
    ; adjust : Cell number number -> Cell
    ; Produce a Cell like 'cell' but with 'x' and 'y' added to the 'x' and 'y' of 'cell'.
    
    (define (adjust cell x y)
          (make-Cell (+ (Cell-x cell) x) (+  (Cell-y cell) y )))
    
    
    
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

    (define (random-adjacent cell)
      (let ((neighbors (adjacents cell)))
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
    
    

    ; try : Cell list-of-Cells -> list-of-CellsStep
    
    (define (try cell cells)
      (and (not (member? cell cells))
           (<= 0 (Cell-x cell) maze-size)
           (<= 0 (Cell-y cell) maze-size)
           (= 1 (touches cell cells))))
    
    ; extend : list-of-Cells -> list-of-Cells
    ; Produce the version of 'cells' produced from trying to include a random Cell adjacent to one of
    ; the Cells in 'cells'.
    
    (define (extend cells)
          (try (random adjacents cells) cells))
    
    

    ;Check-expect
    (define small-maze (extend (list (make-Cell 0 0))))
    (check-expect (and (= (length small-maze) 2)
                       (member? (make-Cell 0 0) small-maze)
                       (or (member? (make-Cell 0 1) small-maze)
                           (member? (make-Cell 1 0) small-maze)))
                  #true)
