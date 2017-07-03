;;;; game-of-life.lisp

(in-package #:game-of-life)

(defparameter *grid-layout* (list "....."
                                  ".xxx."
                                  ".....")
  "The initial layout of the grid")

(defvar *grid* nil
  "The global variable that will hold the state of the grid.")

(defstruct cell
  "Represents a single cell."
  alive-p
  x
  y)

(defstruct grid
  "Represents the grid, with a flat list of cells."
  cells
  (width 0))

(defun get-total-alive-neighbours (cell grid)
  "Counts the number of alive neighbours of a given cell."
  (let* ((x (cell-x cell))
         (y (cell-y cell))
         ;; A list of coordinates of the neighbouring cells.
         (neighbour-coords (list (list (1- x) (1- y)) ; top-left
                                 (list x (1- y))      ; top
                                 (list (1+ x) (1- y)) ; top-right
                                 (list (1+ x) y)      ; right
                                 (list (1+ x) (1+ y)) ; bottom-right
                                 (list x (1+ y))      ; bottom
                                 (list (1- x) (1+ y)) ; bottom-left
                                 (list (1- x) y)))    ; left
         ;; The actual neighbouring cells.
         (neighbours (mapcar
                      (lambda (pos)
                        (find-if (lambda (cell)
                                   (and
                                    (= (car pos) (cell-x cell))
                                    (= (cadr pos) (cell-y cell))))
                                 (grid-cells grid)))
                      neighbour-coords)))
    (length (remove-if-not #'cell-alive-p (remove nil neighbours)))))

(defun render (grid)
  "Very basic render method. Sends the grid as a string to the output."
  (loop for cell in (grid-cells grid)
        for i from 1 do
          (format t "~A~A"
                  (if (cell-alive-p cell) "x" ".")
                  (if (= (mod i (grid-width grid)) 0) #\Newline ""))))

(defun parse-grid-layout (layout)
  "Parses a grid layout into a list of cell structs. The grid layout should be a
   list of strings, with each string representing a single row of the grid."
  (loop for char across (format nil "~{~A~}" layout)
        for i from 0
        collect
        (let* ((width (length (car layout)))
               (alive (char= #\x char))
               (y (floor (/ i width)))
               (x (mod i width)))
          (make-cell :alive-p alive :x x :y y))))

(defun create-grid (grid-layout)
  "Creates a grid instance from a layout."
  (make-grid
   :cells (parse-grid-layout grid-layout)
   :width (length (car grid-layout))))

(defun init ()
  "Initializes the grid"
  (setf *grid* (create-grid *grid-layout*)))

(defun grid-step (grid)
  "Steps the game forward one turn."
  (loop for cell in (grid-cells grid) do
    (let ((alive-neighbours (get-total-alive-neighbours cell grid)))
      (print (list (cell-x cell) (cell-y cell)))
      (print alive-neighbours)
      (setf
       (cell-alive-p cell)
       (or
        (= alive-neighbours 3)
        (and (cell-alive-p cell) (= alive-neighbours 2)))))))
