;;;; ui.lisp

(in-package #:game-of-life)

(defconstant +cell-alive-color+ :cyan)
(defconstant +cell-alive-char+ #\#)
(defconstant +cell-empty-color+ :white)
(defconstant +cell-empty-char+ #\.)
(defconstant +bg-color+ :black)

(defun get-screen-center (scr)
  "Calculates the center position of the screen. Outputs a list '(x y)."
  (let* ((center-x (floor (/ (.width scr) 2)))
         (center-y (floor (/ (.height scr) 2))))
    (list center-x center-y)))

(defun render (grid scr)
  "Very basic render method. Sends the grid as a string to the screen."
  (let* ((center-coords (get-screen-center scr))
         (start-x (- (car center-coords) (floor (/ (grid-width grid) 2))))
         (start-y (- (cadr center-coords) (floor (/ (grid-height grid) 2)))))
    (clear scr)
    (loop for cell in (grid-cells grid)
          for i from 0 do
            (move scr (+ start-y (cell-y cell)) (+ start-x (cell-x cell)))
            (add scr
                 (if (cell-alive-p cell) +cell-alive-char+ +cell-empty-char+)
                 :color-pair (list
                              (if (cell-alive-p cell)
                                  +cell-alive-color+
                                  +cell-empty-color+)
                              +bg-color+)))
    (refresh scr)))

(defun step-and-render (grid scr)
  "Combines stepping and rendering in a single function."
  (progn (grid-step grid) (render grid scr)))

(defun start-application ()
  "Main entry point."
  (with-screen (scr
                :input-echoing nil
                :input-blocking t
                :enable-colors t
                :cursor-visibility t)
    (init-grid)
    (render *grid* scr)
    (event-case (scr event)
                (#\q (return-from event-case))
                (#\s (step-and-render *grid* scr)))))
