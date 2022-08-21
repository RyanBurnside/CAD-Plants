;;; Copied to window at 11:36 PM 8/20/22

;;;; This is a little program to draw a plant
;;;; Ryan Burnside 2022
;;;; TODO put a little "wiggle" in the lengths and angles

;; Forced a 3D point to 2D
(defun force-2d (pt)
  (if (caddr pt)
    (list (car pt) (cadr pt))
    pt))
	 
;; Defines a "line" pts are 2d
(defun make-line  (pt1 pt2)
  (list	(list 'pt1 pt1)
	(list 'pt2 pt2)
	(list 'angle (angle pt1 pt2))
	(list 'length (distance pt1 pt2))))       


;; Easy accessor ex (get-attr 'pt1 my-line)
(defun get-attr	 (key lis)
  (cadr (assoc key lis)))


;; Wraps the commandline command
(defun draw-line  (line)
  (command "line"
	   (get-attr 'pt1 line)
	   (get-attr 'pt2 line)
	   ""))


;; Used for generating the child lines
(defun draw-and-return-line (line)
  (draw-line line)
  line)


;; Produce a new child branch
(defun new-child-branch (parent angle-delta length-multiplier)
  (setq pt (get-attr 'pt2 parent))
  (make-line pt
    (polar pt
	   (+ (get-attr 'angle parent) angle-delta)
	   (* (get-attr 'length parent) length-multiplier))))
    

;; Draws a tree
;; Example (tree (list (make-line '(0 0) '(0 10))) 7)
(defun tree (line-list iterations)
  (setq old-generation line-list)
  (setq new-generation '())

  (if line-list
    ;; Draw initial list
    (progn
      (foreach l line-list
	(draw-line l))

      ;; Make children lines and draw them
      (repeat iterations
	(foreach l old-generation
	  (setq	child-a (new-child-branch l 0.0 0.55)
		child-b (new-child-branch l 0.785 0.4)
		child-c (new-child-branch l -0.785 0.4))

	  (setq	new-generation
		 (append new-generation (list (draw-and-return-line child-a)
					      (draw-and-return-line child-b)
					      (draw-and-return-line child-c)))))
	(setq old-generation new-generation
	      new-generation '())))))

;; Prompts a user for a tree trunk and fills in the rest
;; I assume 7 iterations here but it cold be made variable.
(defun prompt-tree ()
  (setq base-point (force-2d (getpoint "Pick trunk base"))
	crown-point (force-2d (getpoint base-point "Pick crown base")))
  (tree (list (make-line base-point crown-point)) 7))



;;; End of text
