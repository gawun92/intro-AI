;
; CS161 Hw3: Sokoban
; 
; *********************
;    READ THIS FIRST
; ********************* 
;
; All functions that you need to modify are marked with 'EXERCISE' in their header comments.
; Do not modify a-star.lsp.
; This file also contains many helper functions. You may call any of them in your functions.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any node.
; That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your heuristic functions.
; Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on memory.
; So, it may crash on some hard sokoban problems and there is no easy fix (unless you buy 
; Allegro). 
; Of course, other versions of Lisp may also crash if the problem is too hard, but the amount
; of memory available will be relatively more relaxed.
; Improving the quality of the heuristic will mitigate this problem, as it will allow A* to
; solve hard problems with fewer node expansions.
; 
; In either case, this limitation should not significantly affect your grade.
; 
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition (which will
; affect your score).
;  
;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
; They are not necessary for this homework.
; Use/modify them for your own convenience.
;

;
; For reloading modified code.
; I found this easier than typing (load "filename") every time. 
;
(defun reload()
  (load "hw3.lsp")
  )

;
; For loading a-star.lsp.
;
(defun load-a-star()
  (load "a-star.lsp"))

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all()
  (reload)
  (load-a-star)
  )

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
; 
;
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

; Some helper functions for checking the content of a square
(defun isBlank (v)
  (= v blank)
  )

(defun isWall (v)
  (= v wall)
  )

(defun isBox (v)
  (= v box)
  )

(defun isKeeper (v)
  (= v keeper)
  )

(defun isStar (v)
  (= v star)
  )

(defun isBoxStar (v)
  (= v boxstar)
  )

(defun isKeeperStar (v)
  (= v keeperstar)
  )

;
; Helper function of getKeeperPosition
;
(defun getKeeperColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
  )

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).
; 
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (right) column is the zeroth column.
;
(defun getKeeperPosition (s row)
  (cond ((null s) nil)
	(t (let ((x (getKeeperColumn (car s) 0)))
	     (if x
		 ;keeper is in this row
		 (list x row)
		 ;otherwise move on
		 (getKeeperPosition (cdr s) (+ row 1))
		 );end if
	       );end let
	 );end t
	);end cond
  );end defun

;
; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
(defun cleanUpList (L)
  (cond ((null L) nil)
	(t (let ((cur (car L))
		 (res (cleanUpList (cdr L)))
		 )
	     (if cur 
		 (cons cur res)
		  res
		 )
	     );end let
	   );end t
	);end cond
  );end 

; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of the game.
; (neither any boxes nor the keeper is on a non-goal square)
;
; Currently, it always returns NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.
;



; test case example
;(goal-test '((1 1 1) (1 3 1) (1 2 1) (1 4 1) (1 4 1) (1 1 1))) —> NIL
;(goal-test '((1 1 1) (1 5 1) (1 1 1))) —> T
(defun isThereBoxOrKeeper (s)
	(cond 
	  	((null s) nil)
	  	;; if found the box, return true and it would allow goal-test function to return nil
	  	((isBox (first s)) t)	
	  	((isKeeper (first s)) t)
	  	((isThereBoxOrKeeper (rest s)) t)	
	  	(t nil)
	)
)

; it takes a single argument and return whether the goal of the game is attained or not.
; it check out whether there is a box not to be on the goal.
; if it is on, return true, if not nil
(defun goal-test (s)

	(cond 
		((null s) t)
		((goal-test (rest s))	; it allows to keep going through the set element
			(cond 
				((isThereBoxOrKeeper (first s))  nil)	; checkout whether any box exists or not
				; if there is no box, it returns true and allows this function to keep running & go through other element
				(t t)	
			)
		)
	)
)



; EXERCISE: Modify this function to return the list of 
; sucessor states of s.
;
; This is the top-level next-states (successor) function.
; Some skeleton code is provided below.
; You may delete them totally, depending on your approach.
; 
; If you want to use it, you will need to set 'result' to be 
; the set of states after moving the keeper in each of the 4 directions.
; A pseudo-code for this is:
; 
; ...
; (result (list (try-move s UP) (try-move s DOWN) (try-move s LEFT) (try-move s RIGHT)))
; ...
; 
; You will need to define the function try-move and decide how to represent UP,DOWN,LEFT,RIGHT.
; Any NIL result returned from try-move can be removed by cleanUpList.
; 




; it takes three arguments: state S, row number r, and column number c.
; it returns the integer value which is from the location(r,c) in S
(defun get-square (S r c) 
	(cond
		((null S) nil)
		((or (< r 0) (< c 0))  nil)
		((< (length  S)      (+ r 1))  nil)
		((< (length (first S)) (+  c 1)  )  nil)
		; if row is positive value, get the remain rows and decreasing the value of r
		((< 0 r) (get-square (rest S) (- r 1) c))
		; if the function went through this, it is guarantee that row is 0
		; if column is positive value, in the first row, get the rest elements
		; and decreasing the value of c
    	((< 0 c) (get-square (list (rest (first S))) r (- c 1)))
    	; when r = 0 and c = 0, it return the first element in first row and first column
    	(t (first (first s)))
	)
)



; this is a helpfer function in set-square
; it returns a list after modified the value in the (r,c) 
(defun set-element-in-a-low (r c v)
	(cond
		; if it reaches the target location (r,c)
		((= c 0) (append (list v) (rest r)))
		; append an element column by column
		((> c 0) (append (list (first r)) (set-element-in-a-low (rest r) (- c 1) v)))
		(t nil)
	)
)

(defun set-square (s r c v)
	(cond
		((null s) nil)
		((null r) nil)
		((null c) nil)
		((null v) nil)
		; if set the out of boundary value, it returns nil
		; [start] boundary check
		((or (< r 0) (< c 0))  nil)
		((< (length s)      (+ r 1))  nil)
		((< (length (first s)) (+  c 1)  )  nil)
		; [end] boundary check
		((> r 0) (append (list (first s)) (set-square (rest s) (- r 1) c v)))
		(t (append (list (set-element-in-a-low (first s) c v)) (rest s)))
	)
)




				
(defun next-states (S)
	(cleanUpList 
	    (list 
	      (try-move S 'UP) 
	      (try-move S 'RIGHT)
	      (try-move S 'DOWN)
	      (try-move S 'LEFT)  
	    )
	)
)


; it takes two arguments; list and direction.
; this function is based on the keeper's perspective.
; there are two cases, whether keeper is on the star or not.
(defun try-move (S D)
	(if (null S) NIL)
	(let* 	((pos (getKeeperPosition S 0)) 
		     (c (first  pos)) 
			 (r (second pos))) 

		(cond 
			; when the keeper is moving around the blank
			((isKeeper(get-square S r c))		
				(cond
					((null (set-square S r c blank)) NIL)
					((equal D 'UP) 		(update-position (set-square S r c blank) c (- r 1) c (- r 2)))
					((equal D 'DOWN) 	(update-position (set-square S r c blank) c (+ r 1) c (+ r 2)))
					((equal D 'LEFT) 	(update-position (set-square S r c blank) (- c 1) r (- c 2) r))
					((equal D 'RIGHT) 	(update-position (set-square S r c blank) (+ c 1) r (+ c 2) r))
				)		
			)
			; when the keeper is passing the goal 
			(t 
				(cond
					((null (set-square S r c star)) NIL)
					((equal D 'UP) 		(update-position (set-square S r c star) c (- r 1) c (- r 2)))
					((equal D 'DOWN) 	(update-position (set-square S r c star) c (+ r 1) c (+ r 2)))
					((equal D 'LEFT) 	(update-position (set-square S r c star) (- c 1) r (- c 2) r))
					((equal D 'RIGHT) 	(update-position (set-square S r c star) (+ c 1) r (+ c 2) r))
				)			
			)
		)
	)
)

; thos functions takes five arguments, state set, next row and column (keeper future location)
; and the next next row and column (it would be future box location)
; this function is checking out the next step' s state and update the value in the current location and the next location.
(defun update-position (s c1 r1 c2 r2)
	(cond
		((null s) NIL)
		((< c1 0) nil)
		((< r1 0) nil)
		((< (length s)      (+ r1 1))  nil)
		((< (length (first s)) (+  c1 1)  )  nil)
		; update the value depends on different situation
		((isBlank(get-square s r1 c1)) (set-square s r1 c1 keeper))
		((isStar(get-square s r1 c1))  (set-square s r1 c1 keeperstar))
		
		
		; check the boundary 
		((isBox(get-square s r1 c1))
			(cond
				((< c2 0) nil)
				((< r2 0) nil)
				((< (length s) (+ r2 1) ) nil)
				((< (length (first s)) (+ c2 1) ) nil)
				; when it is not updatable,
				((and (not (isBlank(get-square s r2 c2))) (not (isStar(get-square s r2 c2))) ) nil)
				(t 
					(cond
						((isBlank(get-square s r2 c2)) (set-square (set-square s r2 c2 box) r1 c1 keeper) )
						(t 						  (set-square (set-square s r2 c2 boxstar) r1 c1 keeper))
					)
				)
			) 
		)
		((isBoxStar(get-square s r1 c1)) 
			(cond
				((< c2 0) nil)
				((< r2 0) nil)
				((< (length s) (+ r2 1) ) nil)
				((< (length (first s)) (+ c2 1) ) nil)
				; when it is not updatable,
				((and (not (isBlank(get-square s r2 c2))) (not (isStar(get-square s r2 c2)))) nil)
				(t 
					(cond
						((isBlank(get-square s r2 c2)) (set-square (set-square s r2 c2 box) r1 c1 keeperstar) )
						(t 						  (set-square (set-square s r2 c2 boxstar) r1 c1 keeperstar))
					)
				)

			)
		)
		(t nil)
	)

)



; EXERCISE: Modify this function to compute the trivial 
; admissible heuristic.
;
(defun h0 (s)
	0
)


; it counts all box in a given row and 
; it will be the helper function to count all boxes in an matrix
(defun num-box-in-a-row (r)
	(cond
		((null r) 0)
		((isBox(car r)) (+ 1 ( num-box-in-a-row (cdr r))))
		(t ( num-box-in-a-row (cdr r)))
	)
)
; count all boxes in a matrix.
(defun count-box (S)
	(cond
		((null S) 0)
	 	(t (+ (num-box-in-a-row (car S)) (count-box (rest S)) ))
	)
)

; EXERCISE: Modify this function to compute the 
; number of misplaced boxes in s.
;
; Is this heuristic admissible?
; Yes, in my opinion, if the heuristic is estimated with too large value, it would inefficient.
; so with the number of the boxes, it can be a minimum movement to be required. This is because
; to put a box on a star, each of boxes is required to move at least 1 step.

; number of the box on the matrix
(defun h1 (s)
	(count-box s)	
)




; overflow
;
;
; (defun num-something-in-a-row (r TARGET)
; 	(cond
; 		((null r) 0)
; 		(( equal (car r) TARGET) (+ 1 ( num-something-in-a-row (cdr r) TARGET)))
; 		(t ( num-something-in-a-row (cdr r) TARGET))
; 	)
; )


; (defun find-col (r TARGET)
; 	(cond
; 		;Try to find x coordinate of box or goal in current row, if didn't find any, return NIL
; 		((null r) nil)
; 		((equal (car r) TARGET) 0 )
; 		(t (+ 1 (find-col (cdr r) TARGET))  )
; 	)
; )
; (defun find-row (S TARGET)
; 	(cond
; 		((< 0 (num-something-in-a-row (first S) TARGET)) 0)
; 		(t (+ 1 (find-row (rest S) TARGET)))
; 	)
; )

; ; it takes two arguments, S is for a 
; ; the function is to return a first row to contain a TARGET 
; (defun get-row-containing-target (S TARGET)
; 	(cond
; 		((< 0 (num-something-in-a-row (car S) TARGET)) (car S))
; 		(t (get-row-containing-target (rest S) TARGET))
; 	)
; )


; ; find the distance between box and star
; ; because of complexity, box location is the most close one to (0,0)
; (defun h305186572 (s)
; 	(let* ((column_box 	(find-col (get-row-containing-target s box) box))
; 		  (row_box 		(find-row s box)) 
; 		  (column_star  (find-col (get-row-containing-target s star) star))
; 		  (row_star 	(find-row s star))
; 		  (distance_X (abs (- row_star row_box)))
; 		  (distance_Y (abs (- column_star column_box))))

; 		(+ distance_X distance_Y)
; 	)
; )


; I believe that the above function is more admissble than this.
; However, the above one cannot be runnnable because of overflow
; so I simplified the version. Which is checking the diff between num box and num star
; by each row. 
(defun h305186572 (S)
  (cond
    ((null S) 0)
    (t ( +  (h305186572 (rest S))  (diff (first S) 0) )	;; adding all the counted value (recursive)

     )
  )
)


(defun diff (row gap)
	  (cond
	    ((null row) 	; when finishing the counting in a row
	    	(cond       ; by using absolute value to be positive, and return this value.
	    		(( < gap 0) (- 0 gap))
	    		(t (- gap 0))
	    	))
	    ((isBox  (first row))  (diff (rest row) (+ gap 1)) )	; if there is box, counting +1
	    ((isStar (first row))  (diff (rest row) (- gap 1)) )	; if there is star, counting -1
	    (t                     (diff (rest row) gap)       )
	  )
)
	
















; EXERCISE: Change the name of this function to h<UID> where
; <UID> is your actual student ID number. Then, modify this 
; function to compute an admissible heuristic value of s. 
; 
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the 
; running time of a function call.
;
; (defun hUID (s)
;   )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.
 | Each problem can be visualized by calling (printstate <problem>). For example, (printstate p1).
 | Problems are ordered roughly by their difficulties.
 | For most problems, we also provide a number which indicates the depth of the optimal solution.
 | These numbers are located at the comments of the problems. For example, the first problem below has optimal solution depth 6.
 | As for the solution depth, any admissible heuristic must make A* return an optimal solution. So, the depths of the optimal solutions provided could be used for checking whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible to solve without a good heuristic!
 | 
 |#
;(6)
(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 4 0 4 1)
	   (1 1 1 1 1 1)))

;(15)
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 4 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(13)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 4 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(17)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 4 0)
	   (0 3 1 0 0 0 0)))

;(12)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 4 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(13)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 4 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(47)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 4 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 4 0 4 1)
	   (1 1 1 1 1 1)))

;(34)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 4 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))

;(59)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 4 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(?)
(setq p11 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 4 0)
	    (0 2 0 4 0 0 0)	   
	    (3 2 1 1 1 4 0)
	    (0 0 1 4 0 0 0)))

;(?)
(setq p12 '((1 1 1 1 1 0 0 0)
	    (1 0 0 4 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(?)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 4 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(?)
(setq p14 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)	    
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))

;(?)
(setq p15 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 4 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1)))
	 )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (t (if (> deltaX 0) 'RIGHT 'LEFT))
	  );end cond
    );end let
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	);end cond
  );

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(t (format t "|"))
	);end cond
  )

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)    
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn    
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
  );end defun