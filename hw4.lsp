;;;;;;;;;;;;;;
; Homework 4 ;
;;;;;;;;;;;;;;


; EXERCISE: Modify this function to decide satisifiability of delta.
; If delta is satisfiable, sat? returns a list of n integers that represents a model of delta,  
; otherwise it returns NIL. (See spec for details.)
; param n: number of variables in delta
; param delta: a CNF represented as a list of lists




;(get-all-singles '((1 2) (2) (4) (2 5 3) (7) (7 1))) -->  (7 4 2)
(defun get-all-singles (delta)
  ( cond
    ((null delta) '())
    ((= (length (car delta)) 1)  
          (append (get-all-singles (rest delta)) (car delta)  ))
    (t     (get-all-singles (rest delta)))
  )
)




; (multiple-clauses-checker  '((-1 -2) (2))  '(-1 2))  --> T
; (multiple-clauses-checker  '((1 2) (-2))  '(-1 2))  --> NIL
; this function take two arguments; multiple clause and assignment
; from the help by single-clause-checker, it going through all
; each clause and check out SAT.
(defun multiple-clauses-checker (multi-clauses assignment)
  (cond
    ((null multi-clauses) t)
    ((and (single-clause-checker (car multi-clauses) assignment) 
          (multiple-clauses-checker (cdr multi-clauses) assignment)) 
        t
    )
  )
)


; (single-clause-checker '(-2) '(1 2)) --> NIL
; (single-clause-checker '(2) '(1 2)) --> t
; the function takes two arguments a single clause and
; assignment. If the assignment is sat then it is true if not false.
; the function is checking a single clause.
; this is checking whether it is valid or not
; it is the helper function for the multiple clauses
(defun single-clause-checker (single-clause assignment) 
  (cond
   ((null single-clause) nil)
   (t  
      (cond 
        ((contain-target-neg? assignment (car single-clause)) t)
        (t (single-clause-checker (cdr single-clause) assignment))
      )
   ) 
  )
)

; check it a list contains the target or not.
; especially it focuses on negation difference
; only if the a target's negation exists, it is nil
; else all true.
(defun contain-target-neg? (clause target)
    (cond
      ((null clause) t) 
      ((= target  (- 0 (car clause))) nil)
      (t (contain-target-neg? (cdr clause) target))
    )
)



; by using back-tracking, it returns an assigned list.
; the cases are two true or false(-).
; it keept counting down the number of values and
; when it reaches to 1, it returns the clauses
(defun back-tracking (clause assignment counter)
  (cond
    ((multiple-clauses-checker clause assignment)    
      (cond
        ((= 1 counter) assignment)
        (t (or  (back-tracking clause (append assignment (list (- 0 (+ (length assignment) 1)))) (- counter 1)) 
                (back-tracking  clause (append assignment (list (+ (length assignment) 1))) (- counter 1)))
        )
      )
    )
    (t nil)
  )
)

; this is helper function of remove-all-target.
; since clauses are structed lists in a list.
; so it is going through a single list and remove it.
; it gets rid of its value and the negative value.
(defun remove-target (clause target)
  (cond
    ((null clause) NIL)
    ((or (=  target (car clause)) (= (- 0 target) (car clause)))      
                                      (remove-target (cdr clause) target))
    ; when removing in the middle, keep going through and keep appendeding the front
    ; clause (not removed).
    (t (append (list (car clause))    (remove-target (cdr clause) target)))
    )
  )

; it is telling whether the target(int) is in the list or not
(defun exist-or-not (delta target)
  (cond
    ((null delta) nil)
    ((= target (car delta)) t)
    (t (exist-or-not (cdr delta) target))
    )
  )

; it make getting rid of all number(target) in a list
; ex) target : 1  --> then get rid of 1 and -1 in a list
(defun remove-all-targets (delta target)
  (cond
    ((null delta) NIL)
    ((exist-or-not (car delta) target)
        (remove-all-targets (cdr delta) target)
    )
    (t ; when removing in the middle 
      (append (list (remove-target (car delta) target)) 
              (remove-all-targets (cdr delta) target))
    )
    )
  )


; the function is getting rid of unclear clauses
; from the above helper functions, we got the single clause 
; which is obvious whether it is true or not
; with using this information, it get rid of the known cluase and 
; only getting the remainers.
;  ex)
; (get-unknown-clause '((1 2) (2) (4) (2 5 3) (7) (7 1)) '(1 2)) 
; ((4) (7))
(defun get-unknown-clause (delta singles)
  (cond
    ((null singles) delta)
    (t 
      (get-unknown-clause 
              (remove-all-targets delta (car singles))  ;parameter 1
              (rest singles)                            ;parameter 2
      ) 
    )
  )
)

; get rid of duplicate number in assignment
; (sat? 1 '((1) (1)))  --> (1)
(defun remove-dup (assignment)
  (cond 
    ((null assignment) assignment)
    ((member (car assignment) (rest assignment))
    (remove-dup (cdr assignment)))
    (t (cons (car assignment) (remove-dup (cdr assignment))))
  )
)




; the function is taking two parameters; n is the number of variable and delta is the list for SAT
; it returns a unique clause for SAT and if it is valid, i
(defun sat? (n delta) 
   (remove-dup (back-tracking (get-unknown-clause delta (get-all-singles delta)) (get-all-singles delta) n))
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Functions that help you parse CNF from files in folder cnfs/
; You need not modify any functions in this section
; Usage (solve-cnf <path-to-file>)
; e.g., (solve-cnf "./cnfs/f1/sat_f1.cnf")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun split-line (line)
  (if (equal line :eof)
      :eof
      (with-input-from-string (s line) (loop for x = (read s nil) while x collect x))))

(defun read-cnf (filename)
  (with-open-file (in filename)
    (loop for line = (split-line (read-line in nil :eof)) until (equal line :eof)
      if (equal 'p (first line)) collect (third line)      ; var count
      if (integerp (first line)) collect (butlast line)))) ; clause

(defun parse-cnf (filename)
  (let ((cnf (read-cnf filename))) (list (car cnf) (cdr cnf))))

; Following is a helper function that combines parse-cnf and sat?
(defun solve-cnf (filename)
  (let ((cnf (parse-cnf filename))) (sat? (first cnf) (second cnf))))

