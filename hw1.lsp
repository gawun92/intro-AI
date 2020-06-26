

;prob 1
; this function is checking out whether an atom is contained into tree or not.
; to check out, divide three portion in a tree: left side, root, and right side.
; so keep tracking and comparing the number in the branch with the target number
; and decide to search in the left side or right side. 
(defun TREE-CONTAINS (N TREE)
	(if (numberp TREE) (= N TREE)
	(cond 
		((= (car (cdr TREE)) N) t)
		((> (car (cdr TREE)) N) (TREE-CONTAINS N (car TREE)))
		(t (TREE-CONTAINS N (car (cddr TREE))))
	)
	)
)

;prob 2
; this function is finding the smallest number in the tree.
; based on the structure of the tree, the most left side element(atom) is the smallest number.
; so this function kept getting the head of the tree until it get a number.
(defun TREE-MIN (N)
	(cond 
		((numberp (car N)) (car N))
		( t (TREE-MIN  (car N)))
	)
)

;prob 3
; this function takes one argument TREE and returns an pre-ordered list.
; The list is genereated with the rule(order : root left right)
; in this context, second is indicating root, first is left, and third is 
; right of the tree. each of them is appended and kept going through at 
; the end of the tree. 
(defun TREE-ORDER (TREE)
	(if (numberp TREE) 
		(list TREE)
		(append 
			(append  (list (second TREE)) (TREE-ORDER (first TREE)) )
				(TREE-ORDER (third TREE)))
	)
)

;prob 4
; it takes three arguments. L is tree, start is the index to start to get 
; sub-list and length would indicating the end point of the sublist
; to begin with, the basic concept is to change the list to start with 
; the given index to the end of the list
; when the start index reaches to zero, now make sublist with the given length
; ex)   '(a b c d e f) 3 2
;       '(d e f)       0 2
;       '(d e)         0 0 
(defun SUB-LIST (L START LEN)
    (cond 
   		((= LEN 0) NIL)
		(t (cond
        	((= START 0) (cons (car L) (SUB-LIST (cdr L) START (- LEN 1))))
			(t (SUB-LIST (cdr L) (- START 1) LEN)))
        )
	)
                
)

;prob 5
; this funcftion takes a list and dividing with two lists.
; if the length is even, two lists are equally divided.
; if the length is odd,  two lists are divided but front list has one more element
; than second list.
; to implement the code easily, i used SUB-LIST from question4.
; Specific explanation for implementation.
; when it is even, it is straightforwward.
; when it is odd,  the length of a list is divided and the value would be not integer.
; for example, the length is 5.   it would be 5/2 and 5/2
; for this reason, for the front list, i added 1/2 and for the second list, i subtracted 1/2
(defun SPLIT-LIST (L)
	(let* ((len (/ (length L) 2)))
		(cond
			((evenp (length L)) (append (list (SUB-LIST L 0 len)) (list (SUB-LIST L len len))))
			( t (append (list (SUB-LIST L 0 (+ len 1/2)) (SUB-LIST L (+ len 1/2) (- len 1/2)))))
		)
	)
)

;prob 6
;  it takes a signle argument which is the structure of tree and returns the deepest height.
;  In detail of the implementation, there are two cases; left side and right side.
;  and counting down of the depth is starting from each of tips of the tree.
;  and by using recursive, keep counting up and finich counting when reach the root of the tree.
(defun BTREE-HEIGHT (TREE)
  (cond
  	((null TREE) 0)
    ((numberp TREE) 0)
    ( t 
    	(if (< (BTREE-HEIGHT (car TREE)) (BTREE-HEIGHT (car (cdr TREE))))    
	    		( + 1 (BTREE-HEIGHT (car (cdr TREE))))
	    		( + 1 (BTREE-HEIGHT (car TREE)))
    	)
    )
  )
)

;prob 7
; the program is receiving list of atoms LEAVES, and returns a binary tree.
; in the process, three of cases are divided. when LEAVES length is 1, 2, or more
; since it is easy to get the atom(element) value when leaves length is 1 and 2.
; but otherwise, it is kept going through the given input recusively and reach out the other two cases
(defun LIST2BTREE (LEAVES)
  (cond
    ( ( = (length LEAVES) 1) 	(car LEAVES))
    ( ( = (length LEAVES) 2) 	LEAVES)
    ( t 	(list (LIST2BTREE (car (SPLIT-LIST LEAVES))) (LIST2BTREE (car (cdr (SPLIT-LIST LEAVES))))))
  )
)
;prob 8
; this function receives a single arguments and it is the expression of tree structure.
; it is simply coverting from tree structural expression to a simple list format.
; the program keep running recusively and append each of elements from the list
(defun BTREE2LIST (TREE)
  (cond
    ((numberp TREE) (list TREE))
    (t (append (BTREE2LIST (car TREE)) (BTREE2LIST (car (cdr TREE)))))
  )
)

;prob 9
; this is checking out whether the given two tree structures are same or not.
; for the check, it is checking out whether the both of the structures are same type or not.
; specifically, checkout out their type is identical or not.
; if they are identical number by number and its structure, the program makes decision to be truth.
; if they are identical but list by list (with identical structure), the program keeps going through into the list
; and check out.
(defun IS-SAME (E1 E2)
	(cond 
		( (and (null E1) (null E2)) t)
		( (or  (null E1) (null E2)) NIL)
		( (or  ( and (null E1) ( not (null E2)))  ( and ( not (null E1)) (null E2))) NIL) 
		( (and (numberp E1) (numberp E2))  (if(= E1 E2) t NIL) )
		( (or (and (numberp E1) (listp E2)) (and (listp E1) (numberp E2))) NIL)
		(t (if (IS-SAME (car E1) (car E2))
			(IS-SAME (cdr E1) (cdr E2))
			NIL))
	)
)

