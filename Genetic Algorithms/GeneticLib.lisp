;;Setup;;
;Must be run first for the program to act reasonably;
(defun setupGenes (genes) (defparameter *genes* genes));;Possible phenotypes
;;Random Starter;;
(defun genChrome (n) (cond
	((= 0 n) '())
	(t (cons (random (length *genes*)) (genChrome (- n 1))))
));;Generates a random chromosome of length n
(defun chros (s n) (cond
	((= 0 n) '())
	(t (cons (genChrome s) (chros s (- n 1))))
));;Generates n random chromosomes of length s
;;Mutate & Crossover;;
(defun mutate (l) (let ((rand (random (* 100 (length *genes*))))) (cond
	((null l) '())
	((< rand (length *genes*)) (cons rand (rest l)))
	(t (cons (first l) (mutate (rest l))))
)));;Mutates chromosome l
(defun crossover (c1 c2) (cond
	((or (null c1) (null c2)) '())
	((<= (random 100) 70) (cons (first c2) (crossover (rest c2) (rest c1))))
	(t (cons (first c1) (crossover (rest c1) (rest c2))))
));;Crosses genetic data from c1 to c2 to produce a new chromosome
;;Breeding;;
(defun breed (c1 c2) (crossover (mutate c1) (mutate c2)));;Breeds c1 and c2
(defun wIndex (l w n) (cond
	((or (null l) (null w)) nil)
	((<= (- n (first w)) 0) (first l))
	(t (wIndex (rest l) (rest w) (- n (first w))))
));;Weighted index, in list l with weights w (Int), gets the nth value
(defun match (l s) (breed (wIndex l s (random (ceiling (apply '+ s)))) (wIndex l s (random (ceiling (apply '+ s))))));;Matches individuals in l based on scores in s
(defun mate (l s) (loop for x in l collect (match l s)));;Matches (length l) individuals in l based on scores in s