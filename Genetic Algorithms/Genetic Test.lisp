;;Genetic Test;;
;;Load Genetic Lib;;
(load "GeneticLib.lisp")
;;Create Data;;
(setupGenes '('0 '1 '2 '3 '4 '5 '6 '7 '8 '9 '+ '- '* '/))
(defvar *target* 21)
(defvar *ops*  '(#'+ #'- #'* #'/))
(defvar *nums* '(0 1 2 3 4 5 6 7 8 9))
(defun isOp?  (n) (>= n 10))
(defun isNum? (n) (< n 10))
;;Eval;;
(defun toGene (n) (nth n *genes*))
(defun genify (l) (map 'list 'toGene l))
(defun toOp (n) (cond
	((eq n '+) (lambda (x y) (+ x y)))
	((eq n '-) (lambda (x y) (- x y)))
	((eq n '*) (lambda (x y) (* x y)))
	(t (lambda (x y) (if (= 0 y) 0 (/ x y))))
))
(defun toNum (n) (nth n *nums*))
(defun parse (l) (if l (cons (if (toNum (first l)) (toNum (first l)) (toOp (first l))) (parse (rest l))) '()))
(defun evalCN (l) (cond
	((null l) '(0))
	((toNum (first l)) l)
	(t (evalCN (rest l)))
))
(defun opsLeft? (l) (cond
	((null l) nil)
	((isOp? (first l)) t)
	(t (opsLeft? (rest l)))
))
(defun evalC (l) (cond
	((null l) 0)
	((or (null (rest l)) (not (opsLeft? l))) (if (isNum? (first l))
		(toNum (first l))
		0
	))
	((isOp? (first l)) (funcall (toOp (toGene (first l))) (first (evalCN l)) (evalC (rest (evalCN l)))))
	(t (evalC (rest l)))
))
(defun evalPop (l) (map 'list 'evalC l))
;;Score
(defun score (l) (cond
	((= *target* (evalC l)) 99999999)
	(t (/ 100 (abs (- *target* (evalC l)))))
))
(defun roundedScore (l) (ceiling (score l)))
(defun scoreAll (l) (map 'list 'score l))
;;Generation
(defun next (l) (mate l (scoreAll l)))
;;Test
(defun test (curr) (format t "Chromes are: ~S~%Values are: ~S~%Scores are: ~S~%" (map 'list 'genify curr) (map 'list 'evalC curr) (map 'list 'roundedScore curr)))
(defun testR (l n) (if (= 0 n) '() (cons (test l) (testR (next l) (- n 1)))))