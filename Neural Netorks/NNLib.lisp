;Network
;; Creates a network where each neuron is a list of weights and each column is a list of neurons. Takes a series of numbers as inputs
(defun make-net (&rest num-in-col) (if (null num-in-col) '() (make-net-l (car num-in-col) (cdr num-in-col))))
;; Creates a network as above but takes a number and a list as inputs
(defun make-net-l (num-in num-in-col)
	(if (null num-in-col) '() (labels (
		(make-neuron (in) (if (= 0 in) '() (cons 0 (make-neuron (1- in)))))
		(make-n-neurons (n) (if (= 0 n) '() (cons (make-neuron num-in) (make-n-neurons (1- n))))))
	(cons (make-n-neurons (car num-in-col)) (make-net-l (car num-in-col) (cdr num-in-col)))))
)
;; Gets the number of columns in network net
(defun numcol (net) (list-length net))
;List functions
(defun sum (l) (cond
	((null l) 0)
	(t (+ (car l) (sum (cdr l))))
))
(defun sum (l) (reduce #'+ l))
(defun map2l (l1 l2 op) (cond
	((or (null l1) (null l2)) nil)
	(t (cons (funcall op (car l1) (car l2)) (map2l (cdr l1) (cdr l2) op)))
))
(defun countTo (n) (if (> 0 n)
	'()
	(cons n (countTo (1- n)))
))
;Neuron functions
;; Decides if the neuron will fire and outputs 1 if it does and -1 if it does not; equivilant to beta function (b)
;; When differentiating, use formula fire = 2(1+e^-x)-1, dfire/dx = -(1/2)(fire^2 - 1)
(defun fire (ins ws) (if (< 0 (sum (map2l ins ws #'*))) 1 -1))
;Run Net
;; Evaluates up to a column col with inputs ins and network net
(defun evalCol (ins net col) (if (> 0 col)
	ins
	(map 'list (lambda (x) (evalNeuron ins net col x)) (countTo (1- (list-length (nth col net)))))
))
;; Evaluates a single neuron with inputs ins in network net and position (col,row)
(defun evalNeuron (ins net col row) (if (> 0 col)
	(nth row ins)
	(fire (evalCol ins net (1- col)) (nth row (nth col net)))
))
;; Evaluates entire network
(defun evalNet (in net) (evalCol in net (1- (list-length net))))