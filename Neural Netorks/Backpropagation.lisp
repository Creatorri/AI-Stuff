;;Learning
(setq *learning-const* 0.1);Determines how far each iteration will move the weights
(defun dbdx (b) (* -0.5 (1- (* b b))));Derivative of the fire function (beta function) with respect to its inputs, given the output of the fire function
(defun dndw (ins net n-col n-row w-col w-row w-con) (if (= n-col w-col)
	(if (= n-row w-row)
		(* (dbdx (evalNeuron ins net n-col n-row)) (evalNeuron ins net (1- n-col) w-con))
		0)
	(* (dbdx (evalNeuron ins net n-col n-row)) (reduce #'+ (map 'list
		(lambda (a-row) (dndw ins net (1- n-col) a-row w-col w-row w-con))
		(countTo (list-length (nth (1- n-col) net)))
	)))
));Derivative of neuron n with respect to weight w
(defun learn-weight (ins desireds net col row con-num) (+ (nth con-num (nth row (nth col net))) (* *learning-const* (if (= (1- (list-length net)) col)
	(let ( (d (nth row desireds)) (z (evalNeuron ins net col row)) (i (evalNeuron ins net col con-num)) ) (* (- d z) (dbdx z) i))
	(reduce #'+ (map 'list
		(lambda (a-row) (let
			( (d (nth a-row desireds)) (z (evalNeuron ins net col a-row)) )
			(* (- d z) (dbdx z) (dndw ins net (1- (list-length net)) a-row col row con-num))))
		(countTo (1- (list-length desireds)))
	))))
));Finds the next iteration of a weight given inputs and desired outputs
(defun learn-net (ins desireds net) (if (null net) '() (cons
	(map 'list
		(lambda (row)
			(map 'list
				(lambda (con-num) (learn-weight ins desireds net 0 row con-num))
				(countTo (1- (list-length ins)))))
		(countTo (list-length (car net))))
	(learn-net (evalCol ins net 0) desireds (cdr net))
)));Finds the next iteration of all weights