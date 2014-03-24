;;;; euclidean ;;;;
(defun eu-square(n) (* n n))

(defun euclidean (list1 list2)
	(sqrt (reduce #'+ (mapcar #'eu-square (mapcar #'- list1 list2))))
)
;;;; euclidean ;;;;