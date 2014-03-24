;(load "C:\\WorkSpace\\Artificial Intelligence\\HW1\\5\\test-case.lisp")
(load "C:\\WorkSpace\\Artificial Intelligence\\HW1\\5\\euclidean.lisp")

(terpri)
(princ "**** test-case euclidean ****")
(print (euclidean '(1 2 3) '(4 5 6)))
(print (euclidean '(1 2 3 4 5 6 7 8 9 10) '(4 5 6 7 8 9 10 11 12 13)))
(print (euclidean '(1) '(2)))
(print (euclidean '(1) '(1)))
(print (euclidean '(1 1) '(1 1)))
(print (euclidean '(-1 -2 -3) '(-4 -5 -6)))
(print (euclidean '(0 0 0) '(0 0 0)))
(terpri)
(princ "**** test-case euclidean ****")



