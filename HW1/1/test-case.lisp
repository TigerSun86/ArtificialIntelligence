;(load "C:\\WorkSpace\\Artificial Intelligence\\HW1\\1\\test-case.lisp")
(load "C:\\WorkSpace\\Artificial Intelligence\\HW1\\1\\positive-count.lisp")

(terpri)
(princ "**** test-case positive-count ****")
;number test
(print (positive-count '(1)))                   ;one positive number
(print (positive-count '(-1)))                  ;one negative number
(print (positive-count '(0)))                   ;one zero
(print (positive-count 1))                      ;non-list positive
(print (positive-count 0))                      ;non-list negative
(print (positive-count '(1 2)))                 ;two positive numbers
(print (positive-count '(-1 -2)))               ;two negative numbers
(print (positive-count '(1 -2)))                ;+ front, - behind
(print (positive-count '(-1 2)))                ;- front, + behind
(print (positive-count '(1 545 23 986 123 22 3)))           ;multiple positive numbers
(print (positive-count '(-1 -545 -23 -986 -123 -22 -3)))    ;multiple negative numbers
(print (positive-count '(-1 545 -23 986 123 -22 3)))        ;multiple + and - numbers
(print (positive-count '(-1 545 0 -23 986 0 123 -22 3)))    ;multiple + and - and 0 numbers
(print (positive-count '((1 -545) (-23 (986) -123) 22 -3))) ;nesting list

;nil test
(print (positive-count '()))                    ;empty list
(print (positive-count nil))                    ;nil
(print (positive-count '(a b c)))               ;character list
(print (positive-count 'a))                     ;character
(print (positive-count '(a b 999 c -2 e f)))    ;characters and numbers
(print (positive-count '(432 876 -382 9572AAA 11 -33)))     ;numbers and character
(print (positive-count '(+ 1 2)))               ;nonalphanumeric character

;special test
;check a list with huge amount of positive and negative numbers
(defun mass-numbers-test ()
    (do ((i 0 (1+ i))           ;initialize
         (list ()))
        ((> i 1000) list)       ;end condition and return
        (setq list (cons i list))     ;body, generate a list with huge amount of
        (setq list (cons (- i) list)) ;      positive and negative numbers
  )
)
(print (positive-count (mass-numbers-test)))

;check a list with huge amount of nestings
(defun mass-nestings-test ()
    (do ((i 0 (1+ i))           ;initialize
         (list ()))
        ((> i 100) list)        ;end condition and return
        (setq list (list i list))     ;body, generate a list with huge amount of
        (setq list (list (- i) list)) ;      nestings
  )
)
(print (positive-count (mass-nestings-test)))
(terpri)
(princ "**** test-case positive-count ****")

