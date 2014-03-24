;;;; positive-count ;;;;
(defvar *gnCorrect* 1)  ;global flag: 1: all is number;
                        ;             0: list is empty or non-number exists

(defun pc-positive-judge (aNumber)
    (if (> aNumber 0)
        1           ;positive return 1
        0           ;else return 0
    )
)

(defun pc-list-check (list)
    (cond ((null list)    0)
          ((numberp list) (pc-positive-judge list)) ;number, judge it
          ((atom list)    (setq *gnCorrect* 0))  	;non-number, make a mark
          (t (+ (pc-list-check (car list))          ;list, decomposit it
                (pc-list-check (cdr list))))
    )
)

(defun positive-count (list)
    (setq *gnCorrect* 1)  ;set AllNumber flag first
    
    (let ((aResult 0))    ;store result
         (if (null list)            
             (setq *gnCorrect* 0)               	;empty list, make a mark
             (setq aResult (pc-list-check list))   	;non-empty, check it
         )
         
         (if (equal *gnCorrect* 1)
             aResult      ;return result
             nil          ;exception case, return nil
         )
    )
)
;;;; positive-count ;;;;
