;;;; nested-member ;;;;
(defun nested-member (element list)
    (cond ((null list) nil)
          ((equal element list) t)
          ((atom list) nil)
          (t (or (nested-member element (car list))
                 (nested-member element (cdr list))
             )
          )
    )
)
;;;; nested-member ;;;;