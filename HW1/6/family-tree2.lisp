;;;; family-tree2 ;;;;
; a list like (parent1 parent2 (children)). 1st element the person in the original family,
; and 2nd element is someone married with 1st element. children are always in parallel lists.
; if one has no spouse, the spouse spot will be nil
; if one has no child, the child spot will be ()
(defconstant *family-tree2* 
'(a b (
	 (c u (
	      (m x (
	           (r nil ())
	           )
	      )
	      (n y ())
	      (o nil ())
	      )
	 )
	 (d v ())
	 (e w (
	      (p nil ())
	      (q nil ())
	      )
	 )
	 (f nil ())
	 )
))
(defvar *ft2-tree* nil)
(defvar *ft2-name* 'q)
(defvar *ft2-search-result* nil)
(defun ft2-search-name (tree)
	(cond ((null tree) nil)
		  ((or (equal *ft2-name* (car tree)) 
	           (equal *ft2-name* (cadr tree))) (setq *ft2-search-result* tree)) ; store the tree with *ft2-name* in it
	      ((atom tree) nil)
          (t  (mapcar #'ft2-search-name (caddr tree))) ; traverse every children
	)
)
(defun spouse (tree name)
	(setq *ft2-name* name) ; kind of input of ft2-search-name
	(setq *ft2-search-result* nil) ; kind of output of ft2-search-name
	(ft2-search-name tree)
	
	(if (equal name (car *ft2-search-result*))
		(cadr *ft2-search-result*)
		(car *ft2-search-result*)
	)
)
(defun children (tree name)
	(setq *ft2-name* name) ; kind of input of ft2-search-name
	(setq *ft2-search-result* nil) ; kind of output of ft2-search-name
	(ft2-search-name tree)

	(mapcar #'car (caddr *ft2-search-result*))
)
(defun ft2-children (name)
	(setq *ft2-name* name) ; kind of input of ft2-search-name
	(setq *ft2-search-result* nil) ; kind of output of ft2-search-name
	(ft2-search-name *ft2-tree*)

	(mapcar #'car (caddr *ft2-search-result*))
)
(defun grandchildren (tree name)
	(setq *ft2-name* name) ; kind of input of ft2-search-name
	(setq *ft2-search-result* nil) ; kind of output of ft2-search-name
	(ft2-search-name tree)
	
	(setq *ft2-tree* tree) ; kind of input of ft2-children
	(reduce #'append (remove nil (mapcar #'ft2-children (children tree name))))
)

(defun ft2-search-parent (tree)
	(cond ((null tree) nil)
		  ((member (car *ft2-name*) (mapcar #'car (caddr tree))) (setq *ft2-search-result* tree)) ; store the tree with *ft2-name* in it
	      ((atom tree) nil)
          (t  (mapcar #'ft2-search-parent (caddr tree))) ; traverse every children
	)
)
(defun ft2-parent2 (tree name)
	(setq *ft2-name* name) ; kind of input of ft2-search-name
	(setq *ft2-search-result* nil) ; kind of output of ft2-search-name
	(ft2-search-name tree)
	(cond ((equal name (cadr *ft2-search-result*)) nil)
		  (t
		  	(setq *ft2-name* *ft2-search-result*) ; kind of input of ft2-search-parent
			(setq *ft2-search-result* nil) ; kind of output of ft2-search-parent
			(ft2-search-parent tree)
			(if (null *ft2-search-result*)
				nil
				(list (car *ft2-search-result*) (cadr *ft2-search-result*))
			)
		  )
	)
)
(defun parent2 (tree name)
	(if (null name)
		nil
		(ft2-parent2 tree name)
	)
)

(defun grandparent2 (tree name)
	(parent2 tree (car (parent2 tree name)))
)

(defun siblings (tree name)	
	(setq *ft2-name* name) ; kind of input of ft2-search-name
	(setq *ft2-search-result* nil) ; kind of output of ft2-search-name
	(ft2-search-name tree)
	(cond ((equal name (cadr *ft2-search-result*)) nil)
	  	  (t
			(setq *ft2-name* *ft2-search-result*) ; kind of input of ft2-search-parent
			(setq *ft2-search-result* nil) ; kind of output of ft2-search-parent
			(ft2-search-parent tree)
			(remove name (mapcar #'car (caddr *ft2-search-result*)))
		  )
	)
	
)
                                           
;;;; family-tree2 ;;;;
