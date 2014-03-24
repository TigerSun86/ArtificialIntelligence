;;;; family-tree ;;;;
;;;; parents ;;;;
; a list like (person (ancestors1) (ancestors2)) to represent the family tree.
; the first element in the list "person" should be a atom,
; and the remaining two elements in the list represent the ancestors of 1st element,
; just like the 2nd element in list is "father and his ancestors", 
; and 3rd one is "mother and her ancestors".
; the last two elements in list should always be list which nestedly contains 
; "father and his ancestors", or maybe () if there is no ancestors.

(defconstant *family-tree* '(John (Mark (James (GeorgeH () ()) (Barbara () ())) (Jane (Bill () ()) (Hillary () ())))
                                  (Mary (Peter (GeorgeW () ()) (Laura () ())) (Pat (Barack () ()) (Michelle () ())))))
                                  
(defun pa-search-name (tree name)
    (cond ((null tree) nil)
          ((equal name (car tree)) (cdr tree))	; return the ancestors if found person
          ((atom tree) nil)
          (t (union (pa-search-name (cadr tree) name)	; 2nd in list
                 (pa-search-name (caddr tree) name)	; 3rd in list
             )
          )
    )
)
(defun parents (tree name)
	; search this name in the tree. return nil if cannot find it.
	(let ((ancestors (pa-search-name tree name))
	     )
	     ; this ancestors should be like ((ancestors1) (ancestors2))
	     (remove nil (list (car (car ancestors)) (car (cadr ancestors))))
	)
)
;;;; parents ;;;;

;;;; grandparents ;;;;
(defvar *gp-g-tree* nil)
; to create a function with just one parameter, so it can be called by mapcar
(defun gp (name)
	(parents *gp-g-tree* name)
)
(defun grandparents (tree name)
	(setq *gp-g-tree* tree)
	(reduce #'append (remove nil (mapcar #'gp (parents tree name))))
)
;;;; grandparents ;;;;
;;;; family-tree ;;;;
