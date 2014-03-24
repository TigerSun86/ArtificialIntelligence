; Name: Xunhu Sun ID: 901614698

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

;;;; storm-categories ;;;;
(defun sc-get-category-name (speed)
    (cond ((> speed 156) 'Hurricane-Cat-5)
          ((> speed 131) 'Hurricane-Cat-4)
          ((> speed 111) 'Hurricane-Cat-3)
          ((> speed 96) 'Hurricane-Cat-2)
          ((> speed 74) 'Hurricane-Cat-1)
          ((> speed 39) 'Tropical-Storm)
          (t nil)
    )
)
; translate (bonnie 65) to (BONNIE TROPICAL-STORM)
(defun sc-get-one-spot (list)
    (list (car list) (sc-get-category-name (cadr list)))
)
(defun storm-categories (list)
    (mapcar #'sc-get-one-spot list)
)
;;;; storm-categories ;;;;

;;;; storm-distribution ;;;;
(setq Tropical-Storm 0)
(setq Hurricane-Cat-1 0)
(setq Hurricane-Cat-2 0)
(setq Hurricane-Cat-3 0)
(setq Hurricane-Cat-4 0)
(setq Hurricane-Cat-5 0)

(defun sd-judge-distribution (speed)
    (cond ((> speed 156) (setq Hurricane-Cat-5 (1+ Hurricane-Cat-5)))
        ((> speed 131) (setq Hurricane-Cat-4 (1+ Hurricane-Cat-4)))
        ((> speed 111) (setq Hurricane-Cat-3 (1+ Hurricane-Cat-3)))
        ((> speed 96) (setq Hurricane-Cat-2 (1+ Hurricane-Cat-2)))
        ((> speed 74) (setq Hurricane-Cat-1 (1+ Hurricane-Cat-1)))
        ((> speed 39) (setq Tropical-Storm (1+ Tropical-Storm)))
        (t nil)
    )  
)
(defun sd-generate-distribution (list)
    (sd-judge-distribution (cadr list))
)

(defconstant *storm-level* '(Tropical-Storm Hurricane-Cat-1 Hurricane-Cat-2 Hurricane-Cat-3 Hurricane-Cat-4 Hurricane-Cat-5))

(defun storm-distribution (list)
    (mapcar #'sd-generate-distribution list)
    (setq storm-value (mapcar #'eval *storm-level*))
    (mapcar #'list *storm-level* storm-value)
)
;;;; storm-distribution ;;;;

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

;;;; euclidean ;;;;
(defun eu-square(n) (* n n))

(defun euclidean (list1 list2)
	(sqrt (reduce #'+ (mapcar #'eu-square (mapcar #'- list1 list2))))
)
;;;; euclidean ;;;;

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
