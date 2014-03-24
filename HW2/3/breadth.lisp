;;;; queue ;;;;
; functions for FIFO queue

; init the queue
(defun que-init (queue)
    nil
)
(defun que-isEmpty (queue)
    (null queue)
)
; put one element into the rear of the queue
(defun que-offer (queue e)
	(append queue e) ; get a list like ((0 0) (0 1) (0 2))
)
; remove one element from the front of the queue
(defun que-pull (queue)
    (let ((front-node nil)
          (new-queue queue)
         )
        (setq front-node (car new-queue))   ; if que is nil, return nil
        (setq new-queue (cdr new-queue))
        (list front-node new-queue)
	)
)

;;;; queue ;;;;

;;;; breadth-first-search ;;;;

(defconstant  *action-set* '(wj-fill-small wj-fill-large wj-empty-small wj-empty-large
                             wj-pour-small-to-large wj-pour-large-to-small))
(defvar *temp-node* nil)
(defvar *frontier-que* nil)
(defvar *frontier-que* nil)
(defvar *element-to-remove* nil)
(defun my-xor (e)
    (cond ((equal *element-to-remove* e) nil)
          (t e)
    )
)
(defun my-remove (element list)
    (setq *element-to-remove* element)
    (remove nil (mapcar #'my-xor list))
)
; get the leaf nodes of this node
(defun bfs-expand-node (node)
    ; get all next states by doing all actions. if the action causes no changes to the state, don't count it
    (print (my-remove node (mapcar #'funcall *action-set*)))
)

(defun breadth-first-search ()
    (let ((frontier-que nil))
        (setq frontier-que (que-init frontier-que))
        (setq frontier-que (que-offer frontier-que '(0 0))) ; initialize the frontier using the initial state of problem
        (print "begin")
        (do ((result nil)
             (front-node nil)
             (i 0 (1+ i))
            )
            ((or result (que-isEmpty frontier-que) (> i 10)) result) ; if the frontier is empty then return failure; if found the goal return t
            
            (setq front-node (que-pull frontier-que)) ; choose a leaf node and remove it from the frontier
            (print "aaa")
            (cond ((equal (cadr front-node) 4) (setq result t)) ; if the node contains a goal state then return the corresponding solution
                  (t (setq frontier-que (que-offer frontier-que (bfs-expand-node front-node)))) ; else expand the chosen node and add the resulting nodes to the frontier
            )
        )
    )
)

;;;; breadth-first-search ;;;;
;;;; water jug ;;;;
(defun wj-fill-small ()
    (list 3 (cadr *temp-node*))
)

(defun wj-fill-large ()
    (list (car *temp-node*) 5)
)

(defun wj-empty-small ()
    (list 0 (cadr *temp-node*))
)

(defun wj-empty-large ()
    (list (car *temp-node*) 0)
)

(defun wj-pour-small-to-large ()
    (let ((small)
          (large)
         )
        
        (setq small 
            (cond ((< 5 (+ (car *temp-node*) (cadr *temp-node*))) (- (+ (car *temp-node*) (cadr *temp-node*)) 5)) ; if total > 5 then small = remaining
                  (t 0))
        )
        (setq large 
            (cond ((< 5 (+ (car *temp-node*) (cadr *temp-node*))) 5)
                  (t (+ (car *temp-node*) (cadr *temp-node*))))
        )

        (list small large)
    )
)

(defun wj-pour-large-to-small ()
    (let ((small)
          (large)
         )
        
        (setq small 
            (cond ((< 5 (+ (car *temp-node*) (cadr *temp-node*))) 0)
                  (t (+ (car *temp-node*) (cadr *temp-node*))))
        )
        (setq large 
            (cond ((< 5 (+ (car *temp-node*) (cadr *temp-node*))) (- (+ (car *temp-node*) (cadr *temp-node*)) 5)) ; if total > 5 then large = remaining
                  (t 0))
        )

        (list small large)
    )
)

;;;; water jug ;;;;

(breadth-first-search)