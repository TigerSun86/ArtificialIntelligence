;(load "C:\\WorkSpace\\Artificial Intelligence\\HW1\\4\\test-case.lisp")
(load "C:\\WorkSpace\\Artificial Intelligence\\HW1\\4\\parents.lisp")

(terpri)
(princ "**** test-case parents ****")
(print (parents *family-tree* 'John))
(print (parents *family-tree* 'Peter))
(print (parents *family-tree* 'GeorgeW))
(terpri)
(princ "**** test-case parents ****")
(terpri)
(princ "**** test-case grandparents ****")
(print (grandparents *family-tree* 'John))
(print (grandparents *family-tree* 'Jane))
(print (grandparents *family-tree* 'Laura))
(terpri)
(princ "**** test-case grandparents ****")



