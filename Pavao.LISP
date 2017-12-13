"Family Definition, reads in input by string"
(defun family()
    (loop for line = (read-line *standard-input* nil)
          until (eq line nil)
          do (format t "~%~a" line) (sortcase (with-input-from-string (in line) (read in)))
    )
)

"Person Definition: name, parents, spouses, children"
(defstruct person (name nil) (parents nil) (spouses nil) (children nil))

"Family Tree Hash Table"
(defparameter *familyTree* (make-hash-table))

"Create Person"
(defun create-person (name parents)
  (when (not (listp parents))
    (error "Parentstructs is not a list."))
  (setq name (make-person
              :name name
              :parents parents))
  (setf(gethash (person-name name) *familyTree*) name)
)

"Make person w/o parents and add to hashtable"
(defun makePerson (name)
  (setq name (make-person
              :name name))
  (setf(gethash (person-name name) *familyTree*) name)
)

"Check to see if person is in familyTree for X"
(defun checkPerson (name)
  (gethash name *familyTree*)
)

"Get person's parents from table"
(defun getParents (name)
  (sort(person-parents(gethash name  *familyTree*))#'string-lessp)
)

"Check to see if person has given parent for X"
(defun checkParent (parent person)
  (member parent (getParents person))
)

"Get all of a person's spouses"
(defun getSpouses (name)
  (person-spouses(gethash name  *familyTree*))
)

"Checks if person has/had a given spouse for X"
(defun checkSpouse (person1 person2)
  (member person1 (getSpouses person2))
)

"Get all children of given a name/person"
(defun getChildren (name)
  (if (checkperson name) 
      (sort(person-children(gethash name  *familyTree*))#'string-lessp))
)

"Check if person has child for X"
(defun checkChildren (person Child)
  (member child (getChildren person))
)

"Adds a child to children list of both parents"
(defun addChild (parent1 parent2 child)
  (setf(person-children(gethash parent1 *familyTree*)) (cons child (person-children(gethash parent1 *familyTree*))))
  (setf(person-children(gethash parent2 *familyTree*)) (cons child (person-children(gethash parent2 *familyTree*))))
)

"Adds spouse to both parties involved"
(defun addSpouse (person1 person2)
  (setf(person-spouses(gethash person1 *familyTree*)) (cons person2 (person-spouses(gethash person1 *familyTree*))))
  (setf(person-spouses(gethash person2 *familyTree*)) (cons person1 (person-spouses(gethash person2 *familyTree*))))
)

"Adds both parents to a person."
(defun addParents (parent1 parent2 person)
  (setf(person-parents(gethash person *familyTree*)) (cons parent1 (person-parents(gethash person *familyTree*))))
  (setf(person-parents(gethash person *familyTree*)) (cons parent2 (person-parents(gethash person *familyTree*))))
)

"Adds parents, children, and spouses at once with the three helper methods"
(defun addAllConnections (parent1 parent2 child)
  (addParents parent1 parent2 child)
  (addChild parent1 parent2 child)
  (if(not(checkSpouse parent1 parent2))
      (addspouse parent1 parent2))
)

"Checks for siblings for X, uses set helper method"
(defun checkSibs (person1 person2)
  (cond
   ((getParents person1)(set= (getParents person1) (getParents person2)))
   (t nil))
)

"Sets two objects equal"
(defun set= (set1 set2)
  (let ((result t))
    (loop for i in set1 doing
          (when (not (member i set2 :test #'equal))
            (setf result nil)
            (return nil)))
    (when result
      (=(length set1)(length set2))))
)

"Lists all a person's siblings for X."
(defun getFullSibs(person)
  (setq allSibs (remove-duplicates (append (getChildren(first(getParents person)))(getChildren (second (getParents person))))))
  (setq allSibs (delete person allSibs))
  (loop for x in allSibs
        do (if (not(checkSibs x person))
               (setq allSibs (remove x allSibs))))
  (setq allSibs (sort allSibs #'string-lessp))
)

"Gets all direct ancestors"
(defun getAncestors (person)
  (setq personList1 (list person))
  (setq ancestors nil)
  (if (not(getParents person)) 
      (setq ancestors (append (list person) ancestors)))
  (loop while (not(null personList1))
      do(setq PersonList2 nil)
        (loop for x in PersonList1
              do (setq PersonList2 (remove-duplicates(append PersonList2 (getParents x)))))
        (setq ancestors (append PersonList2 ancestors))
        (setq PersonList1 PersonList2)
  )
  (setq ancestors (remove-duplicates(sort ancestors #'string-lessp)))
)

"Checks for direct ancestor."
(defun checkAncestor (person1 person2)
  (member person1 (getAncestors person2))
)

"Start from the top (Adam and Eve) and work your way down to get all the relatives."
(defun getRelatives (person)
  (setq relativeslist nil)
  (loop for KEY being the hash-key of *familyTree* for VALUE being the hash-value of *familyTree*
      do(if (checkRelatives person key) 
          (setq relativesList (append relativesList (list key))))
  )
  (delete person relativesList)
  (setq relativesList (remove-duplicates(sort relativesList #'string-lessp)))
)

"Check to see if the ancestors of two people have overlap."
(defun checkRelatives (person1 person2)
  (setq commonAncestors nil)
  (setq p1Ancestors (getAncestors person1))
  (setq p2Ancestors (getAncestors person2))
  (loop for x in p1Ancestors
        do(if (member x p2Ancestors)
              (setq commonAncestors (append commonAncestors (list x))))
  )
  (setq ancestors (remove-duplicates(sort commonAncestors #'string-lessp)))
)

"FIRST: Get all the first cousins of a person."
"SECOND: Get Parents of a person."
"THIRD: Get Parents siblings of a person."
"LAST: Get Parents siblings kids of a person."
(defun getCousins (person numCousin numRemoved)
  (setq PersonList1 person)
  (dotimes (num numCousin)
    (setq PersonList2 nil)
    (loop for x in PersonList1
          do (setq PersonList2 (remove-duplicates(append PersonList2 (getParents x)))))
    (setq PersonList1 PersonList2)
  )
  (setq PersonList2 nil)
  (loop for x in PersonList1
        do (setq PersonList2 (remove-duplicates(append PersonList2 (getFullSibs x)))))
  (setq PersonList1 PersonList2)
  (dotimes (num (+ numCousin numRemoved))
    (setq PersonList2 nil)
    (loop for x in PersonList1
          do (setq PersonList2 (remove-duplicates(append PersonList2 (getChildren x)))))
    (setq PersonList1 PersonList2)
  )
  (setq PersonList1 (remove-duplicates(sort PersonList1 #'string-lessp)))
)

"Check is person1 is the 'cousinNum' cousin 'removalNum' of person2."
(defun checkCousins (person1 cousinNum removalNum person2)
  (member person1 (getCousins person2 cousinNum removalNum))
)

"Check is person is unrelated"
(defun getUnrelated (person)
  (setq unrelated nil)
  "This took forever to figure out"
  (loop for KEY being the hash-key of *familyTree* for VALUE being the hash-value of *familyTree*
        do(if (not(member key (getRelatives person)))
              (setq unrelated (append unrelated (list key))))
  )
  (delete person unrelated)  
  (setq unrelated (remove-duplicates(sort unrelated #'string-lessp)))
)

"Check if person is unrelated for X"
(defun checkUnrelated (person1 person2)
  (member person2 (getUnrelated person1))
)

"Determines whether e/x/w query"
(defun sortCase (inputLine)
  (case (nth 0 inputLine)
    (e (e inputLine))
    (x (x inputLine))
    (w (w inputLine))
  )
)


"E call"
(defun e (inputLine)
  (case (nth 3 inputLine)
  ((nil)
   (if (not(checkPerson (nth 1 inputLine))) (makePerson (nth 1 inputLine)))
   (if (not(checkPerson (nth 2 inputLine))) (makePerson (nth 2 inputLine)))
   (addSpouse (nth 1 inputLine) (nth 2 inputLine)))
  (otherwise
   (if (not(checkPerson (nth 1 inputLine))) (makePerson (nth 1 inputLine)))
   (if (not(checkPerson (nth 2 inputLine))) (makePerson (nth 2 inputLine)))
   (if (not(checkPerson (nth 3 inputLine))) (makePerson (nth 3 inputLine)))
   (addAllConnections (nth 1 inputLine) (nth 2 inputLine) (nth 3 inputLine))))
)

"X call"
(defun x (inputLine)
  (handler-case
      (case (nth 2 inputLine)
        (spouse (if(checkSpouse (nth 1 inputline) (nth 3 inputline)) (print "Yes.") (print "No.")))
        (parent (if(checkParent (nth 1 inputline) (nth 3 inputline)) (print "Yes.") (print "No.")))
        (sibling (if(checkSibs (nth 1 inputline) (nth 3 inputline)) (print "Yes.") (print "No.")))
        (ancestor (if(checkAncestor (nth 1 inputline) (nth 3 inputline)) (print "Yes.") (print "No.")))
        (relative (if(checkRelatives (nth 1 inputline) (nth 3 inputline)) (print "Yes.") (print "No.")))
        (unrelated (if(checkUnrelated (nth 1 inputline) (nth 3 inputline)) (print "Yes.") (print "No.")))
        (otherwise (if(checkCousins (nth 1 inputline) (second(nth 2 inputline)) (third(nth 2 inputline)) (list (nth 3 inputline))) (print "Yes.") (print "No.")))
      )
  (error (er) 
      (case (nth 2 inputLine)
        (unrelated (if (not(checkPerson (nth 3 inputLine))) (print "Yes.") (print "No.")))
        (sibling (if (string= (nth 3 inputLine) (nth 1 inputline)) (print "Yes.") (print "No.")))
        (otherwise (print "No."))))) 
)

"W call"
(defun w (inputLine)
  (handler-case
      (case (nth 1 inputLine)
        (spouse (print(getSpouses (nth 2 inputLine))))
        (parent (print(getParents (nth 2 inputLine))))
        (sibling 
         (setf siblist (append (getFullSibs (nth 2 inputLine)) (list (nth 2 inputLine))))
         (print(setq siblist (remove-duplicates(sort siblist #'string-lessp))))
       )
        (ancestor (print(getAncestors (nth 2 inputLine))))
        (relative (print(getRelatives (nth 2 inputLine))))
        (unrelated (print(getUnrelated (nth 2 inputLine))))
        (otherwise (print(getCousins (list (nth 2 inputLine)) (second(nth 1 inputline)) (third(nth 1 inputline)))))
      )
  (error (er) (print "None.")))
)