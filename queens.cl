;;;; Author - Marc Khristian Partoriza
;;;; Date   - March 1st, 2015
;;;; Class  - ICS 313
;;;;
;;;; Program  finds the best placement of 2 chess queens on a n by n sized chess
;;;; board so that the coverage of the 2 queens are either minimized or maximized.
;;;; Returns a list of the number of squares covered by the queens and the location
;;;; of the two queens.



;;; maxqueens - Takes in a number n finds the best placement of 2 chess queens on a n by n
;;;            sized chess board so that coverage is maximized. Returns (# covered (2 queen locations))
(defun maxqueens (n)
  
  ;; max          - current highest # of squares.
  ;; maxpair      - the queen locations with the highest # of squares.
  ;; list         - the n x n board
  ;; queens pairs - list of all possible queen pairings.
  (setf max 0)
  (setf maxpair '())
  (setf list (build-p (build-list n)))
  (setf queenspairs (build-pndu list))
  
  ;; Go through every pair, checking how many squares the queens cover, updating max if a pair
  ;; with higher square count is found.
  (dolist (i queenspairs)
    (setf squarecount (queenscount (first i) (first (rest i)) n))
    (when (> squarecount max)
      (setf max squarecount)
      (setf maxpair (list (first i) (first (rest i))))))
  
  (setf maxpair (cons max maxpair))
  maxpair)



;;; minqueens - Takes in a number n finds the best placement of 2 chess queens on a n by n
;;;            sized chess board so that coverage is minimized. Returns (# covered (2 queen locations))
(defun minqueens (n)
  
  ;; min          - current lowest # of squares.
  ;; minpair      - the queen locations with the lowest # of squares.
  ;; list         - the n x n board
  ;; queens pairs - list of all possible queen pairings.
  (setf min 10000)
  (setf minpair '())
  (setf list (build-p (build-list n)))
  (setf queenspairs (build-pndu list))
  
  ;; Go through every pair, checking how many squares the queens cover, updating min if a pair
  ;; with lower square count is found.  
  (dolist (i queenspairs)
    (setf squarecount (queenscount (first i) (first (rest i)) n))
    (when (< squarecount min)
      (setf min squarecount)
      (setf minpair (list (first i) (first (rest i))))))
  
  (setf minpair (cons min minpair))
  minpair)



;;; queenscount - Takes in two queen positions and a number n for a n x n board.
;;;               Returns the # of squares the two queen positions cover on the n x n board.
(defun queenscount (queenpos1 queenpos2 n)
  
  ;; counter    - A counter to count the times a square intersect with a queens square
  ;; list       - The n x n board
  (setf counter 0)
  (setf list (build-p (build-list n)))
  
  ;;Goes through every square, checks if it intersects with any queen, if so, increment the counter.
  (dolist (i list)
    (when (or (intersect i queenpos1 list)
              (intersect i queenpos2 list))
      (incf counter 1)))
  counter)



;;; build-list - Takes in a number n and returns a list from 1 - n
(defun build-list(n)
  
  ;; list - empty list
  ;; count - counter to keep track of which # to push into list
  (setf list '())
  (setf count 0)
  
  ;; Adds 1 - n to the list.
  (dotimes (number n)
    (incf count 1)
    (setf list (cons count list)))
  (REVERSE list))



;;; build-p - Takes in a list and returns a list of permutation pairs of elements from the list
(defun build-p (list)
  
  ;; Create an empty permutation list
  ;; Go through each number(s) in the list, creates pairs with every other number(s)
  ;; and pushes them to the permutation list.
  (setf p-list '())
  (dolist (i list)
    (dolist (j list)
      (setf pair '())
      (setf pair (cons j pair))
      (setf pair (cons i pair))
      (setf p-list(cons pair p-list))))
  (REVERSE p-list))



;;; build-pndu - Takes in a list and returns a list of permutation pairs of elements from the list
;;;              Excluding pairs holding two of the same elements EX: ((1 1) (1 1))
(defun build-pndu (list)
  
  ;; Create an empty permutation list
  ;; Go through each number(s) in the list, creates pairs with every other number(s)
  ;; and pushes them to the permutation list if the pair doesn't hold two of the same elements.
  (setf p-list '())
  (dolist (i list)
    (dolist (j list)
      (cond
       ((not (equal i j))
        (setf pair '())
        (setf pair (cons j pair))
         (setf pair (cons i pair))
         (setf p-list(cons pair p-list)))
       (t nil))))
  (REVERSE p-list))



;;; intersect - Takes in two square locations and a list (the n x n board)
;;;           - Returns true if the squares intersect each other. That meaning the square
;;;           - is able to get to another square via moving (horizontally, vertically and
;;;           - diagonally.
(defun intersect (sq1 sq2 list)
  
  ;;left
  (setf temp sq1)
  (while (member temp list :test 'equal)
    (cond ((equal temp sq2) (return-from intersect T))
          (T (setf temp (list (first temp) (- (first (rest temp)) 1))))))
  
  ;;right
  (setf temp sq1)
  (while (member temp list :test 'equal)
    (cond ((equal temp sq2) (return-from intersect T))
          (T (setf temp (list (first temp) (+ (first (rest temp)) 1))))))
  
  ;;up
  (setf temp sq1)
  (while (member temp list :test 'equal)
    (cond ((equal temp sq2) (return-from intersect T))
          (T (setf temp (list (- (first temp) 1) (first (rest temp)))))))
  
  ;;down
  (setf temp sq1)
  (while (member temp list :test 'equal)
    (cond ((equal temp sq2) (return-from intersect T))
          (T (setf temp (list (+ (first temp) 1) (first (rest temp)))))))
  
  ;;diagonal top left
  (setf temp sq1)
  (while (member temp list :test 'equal)
    (cond ((equal temp sq2) (return-from intersect T))
          (T (setf temp (list (- (first temp) 1) (- (first (rest temp)) 1))))))
  
  ;;diagonal bottom right
  (setf temp sq1)
  (while (member temp list :test 'equal)
    (cond ((equal temp sq2) (return-from intersect T))
          (T (setf temp (list (+ (first temp) 1) (+ (first (rest temp)) 1))))))
  
  ;;diagonal top right
  (setf temp sq1)
  (while (member temp list :test 'equal)
    (cond ((equal temp sq2) (return-from intersect T))
          (T (setf temp (list (- (first temp) 1) (+ (first (rest temp)) 1))))))
  
  ;;diagonal bottomleft
  (setf temp sq1)
  (while (member temp list :test 'equal)
    (cond ((equal temp sq2) (return-from intersect T))
          (T (setf temp (list (+ (first temp) 1) (- (first (rest temp)) 1)))))))




  
  
