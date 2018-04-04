(defun turing (initial terminal blank rules tape &optional (verbose NIL))
  (labels ((run (state front back)
   (if (equal state terminal)
     (progn
       (when verbose
         (show-tape front back))
       (combine front back))
     (let ((current-content (or (car back) blank)))
       (destructuring-bind 
         (new-state new-content move) 
         (gethash (cons state current-content) rules)
         (when verbose
           (show-tape front back))
         (cond ((eq move 'right)
          (run new-state 
           (cons new-content front) 
           (cdr back)))
         ((eq move 'left)
          (run new-state 
           (cdr front) 
           (list* (car front) new-content (cdr back))))
         (T (run new-state 
           front 
           (cons new-content (cdr back)))))))))

  (show-tape (front back)
    (format T "~{~a~}[~a]~{~a~}~%"
      (nreverse (subseq front 0 (min 10 (length front))))
      (or (car back) blank)
      (subseq (cdr back) 0 (min 10 (length (cdr back))))))

  (combine (front back)
   (if front
     (combine (cdr front) (cons (car front) back))
     back)))

  (run initial '() tape)))

(defun make-rules-table (rules-list)
  (let ((rules (make-hash-table :test 'equal)))
    (loop for (state content new-content dir new-state) in rules-list
      do (setf (gethash (cons state content) rules) 
       (list new-state new-content dir)))
    rules))

(format T "Simple incrementer~%")
(turing 'q0 'qf 'B (make-rules-table '((q0 1 1 right q0) (q0 B 1 stay qf))) '(1 1 1) T)

(format T "Three-state busy beaver~%")
(turing 'a 'halt 0 
  (make-rules-table '((a 0 1 right b)
    (a 1 1 left c)
    (b 0 1 left a)
    (b 1 1 right b)
    (c 0 1 left b)
    (c 1 1 stay halt)))
  '() T)