(defun turing (initial terminal blank rules tape &optional (verbose NIL))
  (labels ((combine (front back)
   (if front
     (combine (cdr front) (cons (car front) back))
     back))

  (update-tape (old-front old-back new-content move)
   (cond ((eq move 'right)
    (list (cons new-content old-front)
      (cdr old-back)))
   ((eq move 'left)
    (list (cdr old-front) 
      (list* (car old-front) new-content (cdr old-back))))
   (T (list old-front 
    (cons new-content (cdr old-back))))))

  (show-tape (front back)
   (format T "~{~a~}[~a]~{~a~}~%"
     (nreverse (subseq front 0 (min 10 (length front))))
     (or (car back) blank)
     (subseq (cdr back) 0 (min 10 (length (cdr back)))))))

  (loop for back = tape then new-back
    for front = '() then new-front
    for state = initial then new-state
    for content = (or (car back) blank)
    for (new-state new-content move) = (gethash (cons state content) rules)
    for (new-front new-back) = (update-tape front back new-content move)
    until (equal state terminal)
    do (when verbose
     (show-tape front back))
    finally (progn
      (when verbose
        (show-tape front back))
      (return (combine front back))))))

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