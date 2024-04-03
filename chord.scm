;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                                        ;;;
;;;     __  .______     ______   .__   __.    .______    __    _______.                    ;;;
;;;    |  | |   _  \   /  __  \  |  \ |  |    |   _  \  |  |  /  _____|        _____       ;;;   
;;;    |  | |  |_)  | |  |  |  | |   \|  |    |  |_)  | |  | |  |  __      ^..^     \9     ;;;
;;;    |  | |      /  |  |  |  | |  . `  |    |   ___/  |  | |  | |_ |     (oo)_____/      ;;;
;;;    |  | |  |\  \  |  `--'  | |  |\   |    |  |      |  | |  |__| |        WW  WW       ;;;
;;;    |__| | _| `._|  \______/  |__| \__|    | _|      |__|  \______|                     ;;;
;;;                                                                                        ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;; Author: Ivan Jordaan
;; Date: 2024-04-01
;; email: ivan@axoinvent.com
;; Project: Chord File for BroodB 
;;

;;;
;;;; Ring Size
;;;

(define m 20)
(define ring-size (expt 2 m))

(define initial-node-count m)

;;;
;;;; initialize nodes
;;;

(define (initialize-system)
  (reset-clean-dir "nodes")
  (let ((e (ceiling (/ ring-size (+ 1 initial-node-count)))) (base '()) (fingers '()))
    (let loop ((n e))
      (if (>= n ring-size)
	   (with-output-to-file "nodes/0"
	     (lambda ()			       
	       (display (cons (reverse base) (list 0)))
	       (newline)
	       (display fingers)))
	  (and
	   (with-output-to-file
	       (list path: (string-append "nodes/" (number->string n))
		     create: #t)
	     (lambda ()
	       (display (list "version" 0 "size" e "load" 0))
	       (newline)))
	   (set! fingers (cons (list n 1 (list (list 1 (if (>= (+ n e) ring-size) e (+ n e))))) fingers))
	   (set! base (cons n base))
	   (cons n (loop (+ n e))))))))

;;;
;;;; Find Successor
;;;

(define (find-successor id)
  (let* ((system-info (with-input-from-file "nodes/0" read-all))
	 (version-info (car system-info))
	 (node-info (car version-info))
	 (version-number (car (reverse version-info)))
	 (finger-info (cadr system-info))
	 (id (modulo id ring-size)))
    (let loop ((l 0) (nodes node-info))
      (cond
       ((null? nodes) (car node-info))
       ((between id l (car nodes)) (car nodes))
       (#t (loop (car nodes) (cdr nodes)))))))

;;;
;;;; Fix Finger
;;;

(define (fix-finger node)
  (let* ((system-info (with-input-from-file "nodes/0" read-all))
	 (version-info (car system-info)))
    (with-output-to-file "nodes/t0"
      (lambda ()
	(display version-info)
	(newline)
	(display
	 (let loop ((fingers (cadr system-info)))
	   (cond
	    ((null? fingers) '())
	    ((equal? node (car (car fingers)))
	     (let* ((entry (car fingers))
		    (next (+ 1 (cadr entry)))
		    (next (if (> next m) 1 next))
		    (finger-table (list->table (list-ref entry 2))))
		   (table-set! finger-table next (list (find-successor (+ (car entry) (expt 2 (- next 1))))))
	       (cons (list (car entry) next (table->list finger-table)) (loop (cdr fingers)))))
	    (#t (cons (car fingers) (loop (cdr fingers)))))))))
    (rename-file "nodes/t0" "nodes/0" #t)))

;;;
;;;; update Version (TODO OPTIMIZE)
;;;

(define (update-version)
  (let* ((system-info (with-input-from-file "nodes/0" read-all))
	 (version-info (car system-info))
	 (node-info (car version-info))
	 (version-number (car (reverse version-info)))
	 (finger-info (cadr system-info)))
    (println "Updating to version " (+ 1 version-number))
    (let loop ((i m))
      (if (zero? i) (println "Updated")
	  (and (let inner-loop ((node-info node-info))
		 (if (not (null? node-info))
		    (and 
		     (fix-finger (car node-info))
		     (inner-loop (cdr node-info)))))
	       (loop (- i 1)))))))

;;;
;;;; Temp Utility (might be moved to permament utilities files ion the future)
;;;

(define (reset-clean-dir p)
  (if (member p (directory-files)) (let ()
				     (delete-file-or-directory p #t)
				     (display "Destroying old ")
				     (display p)
				     (newline)))
  (create-directory p)
  (display "Creating new ")
  (display p)
  (newline))

(define (between n l u #!key (include? #t))
  (and (> n l) (if include? (<= n u) (< n u))))

;;;
;;;; tests
;;;

;;(step-level-set! 5)
;;(break fix-finger)
(initialize-system)
(update-version)
