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
;;;; initialize nodes
;;;

(define (initialize-system)
  (reset-clean-dir "nodes")
  (let ((e (floor (/ ring-size (+ 1 initial-node-count)))) (base '()) (fingers '()))
    (let loop ((n e))
      (if (>= n ring-size)
	  (with-output-to-file host-path
	    (lambda ()
	      (display (cons (reverse base) (list 0)))
	      (newline)
	      (display (cons (list 0 1 (list (list 1 e))) fingers))))
	  (and
	   (with-output-to-file
	       (list path: (node-path n)
		     create: #t)
	     (lambda ()
	       (display (list (list 'version version-number) (list 'size e) (list 'load 0) (list 'entries)))
	       (newline)))
	   (set! fingers (cons (list n 1 (list (list 1 (if (>= (+ n e) ring-size) e (+ n e))))) fingers))
	   (set! base (cons n base))
	   (cons n (loop (+ n e))))))))

;;;
;;;; Tablize
;;;

(define (tablize fi)
  (list->table (let loop ((fi fi))
		 (if (null? fi) '()
		     (cons (cons (car (car fi)) (list->table (caddr (car fi)))) (loop (cdr fi)))))))

;;;
;;;; Find Successor
;;;

(define (find-successor node id)
  (let* ((system-info (with-input-from-file host-path read-all))
	 (version-info (car system-info))
	 (node-info (cons 0 (append (car version-info) (list (car (car version-info))))))
	 (version-number (car (reverse version-info)))
	 (finger-info (cadr system-info))
	 (finger-tables (tablize finger-info))
	 (id (+ 1 (modulo id (- ring-size 1)))))
    (let loop ((n node))
      (let ((finger-table (table-ref finger-tables n))
	    (succ (cadr (member n node-info))))
	(if (between id n succ) succ
	    (let ((cn (closest-proceeding-node n finger-table id)))
	      (loop cn)))))))

;;;
;;;; Closest Proceeding Node
;;;

(define (closest-proceeding-node n finger-table id)
  (let loop ((i m))
    (let ((val (table-ref finger-table i #f)))
      (cond
       ((= i 1) (car val))
       ((and val (between (car val) n id include?: #f)) (car val))
       (#t (loop (- i 1)))))))

;;;
;;;; Fix Finger
;;;

(define (fix-finger node)
  (let* ((system-info (with-input-from-file host-path read-all))
	 (version-info (car system-info))
	 (node-info (car version-info))
	 (finger-table
	  (let loop ((fingers (cadr system-info)))
	    (cond
	     ((null? fingers) '())
	     ((equal? node (car (car fingers)))
	      (let* ((entry (car fingers))
		     (next (+ 1 (cadr entry)))
		     (next (if (> next m) 1 next))
		     (finger-table (list->table (list-ref entry 2)))
		     (id (+ (car entry) (expt 2 (- next 1)))))
		(table-set! finger-table next (list (find-successor 0 id)))
		(cons (list (car entry) next (table->list finger-table)) (loop (cdr fingers)))))
	     (#t (cons (car fingers) (loop (cdr fingers))))))))
    (with-output-to-file (tmp-path 0)
      (lambda ()
	(display version-info)
	(newline)
	(display finger-table)))
    (rename-file (tmp-path 0) host-path #t)))

;;;
;;;; Sort node info
;;;

(define (maybe-sort-node-info)
  (let* ((version-info (with-input-from-file host-path read))
	 (node-info (car version-info)))
    (if (not (apply < node-info))
	(let ((system-info (with-input-from-file host-path read-all)))
	  (with-output-to-file (tmp-path 0)
	    (lambda ()
	      (display
	       (cons (sort node-info) (cdr version-info)))
	      (newline)
	      (display (cadr system-info))))
	  (rename-file (tmp-path 0) host-path #t)))))

;;;
;;;; Update Fingers
;;;

(define (update-fingers)
  (let* ((system-info (with-input-from-file host-path read-all))
	 (version-info (car system-info))
	 (node-info (car version-info))
	 (version-number (car (reverse version-info)))
	 (finger-info (cadr system-info)))
    (and
     (set! version-number (+ 1 version-number))
     (display (string-append "Updating to version " (number->string version-number) "\n")))
    (let loop ((i m))
      (if (zero? i)
	  (let loop ((i m))
	    (if (zero? i)
                (display "Updated")
		(and
		 (fix-finger 0)
		 (loop (- i 1)))))
	  (and (let inner-loop ((node-info node-info))
		 (if (not (null? node-info))
		     (and
		      (fix-finger (car node-info))
		      (inner-loop (cdr node-info)))))
	       (loop (- i 1)))))))

;;;
;;;; Perm Specific Utilities
;;;


(define (between n l u #!key (include? #t))
  (if (> l u) (between n l (+ u ring-size) include?: include?)
      (and (> n l) (if include? (<= n u) (< n u)))))
