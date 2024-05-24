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
;; Date: 2024-05-13
;; email: ivan@axoinvent.com
;; Project: executable for broodb
;;

(include "../lib/init.scm")

;;;
;;;; Listen for command
;;;

(define command (command-line))
;;;
;;;;
;;;

(define (arg-equality arg one two)
  (or (equal? arg one) (equal? arg two)))

(define (you-have-no-brood)
  (println "You have no brood, run 'brood --configure --init' to get started"))

(define (simple-response response #!key (fnq println))
  (if (file-exists? data-path)
      (fnq response)
      (you-have-no-brood)))

(define (args-done)
  (simple-response ""))

(define (gc-error arg)
  (simple-response (string-append arg " -> requires an integer value")))

(define (cc-message id)
  (simple-response (string-append "Empty concept with id: '" (number->string id) "' created")))

(define (unknown-arg-error arg)
  (simple-response (string-append arg " -> is not supported") fnq: display))

(define (version-answer)
  (display (string-append "Broodb ver: " brood-version)))

(define (dont-destroy)
  (newline)
  (println "The brood will be saved"))

(define (command-loop)
  (let ((args (cdr command)))
    (if (null? args) (simple-response "You witness your brood")
        (let loop ((args args))
          (if (null? args) (args-done)
              (let ((arg (car args)))
                (cond
                 ((arg-equality arg "--create-concept" "-cc") (command-create-concept (cdr args) loop))
                 ((arg-equality arg "--get-concept" "-gc") (command-get-concept (cdr args) loop))
                 ((arg-equality arg "--init" "--reset") (and (init-system) (loop (cdr args))))
                 ((arg-equality arg "--version" "-v") (and (version-answer) (loop (cdr args))))
                 ((arg-equality arg "--help" "-h") (display-help))
                 ((arg-equality arg "--replace-concept" "-rc") (command-replace-concept (cdr args) loop))
                 ((arg-equality arg "--list" "-l") (command-list-perm (cdr args) loop))
                 (#t (unknown-arg-error arg)))))))))

(define (check-input args)
  (if (and (not (null? args)) (equal? 'input (car args))) (list (read)) args))

(define (command-get-concept args loop)
  (let* ((id (and (not (null? args))
                  (let* ((possible-id (car args))
                         (possible-id (string->number possible-id)))
                    (if (integer? possible-id) possible-id #f)))))
    (if id (and (write (get-concept id)) (loop (cdr args))) (gc-error possible-id))))

(define (command-create-concept args loop)
  (let ((id (create-concept)))
    (and
     (cc-message id)
     (loop args))))

(define (command-replace-concept args loop)
  (if (not (null? args))
      (let ((concept (string->concept (car args))))
        (if concept (and (replace-concept concept) (loop (cdr args))) (display "Incorrect syntax for replacement concept")))
      (display "A concept is required to replace another")))

(define (string->concept s)
  (let ((possible-concept (read (open-input-string s))))
    (if (and (list-of-lists? possible-concept) (pair? (car possible-concept)) (equal? (car (car possible-concept)) 'id)) possible-concept
        #f)))

(define (list-of-lists? l)
  (and-map list? l))

(define (command-list-perm args loop)
  (let ((perm (with-input-from-file perm-path read-all)))
    (for-each (lambda (concept)
                (pp concept)
                (newline)) perm)
    (loop args)))

(define (yes-no-loop yes no #!key (yes-args '()) (no-args '()))
  (let ((ans (read)))
    (cond
     ((equal? ans 'Y)
      (apply yes (check-input yes-args)))
     ((equal? ans 'n)
      (apply no (check-input no-args)))
     (#t
      (newline)
      (display "Y/n are the only acceptable answers (Y/n) ")
      (yes-no-loop yes no yes-args: yes-args no-args: no-args)))))

(define (init-system)
  (if (file-exists? data-path)
      (and (println "A brood already exists at " brood-home)
           (display "Destroy brood and populate a new one? (Y/n) ")
           (yes-no-loop init dont-destroy))
      (init)))

(define (display-help)
  (println "Implement help display"))

#|

TODO: SYSTEM CONFIG

(define (configure-system args)
(if (file-exists? data-path)
(display "A brood already exists, aborting...")
(let loop ((args args))
(if (null? args) (display (string-append "Brood configured for " (number->string initial-node-count) " nodes with a total of " (number->string (- (expt 2 m) 1)) " entries"))
(let ((arg (car args)))
(cond
((equal? arg "--ring-size")
(let ((int (if (null? (cdr args)) #f (string->number (cadr args)))))
(if (or (not (integer? int)) (> int initial-node-count)) (display (string-append "--ring-size requires an int input greater than " (number->string initial-node-count)))
(and (ring-size-set! int)
(loop (cddr args))))))
((equal? arg "--node-count")
(let ((int (if (null? (cdr args)) #f (string->number (cadr args))))
(upper-limit (- (expt 2 m) 1)))
(if (or (not (integer? int)) (> int upper-limit)) (display (string-append "--node-count requires an int input less than" (number->string upper-limit)))
(and (node-count-set! int)
(loop (cddr args))))))
(#t (println arg " is not a config option, use --help to see available options")
(loop (cdr args)))))))))
|#

(define (ring-size-set! m)
  (set! m m))

(define (node-count-set! i)
  (set! initial-node-count i))

(command-loop)
