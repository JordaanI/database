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
;;;; Command loop
;;;

#|
Possible args:
Concept
- create
- update
- remove
- get
Nodes
- list
- system info
Help

|#
(define (command-loop)
  (let* ((args (cdr command))
         (cmd (if (null? args) "" (car args))))
    (cond
     ((equal? cmd "init") (init))
     ((equal? cmd "concept") (concept-command (cdr args)))
     ((equal? cmd "nodes"))
     ((or (equal? cmd "--help") (equal? cmd "-h")))
     ((or (equal? cmd "--version") (equal? cmd "-v")))
     (#t (default-return cmd)))
    (newline)))

;;;
;;;; concept-command
;;;

(define (concept-command args)
  (let ((action (car args)))
    (cond
     ((equal? action "create") (create-concept-command (cdr args)))
     ((equal? action "update") (update-concept-command (cdr args)))
     ((equal? action "remove") (remove-concept-command (cdr args)))
     ((equal? action "get") (get-concept-command (cdr args)))
     (#t (concept-help action)))))

  ;;;
  ;;;; create concept command
  ;;;

(define (create-concept-command args)
  (if (= 1 (length args))
      (let ((possible-concept (with-input-from-string (car args) read-all)))
        (display (string-append "Concept created with id: " (number->string (apply create-concept possible-concept)))))
      (raise "FOR FUTURE DEVELOPMENT")))

  ;;;
  ;;;; Remove concept
  ;;;

(define (remove-concept-command args)
  (if (= 1 (length args))
      (let ((possible-id (string->number (car args))))
        (if (integer? possible-id) (and
                                    (remove-concept possible-id)
                                    (display (string-append "Removed concept with id" (car args))))))
      (raise "FOR FUTURE DEVELOPMENT")))

  ;;;
  ;;;; update concept
  ;;;

(define (update-concept-command args)
  (if (= 1 (length args))
      (let ((possible-concept (with-input-from-string (car args) read-all)))
        (update-concept possible-concept)
        (display (string-append "Updated concept successfully")))
      (raise "FOR FUTURE DEVELOPMENT")))


  ;;;
  ;;;; get concept
  ;;;

(define (get-concept-command args)
  (if (= 1 (length args))
      (let ((possible-id (string->number (car args))))
        (if (integer? possible-id)
            (display (get-concept possible-id))))
      (raise "FOR FUTURE DEVELOPMENT")))

  ;;;
  ;;;; concept help
  ;;;

(define (concept-help action)
  (display "FOR FUTURE DEVELOPMENT"))

;;;
;;;; Default Return
;;;

(define (default-return arg)
  (display (string-append "Unknown command '" arg "'. Use -h/--help for a list of available commands.")))

;;;
;;;; start the thing
;;;

(command-loop)
