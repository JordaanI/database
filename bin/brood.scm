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

(include "../lib/system.scm")

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
         (cmd (maybe-read-args args)))
    (cond
     ((equal? cmd "init") (init-command))
     ((equal? cmd "delete") (delete-brood-command))
     ((equal? cmd "concept") (concept-command (cdr args)))
     ((equal? cmd "nodes") (nodes-command (cdr args)))
     ((or (equal? cmd "--help") (equal? cmd "-h")) (brood-help))
     ((or (equal? cmd "--version") (equal? cmd "-v")))
     (#t (default-return cmd)))
    (newline)))


(define (maybe-read-args args)
  (if (null? args) "" (car args)))

;;;
;;;; concept-command
;;;

(define (concept-command args)
  (let ((action (maybe-read-args args)))
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
;;;; Node Command
;;;

(define (nodes-command args)
  (let ((action (maybe-read-args args)))
    (cond
     ((equal? action "list") (list-nodes-command))
     (#t (nodes-help action)))))

  ;;;
  ;;;; List Node
  ;;;

(define (list-nodes-command)
  (let* ((host-info (with-input-from-file host-path read-all))
         (active-nodes (car (car host-info))))
    (for-each (lambda (node)
                (display node)
                (newline))
              active-nodes)
    (newline)
    (display (string-append (number->string (length active-nodes)) " active node(s)"))))

  ;;;
  ;;;; Node Help
  ;;;

(define (nodes-help action)
  (display "FOR FUTURE DEVELOPMENT"))

;;;
;;;; Default Return
;;;

(define (default-return arg)
  (display (string-append "Unknown command '" arg "'. Use -h/--help for a list of available commands.")))

;;;
;;;; Brood Help
;;;

(define (brood-help)
  (display (string-append
            "This is Broodb system\n\n"
            "Commands have the format 'brood command [args]'\n"
            "Args default to 'help' if not specified\n\n"
            "Commands are:\n"
            "\t- concept\n"
            "\t- nodes\n"
            "\t- init\n"
            "\t- delete")))

;;;
;;;; INIT Command
;;;

(define (init-command)
  (if (file-exists? brood-home)
      (display "The Brood is already alive")
      (and
       (display "Generating a new Brood\n")
       (init))))

;;;
;;;; Delete Brood
;;;

(define (delete-brood-command)
  (display "This will permanently cull your Brood\nThis cannot be undone.\nProceed (Y/n) ")
  (let loop ((answer (symbol->string (read))))
    (cond
     ((or (equal? answer "y") (equal? answer "Y")) (delete-brood))
     ((equal? answer "n") (display "The Brood was spared"))
     (#t (and (display "(Y/n) ") (loop (symbol->string (read))))))))

;;;
;;;; start the thing
;;;

(command-loop)
