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
  (println "You have no brood, run 'brood --init' to get started"))

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

(define (command-loop)
  (let ((args (cdr command)))
    (if (null? args) (simple-response "You witness your brood")
        (let loop ((args args))
          (if (null? args) (args-done)
              (let ((arg (car args)))
                (cond
                 ((arg-equality arg "--create-concept" "-cc") (let ((id (create-concept)))
                                                                (and
                                                                 (cc-message id)
                                                                 (loop (cdr args)))))
                 ((arg-equality arg "--get-concept" "-gc") (let* ((new-args (cdr args))
                                                                  (id (and (not (null? new-args))
                                                                           (let* ((possible-id (car new-args))
                                                                                  (possible-id (string->number possible-id)))
                                                                             (if (integer? possible-id) possible-id #f)))))
                                                             (if id (and (write (get-concept id)) (loop (cdr new-args))) (gc-error arg))))
                 ((arg-equality arg "--init" "--reset") (and
                                                         (init-system)
                                                         (loop (cdr args))))
                 (#t (unknown-arg-error arg)))))))))

(define (init-system)
  (if (file-exists? data-path)
      (and (println "A brood already exists at " brood-home)
           (display "Destroy brood and populate a new one? (Y/n) ")
           (let loop ((ans (read)))
             (cond
              ((equal? ans 'Y)
               (init))
              ((equal? ans 'n)
               (newline)
               (println "You saved the brood"))
              (#t
               (newline)
               (display "Y/n are the only acceptable answers (Y/n) ")
               (loop (read))))))
      (init)))

(command-loop)
