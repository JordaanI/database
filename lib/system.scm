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
;; Date: 2024-04-05
;; email: ivan@axoinvent.com
;; Project: Temp Run folder for testing purposes
;;

(include "../lib/utilities.scm")
(include "../lib/config.scm")
(include "../lib/perm.scm")
(include "../lib/chord.scm")
(include "../lib/version.scm")

;; Initialization
(define (maybe-create-dir dir)
  (if (not (file-exists? dir)) (create-directory dir)))

(define (record key val)
  (string-append
   "("
   key
   " . "
   (with-output-to-string (string) (lambda () (display val)))
   ")"))

(define (init)
  (let ((ring-size (list))
        (home (list))
        (nodes (list)))

    (display "What is the initial ring size 2^m: (default 8)")
    (set! ring-size (let loop ((in (read)))
                      (if (number? in) in
                          (and
                           (newline)
                           (display "Please specify a number: ")
                           (loop (read))))))

    (newline)
    (display "What is the initial amount of nodes: (default 5)")
    (set! nodes (let loop ((in (read)))
                  (if (number? in) in
                      (and
                       (newline)
                       (display "Please specify a number: ")
                       (loop (read))))))

    (newline)
    (display "Select storage location (defaut current-directory): ")
    (set! home (let loop ((ho (read)))
                 (if (string? ho) ho
                     (and
                      (newline)
                      (display "Invalid Location, try again: ")
                      (loop (read))))))

    (with-output-to-file (list path: ".brood"
                               create: #t)
      (lambda ()
        (display (record "m" ring-size))
        (display " ")
        (display (record "home" home))
        (display " ")
        (display (record "initial-node-count" nodes))))

    (maybe-create-dir brood-home)
    (maybe-create-dir data-path)

    (initialize-system)
    (initialize-perm)
    (initialize-version)
    (update-fingers)
    ))

;; Delete brood

(define (delete-brood)
  (if (file-exists? brood-home)
      (and (delete-file-or-directory brood-home #t)
           (delete-file-or-directory ".brood")
           (display "Brood culled successfully"))
      (display "No Brood Found")))
