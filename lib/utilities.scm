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
;; Date: 2024-06-24
;; email: ivan@axoinvent.com
;; Project: Utilities file for a standalone brood implementation
;;

                                        ; Global utilities
;; lists

;;; And map

(define (and-map p l)
  (if (null? l) #t
      (and (p (car l)) (and-map p (cdr l)))))

;;; Remove value from list

(define (remove-first-value-from-list v l)
  (cond
   ((null? l) '())
   ((equal? v (car l)) (cdr l))
   (#t (car l) (remove-first-value-from-list (cdr l)))))

;; Directory

;;; Clean

(define (reset-clean-dir p)
  (let ((abs-path (string-append data-path "/" p)))
    (if (member p (directory-files data-path))
	(let ()
	  (delete-file-or-directory abs-path #t)
	  (display "Destroying old ")
	  (display p)
	  (newline)))
    (create-directory abs-path)
    (display "Creating new ")
    (display p)
    (newline)))
