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
;; Date: 2024-03-26
;; email: ivan@axoinvent.com
;; Project: Database file to serve as append-only ledger 
;;

;;;
;;;; perm-path
;;;

(define perm-path "perm.txt")

;;;
;;;; Generate internal ID
;;;

(define (gi)
  (string-append "iID-" (number->string (random-integer 9999999999999999)) "-"  (number->string (current-jiffy))))

;;;
;;;; Create Entry
;;;

(define (create-entry . a)
  (cons (cons "iID" (gi)) a))

;;;
;;;; Add Entry To Perm
;;;

(define (add-entry-to-perm e)
  (with-output-to-file (list
			path: perm-path
			append: #t)
    (lambda ()
      (display e))))

;;;
;;;; Find in Perm
;;;

(define (find-in-perm iID)
  (let ((p (open-file (list path: perm-path char-encoding: UTF-8))))
    (let loop ((pe (read p)))
      (if
       (eof-object? pe) (make-table)
	 (if (equal? iID (table-ref "iID" pe)) pe
	     (loop (read p)))))))

;;;
;;;; Tests
;;;

(add-entry-to-perm (create-entry (cons "1"  1) (cons "2" (list 1 2 3))))
