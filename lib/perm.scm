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

(define brood-version "alpha")

;;;
;;;; Generate internal ID
;;;

(define (gen-id)
  (+ 1 (random-integer (- ring-size 1))))

;;;
;;;; Add Entry To Perm
;;;

(define (add-to-perm l)
  (with-output-to-file (list
			path: perm-path
			append: #t)
    (lambda ()
      (display l))))

;;;
;;;; Initialize-perm
;;;

(define (initialize-perm)
  (reset-clean-dir "perm")
  (create-directory archive-path)
  (close-port (open-file (list
			  path: perm-path
			  create: #t))))
