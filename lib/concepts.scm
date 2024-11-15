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
;; Date: 2024-11-14
;; email: ivan@axoinvent.com
;; Project:
;;

;; Properties Strucure

(define concept_ID 0)
(define concept_name 1)
(define concept_properties 2)

;; Encoding a concept

;; Concepts cannot be modified only be created

(define (create-concept name . properties)
  (vector (hash name) name properties))

(define (concept-id concept)
  (vector-ref concept concept_ID))

(define (concept-name concept)
  (vector-ref concept concept_name))

(define (concept-properties concept)
  (vector-ref concept concept_properties))

(define (hash name)
  (let* ((s (strip-spaces-to-char name))
         (l (map char->integer s))
         (ll (length l)))
    (modulo (* (string-length name) (apply + (map (lambda (c) (+ ll c)) l))) (string->number (getenv SIZE)))))


(define test-concepts
  (map create-concept (directory-files "~/projects/pigout/ChefTapExport")))
