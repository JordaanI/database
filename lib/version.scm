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
;; Project: Versioning File of BroodB 
;;

;;
;;; Initialize version
;;

(define version-number 0)
(define version-load 0)

(define (initialize-version)
  (reset-clean-dir "version")
  (set! version-number 0))

;;;
;;;; Types
;;;

;; Concept

(define (create-concept #!key label (description '()) (sub-concepts '()) (super-concepts '()) (instances '()) (properties '()))
  (if (and-map list? (list description sub-concepts super-concepts instances properties))
      (let* ((id (gen-id))
	     (entry `((label (,label))
		      (description ,@description)
		      (sub-concepts ,@sub-concepts)
		      (super-concepts ,@super-concepts)
		      (properties ,@properties)
		      (instances ,@instances)))
	     (node (find-successor 0 id))
	     (node-info (with-input-from-file (node-path node) read-all)))
	(if (not (member id (cdr (assoc 'entries (car node-info)))))
	 (begin
	   (add-entry-to-perm id entry)
	   (add-entry-to-node node node-info id entry)
	   (set! version-load (/ (+ version-load 1) ring-size)))
	 (create-concept
	  label: label
	  description: description
	  sub-concepts: sub-concepts
	  super-concepts: super-concepts
	  instances: instances
	  properties: properties)))))


;; add-entry-to-node

(define (add-entry-to-node node node-info id entry)
  (let ((node-path-local (node-path node))
	(tmp-path-local (tmp-path node)))
    (with-output-to-file tmp-path-local
	  (lambda ()
	    (let ((t (list->table (car node-info))))
	      (table-set! t 'entries (cons id (table-ref t 'entries)))
	      (table-set! t 'load (list (/ (length (table-ref t 'entries)) (car (table-ref t 'size)))))
	      (display (table->list t))
	      (newline)
	      (display (cons entry (cadr node-info))))))
	(rename-file tmp-path-local node-path-local #t)))
