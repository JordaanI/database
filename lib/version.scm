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
  (close-port (open-file (list
			  path: (version-path version-number)
			  create: #t)))
  (set! version-number 0))

;;;
;;;; Utilities
;;;

(define (node-info-contain? id node-info)
  (member id (cdr (assoc 'entries (car node-info)))))

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
	(if (not (node-info-contain? id node-info))
	 (and
	   (add-entry-to-perm id entry)
	   (add-entry-to-node node node-info id entry)
	   (set! version-load (/ (+ version-load 1) ring-size))
	   id)
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
    (with-output-to-file (list
			  path: (version-path version-number)
			  append: #t)
      (lambda ()
	(display entry)
	(newline)))
    (with-output-to-file tmp-path-local
	  (lambda ()
	    (let ((t (list->table (car node-info))))
	      (table-set! t 'entries (cons id (table-ref t 'entries)))
	      (table-set! t 'load (list (/ (length (table-ref t 'entries)) (car (table-ref t 'size)))))
	      (display (table->list t))
	      (newline)
	      (display (cons (cons id entry) (cadr node-info))))))
	(rename-file tmp-path-local node-path-local #t)))

;;;
;;;; Get Entry
;;;

(define (get-concept id)
  (let* ((node (find-successor 0 id))
	 (node-path-local (node-path node))
	 (node-info (with-input-from-file node-path-local read)))
    (if (node-info-contain? id (list node-info))
	(cdr (assoc id (cadr (with-input-from-file node-path-local read-all))))
	(raise 'record-does-not-exist))))
