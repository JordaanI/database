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
  (with-output-to-file
      (list
       path: (version-path version-number)
       create: #t)
    (lambda ()
      (display `(active))
      (newline)
      (display `(remove)))))

;;;
;;;; Version Utilities
;;;

(define (get-property-values property concept)
  (cdr (assoc property concept)))

(define (get-first-value property l)
  (car (get-property-values property l)))

(define (get-active version)
  (get-property-values 'active version))

(define (get-remove version)
  (get-property-values 'remove version))

;;;
;;;; Create concept
;;;

(define (create-concept-template id properties)
  `((id ,id) ,@properties))

(define (get-id concept)
  (get-first-value 'id concept))
;;;
;;;; Concept id taken?
;;;

(define (id-available id)
  (let* ((successor (find-successor 0 id))
         (node-info (with-input-from-file (node-path successor) read)))
    (if (not (member id (cdr (assoc 'entries node-info)))) successor #f)))

;;;
;;;; Add to version
;;;

(define (add-to-version id)
  (let* ((version (with-input-from-file (version-path version-number) read-all))
         (active (get-active version))
         (remove (get-remove version)))
    (with-output-to-file (list
                          path: (tmp-version version-number)
                          create: #t)
      (lambda ()
        (display `(active ,id ,@active))
        (newline)
        (display `(remove ,@remove))))
    (rename-file (tmp-version version-number) (version-path version-number))))

;;;
;;;; Add to node
;;;

(define (add-to-node id concept successor)
  (let* ((node (with-input-from-file (node-path successor) read-all))
         (node-info (car node))
         (node-entries (cdr node)))
    (with-output-to-file (list
                          path: (tmp-path successor)
                          create: #t)
      (lambda ()
        (display `(,(assoc 'version node-info)
                   ,(assoc 'size node-info)
                   (load ,(+ 1 (get-first-value 'load node-info)))
                   (entries ,id ,@(get-property-values 'entries node-info))))
        (display (list id concept))
        (for-each display node-entries)))
    (rename-file (tmp-path successor) (node-path successor))))

;;;
;;;; Maybe add concept to version
;;;

(define (add-concept-to-version id concept)
  (let ((successor (find-successor 0 id)))
    (add-to-perm concept)
    (add-to-version id)
    (add-to-node id concept successor)
    id))

;;;
;;;; Next available id (could loop infinitely, needs to be restricted when all id's are taken)
;;;

(define (next-available-id)
  (let ((id (gen-id)))
    (if (id-available id) id (next-available-id))))

;;;
;;;; Create Concept
;;;

(define (create-concept . properties)
  (let ((id (next-available-id)))
    (add-concept-to-version id (create-concept-template id properties))))

;;;
;;;; Get Concept
;;;

(define (get-concept id)
  (let* ((successor (find-successor 0 id))
         (node (open-file (node-path successor))))
    (if (member id (get-property-values 'entries (read node)))
        (find-concept id node)
        (and
         (close-port node)
         (raise "Entry does not exist")))))

;;;
;;;; Find concept
;;;

(define (find-concept id node)
  (let ((entry (read node)))
    (cond
     ((eof-object? entry) (raise "Entry does not exist"))
     ((equal? (car entry) id) (and (close-port node) (cadr entry)))
     (#t (find-concept id node)))))

;;;
;;;; Update Concept
;;;

(define (replace-concept concept)
  (let* ((id (get-first-value 'id concept))
         (successor (find-successor 0 id))
         (node (with-input-from-file (node-path successor) read-all))
         (successor-path (node-path successor)))
    (add-to-perm concept)
    (shell-command (string-append "zip " (zip-path successor) " " successor-path))
    (with-output-to-file (list
                          path: (tmp-path successor)
                          create: #t)
      (lambda ()
        (display (car node))
        (let loop ((concepts (cdr node)))
          (if
           (not (null? concepts))
           (let* ((old-concept (car concepts))
                  (old-id (car old-concept)))
             (if (equal? id old-id) (display (list id concept))
                 (display old-concept))
             (loop (cdr concepts)))))))
    (rename-file (tmp-path successor) successor-path)
    (display (string-append (number->string id) " successfully updated"))))
