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
      (display
       `((active) (removed))))))

;;;
;;;; Version specific Utilities
;;;

(define (get-property-values property concept)
  (cdr (assoc property concept)))

(define (get-first-value property l)
  (car (get-property-values property l)))

(define (get-active version)
  (get-property-values 'active version))

(define (get-removed version)
  (get-property-values 'removed version))

(define (get-id concept)
  (get-first-value 'id concept))

(define (get-entries node)
  (get-property-values 'entries node))

(define (get-node-load node)
  (get-first-value 'load node))

(define (get-key concept)
  (get-first-value 'key concept))

(define (archive node)
  (shell-command (string-append "zip " (zip-path node) " " (node-path node))))

;;;
;;;; Create concept
;;;

(define (create-concept-template id key properties)
  `((id ,id) (key ,key) ,@properties))

;;;
;;;; Add to version
;;;

(define (add-to-version id key)
  (let* ((version-info (with-input-from-file (version-path version-number) read))
         (active (get-active version-info)))
    (with-output-to-file (list
                          path: (tmp-version version-number)
                          create: #t)
      (lambda ()
        (display (update-version-info
                  version-info
                  active: (cons id active)))))
    (rename-file (tmp-version version-number) (version-path version-number))))

(define (update-version-info version-info #!key (removed #f) (active #f))
  `(,(if removed `(removed ,@removed) (assoc 'removed version-info))
    ,(if active `(active ,@active) (assoc 'active version-info))))

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
        (display (update-node-info
                  node-info
                  load: (+ 1 (get-first-value 'load node-info))
                  entries: (cons id (get-entries node-info))))
        (display concept)
        (for-each display node-entries)))
    (rename-file (tmp-path successor) (node-path successor))))

(define (update-node-info node-info #!key (version #f) (size #f) (load #f) (entries #f))
  `(,(if version `(version ,version) (assoc 'version node-info))
    ,(if size `(size ,size) (assoc 'size node-info))
    ,(if load `(load ,load) (assoc 'load node-info))
    ,(if entries `(entries ,@entries) (assoc 'entries node-info))))

;;;
;;;; Maybe add concept to version
;;;

(define (add-concept-to-version id key concept)
  (if (validate-concept concept)
      (let ((successor (find-successor 0 id)))
        (add-to-perm concept)
        (add-to-version id key)
        (add-to-node id concept successor)
        id)
      (raise "Concept is not valid")))

;;;
;;;; Validate Concept
;;;

(define (validate-concept concept)
  (and (and-map list? concept) (assoc 'key concept)))

;;;
;;;; Create Concept
;;;

(define (concat-integer l)
  (string->number (apply string-append l)))

(define (shift-char sl)
  (lambda (c)
    (let* ((i (char->integer c))
           (si (if (< sl 4) (+ i sl)
                   (- i 1))))
      (number->string si))))

(define (hash key)
  (let* ((sl (string-length key))
         (hv (modulo (+ sl (* (expt sl 2) (concat-integer (map (shift-char sl) (string->list key))))) ring-size))
         (concept (%get-concept hv)))
    (if (and concept (equal? key (get-key concept))) (probe hv key 1) hv)))

(define (probe hv key p)
  (let* ((nhv (modulo (+ hv (* (string-length key) p)) ring-size))
         (concept (%get-concept nhv)))
    (if (and concept (equal? key (get-key concept))) (probe hv key (+ p 1)) nhv)))

(define (create-concept key . properties)
  (let* ((id (hash key)))
    (add-concept-to-version id key (create-concept-template id key properties))))

;;;
;;;; Get Concept
;;;

(define (get-concept key)
  (let* ((id (hash key)))
    (%get-concept id)))

(define (%get-concept id)
  (let* ((successor (find-successor 0 id))
         (node (open-file (node-path successor))))
    (if (member id (get-property-values 'entries (read node)))
        (find-concept id node)
        (and
         (close-port node)
         #f))))

;;;
;;;; Find concept
;;;

(define (find-concept id node)
  (let ((entry (read node)))
    (cond
     ((eof-object? entry) '())
     ((equal? (get-id entry) id) (and (close-port node) entry))
     (#t (find-concept ID node)))))

;;;
;;;; Update Concept (checks need to be added to updated concepts)
;;;

(define (update-concept key updated-concept)
  (let ((id (hash key)))
    (remove-concept key)
    (add-concept-to-version id key updated-concept)))

;;;
;;;; Remove Concept (DISPLAY INEFFICIENCY)
;;;

(define (remove-concept key)
  (let* ((id (hash key))
         (successor (find-successor 0 id))
         (successor-path (node-path successor))
         (node (open-file successor-path))
         (node-info (read node))
         (version-path (version-path version-number))
         (version-info (with-input-from-file version-path read)))
    (if (member id (get-entries node-info))
        (and
         (archive successor)
         (with-output-to-file (list path: (tmp-path successor) create: #t)
           (lambda ()
             (display (update-node-info
                       node-info
                       load: (- (get-node-load node-info) 1)
                       entries: (remove-first-value-from-list id (get-entries node-info))))
             (let loop ((concept (read node)))
               (cond
                ((eof-object? concept)
                 (close-port node)
                 #f)
                ((equal? id (get-id concept))
                 (for-each display (read-all node))
                 (close-port node))
                (#t
                 (display concept)
                 (loop (read node)))))))
         (rename-file (tmp-path successor) successor-path)
         (with-output-to-file (list path: (tmp-version version-number) create: #t)
           (lambda ()
             (display (update-version-info
                       version-info
                       active: (remove-first-value-from-list id (get-active version-info))
                       removed: `(,id ,@(get-removed version-info))))))
         (rename-file (tmp-version version-number) version-path)
         (display (string-append "Entry " key " removed")))
        (and
         (display (string-append "Entry " key " does not exist"))
         (close-port node)))))
