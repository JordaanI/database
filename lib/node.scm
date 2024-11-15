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
;; Project: Nodes
;;

;; Node structure


(define (create-node id succ)
  (let ((fingers (make-vector (+ 1 SIZE_PARAM_NUM) succ)))
    (vector-set! fingers 0 1)
    (vector id fingers 0)))

(define _id 0)
(define _fingers 1)
(define _load 2)

;; Node Props

(define (node-id node)
  (vector-ref node _id))

(define (node-fingers node)
  (vector-ref node _fingers))

(define (node-load node)
  (vector-ref node _load))

(define (node-load-set! node load)
  (vector-set! node _load load))

(define (node-finger-index node)
  (vector-ref (node-fingers node) 0))

(define (node-finger node finger)
  (vector-ref (node-fingers node) finger))

(define (node-finger-index-set! node value)
  (vector-set! (node-fingers node) 0 value))

(define (node-finger-set! node finger value)
  (vector-set! (node-fingers node) finger value))

;; Adding to node

(define (add-to-node concept)
  (let* ((id (concept-id concept))
         (ID (generate-ID))
         (node (%find-successor (pick-random-node) id))
         (node-details (look-up-node node)))
    (add-to-perm (cons ID concept))
    (node-load-set! node-details (+ 1 (node-load node-details)))
    (write-out (register node) (cons (concept-name concept) ID) append: #t)
    (write-out (concept-path node ID) concept)))

(define (get-concept name)
  (let* ((n (%find-successor (pick-random-node) (hash name)))
         (reg (read-in (register n)))
         (concept (assoc name reg)))
    (if concept (read-in (concept-path n (cdr concept))) #f)))

(define (remove-concept concept)
  (let* ((n (%find-successor (pick-random-node) (concept-id concept)))
         (node-details (look-up-node n))
         (reg-path (register n))
         (reg (read-in reg-path)))
    (node-load-set! node-details (- (node-load node-details) 1))
    (write-out reg-path (let loop ((reg reg))
                          (if (equal? (concept-name concept) (car (car reg))) (cdr reg)
                              (loop (cdr reg)))))))

(define (remove-concept-by-name name)
  (let ((concept (get-concept name)))
    (if concept (remove-concept concept) #f)))

(define (update-properties o n)
  (let loop ((n n))
    (if (null? n) o
        (replace!
         (loop (cdr n))
         (assoc (car (car n)) o)
         (car n)))))

(define (update-concept concept . properties)
  (let* ((old-properties (concept-properties concept))
         (new-concept (apply create-concept (cons (concept-name concept) (update-properties old-properties properties)))))
    (remove-concept concept)
    (add-to-node new-concept)))

(define (update-concept-by-name name . properties)
  (apply update-concept (cons (get-concept name) properties)))
