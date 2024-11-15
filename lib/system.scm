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

(define %node-info (list))

(define (initialize-system)
  (if (not (file-exists? node-info))
      (let* ((size (string->number (getenv SIZE)))
             (step (floor (/ size (string->number (getenv NODES))))))
        (let loop ((n step))
          (if (< n size) (let* ((next (+ n step))
                                (succ (if (> next size) step next)))
                           (create-directory (node n))
                           (write-out node-info (cons n (create-node n succ)) append: #t)
                           (loop next))))))
  (if (not (file-exists? perm-file))
      (write-out perm-file (list))))

(define (look-up-node n)
  (let ((found (assoc n %node-info)))
    (if found (cdr found)
        (error "Node not found: " n))))

(define (generate-ID)
  (string-append (number->string (current-jiffy)) "-" (number->string (random-integer 100000000000))))

(define (pick-random-node)
  (cdr (pick-random %node-info)))

(define sustain-mutex (make-mutex))

(define (sustain-system)
  (set! %node-info (read-in node-info))
  ;;  (fix-all-fingers)
  (thread (lambda ()
            (let loop ()
              (mutex-lock! sustain-mutex)
              (thread-sleep! 1)
              (fix-finger (pick-random-node))
              (write-out node-info %node-info)
              (mutex-unlock! sustain-mutex)
              (loop)))))

;; System Start

(initialize-system)
(sustain-system)
