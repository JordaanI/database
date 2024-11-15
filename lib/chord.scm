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
;; Project: Chord
;;

(define (%find-successor n id)
  (let ((succ (node-finger n 1)))
    (if (between id (node-id n) succ) succ
        (%find-successor (look-up-node (closest-node n id)) id))))

(define (fix-finger n)
  (let* ((ref (+ (node-finger-index n) 1))
         (finger-index (if (> ref (string->number SIZE_PARAM)) 1 ref)))
    (node-finger-index-set! n finger-index)
    (node-finger-set! n finger-index (%find-successor n (+ (node-id n) (expt 2 (- finger-index 1)))))))

(define (fix-fingers n)
  (for-each fix-finger (make-list SIZE_PARAM_NUM n)))

;; Optimizations exist here, node-finger
(define (closest-node n id)
  (let ((l (node-id n)))
    (let loop ((i SIZE_PARAM_NUM))
      (if (= i 1) l
          (let ((finger (node-finger n i)))
            (if (between finger l id include?: #f) finger
                (loop (- i 1))))))))
