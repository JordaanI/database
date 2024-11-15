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
;; Project: Utilities
;;

;;; Read/Write utilities

(define (write-out path obj #!key (append #f))
  (let ((data (if (file-exists? path) (read-in path) #!eof)))
    (with-output-to-file
        path
      (lambda ()
        (display (object->u8vector (if append  (cond
                                                ((eof-object? data) obj)
                                                ((list? data) (cons obj data))
                                                (#t (list obj data)))
                                       obj)))))))

(define (read-in path)
  (let ((v (with-input-from-file path read)))
    (if (not (eof-object? v)) (u8vector->object v)
        v)))

(define (strip-spaces-to-char s)
  (let loop ((l (string->list s)))
    (if (null? l) (list)
        (let ((c (car l)))
          (if (char=? (car l) #\space) (loop (cdr l))
              (cons c (loop (cdr l))))))))

(define (between n l h #!key (include? #t))
  (let ((op (if include? <= <)))
    (or
     (= n h)
     (and (> n l) (op n h))
     (and (< h l) (or (op n h) (> n l))))))


(define (pick-random l)
  (list-ref l (random-integer (length l))))

(define (replace l o n)
  (cond
   ((null? l) (list))
   ((equal? o (car l)) (cons n (replace (cdr l) o n)))
   (#t (cons (car l) (replace (cdr l) o n)))))

(define (replace! l o n)
  (if (member o l) (replace l o n)
      (cons n l)))
