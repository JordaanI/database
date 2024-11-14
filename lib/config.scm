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
;; Project: Config File for BroodB
;;

;; Consts

(define (conf)
  (let* ((path-string (cdr (shell-command "pwd" #t)))
         (path (substring path-string 0 (- (string-length path-string) 1)))
         (brood (string-append path ".brood")))
    (if (file-exists? brood) (with-input-from-file brood read-all)
        `((m . 8) (nodes . 5) (home . ,path)))))

;; m value

(define m (cdr (assoc 'm (conf))))

;; Ring Size

(define ring-size (expt 2 m))

;; Amount of initial nodes

(define initial-node-count (cdr (assoc 'nodes (conf))))

;; relative path

(define home (cdr (assoc 'home (conf))))

(define brood-home (string-append home "/brood"))

;;

(define extension ".txt")

;; data path

(define data-path (string-append brood-home "/data"))

;; nodes path

(define nodes-path (string-append data-path "/nodes"))

;; perm-path

(define perm-loc (string-append data-path "/perm"))

(define perm-path (string-append perm-loc "/perm" extension))

(define archive-path (string-append perm-loc "/archive/"))

(define (zip-path file) (string-append archive-path (number->string file) "-" (number->string (current-jiffy)) ".zip"))

;; version path

(define (version-path version) (string-append data-path "/version/" (number->string version) extension))

(define (tmp-version version) (string-append data-path "/version/tmp-" (number->string version)))

;; tmp path

(define (tmp-path node) (string-append nodes-path "/tmp-" (number->string node) extension))

;; node-path

(define (node-path node) (string-append nodes-path "/" (number->string node) extension))

;; host path

(define host-path (node-path 0))
