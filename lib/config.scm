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


;; m value

(define m 8)

;; Ring Size

(define ring-size (expt 2 m))

;; Amount of initial nodes

(define initial-node-count 4)
;; data path

(define data-path "../data")

;; nodes path

(define nodes-path (string-append data-path "/nodes"))

;; perm-path

(define perm-path (string-append data-path "/perm/perm"))

;; version path

(define (version-path version) (string-append data-path "/version/" (number->string version)))

;; tmp path

(define (tmp-path node) (string-append nodes-path "/tmp-" (number->string node)))

;; node-path

(define (node-path node) (string-append nodes-path "/" (number->string node)))

;; host path

(define host-path (node-path 0))
