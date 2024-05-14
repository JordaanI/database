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

;; relative path

(define home (get-environment-variable "HOME"))

(define brood-home (string-append home "/brood"))

;;

(define extension ".txt")

;; data path

(define data-path (string-append brood-home "/data"))

;; nodes path

(define nodes-path (string-append data-path "/nodes"))

;; perm-path

(define perm-path (string-append data-path "/perm/perm" extension))

;; version path

(define (version-path version) (string-append data-path "/version/" (number->string version) extension))

(define (tmp-version version) (string-append data-path "/version/tmp-" (number->string version)))
;; tmp path

(define (tmp-path node) (string-append nodes-path "/tmp-" (number->string node) extension))

;; node-path

(define (node-path node) (string-append nodes-path "/" (number->string node) extension))

;; host path

(define host-path (node-path 0))
