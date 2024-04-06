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

(define m 20)

;; Ring Size

(define ring-size (expt 2 m))

;; Amount of initial nodes

(define initial-node-count 10)

;; data path

(define data-path "../data")

;; node path

(define node-path (string-append data-path "/nodes"))

;; perm-path

(define perm-path (string-append data-path "/perm/perm"))

;; host path

(define host-path (string-append node-path "/0"))

;; tmp path

(define tmp-path (string-append node-path "/tmp"))
