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
;; Project: Config File
;;

;;; Consts

(define brood-folder ".brood/")

(define config-file (string-append brood-folder "brood.conf"))
(define brood-files `("perm" "data"))

(define BROOD_HOME "BROOD_HOME")
(define NODES "BROOD_NODES")
(define SIZE "BROOD_SIZE")


(define NUMBER_NODES "10")
(define SIZE_PARAM "12")
(define SIZE_PARAM_NUM (string->number SIZE_PARAM))

;; Config Functions

(define (config)
  (if (file-exists? config-file)
      (let ((config (read-in config-file)))
        (setenv BROOD_HOME (table-ref config 'brood-home))
        (setenv NODES (table-ref config 'nodes))
        (setenv SIZE (table-ref config 'size)))
      (let ((brood-home (string-append (initial-current-directory) brood-folder))
            (brood-size (number->string (expt 2 (string->number SIZE_PARAM)))))
        (write-out config-file (list->table `((brood-home . ,brood-home)
                                              (nodes . ,NUMBER_NODES)
                                              (size . ,brood-size))))
        (setenv BROOD_HOME brood-home)
        (setenv NODES NUMBER_NODES)
        (setenv SIZE brood-size))))


(define (maybe-set-up-data-files files)
  (if (not (null? files))
      (let ((path (string-append (getenv BROOD_HOME) (car files))))
        (if (file-exists? path) (maybe-set-up-data-files (cdr files))
            (and
             (create-directory path)
             (display (string-append "Created " (car files)))
             (newline)
             (maybe-set-up-data-files (cdr files)))))))


(define (maybe-set-up-home-folder)
  (if (not (file-exists? brood-folder))
      (create-directory (string-append (initial-current-directory) brood-folder))))

;; Execute config on load

(maybe-set-up-home-folder)
(config)
(maybe-set-up-data-files brood-files)

;; Set relative paths

(define brood-home (getenv BROOD_HOME))

(define node-info (string-append brood-home "node.brood"))

(define data (string-append brood-home "data/"))

(define perm (string-append brood-home "perm/"))

(define perm-file (string-append perm "perm.brood"))

(define (node n) (string-append data (number->string n) "/"))

(define (register n) (string-append (node n) "register.brood"))

(define (concept-path n id) (string-append (node n) id ".brood"))
