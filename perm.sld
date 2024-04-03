(define-library (database)

  (export
    find-entry-in-perm)

  (import
    (gambit)
    (github.com/JordaanI/utilities utilities))
    (begin
      (include "perm.scm")))