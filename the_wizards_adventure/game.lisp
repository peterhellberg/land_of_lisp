;===============================================================================
; Describing the Scenery with an Association List
;===============================================================================

(defparameter *nodes* '(
    (living-room
      (you are in the living-room. a wizard is snoring loudly on the couch))
    (garden
      (you are in a beautiful garden. there is a well in front of you.))
    (attic
      (you are in the attic. there is a giant welding torch in the corner.))
  ))

;===============================================================================
; Describing the Location
;===============================================================================

; Describe the location using assoc
(defun describe-location (location nodes)
  (cadr (assoc location nodes)))

;===============================================================================
; Describing the Paths
;===============================================================================

; The edges
;  (paths that the player can take)
;
(defparameter *edges* '(
  (living-room
    (garden west door)
    (attic upstairs ladder))
  (garden
    (living-room east door))
  (attic
    (living-room downstairs ladder))
  ))

; Describe the path
;  (using quasiquoting)
;
(defun describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))

; Describing multiple paths at once
;  (using higher-order functions)
;
(defun describe-paths (location edges)
  (apply #'append
    (mapcar #'describe-path
      (cdr (assoc location edges)))))

;===============================================================================
; OUTPUT!
;===============================================================================

(print (describe-paths 'living-room *edges*))
