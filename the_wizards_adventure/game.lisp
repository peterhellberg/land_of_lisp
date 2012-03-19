;===============================================================================
; Describing the Scenery with an Association List
;===============================================================================

(defparameter *nodes* '(
    (living-room
      (you are in the living-room. a wizard is snoring loudly on the couch))
    (garden
      (you are in a beautiful garden. there is a well in front of you.))
    (attic
      (you are in the attic. there is a giant welding torch in the corner.))))

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
    (living-room downstairs ladder))))

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
; Describing Objects at a Specific Location
;===============================================================================

(defparameter *objects* '(whisky bucket frog chain))

; Keep track on the location of each object
(defparameter *object-locations* '(
  (whisky living-room)
  (bucket living-room)
  (chain  garden)
  (frog   garden)))

; List objects visible from a given location
(defun objects-at (loc objs obj-locs)
  (labels ((at-loc-p (obj)
    (eq (cadr (assoc obj obj-locs)) loc)))
      (remove-if-not #'at-loc-p objs)))

; Describing visible objects
(defun describe-objects (loc objs obj-loc)
  (labels ((describe-obj (obj)
    `(you see a ,obj on the floor.)))
      (apply #'append
        (mapcar #'describe-obj (objects-at loc objs obj-loc)))))

;===============================================================================
; OUTPUT!
;===============================================================================

(print (describe-objects 'living-room *objects* *object-locations*))
