;; Globals
;;
;; Copyright (C) 2015 by Robert Bechtel.
;; This work is licensed under a Creating Commons Attribution-ShareAlike 4.0 International License

; (in-package "NNGM") ; removed for development/debug 151108

(defvar *default-tense* 'past)

(defvar *story-sequence* nil)

(defvar *sentence-queue* nil)

;  Definition of Globals

;  Declare globals used in forward-chaining through goals and plans.
(defvar *actions*)
(defvar *plans*)
(defvar *conseqs*)

;  Declare globals used to track objects in the simulation
(defvar *personae*)
(defvar *deceased*) ; keep track of actors that die (so you can add them back)
(defvar *goals*)
(defvar *all-locations*)
(defvar *all-objects*)

;  This is the initial data base.  It can be extended before 
;  running a story.
(defvar *init-facts*)

; Globals useful for testing

(defvar *cd-actions*)
(defvar *cd-states*)
(defvar *cd-modes*)
(defvar *cd-tense*)
(defvar *state-fillers*)
(defvar *mode-fillers*)
(defvar *relation-fillers*)

(defvar *chapter-counter*)

; Global for unifier (from talesim)

(defvar *OccursCheck-P* T)


