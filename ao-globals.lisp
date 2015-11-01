;; Globals
;;
;; Copyright (C) 2015 by Robert Bechtel.
;; This work is licensed under a Creating Commons Attribution-ShareAlike 4.0 International License

(in-package "NNGM")

(defvar *default-tense* 'past)

(defvar *story-sequence* nil)

;  Definition of Globals

;  Declare globals used in forward-chaining through goals and plans.
(defvar *actions*)
(defvar *plans*)
(defvar *conseqs*)

;  Declare globals used to track objects in the simulation
(defvar *personae*)
(defvar *goals*)
(defvar *all-locations*)
(defvar *all-objects*)

;  This is the initial data base.  It can be extended before 
;  running a story.
(defvar *init-facts*)


