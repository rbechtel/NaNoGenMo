; MIT license, Creative Commons Attribution-ShareAlike 4.0 International License
; 
; structures.lisp

; (in-package "NNGM") ; removed for development/debug 151108

; Episodes (what the original micro-talespin called stories) have a
; main-character, a problem, and a list of (new) facts

(defstruct episode protagonist problem factset)

; A fact, like in a factset, has a knower and knowledge

(defstruct fact knower knowledge)

; The mental state of a character is the set of facts, goals,
; and demons that they hold (when the structure is created)

(defstruct mental-state who facts goals demons)

; What does a state of the world look like? (A snapshot)
; We know who the players are (*personae*), what all the players
; know (mental states across all *personae* + world),
; what locations exist (*all-locations*) and what objects
; exist (*all-objects*), remembering that every location is 
; also an object.

(defstruct world-state personae mental-states locations objects)


