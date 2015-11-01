; micro-talespin-demo variables for sample stories
; Original header note:
;*****************************************************************
;  MICRO-TALESPIN: A STORY GENERATOR
;
;  A reconstruction, in Common Lisp, of James Meehan's program in
;  _Inside_Computer_Understanding:_Five_Programs_Plus_Miniatures_
;  Roger Schank and Christopher Riesbeck (eds.)
;
;  Warren Sack                 
;  MIT Media Lab
;  20 Ames Street, E15-320F
;  Cambridge MA 02139
;  wsack@media.mit.edu
;
;  October 1992
;
;  I translated Micro-Talespin into Common Lisp as a
;  "literature review exercise":  I wanted to see and play
;  with storyteller systems that had been written in the past.
;  I was working on creating storyteller systems which
;  produce not only text (as Micro-Talespin does) but also
;  audio and video.  If you are working on a similar project
;  I'd love to hear from you.  I can be reached at the
;  above address.
;
;*****************************************************************
;
; All changes by Robert Bechtel for NaNoGenMo 2015 are licensed
; under a Creating Commons Attribution-ShareAlike 4.0 International License

(in-package "NNGM")

(export (list '*STORY1* '*STORY2* '*STORY3* '*STORY4* '*STORY5* '*STORY6* '*STORY7*))

; More Initial Facts
;  Joe is a bear.
;  Joe's home is the cave.
;  Louise is also a bear.
;  Louise's home is the valley.
;  Irving is a bird.
;  Irving's home is a tree.
;  Bears eat honey, berries, and fish.
;  Birds eat worms.
;  Joe, Irving, and Louise are personae (in the theatrical sense).
;  Hunger and thirst are possible goals.
;  The cave, the oak tree, the elm tree, the pine-tree, the ground,
;  the valley, and the river are all locations.
;  All locations are also objects.
;  Honey, berries, fish, worms, and water are also objects.

  (put 'joe  'is-a 'bear)
  (put 'joe 'home 'cave)
  (put 'joe 'gender 'male) ; 151024 to support pronouns
  (put 'irving 'is-a 'bird)
  (put 'irving 'home 'tree)
  (put 'irving 'gender 'male) ; 151024 to support pronouns
  (put 'louise 'is-a 'bear)   ; 151025 adding Louise
  (put 'louise 'home 'valley)
  (put 'louise 'gender 'female)
  (put 'bear 'food '(honey berries fish))
  (put 'bird 'food '(worm))


; Story 1
; Synopsis:
; No plot: joe gets a drink of water.
; Representation:
; Joe is thirsty.
(defvar *story1*
  '(joe thirsty))


; Story 2
; Synopsis:
; Irving kills Joe.
; Representation:
; Irving is thirsty.
; Irving thinks that Joe does not like him.
; Irving thinks that Joe does not dominate him.
; Irving thinks that Joe deceives him.
; Irving thinks that he does not like Joe.
; Joe thinks that Irving does not deceive him.
(defvar *story2*
  '(irving thirsty
    (irving (like (actor joe) (to irving) (mode (neg))))
    (irving (dominate (actor joe) (to irving) (mode (neg))))
    (irving (deceive (actor joe) (to irving) (mode (pos))))
    (irving (like (actor irving) (to joe) (mode (neg))))
    (joe (deceive (actor irving) (to joe) (mode (neg))))))


; Story 3
; Synopsis:
; Joe is frustrated at every turn.
; Representation:
; Joe is hungry.
; Joe thinks that Irving does not like him.
; Joe thinks that Irving dominates him.
(defvar *story3*
  '(joe hungry
    (joe (like (actor irving) (to joe) (mode (neg))))
    (joe (dominate (actor irving) (to joe) (mode (pos))))))

; Story 4
; Synopsis:
; Joe and Irving strike a deal.
; Representation:
; Joe is hungry.
; The world thinks that Irving is hungry.
; Joe thinks that Irving likes him.
; Joe thinks that Irving does not deceive him.
; Joes thinks that he likes Irving.
; Irving thinks that he like Joe.
; Irving thinks that he does not dominate Joe.
; Irving thinks that he does not deceive Joe.
(defvar *story4*
  '(joe hungry
    (world (hungry (actor irving) (mode (pos))))
    (joe (like (actor irving) (to joe) (mode (pos))))
    (joe (deceive (actor irving) (to joe) (mode (neg))))
    (joe (like (actor joe) (to irving) (mode (pos))))
    (irving (like (actor irving) (to joe) (mode (pos))))
    (irving (dominate (actor irving) (to joe) (mode (neg))))
    (irving (deceive (actor irving) (to joe) (mode (neg))))))

; Story 5
; Synopsis:
; Joe tricks Irving.
; Representation:
; Irving is thirsty.
; Irving thinks that he likes Joe.
; Irving thinks that Joe likes him.
; Irving thinks that Joe does not deceive him.
; Irving thinks that Joe dominates him.
; The world thinks that Joe is hungry.
; Joe thinks that he does not like Irving.
; Joe thinks that he deceives Irving.
(defvar *story5*
  '(irving thirsty
; Irving thinks that there is honey in the elm tree.
; The world thinks that there is a worm in the ground.
; Joe thinks that there is a worm in the ground.
; Irving thinks that Joe is in the cave.
; The world thinks that there are fish in the river.
; Irving thinks that there are fish in the river.
    (irving (like (actor irving) (to joe) (mode (pos))))
    (irving (like (actor joe) (to irving) (mode (pos))))
    (irving (deceive (actor joe) (to irving) (mode (neg))))
    (irving (dominate (actor joe) (to irving) (mode (pos))))
    (world (hungry (actor joe) (mode (pos))))
    (joe (like (actor joe) (to irving) (mode (neg))))
    (joe (deceive (actor joe) (to irving) (mode (pos))))))


; Story 6
; Synopsis:
; This is an interactive version of story kernels 4 and/or 5.
; Representation:
; Joe is hungry.
; Joe thinks that Irving likes him.
; Joe thinks that Irving does not dominate him.
(defvar *story6*
  '(joe hungry
    (joe (like (actor irving) (to joe) (mode (pos))))
    (joe (dominate (actor irving) (to joe) (mode (neg))))))


; Story 7 [151025]
; Synopsis:
; Louise is hungry.
; She likes Joe, and thinks Joe likes her.
; She doesn't like Irving, but thinks that she dominates him.

(defvar *story7*
  '(louise hungry
           (louise (like (actor louise) (to joe) (mode (pos))))
           (louise (like (actor joe) (to louise) (mode (pos))))
           (louise (like (actor louise) (to irving) (mode (neg))))
           (louise (dominate (actor louise) (to irving) (mode (pos))))))
