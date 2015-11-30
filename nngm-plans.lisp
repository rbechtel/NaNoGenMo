;;; Plans and goals from MTS/NaNoGenMo 2015
; Split off from Warren Sack's Common Lisp reconstruction of JRM's
; micro-Talespin from
;  _Inside_Computer_Understanding:_Five_Programs_Plus_Miniatures_
;  Roger Schank and Christopher Riesbeck (eds.)
;
; Original header comment:
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

; (in-package "NNGM") ; removed for development/debug 151108

;; Hmm. Are these planboxes, in the sense of precondition+action sequence?
;; They're bundled as AND (presumably so you blow out if anything doesn't
;; work), but it looks like you can't separate the test and the execution.
;; And where do you decide which plan to use (or maybe there's just a fixed
;; order)?
;;
;; Notice a weakness in bundling - both ask-plan and bargain-plan begin
;; by testing (not (relate ',actor ',agent ',actor 'deceive)) - so if
;; you try ask first and fail on that, there's no point in trying bargain.
;;
;; This also means that if we want to include the character's reasoning
;; process in the *story-sequence* we'll have to SAY things about the
;; tests in these plan functions. E.g., if the character attempts ASK-PLAN
;; we want them to SAY "Actor believed that agent would not deceive actor."
;; and "Actor believed that actor liked agent." before we get to "Actor
;; asked agent if agent would do action."
;;
;; Looks like we could maybe do the SAY within RELATE, KNOWS, HAS-GOAL-OF,
;; DCONT, DPROX, etc. Might need to add a negation flag to these functions.
;; (So instead of testing (not (relate ....)) you would test (relate ... 'not)

;  Two S-goals (satisfaction) -- thirst and hunger:

;  To satisfy thirst, go to some water and drink it.
(defun sthirst (actor)
;  (say "     Doing goal-eval for STHIRST")
  (goal-eval actor 
             (state actor 'thirsty 'neg)
             (list (sthirst-plan actor))))

(defun sthirst-plan (actor)
;  (say "     Doing STHIRST-PLAN")
  `(and (dprox ',actor ',actor 'water)
        (doit (ingest ',actor 'water))))

;  To satisfy hunger, get some food and eat it.
(defun shunger (actor)
;  (say "     Doing goal-eval for SHUNGER")
  (goal-eval actor 
             (state actor 'hungry 'neg)
             (gen-plans 'food
                        (reverse (get-isa 'food actor)) ; 151127 stuck a reverse on this to get fish first - better approach is to 
                                                        ; introduce parallelism into goal-eval
                        (shunger-plan actor))))

(defun shunger-plan (actor)
;  (say "     Doing SHUNGER-PLAN")
  `(and (dcont ',actor 'food)
        (doit (ingest ',actor 'food))))

;  Three D-goals (delta - instrumental and use plans for accomplishment)
;  -- dcont, dknow, dprox:

;  To get an object: if you know someone has it, persuade them to
;  give it to you; otherwise try to find out where the object is,
;  go there and take it.
(defun dcont (actor object)
  (let ((owner (knows-owner actor object)))
    (goal-eval actor 
               (has actor object)
               (if owner 
                 (list (dcont-plan1 actor object owner))
                 (list (dcont-plan2 actor object))))))

(defun dcont-plan1 (actor object owner)
  `(persuade ',actor 
             ',owner 
             (atrans ',owner ',object ',actor ',owner)))

(defun dcont-plan2 (actor object)
  `(and (dknow ',actor (where-is ',object))
        (dprox ',actor ',actor ',object)
        (doit (atrans ',actor ',object ',actor nil))))

;  To find out something: find a friend to tell you
(defun dknow (actor info)
  (goal-eval actor
             (mloc actor info)
             (gen-plans 'agent
                        (remove actor *personae*)
                        (dknow-plan actor info))))

(defun dknow-plan (actor info)
  `(and (knows-loc ',actor 'agent)
        (or (is-friend-of 'agent ',actor)
            (not (relate ',actor 'agent ',actor 'dominate)))
        (persuade ',actor
                  'agent
                  (mtrans 'agent ',info ',actor 'agent)))) 

;  To move an object (including yourself) to where some other
;  person or object is: get the first object (if not yourself), then
;  find out where the second object is and go there with the first
;  object.  If this doesn't work, try persuading the object to go
;  there itself.
(defun dprox (actor object new-object)
  (goal-eval actor 
             (is-at object new-object)
             (list (dprox-plan1 actor object new-object)
                   (dprox-plan2 actor object new-object))))

(defun dprox-plan1 (actor object new-object)
  `(and (or (equal ',actor ',object)
            (dprox ',actor ',actor ',object))
        (dknow ',actor (where-is ',new-object))
        (or (equal ',actor ',object)
            (doit (grasp ',actor ',object)))
        (or (is-prox ',actor (loc-name-of ',new-object))
            (doit (ptrans ',actor
                          ',object
                          (knows-loc ',actor ',new-object)
                          (knows-loc ',actor ',actor))))
        (or (equal ',actor ',object)
            (doit (un-grasp ',actor ',object)))))

(defun dprox-plan2 (actor object new-object)
  `(and (not (equal ',actor ',object))
        (member ',object *personae*)
        (persuade ',actor ; 151109 corrected typo "peruade"
                 ',object
                 (ptrans ',object
                         ',object
                         ',new-object
                         (loc-name-of ',object))
                 goal)))

;  Subgoals and plans -- persuade, ask, bargain, threaten, and tell:

;  You can persuade someone to do something by either asking them,
;  giving them food or threatening them.
(defun persuade (actor agent action)
  (goal-eval actor 
             action
             (append (gen-plans 'food 
                                (get-isa 'food agent) 
                                (bargain-plan actor agent action))
                     (list (ask-plan actor agent action))
                     (list (threat-plan actor agent action)))))

;  The success of asking something depends upon whether the other person
;  is honest and likes you.
(defun ask-plan (actor agent action)
  `(and (not (relate ',actor ',agent ',actor 'deceive))
        (relate ',actor ',actor ',agent 'like)
        (tell ',actor ',agent (question ',action))
        ;(is-true ',result)
        ))

;  The success of bargaining with someone by giving them food depends
;  on whether the other person is honest, you don't already have the
;  goal of getting the food you're going to bargain with, and you can
;  get the food to the other person.
(defun bargain-plan (actor agent action)
  (let ((atrans-food (atrans actor 'food agent actor)))
    `(and (not (relate ',actor ',agent ',actor 'deceive))
          (not (knows ',actor (has ',agent 'food)))
          (not (has-goal-of ',actor (has ',actor 'food)))
          (doit (mbuild ',actor (cause ',atrans-food (maybe ',action))))
          (tell ',actor 
                ',agent 
                (question (cause ',atrans-food (future ',action))))
          (dcont ',actor 'food)
          (dprox ',actor ',actor ',agent)
          (doit ',atrans-food)
          (is-true ',action))))

;  The success of threatening depends upon whether you dominate
;  the other person.
(defun threat-plan (actor agent action)
  `(and (not (relate ',actor ',agent ',actor 'dominate))
        (tell ',actor 
              ',agent 
              (cause (negate ',action) (future (propel ',actor 'hand ',agent))))
        (or (is-true ',action)
            (and (doit (propel ',actor 'hand ',agent))
                 (is-true ',action)))))

;  To tell someone something, go there and say it.
(defun tell (actor agent info)
  (goal-eval actor 
             (mloc agent info)
             (list (tell-plan actor agent info))))

(defun tell-plan (actor agent info)
  `(and (dprox ',actor ',actor ',agent)
        (doit (mtrans ',actor ',info ',agent ',actor))))

