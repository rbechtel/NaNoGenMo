
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
; Usage: Start up Common Lisp and load in this file.  Then, at the Lisp 
; prompt "?" type (micro-talespin-demo *story1*) in order to see Talespin 
; generate a  story using story kernel 1.

; All changes by Robert Bechtel for NaNoGenMo 2015 are licensed
; under a Creating Commons Attribution-ShareAlike 4.0 International License

;;; 151015: I've pulled the micro-mumble components out of this file
;;; into their own so I can focus on the generation issues without
;;; dealing with simulator stuff.
;;;
;;; This holds the simulator components and non-generation-specific data
;;; You'll want to load both this and micro-mumble.lisp before trying to
;;; run anything.

; (in-package "NNGM") ; removed for development/debug 151108

;  Standard definition of put.

(defmacro put (x y z)
  `(setf (get ,x ,y) ,z))

;  Definitions necessary for pattern variables.

(defstruct (pcvar (:print-function print-pcvar)) id)

(defun print-pcvar (var stream depth)
  (declare (ignore depth))
  (format stream "?~s" (pcvar-id var)))

(set-macro-character #\?
                     #'(lambda (stream char)
                         (declare (ignore char))
                         (make-pcvar :id (read stream t nil t)))
                     t)

; Initial Facts
; The world thinks that Joe is in the cave.
; Joe thinks that he is in the cave.
; The world thinks that Irving is in the oak tree.
; Irving thinks that he is in the oak tree.
; Joe thinks that Irving is in the oak tree.
; The world thinks that there is water in the river.
; Joe thinks that there is water in the river.
; The world thinks that there is honey in the elm tree.
; Irving thinks that there is honey in the elm tree.
; The world thinks that there is a worm in the ground.
; Joe thinks that there is a worm in the ground.
; Irving thinks that Joe is in the cave.
; The world thinks that there are fish in the river.
; Irving thinks that there are fish in the river.
;; 151025 additional initial facts - new female character, Louise
;; The world thinks that Louise is near the valley.
;; Louise thinks that she is near the valley.
;; Louise thinks the water is near the river.
;; Louise thinks Irving is near the oak-tree.
;; Louise thinks Joe is near the cave.
;; Louise thinks the worm is near the ground.
;; The world thinks the berries are near the pine-tree.
;; Irving thinks the berries are near the pine-tree.
;; 151025 revised so you can hand it a story and get those
;;   facts added as well.

(defun init-facts (&optional story)
  (setf *init-facts*
        (append (if story (cddr story))
                '((world (loc (actor joe) (val cave)))
                  (joe (loc (actor joe) (val cave)))
                  (world (loc (actor irving) (val oak-tree)))
                  (irving (loc (actor irving) (val oak-tree)))
                  (joe (loc (actor irving) (val oak-tree)))
                  (world (loc (actor water) (val river)))
                  (joe (loc (actor water) (val river)))
                  (world (loc (actor honey) (val elm-tree)))
                  (irving (loc (actor honey) (val elm-tree)))
                  (world (loc (actor worm) (val ground)))
                  (joe (loc (actor worm) (val ground)))
                  (irving (loc (actor joe) (val cave)))
                  (world (loc (actor fish) (val river)))
                  (irving (loc (actor fish) (val river)))
                  (world (loc (actor louise) (val valley)))
                  (louise (loc (actor louise) (val valley)))
                  (louise (loc (actor water) (val river)))
                  (louise (loc (actor irving) (val oak-tree)))
                  (louise (loc (actor joe) (val cave)))
                  (louise (loc (actor worm) (val ground)))
                  (world (loc (actor berries) (val pine-tree)))
                  (irving (loc (actor berries) (val pine-tree)))
                  ))))

;  init-world sets up the world.  
;
; Clears facts, goals, and demons from everyone in *personae*
;
; Then invokes ESTABLISH-FACTS to add CD-shaped knowledge to various
; actors, as defined in *init-facts*

(defun init-world ()
  (setf *personae* '(joe irving louise))
  (setf *deceased* nil)
  (setf *goals* '(hungry thirsty))
  (setf *all-locations* '(cave oak-tree elm-tree pine-tree ground river valley))
  (setf *all-objects* (append *all-locations* 
                              '(honey berries fish worm water)))
  (mapc #'(lambda (persona)
            (put persona 'facts nil)
            (put persona 'goals nil)
            (put persona 'demons nil))
        (cons 'world *personae*))
  (establish-facts))

; The variable *init-facts* contains location
; and relationship facts, along with which character knows them.
; Everything in *init-facts* is expressed as a CD structure.

(defun establish-facts ()
  (mapc #'(lambda (fact)
            (now-knows (car fact) (cadr fact) t)) ; t as last arg ensures all facts are added to *story-sequence*
        *init-facts*))


;; Top-level functions

;; 151016: Added call to RECITE at the end to actually trigger 
;; story generation. Also changed calls to format into calls
;; to SAY (which means we have to be ready to get non-CD arguments
;; to SAY and downstream functions).
;; 151024: Added optional parameter keep-sequence. If T, then recite won't
;; reset *story-sequence* to NIL after generating the story.

(defun micro-talespin (&optional keep-sequence)
  (setf *story-sequence* nil) ; clear it out just in case
  (init-facts)
  ; this variable binding in LET can't work! *personae* and *goals* are NIL
  ; until you've executed (init-world) 151025
  (let ((main-character (pick-one 'character *personae*))
        (problem (pick-one 'problem *goals*)))
     (say "~%Once upon a time ...") ; originally (format t "~%Once upon a time ...")
     (init-world)
     (say "~%One day,") ; originally (format t "~%One day,")
     (assert-fact (mloc 'world (state main-character problem 'pos)))
     (say "~%The end.~%") ; originally (format t "~%The end.")
     (recite keep-sequence)))

;  micro-talespin-demo lets you predefine more facts for a story.
;  story should be a list of the form (character problem fact fact ...)
;  where
;     character is either joe or irving,
;     problem is either hunger or thirst,
;     facts have the for (character 'CD-form).  The character field
;           says who knows this fact.
;; 151016: Added call to RECITE at the end to actually trigger 
;; story generation. Also changed calls to format into calls
;; to SAY (which means we have to be ready to get non-CD arguments
;; to SAY and downstream functions)
;; 151024: Added optional parameter keep-sequence. If T, then recite won't
;; reset *story-sequence* to NIL after generating the story.

(defun micro-talespin-demo (story &optional keep-sequence)
  (setf *story-sequence* nil) ; clear it out just in case
  (init-facts)
  (setf *init-facts* 
        (append *init-facts* (cddr story)))
  (let ((main-character (car story))
        (problem (cadr story)))
    (say "~%Once upon a time ...") ; originally (format t "~%Once upon a time ...")
    (init-world)
    (say "~%One day, ") ; originally (format t "~%One day, ")
    (assert-fact (mloc 'world (state main-character problem 'pos)))
    (say "~%The end.") ; originally (format t "~%The end.")
    (recite keep-sequence)))

;; 151025 Let's generalize over the top-level calls
;;   If you give it a story, it works like micro-talespin-demo
;;   If you don't give it a story, it works like micro-talespin
;; 151101 Changed to do random-choice in place of pick-one so it's
;;   entirely automated rather than being interactive 

(defun spin-tale (&optional story keep-sequence)
  (let (main-character problem)
    (setf *story-sequence* nil)
    (init-facts story) ; if no story, just does basic facts
    (say "~%Once upon a time ...") ; originally (format t "~%Once upon a time ...")
    (init-world)
    (setf main-character (if story
                             (if (first story) (first story) (random-choice *personae*))
                           (random-choice *personae*)))
    (setf problem (if story 
                      (if (second story) (second story) (random-choice *goals*))
                    (random-choice *goals*)))
    (say "~%One day, ") ; originally (format t "~%One day, ")
    (assert-fact (mloc 'world (state main-character problem 'pos)))
    (say "~%The end.") ; originally (format t "~%The end.")
    (recite keep-sequence)))

;  pick-one is used to get the character and problem from the terminal.

(defun pick-one (name l)
  (format t "~%Choose a ~s from this list:~%~s~%> " name l)
  (let ((a (read)))
    (if (member a l) a (pick-one name l))))

; Pick an item at random from a list

(defun random-choice (list) (nth (random (length list)) list))

;  goal evaluator: executes each plan until one works and the goal
;  can be removed, or until none do and the character fails to get the
;  goal.  If the goal is already true (and the actor knows that), then
;  return success immediately.  If the actor already has the goal,
;  then he's in a loop and has failed.  Otherwise, set up the goal and go.

(defun goal-eval (actor goal plans)
  (cond ((knows actor goal) t)                 ; already succeeded
        ((has-goal-of actor goal) nil)         ; already had the goal (fail)
        (t                                     ; otherwise
         (gets-new-goal-of actor goal)           ; add to goal list
         (cond ((run-plans plans)                ; if any plan works
                (forgets-goal-of actor goal)     ; forget the goal and succeed
                t)
               (t                                ; otherwise
                (now-knows actor (negate (future goal)) t) ; realize you're not going to succeed, fail
                nil)))))

;; 151121 This is where some things probably need to change. As soon as you add a goal,
;; you (apparently) start working on it. But you only work on one plan at a time - you
;; don't seem to be able to detect that another plan you have has had its preconditions
;; satisfied unexpectedly, so you could switch to that plan to achieve your current goal.
;; Similarly, you don't seem to be able to figure out that you could use a plan for
;; another goal immediately (e.g., drinking as soon as you get to the river, even though
;; you're currently working on your hunger).

(defun run-plans (plans)
  (let ((plan (car plans)))
    (if plan
       (if (eval plan)
          t
          (run-plans (cdr plans))))))

;  gen-plans replicates the same plan with different objects
;  e.g., trying to get any one of the several foods with the
;  same bargaining plan.

(defun gen-plans (var possibilities plan-form)
  (mapcar #'(lambda (possibility)
             (subst possibility var plan-form))
        possibilities))

;  The simulator

;  doit adds a CD and its consequences to the data base, by calling
;  assert-fact.  mtranses with '?unspecified have to be filled out, as in
;  "Irving told Joe where the honey was" -- the "where" being represented
;  in the CD with an '?unspecified form.

(defun doit (cd)
  (let ((newcd 
         (if (and (equal (header-cd cd) 'mtrans)
                  (knows (cdpath '(actor) cd) 
                         (cdpath '(object) cd)))
           (setrole 'object 
                    (knows (cdpath '(actor) cd) 
                           (cdpath '(object) cd)) 
                    cd)
           cd)))
    (assert-fact newcd)
    newcd))

;  assert-fact is one of the central control functions.  It starts with
;  one fact, infers the consequences, infers the consequences of the
;  consequences, etc.  Besides the simple result put in *conseqs*
;  (e.g., ptrans changes locs), new states may lead to response actions
;  (put in *actions*) or new plans (put in *plans*).  The plans are
;  done after all the consequences are inferred.
;
; 151122 Grumble. *plans* aren't actually plans, they're goals. Currently
; the only things that would wind up in *plans* are (SHUNGER ...) or
; (STHIRST ...). All other goals are instrumental to those. (Of course,
; this is MTS and only illustrates using fable-like activities - could
; probably extend per A&O to add things like E-goals, etc.

;(defun assert-fact (x)
;  (setf *actions* nil)
;  (setf *plans* nil)
;  (forward-chain (list x))
;  (mapc #'(lambda (cd) (doit (setrole 'time *default-tense* cd)))
;        *actions*)
;  (mapc #'eval *plans*))

;; 151121 Variant on assert-fact that takes a list of facts as input
;; rather than a single fact. Only change is that call to forward-chain
;; doesn't need to wrap the (no longer single) input in a list
;; Hmm. Maybe this could just be a simple direct replacement for
;; assert-fact - if you give it a single fact, it will wrap it
;; for you, but you can also give it multiple facts. Try it.
;; Woot! works fine.

(defun assert-fact (&rest facts)
  (setf *actions* nil)
  (setf *plans* nil)
  (forward-chain facts)
  (mapc #'(lambda (cd) (doit (setrole 'time *default-tense* cd)))
        *actions*)
  (mapc #'eval *plans*))

(defun forward-chain (l)
  (setf *conseqs* nil)
  (mapc #'(lambda (i) 
            (now-knows 'world i t) ; 151121 - changing last arg from nil to T to get it into *story-sequence*
            (conseqs i))
        l)
  (if *conseqs* (forward-chain *conseqs*)))

;  Each act and state is associated with a function for 
;  calculating the consequences.

(defun conseqs (cd)
  (case (header-cd cd)
    (atrans (atrans-conseqs cd))
    (grasp (grasp-conseqs cd))
    (ingest (ingest-conseqs cd))
    (loc (loc-conseqs cd))
    (mbuild (mbuild-conseqs cd))
    (mloc (mloc-conseqs cd))
    (mtrans (mtrans-conseqs cd))
    (plan (plan-conseqs cd))
    (propel (propel-conseqs cd))
    (ptrans (ptrans-conseqs cd))
    (t nil)))
 
;  add-conseq adds and returns a CD to the list of consequences

(defun add-conseq (x)
  (push x *conseqs*)
  x)

;  Consequences of an atrans: everyone in the area notices it and the
;  resulting change of possesion, the object changes locations, and the
;  from filler knows he no longer has it.

(defun atrans-conseqs (cd)
  (notice (cdpath '(actor) cd) 
          cd)
  (notice (cdpath '(actor) cd) 
          (add-conseq (has (cdpath '(to) cd) 
                           (cdpath '(object) cd))))
  (add-conseq (is-at (cdpath '(object) cd)
                     (cdpath '(to) cd)))
  (if (cdpath '(from) cd)
      (notice (cdpath '(actor) cd)
              (add-conseq (negate (has (cdpath '(from) cd)
                                       (cdpath '(object) cd)))))))

;  Consequences of a grasp: everyone knows that the actor either has or
;  (in the case of a tf (transition final or the end of an action) of the
;  grasp)  doesn't have the object

(defun grasp-conseqs (cd)
  (notice (cdpath '(actor) cd) cd) ; everyone in the area knows about the action
  (notice (cdpath '(actor) cd)
          (add-conseq (if (in-mode cd 'tf)
                        (negate (has (cdpath '(actor) cd)
                                     (cdpath '(object) cd)))
                        (has (cdpath '(actor) cd)
                             (cdpath '(object) cd))))))

;  Consequences of an ingest: everyone knows that the actor 
;  is no longer hungry or thirsty.
; 151129 The thing ingested should no longer exist - so it shouldn't
; be (LOC (ACTOR food) (VAL eater)) or (CONT (ACTOR food) (VAL eater))
; Arguably, it should cease to exist altogether - there should be 
; no more (LOC (ACTOR food) (VAL don't-care)) or
; (CONT (ACTOR food) (VAL don't-care)).
;  Easiest way to deal with renewable food is to just clear the LOC
; and CONT that deal with eater.
;  Particularly nasty because you have to tweak the facts property
; of everyone who might know these things - at a minimum the eater
; and WORLD, but certainly everyone who would notice as well.

(defun ingest-conseqs (cd)
  (let ((eater (cdpath '(actor) cd))
        (food (cdpath '(object) cd)))
    (notice eater cd) ; everyone in the area knows about the action
    (loop for actor in (cons 'world *personae*) do
          (forgets-fact-of actor (is-at food eater))
          (forgets-fact-of actor (has eater food))) ; actor and val are reversed from what you'd expect
    (notice eater
            (add-conseq (state eater
                               (if (equal food 'water)
                                   'thirsty
                                 'hungry)
                               'neg)))))

;  Consequences of a loc change: everyone knows it.

(defun loc-conseqs (cd)
  (notice (cdpath '(actor) cd) cd))

;  Consequences of an mbuild: if the object is a causal then a demon
;  is set up for the actor that will be triggered by the antecedent.

(defun mbuild-conseqs (cd)
  (if (equal (cdpath '(actor) cd)
             (cdpath '(object conseq actor) cd))
    (put (cdpath '(actor) cd)
         'demons
         (cons (cons (cdpath '(object ante) cd)
                     (cdpath '(object conseq) cd))
               (get (cdpath '(actor) cd) 'demons))))
  nil)

;  Consequences of an mloc change: check the demons to see if the
;  learned fact affects the learner.  Also check the reaction list
;  for general responses to learning such facts.

(defun mloc-conseqs (cd)
  (demon-check (cdpath '(val part) cd)
               (cdpath '(con) cd))
  ; 151121 - if this is the world knowing something about an actor's
  ; physiological state (hungry, thirsty, smart, health), then
  ; let that actor realize it as well.
  (if (and (eq (cdpath '(val part) cd) 'world)
           (is-physiological-state (cdpath '(con) cd)))
      (realize (cdpath '(con actor) cd) (cdpath '(con) cd)))
  (if (not (member 'neg (cdpath '(con mode) cd)))
    (case (header-cd (cdpath '(con) cd))
      (loc (loc-react cd))
      (mloc (mloc-react cd))
      (hungry (hunger-react cd))
      (thirsty (thirst-react cd))
      (t nil))))

;  Stored under each character is a list of "demons."  A demon is
;  a CD pattern plus an action.  Whenever the character learns
;  something this list is checked to see if there is a response to make.
;  Demons are set up by things like the mbuild in a bargain-plan.

(defun demon-check (who event)
  (put who
       'demons
       (remove-if #'null
                  (mapc #'(lambda (demon)
                            (cond ((unify-cds (car demon) event)
                                   (push (cdr demon) *actions*)
                                   nil)
                                  (t
                                   demon)))
                        (get who 'demons)))))

;  Consequences of an mtrans: if there is a ques in the CD mtransed,
;  and if it is a causal, then it is a bargaining promise; otherwise,
;  it is a request (assuming the actors in the sub-CD are in the right
;  places).  If there is no ques in the CD mtransed, then the hearer
;  knows about the mtrans, and if he believes the speaker, then he
;  believes what the speaker believes.

(defun mtrans-conseqs (cd)
  (let ((actor (cdpath '(actor) cd))
        (object (cdpath '(object) cd))
        (hearer (cdpath '(to part) cd)))
    (cond ((member 'ques (cdpath '(object mode) cd))
           (cond ((and (equal (header-cd object) 'cause)
                       (equal actor (cdpath '(object ante actor) cd))
                       (equal hearer (cdpath '(object conseq actor) cd)))
                  (promise-conseqs hearer
                                   (cdpath '(object conseq) cd)
                                   actor
                                   (cdpath '(object ante) cd)))
                 ((equal (cdpath '(object actor) cd) hearer)
                  (request-conseqs actor
                                   hearer
                                   (future (un-question object))))))
          ((not (equal actor hearer))
           (add-conseq (mloc hearer cd))
           (cond ((not (relate hearer actor hearer 'deceive))
                  (add-conseq (mloc hearer (mloc actor object)))))))))

;  Consequences of y asking x to promise to do xdo if y does ydo:
;  If x deceives y, then after ydo, x will call y stupid, but says
;  that he will do xdo in return for ydo;
;  else if x likes y, then x will do xdo after ydo and says so.
;  Otherwise x says no.

(defun promise-conseqs (x xdo y ydo)
  (let ((a (cause ydo (affirm xdo))))
    (cond ((relate x x y 'deceive)
           (add-conseq (mbuild x
                               (cause ydo
                                      (future (mtrans x
                                                      (state y 'smart 'neg)
                                                      y
                                                      x)))))
           (add-conseq (mtrans x a y x)))
          ((relate x x y 'like)
           (add-conseq (mbuild x a))
           (add-conseq (mtrans x a y x)))
          (t
           (add-conseq (mtrans x (negate a) y x))))))

;  Consequences of x asking y to do z: 
;  If y doesn't like x or dominates x, then y will say no; otherwise
;  y will do z.

(defun request-conseqs (x y z)
  (add-conseq (if (or (not (relate y y x 'like))
                      (relate y y x 'dominate))
                (plan y (future (mtrans y (negate z) x y)))
                (plan y z))))

;  Consequences of a plan: If the actor of the plan act is the actor of 
;  the object of the plan, then add the object to the list of actions.

(defun plan-conseqs (cd)
  (if (equal (cdpath '(actor) cd) (cdpath '(object actor) cd))
    (push (cdpath '(object) cd) *actions*))
  nil)


;  Consequences of a propel: the object struck dies
; 151122 - this seems a bit excessive. Will want to examine further.

(defun propel-conseqs (cd)
  (notice (cdpath '(actor) cd) cd) ; everyone in the area knows about the action
  (if (member (cdpath '(to) cd) *personae*)
    (add-conseq (state (cdpath '(to) cd) 'health 'neg))))

;  Consequences of a ptrans: location change, for both actor
;  and object.

(defun ptrans-conseqs (cd)
  (notice (cdpath '(actor) cd) cd) ; everyone in the area knows about the action
  (add-conseq (is-at (cdpath '(object) cd) (cdpath '(to) cd)))
  (if (not (equal (cdpath '(actor) cd) (cdpath '(object) cd)))
    (add-conseq (is-at (cdpath '(actor) cd) (cdpath '(to) cd)))))

;  Reactions to learning of a location change: if it's food or water,
;  check to see if learner is hungry or thirsty.

(defun loc-react (cd)
  (and (or (member (cdpath '(con actor) cd)
                   (get-isa 'food (cdpath '(val part) cd)))
           (equal (cdpath '(con actor) cd) 'water))
       (sgoal-check (cdpath '(val part) cd)
                    (if (equal (cdpath '(con actor) cd) 'water) 
                      'thirsty
                      'hungry))))

;  If a character is hungry or thirsty, add the appropriate s-goal
;  to the list of plans.

(defun sgoal-check (actor scale)
  (and (in-state actor scale)
       (push (list (if (equal scale 'thirsty)
                     'sthirst
                     'shunger)
                   (list 'quote actor))
             *plans*)))

;  Reactions to learning that someone has learned something:
;  if it's someone else, and it's about himself or you believe he
;  doesn't deceive you, then you believe it too.

(defun mloc-react (cd)
  (and (not (equal (cdpath '(val part) cd) (cdpath '(con val part) cd)))
       (or (equal (cdpath '(con con actor) cd) (cdpath '(con val part) cd))
           (not (relate (cdpath '(val part) cd) 
                        (cdpath '(con val part) cd)
                        (cdpath '(val part) cd)
                        'deceive)))
       (add-conseq (mloc (cdpath '(val part) cd)
                         (cdpath '(con con) cd)))))

;  Reactions to learning that you're hungry: add s-goal to list
;  of plans.
;; 151121 Grumble. What this actually does is add the s-goal whenever
;; _any_ actor thinks you're hungry. Thirst has the same problem.
;; Notice that mloc-react starts by testing who is the knower

(defun hunger-react (cd)
  (if (eq (cdpath '(val part) cd) (cdpath '(con actor) cd)) ; knower is the hungry one
      (push (list 'shunger (list 'quote (cdpath '(con actor) cd))) *plans*)))

;  Reactions to learning you're thirsty: add s-goal to list 
;  of plans.
;; 151121 Fixed analogous to hunger-react

(defun thirst-react (cd)
  (if (eq (cdpath '(val part) cd) (cdpath '(con actor) cd)) ; knower is the thirsty one
      (push (list 'sthirst (list 'quote (cdpath '(con actor) cd))) *plans*)))

;  Notice says that everyone in the same location as who knows
;  about CD.
;
; 151121 When who arrives at where, they get to know the location of 
; objects that are at that location (even if they already do know that)

(defun notice (who cd)
  (let ((where (loc-name-of who)))
    (mapc #'(lambda (persona)
              (if (equal (loc persona) where)
                  (add-conseq (mloc persona cd))))
          *personae*)
    (mapc #'(lambda (object)
              (if (and (equal (loc-name-of object) where) ; there's an object at new location
                       (not (knows-loc who object)))      ; and the person doesn't know that
                  (add-conseq (mloc who (is-at object (loc object)))))) ; now they do
          (set-difference *all-objects* *all-locations*))))

; 151121 So, if the world learns something about the physiological state of an
; actor, then presumably that actor should know it too.

(defun realize (who what) 
  (add-conseq (mloc who what)))

;  Memory functions and pattern matcher
;  addfact adds a CD to knower's knowledge set.  Also if world
;  learns a character has died, then the character is removed from the
;  global list of characters.
;  The CD is added to the front of the fact list, so that memquery
;  will get the most recent CD that matches its query pattern.  Older
;  contradicted facts are still on the list but are not seen.

(defun addfact (knower cd)
  (put knower 'facts (cons cd (get knower 'facts)))
  ;;; Now check for deceased people.
  (when (and (equal knower 'world)
           (equal (header-cd cd) 'health)
           (member 'neg (cdpath '(mode) cd)))
    (setf *personae* 
          (remove (cdpath '(actor) cd)
                  *personae*))
    (setf *deceased* (cons (cdpath '(actor) cd) *deceased*)))
  nil)

;  Removes fact from data base

(defun forgets-fact-of (actor fact)
  (let ((fact-to-be-forgotten (has-fact-of actor fact)))
    (put actor
         'facts
         (remove-if #'(lambda (g)
                        (equal g fact-to-be-forgotten))
                   (get actor 'facts)))))

(defun has-fact-of (actor pat)
  (car (pat-member pat (get actor 'facts))))

;  now-knows adds what to the data base for who.  It also prints in
;  English this new fact.  If who = world (a true fact) and what is
;  an mloc, then save the content of the mloc under the person who
;  learned it.  If say-flag is t, then mlocs are always generated in
;  English; otherwise only facts (who = world) are generated.  This
;  reduces the volume of the output.

(defun now-knows (who what say-flag)
  (let* ((world-knowing-mental-content?
          (and (equal who 'world)
               (equal (header-cd what) 'mloc)))
         (newwho
          (if world-knowing-mental-content?
              (cdpath '(val part) what)
            who))
         (newwhat
          (if world-knowing-mental-content?
              (cdpath '(con) what)
            what))
         experiencer)
    (if (or say-flag 
            (equal newwho 'world))
        (say (mloc newwho newwhat)))
    (addfact newwho newwhat)))

;  knows(knower,fact) returns fact if fact is in data base for knower:
;  -- if fact = knows(knower,subfact), assume everyone knows what they
;     know and look up subfact,
;  -- if fact has a ?unspec, then return the filler that replaces
;    the ?unspec in the data base.

(defun knows (knower fact)
  (let ((newfact
         (if (and (equal (header-cd fact) 'mloc)
                  (equal (cdpath '(val part) fact) knower))
           (cdpath '(con) fact)
           fact)))
  (memquery knower newfact)))

(defun knows-loc (knower object)
  (cdpath '(val) (knows knower (where-is object))))

(defun knows-owner (knower object)
  (cdpath '(val) (knows knower (who-has object))))

(defun knows-if (knower cd)
  (cdpath '(mode) (knows knower (setrole 'mode '?unspecified cd))))

;  memquery find the first item in knower's data base that
;  matches fact.

(defun memquery (knower pat)
  (car (pat-member pat (get knower 'facts))))

;  pat-member finds the first item in cd-list that matches
;  pat and returns cd-list from that item on.

(defun pat-member (pat cd-list)
  (if cd-list
    (let ((cd (car cd-list)))
      (if (unify-cds pat cd)
        cd-list
        (pat-member pat (cdr cd-list))))))

;  Returns non-nil if actor has goal.

(defun has-goal-of (actor pat)
  (car (pat-member pat (get actor 'goals))))

;  Adds goal to data base.

(defun gets-new-goal-of (actor goal)
  (put actor 'goals (cons goal (get actor 'goals)))
  (say (wants actor goal)))

;  Removes goal from data base

(defun forgets-goal-of (actor goal)
  (let ((goal-to-be-forgotten (has-goal-of actor goal)))
    (put actor
         'goals
         (remove-if #'(lambda (g)
                        (equal g goal-to-be-forgotten))
                   (get actor 'goals)))))

;  Returns non-nil if x is in a state, e.g., hungry.

(defun in-state (x st)
  (find-out 'world (state x st 'pos)))

;  Returns non-nil if X believes that y relates to z in a certain way.
;  Usually either y or z is x.

(defun relate (x y z rel)
  (find-out x (relation y z rel 'pos)))

;  Looks up CD in the data base for who.  If there, return non-nil if
;  the CD is not a negative fact.  If not there, ask the user at the
;  terminal and save the result.  Note that the generator is used to
;  ask questions.
;
;  find-out is used to determine if a given character is in a
;  given state (e.g., is the character hungry or thirsty) and is
;  also used to determine how two characters relate to on another
;  (e.g., do they like one another?, does one have a tendency to
;  deceive the other, etc.).
;
; 151101: Don't actually ask the user. Instead, do a random choice.
; 151127: This is a problem - example is when Joe starts as thirsty
;  (with no explicit statement about his hunger). When he goes to 
;  the river to get a drink, he becomes aware of fish in the river,
;  which triggers sgoal-check to see if he's hungry, which eventually
;  winds up here. Since there's no previous knowledge of his hunger
;  state, one gets generated (seemingly biased toward 'pos), which
;  distracts Joe from taking a drink. Maybe more importantly, Joe's
;  hunger never gets pushed through SAY. Let's just check if we're
;  who = world, and if so, let's (realize [CD actor] CD), which
;  should make the state explicit for the character, as well as
;  adding it to the world's knowledge.

(defun find-out (who cd)
  (let ((mode (knows-if who cd)))
    (cond (mode 
           (member 'pos mode))
          (t
;           (say-immediate (mloc who cd)) ; 151018 changed to reflect delayed generation
;           (format t "~% [Y/N]? ~%>")
           (let* ((answer (random-choice '(nil t)))
                  (newfact (setrole 'mode
                                    (list (if answer 'pos 'neg))
                                    cd))
                  (cdactor (cdpath '(actor) newfact)))
             (addfact who newfact)
;             (say (mloc who newfact)) ; 151127 - new, to force it into *story-sequence*
;                                      ; 151128 - wrap as MLOC - who thinks this (so we can
;                                      ; distinguish between Joe liking Louise and Louise
;                                      ; thinking that Joe likes her...
             (cond ((eq who 'world)
                    (say (mloc cdactor newfact))
                    (realize cdactor newfact))
                   (t (say (mloc who newfact))))
             ; So, you've made a random choice. If this is something the
             ; world has just learned, then whatever actor it applies to
             ; should also get to learn it - which is what realize will do.
;             (if (eq who 'world)
;                (realize (cdpath '(actor) newfact) newfact))
             answer)))))

; Original version
;(defun find-out (who cd)
;  (let ((mode (knows-if who cd)))
;    (cond (mode 
;           (member 'pos mode))
;          (t
;           (say-immediate (mloc who cd)) ; 151018 changed to reflect delayed generation
;           (format t "~% [Y/N]? ~%>")
;           (let ((answer (equal (read) 'y)))
;             (addfact who
;                      (setrole 'mode
;                               (list (if answer 'pos 'neg))
;                               cd))
;             answer)))))


;  True if y thinks x is a friend of his.

(defun is-friend-of (x y)
  (and (not (equal x y))
       (relate y x y 'like)))

;  Returns location of x.
(defun loc (x) (knows-loc 'world x))

;  True if x and y are in the same place.

(defun is-prox (x y)
  (equal (loc-name-of x)
         (loc-name-of y)))

;  A CD is true if it's an mloc and the content is in the person's
;  data base, or it's in the data base for world.

(defun is-true (cd)
  (if (equal (header-cd cd) 'mloc)
    (knows (cdpath '(val part) cd) (cdpath '(con) cd))
    (knows 'world cd)))

;  loc-name-of returns the real location of x.  This may involve going
;  up several levels -- e.g., when Joe takes a worm, its location is
;  stored as joe, but its real location is the location Joe is at.

(defun loc-name-of (x)
  (let ((loc-of-x (loc x)))
    (cond ((member x *all-locations*)
           x)
          ((member loc-of-x *all-locations*)
           loc-of-x)
          ;;; If something isn't anywhere in particular, 
          ;;; then it on the ground.
          ((null loc-of-x)
           'ground)
          (t
           (loc-name-of loc-of-x)))))

;  get-isa is like get but checks is-a node for x if x has no
;  y property.				

(defun get-isa (x y)
  (or (get y x)
      (get (get y 'is-a) x)))

;  Functions to build CD forms

;  Acts

(defun atrans (actor object to from)
  (list 'atrans 
        (list 'actor actor)
        (list 'object object)
        (list 'to to)
        (list 'from from)))

(defun cause (x y)
  (list 'cause
        (list 'ante x)
        (list 'conseq y)))

(defun grasp (actor object)
  (list 'grasp
        (list 'actor actor)
        (list 'object object)))

(defun un-grasp (actor object)
  (tf (grasp actor object)))

(defun ingest (actor object)
  (list 'ingest
        (list 'actor actor)
        (list 'object object)))

(defun mbuild (actor object)
  (list 'mbuild
        (list 'actor actor)
        (list 'object object)))

(defun mtrans (actor object to from)
  (list 'mtrans 
        (list 'actor actor)
        (list 'object object)
        (list 'to (list 'cp (list 'part to)))
        (list 'from from)))

(defun plan (actor object)
  (list 'plan
        (list 'actor actor)
        (list 'object object)))

(defun propel (actor object to)
  (list 'propel 
        (list 'actor actor)
        (list 'object object)
        (list 'to to)))

(defun ptrans (actor object to from)
  (if to
    (list 'ptrans 
          (list 'actor actor)
          (list 'object object)
          (list 'to to)
          (list 'from from))))

(defun wants (actor goal)
  (list 'want
        (list 'actor actor)
        (list 'object goal)))

;  States

(defun has (actor object)
  (list 'cont
        (list 'actor object)
        (list 'val actor)))

(defun is-at (actor loc)
  (list 'loc
        (list 'actor actor)
        (list 'val loc)))

(defun mloc (actor con)
  (list 'mloc
        (list 'con con)
        (list 'val (list 'cp (list 'part actor)))))

(defun state (actor st mode)
  (list st
        (list 'actor actor)
        (list 'mode (list mode))))

(defun relation (actor object rel mode)
  (list rel
        (list 'actor actor)
        (list 'to object)
        (list 'mode (list mode))))

(defun where-is (x)
  (list 'loc
        (list 'actor x)
        (list 'val '?unspecified)))

(defun who-has (x)
  (list 'cont
        (list 'actor x)
        (list 'val '?unspecified)))

;; helpers - test the type of a CD

;; Actions

(defun is-atrans? (cd) (and (is-cd-p cd) (eq (header-cd cd) 'atrans)))

(defun is-cause? (cd) (and (is-cd-p cd) (eq (header-cd cd) 'cause)))

(defun is-grasp? (cd) (and (is-cd-p cd) (eq (header-cd cd) 'grasp)))

(defun is-un-grasp? (cd) (and (is-cd-p cd) (eq (header-cd cd) 'un-grasp)))

(defun is-ingest? (cd) (and (is-cd-p cd) (eq (header-cd cd) 'ingest)))

(defun is-mbuild? (cd) (and (is-cd-p cd) (eq (header-cd cd) 'mbuild)))

(defun is-mtrans? (cd) (and (is-cd-p cd) (eq (header-cd cd) 'mtrans)))

(defun is-plan? (cd) (and (is-cd-p cd) (eq (header-cd cd) 'plan)))

(defun is-propel? (cd) (and (is-cd-p cd) (eq (header-cd cd) 'propel)))

(defun is-ptrans? (cd) (and (is-cd-p cd) (eq (header-cd cd) 'ptrans)))

(defun is-wants? (cd) (and (is-cd-p cd) (eq (header-cd cd) 'wants)))

;; States

;  is-state returns non-nil if CD is one of the state forms.

(defun is-state (cd)
  (member (header-cd cd)
          '(loc 
            mloc 
            cont 
            like 
            deceive 
            dominate 
            hungry 
            thirsty ; this was thristy in the original
            health 
            smart)))

(defun is-physiological-state (cd)
  (member (header-cd cd)
          '(hungry 
            thirsty
            health 
            smart)))

(defun is-cont? (cd) (and (is-cd-p cd) (eq (header-cd cd) 'cont)))

(defun is-loc? (cd) (and (is-cd-p cd) (eq (header-cd cd) 'loc)))

(defun is-mloc? (cd) (and (is-cd-p cd) (eq (header-cd cd) 'mloc)))

(defun is-like? (cd) (and (is-cd-p cd) (eq (header-cd cd) 'like)))

(defun is-deceive? (cd) (and (is-cd-p cd) (eq (header-cd cd) 'deceive)))

(defun is-dominate? (cd) (and (is-cd-p cd) (eq (header-cd cd) 'dominate)))

(defun is-hungry? (cd) (and (is-cd-p cd) (eq (header-cd cd) 'hungry)))

(defun is-thirsty? (cd) (and (is-cd-p cd) (eq (header-cd cd) 'thirsty)))

(defun is-health? (cd) (and (is-cd-p cd) (eq (header-cd cd) 'health)))

(defun is-smart? (cd) (and (is-cd-p cd) (eq (header-cd cd) 'smart)))

;  Mode functions

(defun mode (cd)
  (cdpath '(mode) cd))

;  Affirm/Negate set the mode of a CD to true/false.

(defun affirm (cd)
  (if (member 'pos (mode cd))
    cd
    (setrole 'mode (cons 'pos (remove 'neg (mode cd))) cd)))

(defun negate (cd)
  (if (member 'neg (mode cd))
    (affirm cd)
    (setrole 'mode (cons 'neg (remove 'pos (mode cd))) cd)))

;  maybe makes a CD hypothetical -- doesn't matter if it's true or false.

(defun maybe (cd)
  (if (member 'maybe (mode cd))
    cd
    (setrole 'mode (cons 'maybe (mode cd)) cd)))

;  question/un-question make a CD a question/non-question -- doesn't
;  matter if it's true or false.

(defun question (cd)
  (if (member 'ques (mode cd))
    cd
    (setrole 'mode (cons 'ques (mode cd)) cd)))

(defun un-question (cd)
  (setrole 'mode (remove 'ques (mode cd)) cd))

;  tf adds "transition final" to a CD -- doesn't matter if it's true
;  or false.

(defun tf (cd)
  (if (member 'tf (mode cd))
    cd
    (setrole 'mode (cons 'tf (mode cd)) cd)))

;  future sets a CD to a future time.

(defun future (cd)
  (setrole 'time 'future cd))

;; 151109 what about past and present? And let's provide a way to 
;; forceably impose the current default tense

(defun past (cd) (setrole 'time 'past cd))

(defun present (cd) (setrole 'time 'present cd))

(defun impose-default-tense (cd)
  (case *default-tense*
    (present (present cd))
    (future (future cd))
    (otherwise (past cd))))


;  Path
;
;  cdpath finds the filler at the end of the role list in a CD.
;
;  For example, if
;  CD = (mtrans (actor joe)
;               (object (ptrans (actor joe) 
;                               (object worm)
;                               (from joe)
;                               (to irving))))
;  then
;  (cdpath '(actor) cd) returns joe;
;  (cdpath '(object) cd) returns (ptrans (actor joe) 
;                                        (object worm)
;                                        (from joe)
;                                        (to irving));
;  (cdpath '(object object) cd) returns worm.
;
;  If a role doesn't exist in a CD form, then cdpath returns nil.

(defun cdpath (rolelist cd)
  (if (null rolelist)
    cd
    (cdpath (cdr rolelist) (filler-role (car rolelist) cd))))


;  CD Functions

;  is-cd-p determines whether a given sexpr is a CD.

(defun is-cd-p (x)
  (and (listp x)
       (atom (header-cd x))
       (list-of-role-filler-pairs-p (roles-cd x))))

(defun list-of-role-filler-pairs-p (x)
  (or (null x)
      (and (listp x)
           (listp (car x))
           (atom (role-pair (car x)))
           (list-of-role-filler-pairs-p (cdr x)))))

;  header-cd gets the head act of a CD form.

(defun header-cd (x) (car x))

;  roles-cd gets the list of role-pairs of a CD form.

(defun roles-cd (x) (cdr x))

;  Role-pairs have the form (role filler).
;  role-pair returns the role.

(defun role-pair (x) (car x))

;  filler-pair returns the filler.

(defun filler-pair (x) (cadr x))

;  A filler for a role is found by looking for the role name in the CD,
;  and returning the filler if a pair is found.

(defun filler-role (role cd)
  (if (listp cd)
    (let ((pair (assoc role (roles-cd cd))))
      (if pair (filler-pair pair)))))

;  setrole makes a new CD form with (role filler) added
;  or replacing the old (role ...) pair. 

(defun setrole (role filler cd)
  (cons (header-cd cd)
        (cons (list role filler)
              (remove-if #'(lambda (pair)
                             (eq (car pair) role))
                         (roles-cd cd)))))

;  Pattern Unifier
;  This unifier is an adapted version of the unify function which appears
;  in the book _Artificial_Intelligence_Programming_ (2nd ed.)
;  Eugene Chaniak, Drew McDermott, and James Meehan.

(defun unify (Pat1 Pat2)
  (unify-1 Pat1 Pat2 NIL))

(defun unify-1 (Pat1 Pat2 Sub)
  (cond ((pcvar-p Pat1)
         (var-unify Pat1 Pat2 Sub))
        ((pcvar-p Pat2)
         (var-unify Pat2 Pat1 Sub))
        ((atom Pat1)
         (cond ((eql Pat1 Pat2) (list Sub))
               (T NIL)))
        ((atom Pat2) 
         NIL)
        (T
         (mapcan #'(lambda (Sub)
                     (unify-1 (cdr Pat1) (cdr Pat2) Sub))
                 (unify-1 (car Pat1) (car Pat2) Sub)))))

(defun var-unify (PCVar Pat Sub)
  (cond ((or (eql PCVar Pat)
             (and (pcvar-p Pat)
                  (eql (pcvar-id PCVar)
                       (pcvar-id Pat))))
         (list Sub))
        (T
         (let ((Binding (pcvar-binding PCVar Sub)))
           (cond (Binding
                  (unify-1 (binding-value Binding) Pat Sub))
                 ((and *OccursCheck-P*
                       (occurs-in-p PCVar Pat Sub))
                  NIL)
                 (T
                  (list (extend-binding PCVar Pat Sub))))))))

(defun occurs-in-p (PCVar Pat Sub)
  (cond ((pcvar-p Pat)
         (or (eq (pcvar-id PCVar) (pcvar-id Pat))
             (let ((Binding (pcvar-binding Pat Sub)))
               (and Binding
                    (occurs-in-p PCVar (binding-value Binding) Sub)))))
        ((atom Pat)
         NIL)
        (T
         (or (occurs-in-p PCVar (car Pat) Sub)
             (occurs-in-p PCVar (cdr Pat) Sub)))))

(defun pcvar-binding (PCVar AList)
  (assoc (pcvar-id PCVar) AList))

(defun extend-binding (PCVar Pat AList)
  (cons (list (pcvar-id PCVar) Pat)
        AList))

(defun binding-value (Binding) (cadr Binding))

(defun pcvar-value (Pat Sub)
  (let ((Binding (pcvar-binding Pat Sub)))
    (cond ((null Binding) 
           Pat)
          (T
           (let ((Value (binding-value Binding)))
             (cond ((eql Value Pat)
                    Pat)
                   (T
                    (replace-variables Value Sub))))))))

(defun replace-variables (Pat Sub)
  (cond ((pcvar-p Pat)
         (pcvar-value Pat Sub))
        ((atom Pat)
         Pat)
        (T
         (cons (replace-variables (car Pat) Sub)
               (replace-variables (cdr Pat) Sub)))))

(defun instantiate (Pat Subs)
  (cond ((pcvar-p Pat)
         (let ((Entry (assoc (pcvar-id Pat) Subs)))
           (if Entry 
             (instantiate (cadr Entry) Subs)
             Pat)))
        ((atom Pat)
         Pat)
        (T
         (cons (instantiate (car Pat) Subs)
               (instantiate (cdr Pat) Subs)))))

;  CD Unifier
;  This replaces the less-general CD pattern matcher that was
;  used in the original Micro-Talespin program.  This unifier
;  allows pattern variables to appear on both of the
;  expressions to be compared while a pattern matcher
;  only allows variables to appear in one of the expressions.

(defun unify-cds (cd1 cd2)
  (unify-cds-1 cd1 cd2 nil))

(defun unify-cds-1 (cd1 cd2 sub)
  (and (eq (header-cd cd1) (header-cd cd2))
       (unify-pairs (roles-cd cd1) (roles-cd cd2) sub)))

;  unify-pairs sees if the roles and fillers of a CD can
;  be matched together.  It is more complicated than the
;  function unify-1 given above because (1) the role-filler pairs
;  do not need to be in the same order in the two CDs being
;  compared; (2) a missing pair in one CD means that that CD
;  is more general than the other CD and can, thus, be matched
;  against it; and, finally, (3) the filler of a pair can be a CD,
;  and most fillers which are lists are CDs, however, fillers which
;  are "modes" are the exception; they are fillers which are lists,
;  but are not CDs, so a special exception has to be made for them
;  in the unification procedure below.

(defun unify-pairs (pairs1 pairs2 sub)
  (if (or (null pairs1) (null pairs2))
    (list sub)
    (let* ((role 
            (role-pair (car pairs1)))
           (pair-from-pairs2
            (assoc role pairs2))
           (rest-of-pairs-from-pairs2
            (remove-if #'(lambda (pair)
                           (equal (role-pair pair) role))
                       pairs2))
           (newsubs
            (cond ((eq role 'mode)
                   (unify-1 (car pairs1) pair-from-pairs2 sub))
                   ((and pair-from-pairs2
                        (or (pcvar-p (cadr pair-from-pairs2))
                            (atom (cadr pair-from-pairs2))))
                   (unify-1 (car pairs1) pair-from-pairs2 sub))
                  ((and pair-from-pairs2
                        (or (pcvar-p (cadr (car pairs1)))
                            (atom (cadr (car pairs1)))))
                   (unify-1 (car pairs1) pair-from-pairs2 sub))
                  (pair-from-pairs2
                   (unify-cds-1 (car pairs1) pair-from-pairs2 sub))
                  (t
                   (list sub)))))
      (mapcan #'(lambda (newsub)
                  (unify-pairs (cdr pairs1)
                               rest-of-pairs-from-pairs2
                               newsub))
              newsubs))))

;  Done loading
;(format t "~%;Done loading talesim.")

