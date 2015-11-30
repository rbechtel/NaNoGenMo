; Test functions for AO/NNGM
; MIT license, Creative Commons Attribution-ShareAlike 4.0 International License
; 
; Probably should use some readily available testing framework (lisp-unit, Stefil, FiveAM)
; but for now I'm more interested in "testing" that generates output for me to examine,
; rather than checking that the output matches some target.

;;; 151101: First tests (that I should have had before) are simply to check that
;;; we generate something reasonable for any "valid" CD input.

;;; To test for reasonable generation over (an easy-to-generate set of) CD structures,
;;; we need to generate CD structures and run them through the language generation process.
;;; Generating can be done using the various construction functions found in
;;; micro-talesim, then we can call say-immediate on each one. To fill in the slots
;;; in the CDs, we can do init-world and then use *personae*, *all-locations*, *all-objects*
;;; (and possibly *goals*, though that may be a stretch).

; (in-package "NNGM") ; removed for development/debug 151108

; Some useful lists of CD types

(setf *cd-actions* '(atrans cause grasp un-grasp ingest mbuild mtrans plan propel ptrans wants))
(setf *cd-states* '(has is-at mloc state relation)) ; where-is who-has)) ; issues with pattern variables in packages
(setf *cd-modes* '(pos neg maybe))
(setf *cd-tense* '(future present past))
(setf *state-fillers* '(hungry thirsty health smart))
(setf *mode-fillers* '(pos neg)) ; other possibilities are tf and ques, but we generate those explicitly
(setf *relation-fillers* '(deceive dominate like))

; What do CD construction functions look like?
;  Acts
; (defun atrans (actor object to from)
; (defun cause (ante conseq) ; ante and conseq should be CDs, probably ante an action and conseq a state, but not required
; (defun grasp (actor object)
; (defun un-grasp (actor object)
; (defun ingest (actor object)
; (defun mbuild (actor object) ; object should be a CD
; (defun mtrans (actor object to from) ; object should be a CD
; (defun plan (actor object) ; object should be an action CD (?)
; (defun propel (actor object to)
; (defun ptrans (actor object to from)
; (defun wants (actor goal) ; object should be a state CD?
;  States
; (defun has (actor object)
; (defun is-at (actor loc)
; (defun mloc (actor con) ; con should be a CD
; (defun state (actor st mode) ; state should be one of hungry, thirsty, health, smart, tired?, lonely?, horny?
; (defun relation (actor object rel mode) ; rel should be one of deceive, dominate, like
; (defun where-is (thing)  ; could be actor, could be object
; (defun who-has (actor) 

(put 'atrans 'gen-fun #'atrans) (put 'atrans 'gen-args '(actor object actor actor)) ; originally actor object to from
(put 'cause 'gen-fun #'cause) (put 'cause 'gen-args '(concept concept)) ; originally ante conseq
(put 'grasp 'gen-fun #'grasp) (put 'grasp 'gen-args '(actor object))
(put 'un-grasp 'gen-fun #'un-grasp) (put 'un-grasp 'gen-args '(actor object))
(put 'ingest 'gen-fun #'ingest) (put 'ingest 'gen-args '(actor object))
(put 'mbuild 'gen-fun #'mbuild) (put 'mbuild 'gen-args '(actor concept)) ; originally actor object
(put 'mtrans 'gen-fun #'mtrans) (put 'mtrans 'gen-args '(actor concept actor actor)) ; originally actor object to from
(put 'plan 'gen-fun #'plan) (put 'plan 'gen-args '(actor concept)) ; originally actor object
(put 'propel 'gen-fun #'propel) (put 'propel 'gen-args '(actor object loc)) ; originally actor object to
(put 'ptrans 'gen-fun #'ptrans) (put 'ptrans 'gen-args '(actor object loc loc)) ; originally actor object to from
(put 'wants 'gen-fun #'wants) (put 'wants 'gen-args '(actor concept)) ; originally actor goal

(put 'has 'gen-fun #'has) (put 'has 'gen-args '(actor object))
(put 'is-at 'gen-fun #'is-at) (put 'is-at 'gen-args '(actor loc))
(put 'mloc 'gen-fun #'mloc) (put 'mloc 'gen-args '(actor concept)) ; originally actor con
(put 'state 'gen-fun #'state) (put 'state 'gen-args '(actor st mode))
(put 'relation 'gen-fun #'relation) (put 'relation 'gen-args '(actor actor rel mode)) ; originally actor object rel mode
(put 'where-is 'gen-fun #'where-is) (put 'where-is 'gen-args '(object)) ; originally thing
(put 'who-has 'gen-fun #'who-has) (put 'who-has 'gen-args '(actor))

; Need to test all CD action and state forms.

; Hmm. We can just stuff CDs into *story-sequence* then invoke recite
; to run the generator - no need to do say-immediate, unless testing
; individual CD constructions.

(defun test-generation-from-cds ()
  (init-world)
  (mapc #'generate-by-tense '(past present future)) )

(defun generate-by-tense (tns)
  (let ((starting-tense *default-tense*))
    (setf *story-sequence* nil)
    (generate-test-cds)
    (recite)))
    
; Stuffs a bunch of CDs into *story-sequence* by using gen-cd-variants to produce
; examples of all possible CD types.

(defun generate-test-cds ()
  (setf *story-sequence*
        (append
         (mapcan #'gen-cd-variants
                 (append *cd-actions* *cd-states*))
         *story-sequence*)))

; Generates variants of a single CD type. Starts with a simple one using
; randomly generated arguments of an appropriate type, then adds that to
; a list of variations on the base example.

(defun gen-cd-variants (cdtype)
  (let ((cd-gen-fun (get cdtype 'gen-fun))
        (cd-gen-args (get cdtype 'gen-args))
        cd-arg-list base-cd)
    (setf cd-arg-list
          (mapcar #'generate-slot-test-filler
                  cd-gen-args))
    (setf base-cd (apply cd-gen-fun cd-arg-list))
    (append (list base-cd) (ring-cd-changes base-cd))))

; Given a basic CD, produces a list of variations on that CD
; Variations within each form
; - null slot fillers
; - compound slot fillers (e.g., CDs)
; - invalid slot fillers [not sure what this means]
; - questions
; - future
; - hypothetical
; - negation
; Probably not doing the first three yet.

;  Mode functions
; (defun mode (cd)
;  Affirm/Negate set the mode of a CD to true/false.
; (defun affirm (cd)
; (defun negate (cd)
;  maybe makes a CD hypothetical -- doesn't matter if it's true or false.
; (defun maybe (cd)
;  question/un-question make a CD a question/non-question -- doesn't matter if it's true or false.
; (defun question (cd)
; (defun un-question (cd)
;  tf adds "transition final" to a CD -- doesn't matter if it's true or false.
; (defun tf (cd)
;  future sets a CD to a future time.
; (defun future (cd)

(defun ring-cd-changes (cd)
  (list (affirm cd)
        (negate cd)
        (maybe cd)
        (question cd)
        (tf cd)
        (future cd)))

; Need fillers for generated CD slots. Try to make up some that are at
; least of an appropriate type.

(defun generate-slot-test-filler (slottype)
  (case slottype
    (actor (random-choice *personae*))
    (loc (random-choice *all-locations*))
    (st (random-choice *state-fillers*))
    (mode (random-choice *mode-fillers*))
    (rel (random-choice *relation-fillers*))
    (concept (is-at 'joe 'river))
    (otherwise (random-choice *all-objects*))))

; Multi-episode testing

;; Two episodes - Joe's thirsty, then hungry
;; Outcome of being hungry will depend on Joe's relations with others
;; which are (mostly) randomly set.

(defun test-simple-multi-episode ()
  (spin-episode '(joe thirsty) nil t) ; story, no world-state, first-episode? T
  (spin-episode '(joe hungry)))

;; Just tell all the pre-canned stories

(defun test-stories-in-sequence ()
  (spin-episode *story1* nil t)
  (spin-episode *story2*)
  (spin-episode *story3*)
  (spin-episode *story4*)
  (spin-episode *story5*)
  (spin-episode *story6*)
  (spin-episode *story7*))

