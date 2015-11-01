;  micro-mumble: micro English generator
;
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

; The primary external interface to micro-mumble (MM) is the function
; SAY. The expected argument to SAY is a conceptual dependency structure.
;
; 151015: I've pulled the micro-mumble functionality out of the 
; main micro-Talespin file so I can focus on actual generation
; issues without getting too tangled up in the event generation
; parts.
; Looks like all actual output is done through calls to format, so
; let's start looking at those.
; - Now to start making changes.
;   1. Case issues.
;      Simplest thing to do is to call string-downcase
;      on every word. Clearly wrong for proper nouns and sentence-
;      initial items. We'll start there for now, however, because
;      we can do it in the format calls by using ~(~).

;  Set the storytelling in the past tense.

(in-package "NNGM")

; In the original version,
;  say prints a CD as an English sentence.  If CD is an mloc of the
;  world, then only the fact itself is said, otherwise the whole mloc
;  is used.  The original CD is returned.  say1 is called with the 
;  infinitive flag off and the say-subject flag on.

;;; 151016: We're going to change things a bit. Instead of actually
;;; uttering something on every call to SAY, instead it will just
;;; add its argument to a variable *story-sequence*. I'll pop over to
;;; micro-talespin-simulator (which I think I'll rename micro-talesim)
;;; and insert a call to a newly written function RECITE that
;;; will work on *story-sequence*. At first, RECITE will just map
;;; a new SAY-ONE over (reverse *story-sequence*), but having the entire 
;;; sequence available before rendering anything will allow lots of
;;; new and interesting manipulations.

(defun say (cd) (setf *story-sequence* (cons cd *story-sequence*)))

;; Since stuff just gets consed onto *story-sequence*, you want to 
;; reverse it before playing it back. You also want to clear it
;; after reciting.

(defun recite (&optional keep-sequence)
  (mapc #'say-thing (reverse *story-sequence*))
  (unless keep-sequence (setf *story-sequence* nil))
  )

;; Revised things in micro-talesim so that all story output goes
;; into *story-sequence*, so we need to be able to handle strings
;; in the *story-sequence* without trying to treat them as CDs.
;; 151017: Perhaps one should also allow "markers" in the form of
;;         atoms, to indicate, e.g., scene shifts. How they get
;;         handled is TBD, but for now, we'll tweak SAY-THING
;;         so it doesn't break.

(defun say-thing (thing)
  (cond ((stringp thing) (format t thing))
        ((atom thing) (say2 thing)) ; SAY2 is TBD at this point
        (t (say-thing1 thing))))    ; and here's the original stuff

;; SAY-THING1 is never called recursively, so we can add a "at beginning" flag
;; when it invokes SAY1.

(defun say-thing1 (cd)  ; in the original, was just SAY
  (let ((cd-to-be-said (if (unify-cds '(mloc (val (cp (part world)))) cd)
                         (cdpath '(con) cd)
                         cd)))
    (format t "~%") ; original
    (say1 cd-to-be-said 
          (or (cdpath '(time) cd-to-be-said)
              *default-tense*)
          nil
          t
          t) ; 151020 - add flag that indicates this is a top-level call
    (format t ".")
    cd))

;; 151018: However, moving to delayed surface form generation means that
;; we can't use SAY to generate prompts to the user, as FIND-OUT in talesim
;; wants to. So, introduce a new SAY-IMMEDIATE function that's just a
;; wrapper around SAY-THING.

(defun say-immediate (thing) (say-thing thing))

;  say1 prints cd according to the program under the head predicate.
;  If no program is there, the CD is printed with <>s around it.
;  
;  These generation programs are lists of expressions to be evaluated.
;  Attached to primitive acts, they are normally concerned with
;  generating subject-verb-object clauses.  Since some of the acts,
;  such as mtrans, want and plan, take subclauses, the generator has to
;  be recursive, so that the atrans program that generates the clause
;  "Joe gave Irving the worm" can also generate the subclause in
;  "Joe planned to give Irving the worm." This means that the programs have
;  to know when to say or not say the subject, when to use the 
;  infinitive form, and what tense to use.
;    subj = true means print the subject,
;    inf = true means use the infinitive form,
;    tense is set to either past, present, or future, or cond (for
;            conditional, i.e., hypothetical)
;; 151024 added optional mentioned to support pronominalization

(defun say1 (cd tense inf subj &optional at-start mentioned)
  (if cd
    (let ((say-fun (get (header-cd cd) 'say-fun)))
      (if say-fun 
        (apply say-fun (list cd tense inf subj at-start mentioned))
        (format t "~% < ~s > " cd)))))

;; SAY2 handles atomic markers. They don't exist in the original MTS.
;; MTS did have the concept, in the form of "Once upon a time...", 
;; "One day,", and "The end.", and just used format to get them
;; out. Initially modified SAY-THING to just dump strings when 
;; encountered, but a more general case would be to note that these
;; strings indicate a story part boundary - intro, story start, story end.
;; It seems likely, especially as stories get more complex, that there
;; could be other markers, so we'll allow atomic markers and figure out
;; what to do with them here. For now, do nothing.

(defun say2 (atom) nil)

;  subclause recursively calls say1 with the subconcept at the 
;  endpoint of rolelist.  word, if non-nil, starts the subclause,
;  unless relative-pronoun has a better idea.  Tense is calculated 
;  by sub-tense.
;; 151017 - suspect we don't want a leading space, so removing
;;          doing surface-prep and changing from ~s to ~a
; 151020 add optional AT-START flag
; 151024 add optional mentioned list - these are things that
;   have been mentioned in a parent clause (like a subject)
;   we can pronominalize them now.

(defun subclause (cd word rolelist tense &optional at-start mentioned)
  (if word
      (if at-start
          (format t "~@(~a~) "
                  (surface-prep (or (relative-pronoun rolelist cd)
                                    word)))
          (format t "~a "
                  (surface-prep (or (relative-pronoun rolelist cd)
                                    word)))))
  (let ((subcd (cdpath rolelist cd)))
    (say1 subcd (sub-tense tense subcd) nil t at-start mentioned)))

;  sub-tense is given a tense and a CD and picks the tense to use.
;  The given tense is used, except with states (i.e., don't
;  say "he told him where the honey would be" even though conceptually
;  that's right), and with past statements about the future (i.e., say
;  "he said he would" rather than "he said he will").

(defun sub-tense (tense subcd)
  (cond ((is-state subcd)
         *default-tense*)
        ((and (equal tense 'past)
              (equal (cdpath '(time) subcd) 'future))
         'cond)
        (t tense)))

;  relative-pronoun returns the word to start the subclause
;  for the CD at the end of the CD role path.

(defun relative-pronoun (rolelist cd)
  (let ((subcd (cdpath rolelist cd)))
    (cond ((and (equal (header-cd subcd) 'loc)
                (pcvar-p (cdpath '(val) subcd)))
           'where)
          ((pcvar-p (next-subject cd)) 
           'who)
          (t
           nil))))

;  next-subject returns the subject of a subconcept, which is normally
;  the actor slot, except for cont (where it's in the val slot) and
;  mloc (where it's in the part slot of the val slot).

(defun next-subject (cd)
  (let ((subcd (cdpath '(object) cd)))
    (cdpath (case (header-cd subcd)
              (cont '(val))
              (mloc '(val part))
              (t '(actor)))
            subcd)))

;  infclause calls recursively say1 with the subconcept at the
;  endpoint of rolelist.  An infinitive is printed, and the subject
;  is suppressed.

(defun infclause (cd rolelist subj-flag tense &optional at-start mentioned)
  (say1 (cdpath rolelist cd) tense t subj-flag at-start mentioned))

;  Store say-funs for each of the CD forms

;  atrans may go to either "take" (if actor = to) or "give."
; 151020 add optional AT-START flag
; 151020: Check to see if there's a "whom" that object is taken from, and don't
;         generate more if not

(defun say-atrans (cd tense inf subj &optional at-start mentioned)
  (let ((mention2 (cons (cdpath '(actor) cd) mentioned)))
    (cond ((equal (cdpath '(actor) cd) (cdpath '(to) cd))
           (say-subj-verb cd tense inf subj '(actor) 'take at-start mentioned)
           (say-filler cd '(object) nil mention2)
           (when (cdpath '(from) cd) ; there's actually a "whom" that object was taken from
             (format t " ") ; 151018 hack - need space after you say what you're taking
             (say-prep cd 'from '(from) nil mention2)
             (format t " ") ; 151018 hack - need space after you say who you're taking from
                            ; this will need to be conditional on whether you're at the
                            ; end of a sentence or not - if at end, don't add space
             ))
          (t
           (say-subj-verb cd tense inf subj '(actor) 'give at-start mentioned)
           (say-filler cd '(to) nil mention2 'obj)
           (format t " ") ; 151018 hack - need space after you say who you're giving to
           (say-filler cd '(object) nil mention2 'obj)
           (format t " ") ; 151018 hack - need space after you say what you're giving
                          ; this will need to be conditional on whether you're at the
                          ; end of a sentence or not - if at end, don't add space
           ))))

(put 'atrans 'say-fun #'say-atrans)

;  mtrans may go to either "ask whether" or "tell that"
; 151020 add optional AT-START flag

(defun say-mtrans (cd tense inf subj &optional at-start mentioned)
  (let ((mention2 (cons (cdpath '(actor) cd) mentioned)))
    (cond ((member 'ques (cdpath '(object mode) cd))
           (say-subj-verb cd tense inf subj '(actor) 'ask at-start mentioned)
           (say-filler cd '(to part) nil mention2)
           (format t " ") ; 151018 hack - need space after you say who you've asked
           (subclause cd 'whether '(object) 'cond nil mention2))
          (t
           (say-subj-verb cd tense inf subj '(actor) 'tell at-start mentioned)
           (say-filler cd '(to part) nil mention2 'obj)
           (format t " ") ; 151017: need a break between who to tell and what
                          ; might need something similar on ask branch
           (subclause cd 'that '(object) (cdpath '(time) cd) nil mention2)))))

(put 'mtrans 'say-fun #'say-mtrans)

;  ptrans may go to either "go" or "move."
; 151020 add optional AT-START flag

(defun say-ptrans (cd tense inf subj &optional at-start mentioned)
  (let ((mention2 (cons (cdpath '(actor) cd) mentioned)))
    (cond ((equal (cdpath '(actor) cd)
                  (cdpath '(object) cd))
           (say-subj-verb cd tense inf subj '(actor) 'go at-start mentioned))
          (t
           (say-subj-verb cd tense inf subj '(actor) 'move at-start mentioned)
           (say-filler cd '(object) nil mention2)))
    (say-prep cd 'to '(to) nil mention2)))

(put 'ptrans 'say-fun #'say-ptrans)

;  mbuild may go to either "decide to" or "decide that."
; 151020 add optional AT-START flag

(defun say-mbuild (cd tense inf subj &optional at-start mentioned)
  (let ((mention2 (cons (cdpath '(actor) cd) mentioned)))
  (say-subj-verb cd tense inf subj '(actor) 'decide at-start mentioned)
  (cond ((equal (cdpath '(actor) cd)
                (cdpath '(object actor) cd))
         (infclause cd '(object) nil 'future nil mention2))
        (t
         (subclause cd 'that '(object) 'future nil mention2)))))

(put 'mbuild 'say-fun #'say-mbuild)

;  propel goes to strike
; 151020 add optional AT-START flag

(defun say-propel (cd tense inf subj &optional at-start mentioned)
  (let ((mention2 (cons (cdpath '(actor) cd) mentioned)))
    (say-subj-verb cd tense inf subj '(actor) 'strike at-start mentioned)
    (say-filler cd '(to) nil mention2)))

(put 'propel 'say-fun #'say-propel)

;  grasp may go to either "let go of" or "grab."
; 151020 add optional AT-START flag

(defun say-grasp (cd tense inf subj &optional at-start mentioned)
  (let ((mention2 (cons (cdpath '(actor) cd) mentioned)))
    (cond ((in-mode cd 'tf)
           (say-subj-verb cd tense inf subj '(actor) 'let at-start mentioned)
           (format t " go of  "))
          (t
           (say-subj-verb cd tense inf subj '(actor) 'grab at-start mentioned)))
    (say-filler cd '(object) nil mention2)))

(put 'grasp 'say-fun #'say-grasp)

;  ingest may go to either "eat" or "drink."
; 151020 add optional AT-START flag

(defun say-ingest (cd tense inf subj &optional at-start mentioned)
  (let ((mention2 (cons (cdpath '(actor) cd) mentioned)))
    (say-subj-verb cd tense inf subj '(actor)
                   (if (equal (cdpath '(object) cd) 'water)
                       'drink
                     'eat)
                   at-start mentioned)
    (say-filler cd '(object) nil mention2 'obj)))

(put 'ingest 'say-fun #'say-ingest)

;  plan goes to "plan."
; 151020 add optional AT-START flag

(defun say-plan (cd tense inf subj &optional at-start mentioned)
  (let ((mention2 (cons (cdpath '(actor) cd) mentioned)))
    (say-subj-verb cd tense inf subj '(actor) 'plan at-start mentioned)
    (infclause cd '(object) nil 'future nil mention2)))

(put 'plan 'say-fun #'say-plan)

;  want goes to "want to" -- the third argument of infclause is set to 
;  true if the subject of the subclause is different that the subject
;  of the main clause.

; 151020 add optional AT-START flag

(defun say-want (cd tense inf subj &optional at-start mentioned)
  (let ((mention2 (cons (cdpath '(actor) cd) mentioned)))
    (say-subj-verb cd tense inf subj '(actor) 'want at-start mentioned)
    (infclause cd 
               '(object) 
               (not (equal (cdpath '(actor) cd)
                           (next-subject cd))) 
               'future
               at-start
               mention2)))

(put 'want 'say-fun #'say-want)

;  loc goes to "be near."
; 151020 add optional AT-START flag

(defun say-loc (cd tense inf subj &optional at-start mentioned)
  (let ((mention2 (cons (cdpath '(actor) cd) mentioned)))
    (say-subj-verb cd tense inf subj '(actor) 'be at-start mentioned)
    (or (pcvar-p (cdpath '(val) cd))
        (say-prep cd 'near '(val) nil mention2))))

(put 'loc 'say-fun #'say-loc)

;  cont goes to "have."
; 151020 add optional AT-START flag

(defun say-cont (cd tense inf subj &optional at-start mentioned)
  (let ((mention2 (cons (cdpath '(val) cd) mentioned)))
  (say-subj-verb cd tense inf subj '(val) 'have at-start mentioned)
  (say-filler cd '(actor) nil mention2)))

(put 'cont 'say-fun #'say-cont)

;  mloc may go to either "know that", "know whether", or "think that."
; 151020 add optional AT-START flag
; 151024 add (list subj) to subclause call - this is to
;  communicate that we've already mentioned the subj,
;  so it could be pronominalized later in this sentence

(defun say-mloc (cd tense inf subj &optional at-start mentioned)
  (let ((mention2 (cons (cdpath '(val part) cd) mentioned)))
;    (format t "~%SUBJ: ~A~%INF: ~A~%TENSE: ~A~%MENTIONED: ~A~%CD: ~A~%" subj inf tense mentioned cd)
    (say-subj-verb cd 
                   tense 
                   inf 
                   subj 
                   '(val part)
                   (if (or (relative-pronoun '(con) cd)
                           (is-true (cdpath '(con) cd)))
                       'know
                       'think)
                   at-start mentioned)
    (subclause cd 'that '(con) *default-tense* nil mention2)))

(put 'mloc 'say-fun #'say-mloc)

;; 151017: For these verb complement forms, removing leading space.
;;         There is an issue with the trailing space when they
;;         appear at the end of a sentence, but not dealing with 
;;         that right now.
;; 151020: Removing trailing space. We'll want to test to see if
;;         we're at the end, and if not, add a space. That will
;;         come later, after we see the need.

;  health goes to "be alive"(defun say-want (cd tense inf subj)
; 151020 add optional AT-START flag

(defun say-health (cd tense inf subj &optional at-start mentioned)
  (say-subj-verb cd tense inf subj '(actor) 'be at-start mentioned)
  (format t "alive"))

(put 'health 'say-fun #'say-health)

;  smart goes to "be bright"
; 151020 add optional AT-START flag

(defun say-smart (cd tense inf subj &optional at-start mentioned)
  (say-subj-verb cd tense inf subj '(actor) 'be at-start mentioned)
  (format t  "bright"))

(put 'smart 'say-fun #'say-smart)

;  hungry goes to "be hungry"
; 151020 add optional AT-START flag

(defun say-hungry (cd tense inf subj &optional at-start mentioned)
  (say-subj-verb cd tense inf subj '(actor) 'be at-start mentioned)
  (format t  "hungry"))

(put 'hungry 'say-fun #'say-hungry)

;  thirsty goes to "be thirsty"
; 151020 add optional AT-START flag

(defun say-thirsty (cd tense inf subj &optional at-start mentioned)
  (say-subj-verb cd tense inf subj '(actor) 'be at-start mentioned)
  (format t "thirsty"))

(put 'thirsty 'say-fun #'say-thirsty)

;; 151017: removing leading spaces, as with verb complements

;  cause may go to either "x if y" or "if x then y"
; 151020 add optional AT-START flag

(defun say-cause (cd tense inf subj &optional at-start mentioned)
  (let (mention2)
    (declare (ignore inf))
    (declare (ignore subj))
    (cond ((in-mode cd 'ques)
           (subclause cd nil '(conseq) 'future at-start mentioned)
           (format t "if ")
           (subclause cd nil '(ante) (case tense
                                       (figure 'present)
                                       (cond *default-tense*)
                                       (t tense))
                      nil
                      (cons (cdpath '(conseq actor) cd) mentioned)))
          (t
           (if at-start (format t "If") (format t "if "))
           (subclause cd nil '(ante) 'future nil mentioned)
           (format t "then ")
           (subclause cd nil '(conseq) 'cond nil ; mentioned)))))
                      (cons (cdpath '(ante actor) cd) mention2))))))

(put 'cause 'say-fun #'say-cause)

;  like goes to "like"
; 151020 add optional AT-START flag

(defun say-like (cd tense inf subj &optional at-start mentioned)
  (let ((mention2 (cons (cdpath '(actor) cd) mentioned)))
    (say-subj-verb cd tense inf subj '(actor) 'like at-start mentioned)
    (say-filler cd '(to) nil mention2 'obj)))

(put 'like 'say-fun #'say-like)

;  dominate goes to "dominate"
; 151020 add optional AT-START flag

(defun say-dominate (cd tense inf subj &optional at-start mentioned)
  (let ((mention2 (cons (cdpath '(actor) cd) mentioned)))
    (say-subj-verb cd tense inf subj '(actor) 'dominate at-start mentioned)
    (say-filler cd '(to) nil mention2 'obj)))

(put 'dominate 'say-fun #'say-dominate)

;  deceive goes to "deceive"
; 151020 add optional AT-START flag

(defun say-deceive (cd tense inf subj &optional at-start mentioned)
  (let ((mention2 (cons (cdpath '(actor) cd) mentioned)))
    (say-subj-verb cd tense inf subj '(actor) 'deceive at-start mentioned)
    (say-filler cd '(to) nil mention2 'obj)))

(put 'deceive 'say-fun #'say-deceive)

;  say-filler prints the CD at the end of a CD role path
(defun say-filler (cd rolelist &optional at-start mentioned pcase)
  (say-pp (cdpath rolelist cd) at-start mentioned pcase))

;  say-pp prints a CD (adds "the" to object).
;;; 151016 - this is kind of the lowest level thing printer
;;;          so this is where we should determine the surface
;;;          form and store it... introduce helper fn
;;; 151015: removed space before "the" in format call
;;; 151020: add optional at-start
;;; 151024: what if this has already been mentioned? Try pronominalizing unless
;;;   it's a member of *all-objects* (should be able to pronominalize those too.

(defun say-pp (cd &optional at-start mentioned (pcase 'subj))
  (cond ((and at-start (member cd *all-objects*))    ; you're at the start of a sentence. Capitalize.
         (format t "The ")
         (format t "~a" (surface-prep cd))) ; changed ~s to ~a because surface-prep will give us a string
        (at-start ; but not an object
         (format t "~@(~a~)" (surface-prep (if (member cd mentioned)
                                               (pronominalize cd pcase)
                                             cd)))) ; capitalize
        ((member cd *all-objects*)
         (format t "the ")
         (format t "~a" (surface-prep cd)))
        (t (format t "~a" (surface-prep (if (member cd mentioned)
                                            (pronominalize cd pcase)
                                          cd))))))

;; 151024 - this is clearly inadequate. Working on case and plurals.
;;   Plurals are currently marked on the 'plural property of a word

(defun pronominalize (item &optional pcase)
  (let ((gender (get item 'gender)))
    (if (get item 'plural)
      (case pcase
        (subj 'they)
        (obj 'them)
        (poss 'their))
      (case gender
        (male 
         (case pcase
           (subj 'he)
           (obj 'him)
           (poss 'his)))
        (female 
         (case pcase
           (subj 'she)
           (otherwise 'her)))
        (otherwise 
         (case pcase
           (poss 'its)
           (otherwise 'it)))))))

;;; SURFACE-PREP generates a surface form for an atom. Usually,
;;; this is just a lowercase string, but if we're in *personae*
;;; then it will be a capitalized string.
;;; Surface form will be stored on the atom under the surface property,
;;; so look there first and only calculate if needed (and then store).

(defun surface-prep (atom)
  (let ((surface (get atom 'surface)))
    (unless surface
      (setf surface
            (if (member atom *personae*)
                (put atom 'surface (format nil "~@(~A~)" atom))
                (put atom 'surface (format nil "~(~A~)" atom)))))
    surface))

;  say-prep prints a preposition plus a CD at the end of a role path,
;  if any exists.
;;; 151016 removed leading space on format that prints the prep
;;; 151024 - here, we can be pretty sure that the case of any
;;;   pronominalization should be objective (e.g., "him/her")

(defun say-prep (cd prep rolelist &optional at-start mentioned (pcase 'obj))
  (let ((subcd (cdpath rolelist cd)))
    (cond (subcd
           (format t "~(~a~) " prep)
           (say-pp subcd at-start mentioned pcase)))))

;  in-mode tests whether x is in CD's mode.
(defun in-mode (cd x)
  (member x (cdpath '(mode) cd)))

;  say-neg prints "not" if CD is negative.
;;; 151015: Do we need the space preceding "not"?
;;; 151016: Removing space preceding "not"
;;; 151017: Putting in space after not, removing leading space 
;;          before "TO" in inf branch of SAY-SUBJ-VERB

(defun say-neg (cd)
  (if (in-mode cd 'neg)
    (format t "not ")))

;  say-subj-verb prints the subject (unless suppressed by
;  subj = nil, infinitives, or an ?unspec as the subject) and verb, 
;  with auxilary and tensing, if any.  Note that future tense is 
;  treated as an auxilary.
;; 151017: Removed leading space before "TO" in inf branch.
;; 151017: when in inf mode and subj is true, then put a space after you
;;         (SAY-PP SUBJECT). This is the space that was taken out of the
;;         emit "to ~A" - but if subj is false, don't want it.
;;         Just a matter of putting it in the right place.

(defun say-subj-verb (cd tense inf subj rolelist infinitive &optional at-start mentioned)
  (let* ((subject (cdpath rolelist cd))
         (mention2 (cons subject mentioned)))
          ; 151017: Interesting. If you're generating in infinitive mode, then
          ; any negation comes before the verb - "not to be thirsty", "not to
          ; tell ...". If you're not in infinitive mode, then if there's an
          ; auxiliary verb, you do <aux> <neg> <infinitive>, so "might not be",
          ; "will not tell...". Not in infinitive mode and no auxiliary,
          ; <infinitive>. Special case is where infinitive is "be" and you're
          ; in negation mode, so you'll say "be <neg>".
          ;
          ; Upshot is that there's a alternate not present - when in infinitive
          ; mode, you could do SAY-NEG after the infinitive (? works for BE, maybe
          ; not for other things?) Not making any changes yet.
    (cond (inf
           (when subj (say-pp subject at-start mentioned) (format t " "))
               ; 151020 grumble. If no subj but neg, at-start won't be handled properly
               ; also not handled if no subj, not neg. Need examples to work through
           (say-neg cd)
           (format t "to ~a " (surface-prep infinitive)))
          (t
           (if (not (pcvar-p subject)) 
             (say-pp subject at-start mentioned))
             ; same issue as under inf branch - what if pcvar-p subject? need to handle at-start
           (let ((plural 
                  (get subject 'plural))
                 (auxilary  ; 151017: Cheating is rife! This is supposed to figure out what auxiliary verb
                            ; to use, and it kind of does that, but the selections (and later processing)
                            ; are inconsistent. Because "do" is both an aux and main verb, it has an
                            ; entry in the tense table, so we can just pass 'do out as an aux and count
                            ; on say-tense to sort it out. The other auxiliaries (maybe/might, future 
                            ; will/would, conditional would), while irregular, don't have entries in
                            ; the tense table, so will not be modified by say-tense.
                            ; The problem is that because do as an aux has a tense table entry,
                            ; say-tense is adding an unneeded space after it.
                  (cond ((in-mode cd 'maybe)
                         'might)
                        ((equal tense 'future)
                         (if (equal *default-tense* 'past)
                           'would
                           'will))
                        ((equal tense 'cond)
                         'would)
                        ((and (in-mode cd 'neg)
                              (not (equal infinitive 'be)))
                         'do))))
             (cond (auxilary
                    (say-tense cd tense inf subj auxilary plural)
                    (unless (eq auxilary 'do) (format t " ")) ; 151017 heavy-handed hack
                    (say-neg cd)
                    (format t "~a " (surface-prep infinitive))) ; 151017 removed leading space
                   (t
                    (say-tense cd tense inf subj infinitive plural)
; 151015                    (format t " ") ; clear out some extra spaces
                                           ; but apparently needed after past tense verbs? told, struck?
                    (if (equal infinitive 'be) (say-neg cd)))))))))

;  say-tense prints a verb, with tense and number inflection.
;  Conjugations of irregular verbs are stored under the past and present
;  properties of the verb, in the format (singular plural) for each.
;  For regular verbs, say-tense adds "d", "ed", or "s" as appropriate.

(defun say-tense (cd tense inf subj infinitive plural)
  (declare (ignore cd))
  (declare (ignore inf))
  (declare (ignore subj))
  (let ((tense-forms (get infinitive tense))) ; only irregulars have tense forms
    (format t " ")
    (cond (tense-forms
           (format t "~a " (if plural ; 151016 added space after irregular verbs
                                          ; this fixed told and struck, broke others?
                                   (surface-prep (cadr tense-forms))
                                   (surface-prep (car tense-forms)))))
          (t
           (format t "~a" (surface-prep infinitive))
           (case tense
             (past
              (if (not (or (equal (lastchar infinitive) #\E)
                           (equal (lastchar infinitive) #\e)))
                (format t "e"))
              (format t "d "))
             (present
              (if (not plural)
                (format t "s "))))))))

;  lastchar returns that last character in x
(defun lastchar (x)
  (car (last (explode x))))

(defun explode (x)
  (coerce (princ-to-string x) 'list))

;  Generator Dictionary
;
;  Set the past and/or present tenses for irregular verbs.
;  Each tense is of the form (singular plural).

(put 'be 'past '(was were))
(put 'be 'present '(is are))
(put 'do 'past '(did did))
(put 'do 'present '(does do))
(put 'drink 'past '(drank drank))
(put 'eat 'past '(ate te))
(put 'give 'past '(gave gave))
(put 'go 'past '(went went))
(put 'go 'present '(goes go))
(put 'grab 'past '(grabbed grabbed))
(put 'have 'past '(had had))
(put 'have 'present '(has have))
(put 'know 'past '(knew knew))
(put 'let 'past '(let let))
(put 'might 'past '(might might))
(put 'might 'present '(might might))
(put 'plan 'past '(planned planned))
(put 'strike 'past '(struck struck))
(put 'take 'past '(took took))
(put 'tell 'past '(told told))
(put 'think 'past '(thought thought))

;  Berries is the only plural in the current set-up.
(put 'berries 'plural t)
