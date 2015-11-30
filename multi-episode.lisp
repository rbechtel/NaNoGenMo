; MIT license, Creative Commons Attribution-ShareAlike 4.0 International License
; 
; multi-episode.lisp
;
; Use the (updated) micro-talespin mechanisms to tell a series of stories.
;
; Each story (which is now an episode defstruct) is characterized by
; 1) a lead character
; 2) a problem faced by the lead character
; 3) a set of initial facts specific to this story, which should include
;    a) locations of characters and objects (might be able to use *initial-facts* for this)
;    b) attitudes of characters toward each other - at least for characters that will be involved
; These can be placed in a list of the form (protagonist problem fact1 ... factn) which
; can then be passed to (spin-tale <story>) to generate an episode. Each fact in a story
; definition is of the form (who-knows CD).

; (in-package "NNGM") ; removed for development/debug 151108

; Tell multiple episodes. (Thought I had a way to do this, but apparently not.)
; Makes no sense to keep *story-sequence* from episode to episode, since
; recite will play everything that's in *story-sequence*.
; Unclear if it's necessary to save/restore world state between episodes - try it out.

(defun spin-multiple-episodes (elist)
  (let (hold-state)
    (spin-episode (first elist) nil t nil) ; start the story
    (setf hold-state (capture-world-state))
    (do* ((episodes (rest elist) (rest episodes))
          (this-episode (first episodes) (first episodes)))
         ((null episodes) (say "And that's all."))
      (spin-episode this-episode hold-state nil)
      (setf hold-state (capture-world-state)))))
    
; SPIN-EPISODE is the SPIN-TALE replacement for individual episodes.
; Tell an episode. Has a flag (first-episode?) that
; should be non-NIL if this is the first sequence in a story, since you'll
; then do overall story/novel initialization before going on to the
; specifics of this episode.
;
; Also has an optional world-state, which is just a complete description of
; some state of the world - characters, mental content, locations, objects
;
; Logic -
;   clear *story-sequence* and insert a 'start-episode marker
;   if world-state -> recreate-world-state
;   else if first-episode? -> init-default-story-world and establish-fact-set *init-facts*
;   establish-fact-set story facts (which should put them in *story-sequence*)
;   insert a 'begin-action marker
;   set up main-character, problem
;   assert that the main-character has the problem, which should trigger forward chaining and saying
;   insert an end-story marker
;   call recite

(defun spin-episode (&optional story world-state first-episode? old-style? keep-sequence?)
  (let (story-parse motivations story-facts)
    (setf story-parse (parse-story-spec story)) ; 151121 pull story details from the specification
    (setf motivations (first story-parse))
    (setf story-facts (second story-parse))
    (setf *story-sequence* nil) ; 151107 might want to move this into an outer function
                                ; so you can generate multiple episodes before rendering
    (if first-episode? (setf *chapter-counter* 0))
    (say 'start-episode) ; originally (format t "~%Once upon a time ...")
    (cond (world-state                           ; when provided with a state of the world, use it
           (recreate-world-state world-state))
          (first-episode?                        ; no world state, but first episode, so set up defaults
           (init-default-story-world))
;           (establish-fact-set *init-facts*))
          (t                                     ; no world state, later episode, clear lingering goals and demons
           (wipe-goals)
           (wipe-demons)))
    (establish-fact-set (if first-episode?
                            ; this is a kludge to account for a change in how we do facts in *story10* and *story11*
                            (append (if old-style? (init-facts) (init-facts2)) story-facts)
                          story-facts))
    (say 'begin-action) ; originally (format t "~%One day, ")
    (apply #'assert-fact motivations)
;    (unless main-character (setf main-character (random-choice *personae*)))
;    (unless problem (setf problem (random-choice *goals*)))
;    (assert-fact (mloc 'world (state main-character problem 'pos)))
    (say 'end-episode) ; originally (format t "~%The end.")
    (recite keep-sequence?))) ; 151107 might want to move this into an outer function
                              ; so you can generate multiple episodes before rendering

;; 151121 Deal with an (extended) story specification. Original story specs are of the
;; form (main-character problem storyfacts) (and if either main-c or problem are nil, they'll
;; get picked at random, and if storyfacts is nil, there's just no story-specific facts!).
;; Extended story spec replaces main-character and problem with a list of the form 
;; ((character problem) ... (character problem)) so you can have multiple characters and/or
;; multiple problems.
;; parse-story-spec 
;; (a) figures out if there actually is a story spec (picking character and problem if not)
;; (b) if it's original or extended story spec
;;     original - create an appropriate MLOC out of main-character and problem, set up
;;                story-facts
;;     extended - create appropriate MLOCS out of (character problem) pairs, set up
;;                story-facts

(defun parse-story-spec (spec)
  (let (motivations story-facts)
    (cond ((null spec)  ; There isn't actually a story spec. Pick a character and problem
           (setf motivations
                 (list (mloc 'world (state (random-choice *personae*)
                                           (random-choice *goals*)
                                           'pos)))))
          ((atom (first spec)) ; This is an "original" story spec
           (setf motivations
                 (list (mloc 'world (state (first spec) (second spec) 'pos))))
           (setf story-facts (rest (rest spec))))
          ((list (first spec))
           (setf story-facts (rest spec))
           (setf motivations (construct-motivations (first spec))))
          (t (format t "~%~%!!! ERROR - ill-formed story specification!~%~%")))
    (list motivations story-facts)))

;; Helper function for parse-story-spec. Turns the ((character problem)...) part of an
;; extended story spec into a list of MLOCs suitable for handing off to assert-fact.
;; Easier than expected, since the mapping is straightforward.

(defun construct-motivations (mspecs)
  (mapcar #'(lambda (mpair) (mloc 'world (state (first mpair) (second mpair) 'pos)))
          mspecs))

; Sets up the default story world (if this is first-episode? and there's no world-state) 
; Defines *personae*, *goals*, *all-locations*, *all-objects*, then
; clears facts, goals, and demons from everyone in *personae*

(defun init-default-story-world ()
  (setf *personae* '(joe irving louise))
  (setf *deceased* nil)
  (setf *goals* '(hungry thirsty)) ; all this is used for is to provide a problem if none is given - delete? 151107
  (setf *all-locations* '(cave log oak-tree elm-tree pine-tree hole ground river valley))
  (setf *all-objects* (append *all-locations* '(honey berries fish worm water)))
  (init-default-characters))

; Initializing the default character set. First, add some
; more initial facts - these are "facts" that are not expressed as CDs
; It feels like they belong here in the stories file, but it would be better
; if they were encapsulated somehow so it would be straightforward to replace them.
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
; Then, go over all the characters (+ world) and clear their memories

(defun init-default-characters ()
 ; all these puts are non-CD facts needed to establish some useful facts
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
  (wipe-character-memories))

; Sometimes you don't need to set up all the characters, you just need to 
; reset their memories.

(defun wipe-character-memories ()
  (wipe-facts)
  (wipe-goals)
  (wipe-demons))

; finer grained brainwashing

(defun wipe-goals ()
  (mapc #'(lambda (persona) (put persona 'goals nil))
        (cons 'world *personae*)))

(defun wipe-demons ()
  (mapc #'(lambda (persona) (put persona 'demons nil))
        (cons 'world *personae*)))

(defun wipe-facts ()
  (mapc #'(lambda (persona) (put persona 'facts nil))
        (cons 'world *personae*)))

; This is just establish-facts from micro-talesim, but with a fact list as an
; argument instead of necessarily using *init-facts*. You'd call it on 
; *init-facts* within initialize-story, then on the story/episode-specific
; facts within each spin-episode.
; The facts list is of the form ((ACTOR FACT) (ACTOR FACT) ... (ACTOR FACT))
; and there's a structure for that (see structures.lisp)

(defun establish-fact-set (facts)
  (mapc #'(lambda (fact)
            (now-knows (first fact) (second fact) t)) ; t as last arg ensures all facts are added to *story-sequence*
        facts))

; What if we want to save the mental states of characters at the end of an
; episode, for example, to "reset" the state of the world at the start of 
; a later episode?
; (defstruct mental-state who facts goals demons)

(defun collect-mental-state (character)
  (make-mental-state
   :who character
   :facts (get character 'facts)
   :goals (get character 'goals)
   :demons (get character 'demons)))

; How about collecting all mental states (need to include world as well
; as characters, though world will presumably not have goals or demons)
; Hmm. Have to allow for the possibility that some characters have died.
; If d-is-d is non-NIL, then just use living characters, otherwise
; add in the deceased.

(defun collect-mental-states (&optional d-is-d)
  (mapcar #'collect-mental-state (cons 'world
                                       (if d-is-d *personae*
                                         (append *personae* *deceased*)))))

; Take a snapshot of the world.
; (defstruct world-state personae mental-states locations objects)
; 151128 realized that characters can get killed off, which removes them
;  from *personae*, so they don't get captured in the world-state.
;  add an optional argument - if non-nil, then just use *personae*
;  (forgetting anyone who has died), otherwise, restore the dead to life.

(defun capture-world-state (&optional dead-is-dead)
  (make-world-state
   :personae (if dead-is-dead *personae* (append *personae* *deceased*))
   :mental-states (collect-mental-states dead-is-dead)
   :locations *all-locations*
   :objects *all-objects*))

; Gee, if we're going to do this capture-restore, should we maybe just
; use the struct and get rid of all the intermediate globals, etc.?

(defun recreate-world-state (state)
  (setf *personae* (world-state-personae state))
  (setf *deceased* nil)
  (setf *all-locations* (world-state-locations state))
  (setf *all-objects* (world-state-objects state))
  (mapc #'rebuild-mental-state (world-state-mental-states state)))

; Actually restore a character's mental state
; (defstruct mental-state who facts goals demons)

(defun rebuild-mental-state (mstate)
  (let ((who (mental-state-who mstate)))
    (put who (mental-state-facts mstate) 'facts)
    (put who (mental-state-goals mstate) 'goals)
    (put who (mental-state-demons mstate) 'demons)))

; The default set of *initial-facts* is
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
;
; Perhaps these things should be limited to what the world knows (what's "true")
; and the other facts should be story-specific.
;
