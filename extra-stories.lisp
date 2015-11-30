; Initial Facts
;  init-world sets up a bunch of facts such as Joe is a bear, birds
;  eat worms, and so on.  The variable *init-facts* contains location
;  and relationship facts, along with which character knows them.
;
; More Initial Facts
; Joe is a bear.
; Joes’ home is the cave.
; Irving is a bird.
; Irving’s home is a tree.
; Bears eat honey, berries, and fish.
; Birds eat worms.
; Joe and Irving are personae (in the theatrical sense).
; Hunger and thirst are possible goals.
; The cave, the oak tree, the elm tree, the ground, and the river are 
; all locations.
; All locations are also objects.
; Honey, berries, fish, worms, and water are also objects.
(defun init-world2 ()
  (put 'joe  'is-a 'bear)
  (put 'joe 'home 'cave)
  (put 'louise 'is-a 'bear)
  (put 'louise 'home 'log)
  (put 'irving 'is-a 'bird)
  (put 'irving 'home 'tree)
  (put 'bear 'food '(honey berries fish))
  (put 'bird 'food '(worm))
  (setf *personae* '(joe irving louise))
  (setf *goals* '(hungry thirsty))
  (setf *all-locations* '(cave log oak-tree elm-tree ground river))
  (setf *all-objects* (append *all-locations* 
                              '(honey berries fish worm water)))
  (mapc #'(lambda (persona)
            (put persona 'facts nil)
            (put persona 'goals nil)
            (put persona 'demons nil))
        (cons 'world *personae*))
  (mapc #'(lambda (fact)
            (now-knows (car fact) (cadr fact) t))
        *init-facts*))

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
(defun init-facts2 ()
      '((world (loc (actor joe) (val cave)))
        (joe (loc (actor joe) (val cave)))
        (world (loc (actor irving) (val oak-tree)))
        (irving (loc (actor irving) (val oak-tree)))
        (world (loc (actor louise) (val log))) ; the world knows louise is at the log
        (louise (loc (actor louise) (val log))) ; louise knows she's at the log
;        (joe (loc (actor irving) (val oak-tree)))
        (world (loc (actor water) (val river)))
;        (joe (loc (actor water) (val river))) ; joe knows where there's water
        (world (loc (actor honey) (val elm-tree)))
;        (irving (loc (actor honey) (val elm-tree))) ; irving knows where there's honey
        (world (loc (actor worm) (val hole)))
;        (louise (loc (actor worm) (val ground))) ; louise knows where there's a worm
;        (joe (loc (actor worm) (val ground))) ; joe doesn't know where the worm is
        (irving (loc (actor joe) (val log))) ; irving knows that louise is at the log
        (world (loc (actor fish) (val river)))
;        (irving (loc (actor fish) (val river)))
))

; Everybody likes everyone else, and no one deceives or dominates anyone else.
; Joe knows where Irving is
; Irving knows where Louise is
; Joe knows where the water is
; Irving knows where there is honey
; Louise knows where there is a worm.
; 
; Irving is thirsty but not hungry
; Louise is hungry but not thirsty
; Joe is neither hungry nor thirsty - but he will become hungry to trigger the story
;
; I was hoping that Joe would offer to tell Irving where the water was if Irving
; would tell him where some food was, with the intent that Irving would then go
; bargain with Louise, but instead, Joe tries to offer a worm (which he neither has
; nor knows where to find) in exchange for food. This doesn't work.

(setf *story10*
      '(joe hungry
            (joe (loc (actor irving) (val oak-tree))) ; joe knows where irving is
            (irving (loc (actor louise) (val log))) ; irving knows that louise is at the log
            (joe (loc (actor water) (val river))) ; joe knows where there's water
            (irving (loc (actor honey) (val elm-tree))) ; irving knows where there's honey
            (louise (loc (actor worm) (val ground))) ; louise knows where there's a worm

            (joe (like (actor joe) (to irving) (mode (pos)))) ; joe likes irving
            (joe (like (actor joe) (to louise) (mode (pos)))) ; joe likes louise
            (joe (like (actor irving) (to joe) (mode (pos)))) ; joe thinks irving likes joe
            (joe (like (actor louise) (to joe) (mode (pos)))) ; joe thinks louise likes joe

            (joe (deceive (actor joe) (to irving) (mode (neg)))) ; joe doesn't deceive irving
            (joe (deceive (actor joe) (to louise) (mode (neg)))) ; joe doesn't deceive louise
            (joe (deceive (actor irving) (to joe) (mode (neg)))) ; joe thinks irving doesn't deceive joe
            (joe (deceive (actor louise) (to joe) (mode (neg)))) ; joe thinks louise doesn't deceive joe

            (joe (dominate (actor joe) (to irving) (mode (neg)))) ; joe doesn't dominate irving
            (joe (dominate (actor joe) (to louise) (mode (neg)))) ; joe doesn't dominate louise
            (joe (dominate (actor irving) (to joe) (mode (neg)))) ; joe thinks irving doesn't dominate joe
            (joe (dominate (actor louise) (to joe) (mode (neg)))) ; joe thinks louise doesn't dominate joe

            (irving (like (actor irving) (to joe) (mode (pos)))) ; irving likes joe
            (irving (like (actor irving) (to louise) (mode (pos)))) ; irving likes louise
            (irving (like (actor joe) (to irving) (mode (pos)))) ; irving thinks joe likes irving
            (irving (like (actor louise) (to irving) (mode (pos)))) ; irving thinks louise likes irving

            (irving (deceive (actor irving) (to joe) (mode (neg)))) ; irving doesn't deceive joe
            (irving (deceive (actor irving) (to louise) (mode (neg)))) ; irving doesn't deceive louise
            (irving (deceive (actor joe) (to irving) (mode (neg)))) ; irving thinks joe doesn't deceive irving
            (irving (deceive (actor louise) (to irving) (mode (neg)))) ; irving thinks louise doesn't deceive irving

            (irving (dominate (actor irving) (to joe) (mode (neg)))) ; irving doesn't dominate joe
            (irving (dominate (actor irving) (to louise) (mode (neg)))) ; irving doesn't dominate louise
            (irving (dominate (actor joe) (to irving) (mode (neg)))) ; irving thinks joe doesn't dominate irving
            (irving (dominate (actor louise) (to irving) (mode (neg)))) ; irving thinks louise doesn't dominate irving

            (louise (like (actor louise) (to joe) (mode (pos)))) ; louise likes joe
            (louise (like (actor louise) (to irving) (mode (pos)))) ; louise likes irving
            (louise (like (actor joe) (to louise) (mode (pos)))) ; louise thinks joe likes louise
            (louise (like (actor irving) (to louise) (mode (pos)))) ; louise thinks irving likes louise

            (louise (deceive (actor louise) (to joe) (mode (neg)))) ; louise doesn't deceive joe
            (louise (deceive (actor louise) (to irving) (mode (neg)))) ; louise doesn't deceive irving
            (louise (deceive (actor joe) (to louise) (mode (neg)))) ; louise thinks joe doesn't deceive louise
            (louise (deceive (actor irving) (to louise) (mode (neg)))) ; louise thinks irving doesn't deceive louise

            (louise (dominate (actor louise) (to joe) (mode (neg)))) ; louise doesn't dominate joe
            (louise (dominate (actor louise) (to irving) (mode (neg)))) ; louise doesn't dominate irving
            (louise (dominate (actor joe) (to louise) (mode (neg)))) ; louise thinks joe doesn't dominate louise
            (louise (dominate (actor irving) (to louise) (mode (neg)))) ; louise thinks irving doesn't dominate louise

            (world (thirsty (actor joe) (mode (neg)))) ; joe isn't thirsty
            (world (thirsty (actor irving) (mode (pos)))) ; irving is thirsty
            (world (thirsty (actor louise) (mode (neg)))) ; louise isn't thirsty

            (world (hungry (actor joe) (mode (neg)))) ; joe isn't hungry
            (world (hungry (actor irving) (mode (neg)))) ; irving isn't hungry
            (world (hungry (actor louise) (mode (pos)))) ; louise is hungry

))

; Everybody likes everyone else, and no one deceives or dominates anyone else.
; Joe knows where Louise is
; Irving knows where the water is
; Irving knows where there is honey
; Louise knows where a worm is
; 
; Irving is hungry but not thirsty
; Louise is thirsty but not hungry
; Joe is neither hungry nor thirsty - but he will become hungry to trigger the story

(setf *story11*
      '(joe hungry
            (joe (loc (actor louise) (val log))) ; joe knows where louise is
            (irving (loc (actor water) (val river))) ; irving knows where there's water
            (irving (loc (actor honey) (val elm-tree))) ; irving knows where there's honey
            (louise (loc (actor worm) (val ground))) ; louise knows where there's a worm

            (joe (like (actor joe) (to irving) (mode (pos)))) ; joe likes irving
            (joe (like (actor joe) (to louise) (mode (pos)))) ; joe likes louise
            (joe (like (actor irving) (to joe) (mode (pos)))) ; joe thinks irving likes joe
            (joe (like (actor louise) (to joe) (mode (pos)))) ; joe thinks louise likes joe

            (joe (deceive (actor joe) (to irving) (mode (neg)))) ; joe doesn't deceive irving
            (joe (deceive (actor joe) (to louise) (mode (neg)))) ; joe doesn't deceive louise
            (joe (deceive (actor irving) (to joe) (mode (neg)))) ; joe thinks irving doesn't deceive joe
            (joe (deceive (actor louise) (to joe) (mode (neg)))) ; joe thinks louise doesn't deceive joe

            (joe (dominate (actor joe) (to irving) (mode (neg)))) ; joe doesn't dominate irving
            (joe (dominate (actor joe) (to louise) (mode (neg)))) ; joe doesn't dominate louise
            (joe (dominate (actor irving) (to joe) (mode (neg)))) ; joe thinks irving doesn't dominate joe
            (joe (dominate (actor louise) (to joe) (mode (neg)))) ; joe thinks louise doesn't dominate joe

            (irving (like (actor irving) (to joe) (mode (pos)))) ; irving likes joe
            (irving (like (actor irving) (to louise) (mode (pos)))) ; irving likes louise
            (irving (like (actor joe) (to irving) (mode (pos)))) ; irving thinks joe likes irving
            (irving (like (actor louise) (to irving) (mode (pos)))) ; irving thinks louise likes irving

            (irving (deceive (actor irving) (to joe) (mode (neg)))) ; irving doesn't deceive joe
            (irving (deceive (actor irving) (to louise) (mode (neg)))) ; irving doesn't deceive louise
            (irving (deceive (actor joe) (to irving) (mode (neg)))) ; irving thinks joe doesn't deceive irving
            (irving (deceive (actor louise) (to irving) (mode (neg)))) ; irving thinks louise doesn't deceive irving

            (irving (dominate (actor irving) (to joe) (mode (neg)))) ; irving doesn't dominate joe
            (irving (dominate (actor irving) (to louise) (mode (neg)))) ; irving doesn't dominate louise
            (irving (dominate (actor joe) (to irving) (mode (neg)))) ; irving thinks joe doesn't dominate irving
            (irving (dominate (actor louise) (to irving) (mode (neg)))) ; irving thinks louise doesn't dominate irving

            (louise (like (actor louise) (to joe) (mode (pos)))) ; louise likes joe
            (louise (like (actor louise) (to irving) (mode (pos)))) ; louise likes irving
            (louise (like (actor joe) (to louise) (mode (pos)))) ; louise thinks joe likes louise
            (louise (like (actor irving) (to louise) (mode (pos)))) ; louise thinks irving likes louise

            (louise (deceive (actor louise) (to joe) (mode (neg)))) ; louise doesn't deceive joe
            (louise (deceive (actor louise) (to irving) (mode (neg)))) ; louise doesn't deceive irving
            (louise (deceive (actor joe) (to louise) (mode (neg)))) ; louise thinks joe doesn't deceive louise
            (louise (deceive (actor irving) (to louise) (mode (neg)))) ; louise thinks irving doesn't deceive louise

            (louise (dominate (actor louise) (to joe) (mode (neg)))) ; louise doesn't dominate joe
            (louise (dominate (actor louise) (to irving) (mode (neg)))) ; louise doesn't dominate irving
            (louise (dominate (actor joe) (to louise) (mode (neg)))) ; louise thinks joe doesn't dominate louise
            (louise (dominate (actor irving) (to louise) (mode (neg)))) ; louise thinks irving doesn't dominate louise

            (world (thirsty (actor joe) (mode (neg)))) ; joe isn't thirsty
            (world (thirsty (actor irving) (mode (neg)))) ; irving isn't thirsty
            (world (thirsty (actor louise) (mode (pos)))) ; louise is thirsty

            (world (hungry (actor joe) (mode (neg)))) ; joe isn't hungry
            (world (hungry (actor irving) (mode (pos)))) ; irving is hungry
            (world (hungry (actor louise) (mode (neg)))) ; louise isn't hungry

))

(setf *story12*
      '(joe hungry
            (joe (loc (actor louise) (val log))) ; joe knows where louise is
            (irving (loc (actor water) (val river))) ; irving knows where there's water
            (irving (loc (actor honey) (val elm-tree))) ; irving knows where there's honey
            (louise (loc (actor worm) (val ground))) ; louise knows where there's a worm

            (joe (like (actor joe) (to irving) (mode (pos)))) ; joe likes irving
            (joe (like (actor joe) (to louise) (mode (pos)))) ; joe likes louise
            (joe (like (actor irving) (to joe) (mode (pos)))) ; joe thinks irving likes joe
            (joe (like (actor louise) (to joe) (mode (pos)))) ; joe thinks louise likes joe

; joe is paranoid

            (joe (deceive (actor joe) (to irving) (mode (pos)))) ; joe doesn't deceive irving
            (joe (deceive (actor joe) (to louise) (mode (pos)))) ; joe doesn't deceive louise
            (joe (deceive (actor irving) (to joe) (mode (pos)))) ; joe thinks irving doesn't deceive joe
            (joe (deceive (actor louise) (to joe) (mode (pos)))) ; joe thinks louise doesn't deceive joe

            (joe (dominate (actor joe) (to irving) (mode (neg)))) ; joe doesn't dominate irving
            (joe (dominate (actor joe) (to louise) (mode (neg)))) ; joe doesn't dominate louise
            (joe (dominate (actor irving) (to joe) (mode (neg)))) ; joe thinks irving doesn't dominate joe
            (joe (dominate (actor louise) (to joe) (mode (neg)))) ; joe thinks louise doesn't dominate joe

            (irving (like (actor irving) (to joe) (mode (pos)))) ; irving likes joe
            (irving (like (actor irving) (to louise) (mode (pos)))) ; irving likes louise
            (irving (like (actor joe) (to irving) (mode (pos)))) ; irving thinks joe likes irving
            (irving (like (actor louise) (to irving) (mode (pos)))) ; irving thinks louise likes irving

            (irving (deceive (actor irving) (to joe) (mode (neg)))) ; irving doesn't deceive joe
            (irving (deceive (actor irving) (to louise) (mode (neg)))) ; irving doesn't deceive louise
            (irving (deceive (actor joe) (to irving) (mode (neg)))) ; irving thinks joe doesn't deceive irving
            (irving (deceive (actor louise) (to irving) (mode (neg)))) ; irving thinks louise doesn't deceive irving

            (irving (dominate (actor irving) (to joe) (mode (neg)))) ; irving doesn't dominate joe
            (irving (dominate (actor irving) (to louise) (mode (neg)))) ; irving doesn't dominate louise
            (irving (dominate (actor joe) (to irving) (mode (neg)))) ; irving thinks joe doesn't dominate irving
            (irving (dominate (actor louise) (to irving) (mode (neg)))) ; irving thinks louise doesn't dominate irving

            (louise (like (actor louise) (to joe) (mode (pos)))) ; louise likes joe
            (louise (like (actor louise) (to irving) (mode (pos)))) ; louise likes irving
            (louise (like (actor joe) (to louise) (mode (pos)))) ; louise thinks joe likes louise
            (louise (like (actor irving) (to louise) (mode (pos)))) ; louise thinks irving likes louise

            (louise (deceive (actor louise) (to joe) (mode (neg)))) ; louise doesn't deceive joe
            (louise (deceive (actor louise) (to irving) (mode (neg)))) ; louise doesn't deceive irving
            (louise (deceive (actor joe) (to louise) (mode (neg)))) ; louise thinks joe doesn't deceive louise
            (louise (deceive (actor irving) (to louise) (mode (neg)))) ; louise thinks irving doesn't deceive louise

            (louise (dominate (actor louise) (to joe) (mode (neg)))) ; louise doesn't dominate joe
            (louise (dominate (actor louise) (to irving) (mode (neg)))) ; louise doesn't dominate irving
            (louise (dominate (actor joe) (to louise) (mode (neg)))) ; louise thinks joe doesn't dominate louise
            (louise (dominate (actor irving) (to louise) (mode (neg)))) ; louise thinks irving doesn't dominate louise

            (world (thirsty (actor joe) (mode (neg)))) ; joe isn't thirsty
            (world (thirsty (actor irving) (mode (neg)))) ; irving isn't thirsty
            (world (thirsty (actor louise) (mode (pos)))) ; louise is thirsty

            (world (hungry (actor joe) (mode (neg)))) ; joe isn't hungry
            (world (hungry (actor irving) (mode (pos)))) ; irving is hungry
            (world (hungry (actor louise) (mode (neg)))) ; louise isn't hungry

))

(setf *story13*
      '(joe hungry
            (joe (loc (actor louise) (val log))) ; joe knows where louise is
            (irving (loc (actor water) (val river))) ; irving knows where there's water
            (irving (loc (actor honey) (val elm-tree))) ; irving knows where there's honey
            (louise (loc (actor worm) (val ground))) ; louise knows where there's a worm

            (joe (like (actor joe) (to irving) (mode (pos)))) ; joe likes irving
            (joe (like (actor joe) (to louise) (mode (pos)))) ; joe likes louise
            (joe (like (actor irving) (to joe) (mode (pos)))) ; joe thinks irving likes joe
            (joe (like (actor louise) (to joe) (mode (pos)))) ; joe thinks louise likes joe

            (joe (deceive (actor joe) (to irving) (mode (neg)))) ; joe doesn't deceive irving
            (joe (deceive (actor joe) (to louise) (mode (neg)))) ; joe doesn't deceive louise
            (joe (deceive (actor irving) (to joe) (mode (neg)))) ; joe thinks irving doesn't deceive joe
            (joe (deceive (actor louise) (to joe) (mode (neg)))) ; joe thinks louise doesn't deceive joe

; Joe believes he's dominant - Irving and Louise agree

            (joe (dominate (actor joe) (to irving) (mode (pos)))) ; joe doesn't dominate irving
            (joe (dominate (actor joe) (to louise) (mode (pos)))) ; joe doesn't dominate louise
            (joe (dominate (actor irving) (to joe) (mode (neg)))) ; joe thinks irving doesn't dominate joe
            (joe (dominate (actor louise) (to joe) (mode (neg)))) ; joe thinks louise doesn't dominate joe

            (irving (like (actor irving) (to joe) (mode (pos)))) ; irving likes joe
            (irving (like (actor irving) (to louise) (mode (pos)))) ; irving likes louise
            (irving (like (actor joe) (to irving) (mode (pos)))) ; irving thinks joe likes irving
            (irving (like (actor louise) (to irving) (mode (pos)))) ; irving thinks louise likes irving

            (irving (deceive (actor irving) (to joe) (mode (neg)))) ; irving doesn't deceive joe
            (irving (deceive (actor irving) (to louise) (mode (neg)))) ; irving doesn't deceive louise
            (irving (deceive (actor joe) (to irving) (mode (neg)))) ; irving thinks joe doesn't deceive irving
            (irving (deceive (actor louise) (to irving) (mode (neg)))) ; irving thinks louise doesn't deceive irving

            (irving (dominate (actor irving) (to joe) (mode (neg)))) ; irving doesn't dominate joe
            (irving (dominate (actor irving) (to louise) (mode (neg)))) ; irving doesn't dominate louise
            (irving (dominate (actor joe) (to irving) (mode (pos)))) ; irving thinks joe doesn't dominate irving
            (irving (dominate (actor louise) (to irving) (mode (neg)))) ; irving thinks louise doesn't dominate irving

            (louise (like (actor louise) (to joe) (mode (pos)))) ; louise likes joe
            (louise (like (actor louise) (to irving) (mode (pos)))) ; louise likes irving
            (louise (like (actor joe) (to louise) (mode (pos)))) ; louise thinks joe likes louise
            (louise (like (actor irving) (to louise) (mode (pos)))) ; louise thinks irving likes louise

            (louise (deceive (actor louise) (to joe) (mode (neg)))) ; louise doesn't deceive joe
            (louise (deceive (actor louise) (to irving) (mode (neg)))) ; louise doesn't deceive irving
            (louise (deceive (actor joe) (to louise) (mode (neg)))) ; louise thinks joe doesn't deceive louise
            (louise (deceive (actor irving) (to louise) (mode (neg)))) ; louise thinks irving doesn't deceive louise

            (louise (dominate (actor louise) (to joe) (mode (neg)))) ; louise doesn't dominate joe
            (louise (dominate (actor louise) (to irving) (mode (neg)))) ; louise doesn't dominate irving
            (louise (dominate (actor joe) (to louise) (mode (pos)))) ; louise thinks joe doesn't dominate louise
            (louise (dominate (actor irving) (to louise) (mode (neg)))) ; louise thinks irving doesn't dominate louise

            (world (thirsty (actor joe) (mode (neg)))) ; joe isn't thirsty
            (world (thirsty (actor irving) (mode (neg)))) ; irving isn't thirsty
            (world (thirsty (actor louise) (mode (pos)))) ; louise is thirsty

            (world (hungry (actor joe) (mode (neg)))) ; joe isn't hungry
            (world (hungry (actor irving) (mode (pos)))) ; irving is hungry
            (world (hungry (actor louise) (mode (neg)))) ; louise isn't hungry

))

(setf *story14*
      '(joe hungry
            (joe (loc (actor louise) (val log))) ; joe knows where louise is
            (irving (loc (actor water) (val river))) ; irving knows where there's water
            (irving (loc (actor honey) (val elm-tree))) ; irving knows where there's honey
            (louise (loc (actor worm) (val ground))) ; louise knows where there's a worm

; Joe is a misanthrope, though his attitude is not shared by Louise and Irving

            (joe (like (actor joe) (to irving) (mode (neg)))) ; joe likes irving
            (joe (like (actor joe) (to louise) (mode (neg)))) ; joe likes louise
            (joe (like (actor irving) (to joe) (mode (neg)))) ; joe thinks irving likes joe
            (joe (like (actor louise) (to joe) (mode (neg)))) ; joe thinks louise likes joe

            (joe (deceive (actor joe) (to irving) (mode (neg)))) ; joe doesn't deceive irving
            (joe (deceive (actor joe) (to louise) (mode (neg)))) ; joe doesn't deceive louise
            (joe (deceive (actor irving) (to joe) (mode (neg)))) ; joe thinks irving doesn't deceive joe
            (joe (deceive (actor louise) (to joe) (mode (neg)))) ; joe thinks louise doesn't deceive joe

            (joe (dominate (actor joe) (to irving) (mode (neg)))) ; joe doesn't dominate irving
            (joe (dominate (actor joe) (to louise) (mode (neg)))) ; joe doesn't dominate louise
            (joe (dominate (actor irving) (to joe) (mode (neg)))) ; joe thinks irving doesn't dominate joe
            (joe (dominate (actor louise) (to joe) (mode (neg)))) ; joe thinks louise doesn't dominate joe

            (irving (like (actor irving) (to joe) (mode (pos)))) ; irving likes joe
            (irving (like (actor irving) (to louise) (mode (pos)))) ; irving likes louise
            (irving (like (actor joe) (to irving) (mode (pos)))) ; irving thinks joe likes irving
            (irving (like (actor louise) (to irving) (mode (pos)))) ; irving thinks louise likes irving

            (irving (deceive (actor irving) (to joe) (mode (neg)))) ; irving doesn't deceive joe
            (irving (deceive (actor irving) (to louise) (mode (neg)))) ; irving doesn't deceive louise
            (irving (deceive (actor joe) (to irving) (mode (neg)))) ; irving thinks joe doesn't deceive irving
            (irving (deceive (actor louise) (to irving) (mode (neg)))) ; irving thinks louise doesn't deceive irving

            (irving (dominate (actor irving) (to joe) (mode (neg)))) ; irving doesn't dominate joe
            (irving (dominate (actor irving) (to louise) (mode (neg)))) ; irving doesn't dominate louise
            (irving (dominate (actor joe) (to irving) (mode (neg)))) ; irving thinks joe doesn't dominate irving
            (irving (dominate (actor louise) (to irving) (mode (neg)))) ; irving thinks louise doesn't dominate irving

            (louise (like (actor louise) (to joe) (mode (pos)))) ; louise likes joe
            (louise (like (actor louise) (to irving) (mode (pos)))) ; louise likes irving
            (louise (like (actor joe) (to louise) (mode (pos)))) ; louise thinks joe likes louise
            (louise (like (actor irving) (to louise) (mode (pos)))) ; louise thinks irving likes louise

            (louise (deceive (actor louise) (to joe) (mode (neg)))) ; louise doesn't deceive joe
            (louise (deceive (actor louise) (to irving) (mode (neg)))) ; louise doesn't deceive irving
            (louise (deceive (actor joe) (to louise) (mode (neg)))) ; louise thinks joe doesn't deceive louise
            (louise (deceive (actor irving) (to louise) (mode (neg)))) ; louise thinks irving doesn't deceive louise

            (louise (dominate (actor louise) (to joe) (mode (neg)))) ; louise doesn't dominate joe
            (louise (dominate (actor louise) (to irving) (mode (neg)))) ; louise doesn't dominate irving
            (louise (dominate (actor joe) (to louise) (mode (neg)))) ; louise thinks joe doesn't dominate louise
            (louise (dominate (actor irving) (to louise) (mode (neg)))) ; louise thinks irving doesn't dominate louise

            (world (thirsty (actor joe) (mode (neg)))) ; joe isn't thirsty
            (world (thirsty (actor irving) (mode (neg)))) ; irving isn't thirsty
            (world (thirsty (actor louise) (mode (pos)))) ; louise is thirsty

            (world (hungry (actor joe) (mode (neg)))) ; joe isn't hungry
            (world (hungry (actor irving) (mode (pos)))) ; irving is hungry
            (world (hungry (actor louise) (mode (neg)))) ; louise isn't hungry

))

(setf *story15*
      '(joe hungry
            (joe (loc (actor louise) (val log))) ; joe knows where louise is
            (irving (loc (actor water) (val river))) ; irving knows where there's water
            (irving (loc (actor honey) (val elm-tree))) ; irving knows where there's honey
            (louise (loc (actor worm) (val ground))) ; louise knows where there's a worm

; Joe is a misanthrope and Irving and Louise agree

            (joe (like (actor joe) (to irving) (mode (neg)))) ; joe likes irving
            (joe (like (actor joe) (to louise) (mode (neg)))) ; joe likes louise
            (joe (like (actor irving) (to joe) (mode (neg)))) ; joe thinks irving likes joe
            (joe (like (actor louise) (to joe) (mode (neg)))) ; joe thinks louise likes joe

            (joe (deceive (actor joe) (to irving) (mode (neg)))) ; joe doesn't deceive irving
            (joe (deceive (actor joe) (to louise) (mode (neg)))) ; joe doesn't deceive louise
            (joe (deceive (actor irving) (to joe) (mode (neg)))) ; joe thinks irving doesn't deceive joe
            (joe (deceive (actor louise) (to joe) (mode (neg)))) ; joe thinks louise doesn't deceive joe

            (joe (dominate (actor joe) (to irving) (mode (neg)))) ; joe doesn't dominate irving
            (joe (dominate (actor joe) (to louise) (mode (neg)))) ; joe doesn't dominate louise
            (joe (dominate (actor irving) (to joe) (mode (neg)))) ; joe thinks irving doesn't dominate joe
            (joe (dominate (actor louise) (to joe) (mode (neg)))) ; joe thinks louise doesn't dominate joe

            (irving (like (actor irving) (to joe) (mode (neg)))) ; irving likes joe
            (irving (like (actor irving) (to louise) (mode (pos)))) ; irving likes louise
            (irving (like (actor joe) (to irving) (mode (neg)))) ; irving thinks joe likes irving
            (irving (like (actor louise) (to irving) (mode (pos)))) ; irving thinks louise likes irving

            (irving (deceive (actor irving) (to joe) (mode (neg)))) ; irving doesn't deceive joe
            (irving (deceive (actor irving) (to louise) (mode (neg)))) ; irving doesn't deceive louise
            (irving (deceive (actor joe) (to irving) (mode (neg)))) ; irving thinks joe doesn't deceive irving
            (irving (deceive (actor louise) (to irving) (mode (neg)))) ; irving thinks louise doesn't deceive irving

            (irving (dominate (actor irving) (to joe) (mode (neg)))) ; irving doesn't dominate joe
            (irving (dominate (actor irving) (to louise) (mode (neg)))) ; irving doesn't dominate louise
            (irving (dominate (actor joe) (to irving) (mode (neg)))) ; irving thinks joe doesn't dominate irving
            (irving (dominate (actor louise) (to irving) (mode (neg)))) ; irving thinks louise doesn't dominate irving

            (louise (like (actor louise) (to joe) (mode (neg)))) ; louise likes joe
            (louise (like (actor louise) (to irving) (mode (pos)))) ; louise likes irving
            (louise (like (actor joe) (to louise) (mode (neg)))) ; louise thinks joe likes louise
            (louise (like (actor irving) (to louise) (mode (pos)))) ; louise thinks irving likes louise

            (louise (deceive (actor louise) (to joe) (mode (neg)))) ; louise doesn't deceive joe
            (louise (deceive (actor louise) (to irving) (mode (neg)))) ; louise doesn't deceive irving
            (louise (deceive (actor joe) (to louise) (mode (neg)))) ; louise thinks joe doesn't deceive louise
            (louise (deceive (actor irving) (to louise) (mode (neg)))) ; louise thinks irving doesn't deceive louise

            (louise (dominate (actor louise) (to joe) (mode (neg)))) ; louise doesn't dominate joe
            (louise (dominate (actor louise) (to irving) (mode (neg)))) ; louise doesn't dominate irving
            (louise (dominate (actor joe) (to louise) (mode (neg)))) ; louise thinks joe doesn't dominate louise
            (louise (dominate (actor irving) (to louise) (mode (neg)))) ; louise thinks irving doesn't dominate louise

            (world (thirsty (actor joe) (mode (neg)))) ; joe isn't thirsty
            (world (thirsty (actor irving) (mode (neg)))) ; irving isn't thirsty
            (world (thirsty (actor louise) (mode (pos)))) ; louise is thirsty

            (world (hungry (actor joe) (mode (neg)))) ; joe isn't hungry
            (world (hungry (actor irving) (mode (pos)))) ; irving is hungry
            (world (hungry (actor louise) (mode (neg)))) ; louise isn't hungry

))

(setf *story16*
      '(joe hungry
            (joe (loc (actor louise) (val log))) ; joe knows where louise is
            (irving (loc (actor water) (val river))) ; irving knows where there's water
            (irving (loc (actor honey) (val elm-tree))) ; irving knows where there's honey
            (louise (loc (actor worm) (val ground))) ; louise knows where there's a worm

            (joe (like (actor joe) (to irving) (mode (pos)))) ; joe likes irving
            (joe (like (actor joe) (to louise) (mode (pos)))) ; joe likes louise
            (joe (like (actor irving) (to joe) (mode (pos)))) ; joe thinks irving likes joe
            (joe (like (actor louise) (to joe) (mode (pos)))) ; joe thinks louise likes joe

            (joe (deceive (actor joe) (to irving) (mode (neg)))) ; joe doesn't deceive irving
            (joe (deceive (actor joe) (to louise) (mode (neg)))) ; joe doesn't deceive louise
            (joe (deceive (actor irving) (to joe) (mode (neg)))) ; joe thinks irving doesn't deceive joe
            (joe (deceive (actor louise) (to joe) (mode (neg)))) ; joe thinks louise doesn't deceive joe

            (joe (dominate (actor joe) (to irving) (mode (neg)))) ; joe doesn't dominate irving
            (joe (dominate (actor joe) (to louise) (mode (neg)))) ; joe doesn't dominate louise
            (joe (dominate (actor irving) (to joe) (mode (neg)))) ; joe thinks irving doesn't dominate joe
            (joe (dominate (actor louise) (to joe) (mode (neg)))) ; joe thinks louise doesn't dominate joe

; In reality, Irving doesn't like Joe, and is willing to deceive him, though Joe doesn't realize this

            (irving (like (actor irving) (to joe) (mode (neg)))) ; irving likes joe
            (irving (like (actor irving) (to louise) (mode (pos)))) ; irving likes louise
            (irving (like (actor joe) (to irving) (mode (pos)))) ; irving thinks joe likes irving
            (irving (like (actor louise) (to irving) (mode (pos)))) ; irving thinks louise likes irving

            (irving (deceive (actor irving) (to joe) (mode (pos)))) ; irving doesn't deceive joe
            (irving (deceive (actor irving) (to louise) (mode (neg)))) ; irving doesn't deceive louise
            (irving (deceive (actor joe) (to irving) (mode (neg)))) ; irving thinks joe doesn't deceive irving
            (irving (deceive (actor louise) (to irving) (mode (neg)))) ; irving thinks louise doesn't deceive irving

            (irving (dominate (actor irving) (to joe) (mode (neg)))) ; irving doesn't dominate joe
            (irving (dominate (actor irving) (to louise) (mode (neg)))) ; irving doesn't dominate louise
            (irving (dominate (actor joe) (to irving) (mode (neg)))) ; irving thinks joe doesn't dominate irving
            (irving (dominate (actor louise) (to irving) (mode (neg)))) ; irving thinks louise doesn't dominate irving

            (louise (like (actor louise) (to joe) (mode (pos)))) ; louise likes joe
            (louise (like (actor louise) (to irving) (mode (pos)))) ; louise likes irving
            (louise (like (actor joe) (to louise) (mode (pos)))) ; louise thinks joe likes louise
            (louise (like (actor irving) (to louise) (mode (pos)))) ; louise thinks irving likes louise

            (louise (deceive (actor louise) (to joe) (mode (neg)))) ; louise doesn't deceive joe
            (louise (deceive (actor louise) (to irving) (mode (neg)))) ; louise doesn't deceive irving
            (louise (deceive (actor joe) (to louise) (mode (neg)))) ; louise thinks joe doesn't deceive louise
            (louise (deceive (actor irving) (to louise) (mode (neg)))) ; louise thinks irving doesn't deceive louise

            (louise (dominate (actor louise) (to joe) (mode (neg)))) ; louise doesn't dominate joe
            (louise (dominate (actor louise) (to irving) (mode (neg)))) ; louise doesn't dominate irving
            (louise (dominate (actor joe) (to louise) (mode (neg)))) ; louise thinks joe doesn't dominate louise
            (louise (dominate (actor irving) (to louise) (mode (neg)))) ; louise thinks irving doesn't dominate louise

            (world (thirsty (actor joe) (mode (neg)))) ; joe isn't thirsty
            (world (thirsty (actor irving) (mode (neg)))) ; irving isn't thirsty
            (world (thirsty (actor louise) (mode (pos)))) ; louise is thirsty

            (world (hungry (actor joe) (mode (neg)))) ; joe isn't hungry
            (world (hungry (actor irving) (mode (pos)))) ; irving is hungry
            (world (hungry (actor louise) (mode (neg)))) ; louise isn't hungry

))

(setf *story17*
      '(joe hungry
            (joe (loc (actor louise) (val log))) ; joe knows where louise is
            (irving (loc (actor water) (val river))) ; irving knows where there's water
            (irving (loc (actor honey) (val elm-tree))) ; irving knows where there's honey
            (louise (loc (actor worm) (val ground))) ; louise knows where there's a worm

            (joe (like (actor joe) (to irving) (mode (pos)))) ; joe likes irving
            (joe (like (actor joe) (to louise) (mode (pos)))) ; joe likes louise
            (joe (like (actor irving) (to joe) (mode (pos)))) ; joe thinks irving likes joe
            (joe (like (actor louise) (to joe) (mode (pos)))) ; joe thinks louise likes joe

            (joe (deceive (actor joe) (to irving) (mode (neg)))) ; joe doesn't deceive irving
            (joe (deceive (actor joe) (to louise) (mode (neg)))) ; joe doesn't deceive louise
            (joe (deceive (actor irving) (to joe) (mode (neg)))) ; joe thinks irving doesn't deceive joe
            (joe (deceive (actor louise) (to joe) (mode (neg)))) ; joe thinks louise doesn't deceive joe

            (joe (dominate (actor joe) (to irving) (mode (neg)))) ; joe doesn't dominate irving
            (joe (dominate (actor joe) (to louise) (mode (neg)))) ; joe doesn't dominate louise
            (joe (dominate (actor irving) (to joe) (mode (neg)))) ; joe thinks irving doesn't dominate joe
            (joe (dominate (actor louise) (to joe) (mode (neg)))) ; joe thinks louise doesn't dominate joe

            (irving (like (actor irving) (to joe) (mode (pos)))) ; irving likes joe
            (irving (like (actor irving) (to louise) (mode (pos)))) ; irving likes louise
            (irving (like (actor joe) (to irving) (mode (pos)))) ; irving thinks joe likes irving
            (irving (like (actor louise) (to irving) (mode (pos)))) ; irving thinks louise likes irving

            (irving (deceive (actor irving) (to joe) (mode (neg)))) ; irving doesn't deceive joe
            (irving (deceive (actor irving) (to louise) (mode (neg)))) ; irving doesn't deceive louise
            (irving (deceive (actor joe) (to irving) (mode (neg)))) ; irving thinks joe doesn't deceive irving
            (irving (deceive (actor louise) (to irving) (mode (neg)))) ; irving thinks louise doesn't deceive irving

            (irving (dominate (actor irving) (to joe) (mode (neg)))) ; irving doesn't dominate joe
            (irving (dominate (actor irving) (to louise) (mode (neg)))) ; irving doesn't dominate louise
            (irving (dominate (actor joe) (to irving) (mode (neg)))) ; irving thinks joe doesn't dominate irving
            (irving (dominate (actor louise) (to irving) (mode (neg)))) ; irving thinks louise doesn't dominate irving

; Louise is super negative

            (louise (like (actor louise) (to joe) (mode (neg)))) ; louise likes joe
            (louise (like (actor louise) (to irving) (mode (neg)))) ; louise likes irving
            (louise (like (actor joe) (to louise) (mode (neg)))) ; louise thinks joe likes louise
            (louise (like (actor irving) (to louise) (mode (neg)))) ; louise thinks irving likes louise

            (louise (deceive (actor louise) (to joe) (mode (pos)))) ; louise doesn't deceive joe
            (louise (deceive (actor louise) (to irving) (mode (pos)))) ; louise doesn't deceive irving
            (louise (deceive (actor joe) (to louise) (mode (pos)))) ; louise thinks joe doesn't deceive louise
            (louise (deceive (actor irving) (to louise) (mode (pos)))) ; louise thinks irving doesn't deceive louise

            (louise (dominate (actor louise) (to joe) (mode (pos)))) ; louise doesn't dominate joe
            (louise (dominate (actor louise) (to irving) (mode (pos)))) ; louise doesn't dominate irving
            (louise (dominate (actor joe) (to louise) (mode (pos)))) ; louise thinks joe doesn't dominate louise
            (louise (dominate (actor irving) (to louise) (mode (pos)))) ; louise thinks irving doesn't dominate louise

            (world (thirsty (actor joe) (mode (neg)))) ; joe isn't thirsty
            (world (thirsty (actor irving) (mode (neg)))) ; irving isn't thirsty
            (world (thirsty (actor louise) (mode (pos)))) ; louise is thirsty

            (world (hungry (actor joe) (mode (neg)))) ; joe isn't hungry
            (world (hungry (actor irving) (mode (pos)))) ; irving is hungry
            (world (hungry (actor louise) (mode (neg)))) ; louise isn't hungry

))

(setf *story18*
      '(joe hungry
            (joe (loc (actor louise) (val log))) ; joe knows where louise is
            (irving (loc (actor water) (val river))) ; irving knows where there's water
            (irving (loc (actor honey) (val elm-tree))) ; irving knows where there's honey
            (louise (loc (actor worm) (val ground))) ; louise knows where there's a worm

; Joe despises Irving, and thinks Irving despises him

            (joe (like (actor joe) (to irving) (mode (neg)))) ; joe likes irving
            (joe (like (actor joe) (to louise) (mode (pos)))) ; joe likes louise
            (joe (like (actor irving) (to joe) (mode (neg)))) ; joe thinks irving likes joe
            (joe (like (actor louise) (to joe) (mode (pos)))) ; joe thinks louise likes joe

            (joe (deceive (actor joe) (to irving) (mode (pos)))) ; joe doesn't deceive irving
            (joe (deceive (actor joe) (to louise) (mode (neg)))) ; joe doesn't deceive louise
            (joe (deceive (actor irving) (to joe) (mode (pos)))) ; joe thinks irving doesn't deceive joe
            (joe (deceive (actor louise) (to joe) (mode (neg)))) ; joe thinks louise doesn't deceive joe

            (joe (dominate (actor joe) (to irving) (mode (pos)))) ; joe doesn't dominate irving
            (joe (dominate (actor joe) (to louise) (mode (neg)))) ; joe doesn't dominate louise
            (joe (dominate (actor irving) (to joe) (mode (neg)))) ; joe thinks irving doesn't dominate joe
            (joe (dominate (actor louise) (to joe) (mode (neg)))) ; joe thinks louise doesn't dominate joe

; Irving is sunny even though that's delusional

            (irving (like (actor irving) (to joe) (mode (pos)))) ; irving likes joe
            (irving (like (actor irving) (to louise) (mode (pos)))) ; irving likes louise
            (irving (like (actor joe) (to irving) (mode (pos)))) ; irving thinks joe likes irving
            (irving (like (actor louise) (to irving) (mode (pos)))) ; irving thinks louise likes irving

            (irving (deceive (actor irving) (to joe) (mode (neg)))) ; irving doesn't deceive joe
            (irving (deceive (actor irving) (to louise) (mode (neg)))) ; irving doesn't deceive louise
            (irving (deceive (actor joe) (to irving) (mode (neg)))) ; irving thinks joe doesn't deceive irving
            (irving (deceive (actor louise) (to irving) (mode (neg)))) ; irving thinks louise doesn't deceive irving

            (irving (dominate (actor irving) (to joe) (mode (neg)))) ; irving doesn't dominate joe
            (irving (dominate (actor irving) (to louise) (mode (neg)))) ; irving doesn't dominate louise
            (irving (dominate (actor joe) (to irving) (mode (neg)))) ; irving thinks joe doesn't dominate irving
            (irving (dominate (actor louise) (to irving) (mode (neg)))) ; irving thinks louise doesn't dominate irving

; Louise doesn't care for Joe, but is OK with Irving

            (louise (like (actor louise) (to joe) (mode (neg)))) ; louise likes joe
            (louise (like (actor louise) (to irving) (mode (pos)))) ; louise likes irving
            (louise (like (actor joe) (to louise) (mode (neg)))) ; louise thinks joe likes louise
            (louise (like (actor irving) (to louise) (mode (pos)))) ; louise thinks irving likes louise

            (louise (deceive (actor louise) (to joe) (mode (pos)))) ; louise doesn't deceive joe
            (louise (deceive (actor louise) (to irving) (mode (neg)))) ; louise doesn't deceive irving
            (louise (deceive (actor joe) (to louise) (mode (pos)))) ; louise thinks joe doesn't deceive louise
            (louise (deceive (actor irving) (to louise) (mode (neg)))) ; louise thinks irving doesn't deceive louise

            (louise (dominate (actor louise) (to joe) (mode (neg)))) ; louise doesn't dominate joe
            (louise (dominate (actor louise) (to irving) (mode (neg)))) ; louise doesn't dominate irving
            (louise (dominate (actor joe) (to louise) (mode (pos)))) ; louise thinks joe doesn't dominate louise
            (louise (dominate (actor irving) (to louise) (mode (neg)))) ; louise thinks irving doesn't dominate louise

            (world (thirsty (actor joe) (mode (neg)))) ; joe isn't thirsty
            (world (thirsty (actor irving) (mode (neg)))) ; irving isn't thirsty
            (world (thirsty (actor louise) (mode (pos)))) ; louise is thirsty

            (world (hungry (actor joe) (mode (neg)))) ; joe isn't hungry
            (world (hungry (actor irving) (mode (pos)))) ; irving is hungry
            (world (hungry (actor louise) (mode (neg)))) ; louise isn't hungry

))

(setf *story19*
      '(irving hungry ; irving is hungry
            (joe (loc (actor louise) (val log))) ; joe knows where louise is
            (irving (loc (actor water) (val river))) ; irving knows where there's water
            (irving (loc (actor honey) (val elm-tree))) ; irving knows where there's honey
            (louise (loc (actor worm) (val ground))) ; louise knows where there's a worm

            (joe (like (actor joe) (to irving) (mode (pos)))) ; joe likes irving
            (joe (like (actor joe) (to louise) (mode (pos)))) ; joe likes louise
            (joe (like (actor irving) (to joe) (mode (pos)))) ; joe thinks irving likes joe
            (joe (like (actor louise) (to joe) (mode (pos)))) ; joe thinks louise likes joe

            (joe (deceive (actor joe) (to irving) (mode (neg)))) ; joe doesn't deceive irving
            (joe (deceive (actor joe) (to louise) (mode (neg)))) ; joe doesn't deceive louise
            (joe (deceive (actor irving) (to joe) (mode (neg)))) ; joe thinks irving doesn't deceive joe
            (joe (deceive (actor louise) (to joe) (mode (neg)))) ; joe thinks louise doesn't deceive joe

            (joe (dominate (actor joe) (to irving) (mode (neg)))) ; joe doesn't dominate irving
            (joe (dominate (actor joe) (to louise) (mode (neg)))) ; joe doesn't dominate louise
            (joe (dominate (actor irving) (to joe) (mode (neg)))) ; joe thinks irving doesn't dominate joe
            (joe (dominate (actor louise) (to joe) (mode (neg)))) ; joe thinks louise doesn't dominate joe

            (irving (like (actor irving) (to joe) (mode (pos)))) ; irving likes joe
            (irving (like (actor irving) (to louise) (mode (pos)))) ; irving likes louise
            (irving (like (actor joe) (to irving) (mode (pos)))) ; irving thinks joe likes irving
            (irving (like (actor louise) (to irving) (mode (pos)))) ; irving thinks louise likes irving

            (irving (deceive (actor irving) (to joe) (mode (neg)))) ; irving doesn't deceive joe
            (irving (deceive (actor irving) (to louise) (mode (neg)))) ; irving doesn't deceive louise
            (irving (deceive (actor joe) (to irving) (mode (neg)))) ; irving thinks joe doesn't deceive irving
            (irving (deceive (actor louise) (to irving) (mode (neg)))) ; irving thinks louise doesn't deceive irving

            (irving (dominate (actor irving) (to joe) (mode (neg)))) ; irving doesn't dominate joe
            (irving (dominate (actor irving) (to louise) (mode (neg)))) ; irving doesn't dominate louise
            (irving (dominate (actor joe) (to irving) (mode (neg)))) ; irving thinks joe doesn't dominate irving
            (irving (dominate (actor louise) (to irving) (mode (neg)))) ; irving thinks louise doesn't dominate irving

            (louise (like (actor louise) (to joe) (mode (pos)))) ; louise likes joe
            (louise (like (actor louise) (to irving) (mode (pos)))) ; louise likes irving
            (louise (like (actor joe) (to louise) (mode (pos)))) ; louise thinks joe likes louise
            (louise (like (actor irving) (to louise) (mode (pos)))) ; louise thinks irving likes louise

            (louise (deceive (actor louise) (to joe) (mode (neg)))) ; louise doesn't deceive joe
            (louise (deceive (actor louise) (to irving) (mode (neg)))) ; louise doesn't deceive irving
            (louise (deceive (actor joe) (to louise) (mode (neg)))) ; louise thinks joe doesn't deceive louise
            (louise (deceive (actor irving) (to louise) (mode (neg)))) ; louise thinks irving doesn't deceive louise

            (louise (dominate (actor louise) (to joe) (mode (neg)))) ; louise doesn't dominate joe
            (louise (dominate (actor louise) (to irving) (mode (neg)))) ; louise doesn't dominate irving
            (louise (dominate (actor joe) (to louise) (mode (neg)))) ; louise thinks joe doesn't dominate louise
            (louise (dominate (actor irving) (to louise) (mode (neg)))) ; louise thinks irving doesn't dominate louise

            (world (thirsty (actor joe) (mode (neg)))) ; joe isn't thirsty
            (world (thirsty (actor irving) (mode (neg)))) ; irving isn't thirsty
            (world (thirsty (actor louise) (mode (pos)))) ; louise is thirsty

            (world (hungry (actor joe) (mode (neg)))) ; joe isn't hungry
            (world (hungry (actor irving) (mode (pos)))) ; irving is hungry
            (world (hungry (actor louise) (mode (neg)))) ; louise isn't hungry

))

(setf *story20*
      '(louise thirsty ; louise is thirsty (this may not work at all)
            (joe (loc (actor louise) (val log))) ; joe knows where louise is
            (irving (loc (actor water) (val river))) ; irving knows where there's water
            (irving (loc (actor honey) (val elm-tree))) ; irving knows where there's honey
            (louise (loc (actor worm) (val ground))) ; louise knows where there's a worm

            (joe (like (actor joe) (to irving) (mode (pos)))) ; joe likes irving
            (joe (like (actor joe) (to louise) (mode (pos)))) ; joe likes louise
            (joe (like (actor irving) (to joe) (mode (pos)))) ; joe thinks irving likes joe
            (joe (like (actor louise) (to joe) (mode (pos)))) ; joe thinks louise likes joe

            (joe (deceive (actor joe) (to irving) (mode (neg)))) ; joe doesn't deceive irving
            (joe (deceive (actor joe) (to louise) (mode (neg)))) ; joe doesn't deceive louise
            (joe (deceive (actor irving) (to joe) (mode (neg)))) ; joe thinks irving doesn't deceive joe
            (joe (deceive (actor louise) (to joe) (mode (neg)))) ; joe thinks louise doesn't deceive joe

            (joe (dominate (actor joe) (to irving) (mode (neg)))) ; joe doesn't dominate irving
            (joe (dominate (actor joe) (to louise) (mode (neg)))) ; joe doesn't dominate louise
            (joe (dominate (actor irving) (to joe) (mode (neg)))) ; joe thinks irving doesn't dominate joe
            (joe (dominate (actor louise) (to joe) (mode (neg)))) ; joe thinks louise doesn't dominate joe

            (irving (like (actor irving) (to joe) (mode (pos)))) ; irving likes joe
            (irving (like (actor irving) (to louise) (mode (pos)))) ; irving likes louise
            (irving (like (actor joe) (to irving) (mode (pos)))) ; irving thinks joe likes irving
            (irving (like (actor louise) (to irving) (mode (pos)))) ; irving thinks louise likes irving

            (irving (deceive (actor irving) (to joe) (mode (neg)))) ; irving doesn't deceive joe
            (irving (deceive (actor irving) (to louise) (mode (neg)))) ; irving doesn't deceive louise
            (irving (deceive (actor joe) (to irving) (mode (neg)))) ; irving thinks joe doesn't deceive irving
            (irving (deceive (actor louise) (to irving) (mode (neg)))) ; irving thinks louise doesn't deceive irving

            (irving (dominate (actor irving) (to joe) (mode (neg)))) ; irving doesn't dominate joe
            (irving (dominate (actor irving) (to louise) (mode (neg)))) ; irving doesn't dominate louise
            (irving (dominate (actor joe) (to irving) (mode (neg)))) ; irving thinks joe doesn't dominate irving
            (irving (dominate (actor louise) (to irving) (mode (neg)))) ; irving thinks louise doesn't dominate irving

            (louise (like (actor louise) (to joe) (mode (pos)))) ; louise likes joe
            (louise (like (actor louise) (to irving) (mode (pos)))) ; louise likes irving
            (louise (like (actor joe) (to louise) (mode (pos)))) ; louise thinks joe likes louise
            (louise (like (actor irving) (to louise) (mode (pos)))) ; louise thinks irving likes louise

            (louise (deceive (actor louise) (to joe) (mode (neg)))) ; louise doesn't deceive joe
            (louise (deceive (actor louise) (to irving) (mode (neg)))) ; louise doesn't deceive irving
            (louise (deceive (actor joe) (to louise) (mode (neg)))) ; louise thinks joe doesn't deceive louise
            (louise (deceive (actor irving) (to louise) (mode (neg)))) ; louise thinks irving doesn't deceive louise

            (louise (dominate (actor louise) (to joe) (mode (neg)))) ; louise doesn't dominate joe
            (louise (dominate (actor louise) (to irving) (mode (neg)))) ; louise doesn't dominate irving
            (louise (dominate (actor joe) (to louise) (mode (neg)))) ; louise thinks joe doesn't dominate louise
            (louise (dominate (actor irving) (to louise) (mode (neg)))) ; louise thinks irving doesn't dominate louise

            (world (thirsty (actor joe) (mode (neg)))) ; joe isn't thirsty
            (world (thirsty (actor irving) (mode (neg)))) ; irving isn't thirsty
            (world (thirsty (actor louise) (mode (pos)))) ; louise is thirsty

            (world (hungry (actor joe) (mode (neg)))) ; joe isn't hungry
            (world (hungry (actor irving) (mode (pos)))) ; irving is hungry
            (world (hungry (actor louise) (mode (neg)))) ; louise isn't hungry

))

;(defvar *story2*
;  '(irving thirsty
;    (irving (like (actor joe) (to irving) (mode (neg))))
;    (irving (dominate (actor joe) (to irving) (mode (neg))))
;    (irving (deceive (actor joe) (to irving) (mode (pos))))
;    (irving (like (actor irving) (to joe) (mode (neg))))
;    (joe (deceive (actor irving) (to joe) (mode (neg))))))

