; The remove-wall function has been written so as to be as close as possible to the specification.
; The walls are made from a single unicode character, specified by the block keyword, e. g. 
; (maze 20 6 :block #\X). The X character is used by default.

(defun shuffle (list)                        ;; Z not uniform
  (sort list '> :key (lambda(x) (random 1.0))))
 
(defun neighbors (x y maze)
  (remove-if-not
   (lambda (x-y) (and (< -1 (first x-y) (array-dimension maze 0))
                 (< -1 (second x-y) (array-dimension maze 1))))
   `((,x ,(+ y 2)) (,(- x 2) ,y) (,x ,(- y 2)) (,(+ x 2) ,y))))
 
(defun remove-wall (maze x y &optional visited)
  (labels ((walk (maze x y)
             (push (list x y) visited)
             (loop for (u v) in (shuffle (neighbors x y maze))
                unless (member (list u v) visited :test #'equal)
                do (setf (aref maze u v) #\space
                         (aref maze (/ (+ x u) 2) (/ (+ y v) 2)) #\space)
                   (walk maze u v))))
    (setf (aref maze x y) #\space)
    (walk maze x y)))

(defun draw-maze (width height &key (block #\X))
  (let ((maze (make-array (list (1+ (* 2 height)) (1+ (* 2 width)))
                          :element-type 'character :initial-element block)))
    (remove-wall maze (1+ (* 2 (random height))) (1+ (* 2 (random width))))
    (loop for i below (array-dimension maze 0)
          do (fresh-line)
             (loop for j below (array-dimension maze 1)
                   do (princ (aref maze i j))))))

; So, what if we want a maze but don't intend to print it? We can use
; ints for the array values, with 1 for a wall and 0 for a space.
 
(defun build-maze (width height)
  (let ((maze (make-array (list (1+ (* 2 height)) (1+ (* 2 width)))
                          :element-type 'integer :initial-element 1)))
    (remove-wall2 maze (1+ (* 2 (random height))) (1+ (* 2 (random width))))
    maze))

; This version of remove-wall puts in 0 instead of a space (since we're not
; going to actually print the resulting maze)
 
(defun remove-wall2 (maze x y &optional visited)
  (labels ((walk (maze x y)
             (push (list x y) visited)
             (loop for (u v) in (shuffle (neighbors x y maze))
                unless (member (list u v) visited :test #'equal)
                do (setf (aref maze u v) 0
                         (aref maze (/ (+ x u) 2) (/ (+ y v) 2)) 0)
                   (walk maze u v))))
    (setf (aref maze x y) 0)
    (walk maze x y)))

;; assumes 2D maze
;; returns a set of links of the form (node1 node2 distance) - each link is bi-directional
;; also builds up a hash table (*geo-nodes*) of node names indexed by locations
;;
;; This is sheer brute force - could probably be a lot neater.

(defun maze-to-graph (maze)
  (let ((x-dim (1- (array-dimension maze 0)))
        (y-dim (1- (array-dimension maze 1)))
        (distance 1)
        links open-node close-node)
    (setf *geo-nodes* (make-hash-table :test #'equal)) ; initialize *nodes*
    ; find horizontal runs (from left to right)
    (loop for y from 1 to y-dim do
          (loop for x from 1 to x-dim do
                (cond ((is-wall maze x y) ; this is a wall, go on
                       (setf open-node (setf close-node nil)))
                      ((left-wall maze x y) ; leftmost, needs to be a node
                       (cond ((right-wall maze x y) ; oh, no it doesn't
                              (setf open-node nil)
                              (setf close-node nil))
                             (t  ; ah, so there's more to go
                              (setf open-node (find-or-build-node x y)) ; get a node name
                              (setf close-node nil))))
                      (open-node ; not left wall and already working on a right-running link
                       (cond ((right-wall maze x y) ; and can't go any further
                              (setf close-node (find-or-build-node x y)) ; this will be the end of run
                              (setf links (cons (list open-node close-node distance) links)) ; add the open-close pair plus distance to links
                              (setf distance 1)
                              (setf open-node nil) ; no open-node (you're at a wall)
                              (setf close-node nil)) ; no close-node
                             ((or (above-open maze x y) ; if branch above
                                  (below-open maze x y)) ; or branch below - but notably, not right-wall
                              (setf close-node (find-or-build-node x y)) ; this will be the end of run
                              (setf links (cons (list open-node close-node distance) links)) ; add the open-close pair plus distance to links
                              (setf distance 1)
                              (setf open-node close-node) ; this becomes the new open-node
                              (setf close-node nil)) ; and clear close-node
                             (t (setf distance (1+ distance))))) ; don't create a node, but increase the distance of run
                      (t ; you don't have an open node, but this isn't a wall and it isn't leftmost - error?
                       (format t "Suspected error at ~A,~A - no wall, leftmost, or open node.~%" x y)))))
    ; find vertical runs (from top to bottom)
    (loop for x from 1 to x-dim do
          (loop for y from 1 to y-dim do
                (cond ((is-wall maze x y) ; this is a wall, go on
                       (setf open-node (setf close-node nil))) ; clear out
                      ((top-wall maze x y) ; topmost, needs to be a node
                       (cond ((bottom-wall maze x y) ; oh, there's a wall below, don't make it a node
                              (setf open-node nil)
                              (setf close-node nil))
                             (t
                              (setf open-node (find-or-build-node x y)) ; get a node name
                              (setf close-node nil))))
                      (open-node ; not at top and already working on a down-running link
                       (cond ((bottom-wall maze x y) ; and can't go any further
                              (setf close-node (find-or-build-node x y)) ; this will be the end of run
                              (setf links (cons (list open-node close-node distance) links)) ; add the open-close pair plus distance to links
                              (setf distance 1)
                              (setf open-node nil) ; no open-node (you're at a wall)
                              (setf close-node nil)) ; no close-node
                             ((or (left-open maze x y) ; if branch to left
                                  (right-open maze x y)) ; or branch to right - but notably, not bottom-wall
                              (setf close-node (find-or-build-node x y)) ; this will be the end of run
                              (setf links (cons (list open-node close-node distance) links)) ; add the open-close pair plus distance to links
                              (setf distance 1)
                              (setf open-node close-node) ; this becomes the new open-node
                              (setf close-node nil)) ; and clear close-node
                             (t (setf distance (1+ distance))))) ; don't create a node, but increase the distance of run
                      (t ; you don't have an open node, but this isn't a wall and it isn't leftmost - error?
                       (format t "Suspected error at ~A,~A - no wall, topmost, or open node.~%" x y)))))
    links))


(defun is-wall (maze x y) (not (zerop (aref maze x y))))

(defun left-wall (maze x y) (not (left-open maze x y)))

(defun right-wall (maze x y) (not (right-open maze x y)))

(defun top-wall (maze x y) (not (above-open maze x y)))

(defun bottom-wall (maze x y) (not (below-open maze x y)))

(defun above-open (maze x y) (zerop (aref maze x (1- y))))

(defun below-open (maze x y) (zerop (aref maze x (1+ y))))

(defun left-open (maze x y) (zerop (aref maze (1- x) y)))

(defun right-open (maze x y) (zerop (aref maze (1+ x) y)))

; See if there's already a node at this location, and if so return it's name
; If not, generate a name and remember it at this location.

(defvar *geo-nodes*)

(defun find-or-build-node (x y)
  (let ((node-name (gethash (list x y) *geo-nodes*)))
    (cond (node-name node-name)
          (t (setf node-name (gentemp "GEONODE"))
             (setf (gethash (list x y) *geo-nodes*) node-name)
             node-name))))

; (draw-maze 20 6)

; output
;
; XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
; X         X       X     X         X     X
; X XXXXXXX X XXX X XXX X XXXXX XXX X XXXXX
; X X     X   X X X     X       X X X     X
; X XXX X XXXXX X XXX XXXXXXXXXXX X X XXX X
; X   X X X     X X   X     X     X X   X X
; XXX X X XXX X X XXX XXXXX X XXX X XXX X X
; X X X X     X X   X   X   X   X X   X X X
; X X X XXXXXXX XXX XXX X XXXXX XXXXX XXX X
; X   X   X X   X X   X X     X     X   X X
; X XXXXX X X XXX XXX XXXXXXX X X XXXXX X X
; X       X         X         X X         X
; XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
