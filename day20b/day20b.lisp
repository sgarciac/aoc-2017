;;;; day20b.lisp

(in-package #:day20b)

(defstruct vals
  x
  y
  z)

(defstruct particle
  id
  pos 
  speed
  acc)

(defun parse-particle (line id)
  (let ((items (mapcar #'parse-integer (remove "" (cl-ppcre:split "[^-\\d]" line) :test #'string=))))
    (make-particle
     :id id
     :pos
     (make-vals
      :x (nth 0 items)
      :y (nth 1 items)
      :z (nth 2 items))
     :speed
     (make-vals
      :x (nth 3 items)
      :y (nth 4 items)
      :z (nth 5 items))
     :acc
     (make-vals
      :x (nth 6 items)
      :y (nth 7 items)
      :z (nth 8 items)))))


(defun distance (p1 p2)
  (let ((d (+ (abs (- (vals-x (particle-pos p1))
                      (vals-x (particle-pos p2))))
              (abs (- (vals-y (particle-pos p1))
                      (vals-y (particle-pos p2))))
              (abs (- (vals-z (particle-pos p1))
                      (vals-z (particle-pos p2)))))))
    d))

(defun read-input (file)
  (with-open-file (in file)
    (loop
       for line = (read-line in nil)
       for i = 0 then (1+ i)
       while line collect (parse-particle line i))))

(defun update-particle! (particle)
  (incf (vals-x (particle-speed particle)) (vals-x (particle-acc particle)))
  (incf (vals-y (particle-speed particle)) (vals-y (particle-acc particle)))
  (incf (vals-z (particle-speed particle)) (vals-z (particle-acc particle)))
  (incf (vals-x (particle-pos particle)) (vals-x (particle-speed particle)))
  (incf (vals-y (particle-pos particle)) (vals-y (particle-speed particle)))
  (incf (vals-z (particle-pos particle)) (vals-z (particle-speed particle))))

(defun min-distance (particles)
  "find the min distance between particles in a set"
  (loop for p1 in particles
     minimizing (loop for p2 in particles
                   when (not (eq p1 p2)) minimizing (distance p1 p2))))

(defun collision-groups (particles)
  (let ((c (make-hash-table :test 'equalp)))
    (loop for particle in particles
       do (progn
            (when (not (gethash (particle-pos particle) c))
              (setf (gethash (particle-pos particle) c) '()))
            (push particle (gethash (particle-pos particle) c))))
    (loop for k being the hash-keys in c using (hash-value v) when (> (length v) 1) collect v)))



(collision-groups (read-input "input"))

(min-distance (read-input "input"))


(loop
   with particles = (read-input "input")
   for i from 0 upto 100
   for mindist = 100000000000000000 then (min-distance particles)
   and previous-mindist = 100000000000000001 then mindist
   do (let ((collisions (collision-groups particles)))
        (format t "~A ~A ~A~%" (length particles) mindist (length collisions))
        (loop for group in collisions
           do (loop for particle in group
                 do (setf particles (remove particle particles))))
        (loop for particle in particles do (update-particle! particle))
        (setf pepe particles)
        ))

(min-distance
 pepe)


