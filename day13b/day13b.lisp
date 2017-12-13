;;;; day13a.lisp

(in-package #:day13b)

(defstruct layer
  depth
  range
  scanner
  direction)

(defun init-layer (depth range)
  (make-layer :depth depth
	      :range range
	      :scanner 0
	      :direction :DOWN))

(defun read-layers(file)
  (with-open-file (in file)
    (loop for line = (read-line in nil)
       while line collect (let ((items (cl-ppcre:split "[^\\d]" line)))
			    (init-layer (parse-integer (first items)) (parse-integer (third items)))))))

(defstruct world
  layers
  packet
  caught
  depth-layers
  )

(defun init-world (layers)
  (let ((world (make-world :layers (make-array (length layers) :initial-contents layers)
			   :depth-layers (make-hash-table)
			   :packet -1
			   :caught nil)))
    (loop for layer in layers do (setf (gethash (layer-depth layer) (world-depth-layers world)) layer))
    world))


(defun read-world(file)
  (init-world (read-layers file)))

(defun packet-layer (world)
  (gethash (world-packet world) (world-depth-layers world)))

(defun world-depth (world)
  (loop for layer across (world-layers world) maximizing (layer-depth layer)))

(defun next-layer!(layer &optional (steps 1))
  (let* ((new-direction (direction-after-ncycles steps (layer-range layer) (layer-scanner layer) (layer-direction layer)))
	 (new-scanner (position-after-ncycles steps (layer-range layer) (layer-scanner layer) (layer-direction layer))))
    (setf (layer-scanner layer) new-scanner)
    (setf (layer-direction layer) new-direction))
  layer)

(defun next-world!(world)
  (incf (world-packet world)) ;; move the packet
  (setf (world-caught world) (and (packet-layer world) (zerop (layer-scanner (packet-layer world))))) ;; got caught?
  (loop for layer across (world-layers world) do (next-layer! layer))
  world)

(defun delay-world!(world cycles)
  (loop for layer across (world-layers world) do (next-layer! layer cycles))
  world)

(defun day13a(world)
  (let ((result 0))
    (dotimes (i (1+ (world-depth world)))
      (next-world! world)
      (when (world-caught world)
	(print "YEAH")
	(incf result (* (layer-depth (packet-layer world)) (layer-range (packet-layer world))))))
    result))

(defun step-in-cicle(position direction range)
  (let ((cycle-size (* 2 (1- range))))
    (if (eq direction :DOWN)
	position
	(- cycle-size position))))

(defun position-after-ncycles (ncycles range position direction)
  (let* ((initial-step-in-cycle (step-in-cicle position direction range))
	 (cycle-size (* 2 (1- range)))
	 (step-in-cycle (mod (+ initial-step-in-cycle ncycles) cycle-size)))
    (if (<= step-in-cycle (1- range))
	step-in-cycle
	(- cycle-size step-in-cycle))))

(defun direction-after-ncycles (ncycles range position direction)
  (let* ((initial-step-in-cycle (step-in-cicle position direction range))
	 (cycle-size (* 2 (1- range)))
	 (step-in-cycle (mod (+ initial-step-in-cycle ncycles) cycle-size)))
    (if (< step-in-cycle (1- range))
	:DOWN
	:UP)))

(defun will-crash-p(world)
  (loop
     for layer across (world-layers world)
     thereis (zerop (position-after-ncycles (layer-depth layer) (layer-range layer) (layer-scanner layer) (layer-direction layer)))))


(defun day13b ()
  (loop for i from 0 upto 10000000
     do (let ((world (read-world "input")))
	  (when (zerop (mod i 1000)) (print i))
	  (delay-world! world i)
	  (let ((result (will-crash-p world)))
	    (unless result (print i) (return i))))))
