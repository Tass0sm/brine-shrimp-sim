(in-package #:brine-shrimp-sim)

(defparameter *buf-stream* nil)
(defparameter *gpu-arr* nil)

(defclass camera ()
  ((pos :initform (v! 0 0 0) :accessor pos)
   (rot :initform (q:identity) :accessor rot)))

(defclass thing ()
  ((pos :initform (v! 0 0 0) :accessor pos)
   (rot :initform (q:identity) :accessor rot)))

(defparameter *camera* (make-instance 'camera))

(defparameter *things* (loop for i below 40 collect
                                            (make-instance 'camera)))



(defun-g vert-stage ((vert g-pnt)
                     &uniform (now :float)
                     (model->world :mat4)
                     (world->view :mat4)
                     (view->clip :mat4))
  (let* ((id gl-instance-id)
         (now (+ now id))

         (pos (pos vert))
         (color (+ pos (v! 0.5 0.5 0.5)))

         ;; position the vertex
         (pos (v! pos 1))
         (pos (* model->world pos))

         ;; camera
         (pos (* world->view pos)))
    (values
     (* view->clip pos)
     color)))

(defun-g frag-stage ((color :vec3))
  (v! color 0))

(defpipeline-g cube-pipeline ()
  (vert-stage g-pnt)
  (frag-stage :vec3))

(defun now ()
  (/ (float (get-internal-real-time))
     1000000))

(defun get-world->view-space ()
  ;; Create transform+rotation matrix from v3 pos and quaternion rot
  (m4:* (m4:translation (v3:negate (pos *camera*)))
        (q:to-mat4 (q:inverse (rot *camera*)))))

(defun get-model->world-space (thing)
  (m4:* (m4:translation (pos thing))
        (q:to-mat4 (rot thing))))

(defun update-thing (thing)
  (with-slots (pos) thing
    (setf (y pos)
          (mod (- (y pos) (* 0.00001 (now))) 40f0))))

(defun draw! ()
  (step-host)

  (setf (resolution (current-viewport))
        (surface-resolution (current-surface)))

  (setf (pos *camera*) (v! 0 0 0))
  (setf (rot *camera*) (q:from-axis-angle (v! 1 0 0)
                                          (radians 20)))

  (clear)

  (loop :for thing :in *things* :do
    (update-thing thing)
    (map-g #'cube-pipeline *buf-stream*
           :now (now)
           :model->world (get-model->world-space thing)
           :world->view (get-world->view-space)
           :view->clip (rtg-math.projection:perspective
                        (x (resolution (current-viewport)))
                        (y (resolution (current-viewport)))
                        0.1
                        30f0
                        60f0)))
  (swap))

(defun init ()
  (loop :for thing :in *things* :do
    (setf (pos thing)
          (v3:+ (v! 0 0 -25)
                (v! (- (random 20) 10)
                    (- (random 20) 10)
                    (- (random 20) 10)))))

  (unless *buf-stream*
    (destructuring-bind (vert index)
        (nineveh.mesh.data.primitives:cube-gpu-arrays)
      (setf *buf-stream*
            (make-buffer-stream vert :index-array index)))

    (setf *gpu-arr* (first (first (buffer-stream-gpu-arrays *buf-stream*))))))

(nineveh:def-simple-main-loop play (:on-start #'init)
  (draw!))
