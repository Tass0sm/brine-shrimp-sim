(in-package #:brine-shrimp-sim)

(defparameter *buf-stream* nil)
(defparameter *gpu-arr* nil)
(defparameter *light-pos* nil)
(defparameter *albedo-sampler* nil)
(defparameter *specular-sampler* nil)

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
         (normal (norm vert))
         (uv (tex vert))

         (color (+ pos (v! 0.5 0.5 0.5)))

         ;; model to world
         (model-pos (v! pos 1))
         (world-pos (* model->world model-pos))
         (world-norm (* (m4:to-mat3 model->world)
                        normal))

         ;; camera
         (view-pos (* world->view world-pos))

         (clip-pos (* view->clip view-pos)))
    (values
     clip-pos
     (s~ world-pos :xyz)
     world-norm
     uv)))

(defun-g frag-stage ((world-pos :vec3)
                     (frag-normal :vec3)
                     (uv :vec2)
                     &uniform
                     (light-pos :vec3)
                     (cam-pos :vec3)
                     (albedo :sampler-2d)
                     (spec-map :sampler-2d))
  (let* ((object-color (texture albedo uv))
         (frag-normal (normalize frag-normal))

         (ambient 0.1)

         (vec-to-light (- light-pos world-pos))
         (dir-to-light (normalize vec-to-light))
         (diffuse (saturate
                   (dot dir-to-light frag-normal)))

         (vec-to-cam (- cam-pos world-pos))
         (dir-to-cam (normalize vec-to-cam))
         (reflection (normalize
                      (reflect (- dir-to-light)
                               frag-normal)))

         (specular-power (texture spec-map uv))
         (specular (* (expt (saturate (dot reflection dir-to-cam))
                            64)
                      2
                      specular-power))

         (light-amount (+ ambient
                          diffuse
                          specular)))
    (* object-color light-amount)))

(defpipeline-g cube-pipeline ()
  (vert-stage g-pnt)
  (frag-stage :vec3 :vec3 :vec2))

(defun now ()
  (/ (float (get-internal-real-time))
     500000))

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
          (mod (- (y pos) (* 0.0001 (now))) 40f0))))

(defun draw! ()
  (step-host)

  (setf (resolution (current-viewport))
        (surface-resolution (current-surface)))

  (setf *light-pos* (v! (* 10 (sin (now)))
                        (* (sin (now)) (sin (* 2.7 (now))))
                        (+ (* 10 (cos (now))) -15)))
  (setf (pos *camera*) (v! 1 -2 -13))
  (setf (rot *camera*) (q:from-axis-angle (v! 1 0 0)
                                          (radians 0)))

  (clear)

  (loop :for thing :in *things* :do
    ;; (update-thing thing)
    (map-g #'cube-pipeline *buf-stream*
           :now (now)
           :light-pos *light-pos*
           :cam-pos (pos *camera*)
           :model->world (get-model->world-space thing)
           :world->view (get-world->view-space)
           :view->clip (rtg-math.projection:perspective
                        (x (resolution (current-viewport)))
                        (y (resolution (current-viewport)))
                        0.1
                        30f0
                        60f0)
           :albedo *albedo-sampler*
           :spec-map *specular-sampler*))
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

    (setf *gpu-arr* (first (first (buffer-stream-gpu-arrays *buf-stream*)))))

  (unless *albedo-sampler*
    (setf *albedo-sampler*
          (sample
           (dirt:load-image-to-texture
            (asdf:system-relative-pathname
             :brine-shrimp-sim "container-albedo.png")))))

  (unless *specular-sampler*
    (setf *specular-sampler*
          (sample
           (dirt:load-image-to-texture
            (asdf:system-relative-pathname
             :brine-shrimp-sim "container-specular.png"))))))

(nineveh:def-simple-main-loop play (:on-start #'init)
  (draw!))
