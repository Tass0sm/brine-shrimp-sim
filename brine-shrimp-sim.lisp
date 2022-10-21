(in-package :brine-shrimp-sim)

(defparameter *gpu-verts-array* nil)
(defparameter *gpu-index-array* nil)
(defparameter *vertex-stream* nil)
(defparameter *viewport* nil)
(defparameter *tex* nil)
(defparameter *sampler* nil)
(defparameter *fbo* nil)
(defparameter *fbo-tex* nil)
(defparameter *fbo-tex-sampler* nil)

(defstruct-g our-vert
  (pos :vec2)
  (uv :vec2)) ;; texture coordinates

;; note the use of implicit uniform capture with *loop*
;; special vars in scope can be used inline. During compilation
;; cepl will try work out the type. It does this by seeing if the
;; symbol is bound to a value, and if it is it checks the type of
;; the value for a suitable matching varjo type
(defun-g calc-pos ((v our-vert))
  (values (v! (our-vert-pos v) 0.0 1.0)
          ;; Second value is passed onto next stage.
          (our-vert-uv v)))

(defun-g frag-shader ((uv :vec2)
                      &uniform (sam :sampler-2d))
  (texture sam uv))

;; Also showing that we can use gpu-lambdas inline in defpipeline-g
;; It's not usually done as reusable functions are generally nicer
;; but I wanted to show that it was possible :)
(defpipeline-g prog-1 ()
  (calc-pos our-vert)
  (frag-shader :vec2))

(defun init ()
  (when *gpu-verts-array*
    (free *gpu-verts-array*))

  (when *gpu-index-array*
    (free *gpu-index-array*))

  (setf *gpu-verts-array*
        (make-gpu-array
         (list (list (vec2 -0.5  0.5) (vec2 0.0 0.0))
               (list (vec2 -0.9 -0.9) (vec2 0.0 1.0))
               (list (vec2  0.5 -0.5) (vec2 1.0 1.0))
               (list (vec2  0.9  0.9) (vec2 1.0 0.0)))
         :element-type 'our-vert))

  (setf *gpu-index-array*
        (make-gpu-array
         (list 0 1 2 0 2 3)
         :element-type :uint))

  (setf *vertex-stream*
        (make-buffer-stream *gpu-verts-array*
                            :index-array *gpu-index-array*))

  (setf *viewport* (make-viewport '(400 400)))

  (setf *tex* (dirt:load-image-to-texture "./images/baggers.png"))

  (setf *sampler* (sample *tex*))

  (setf *fbo* (with-viewport *viewport*
                (make-fbo 0)))

  (setf *fbo-tex* (gpu-array-texture (attachment *fbo* 0))))

(defun draw! ()
  (with-viewport *viewport*
    (clear)
    (map-g #'prog-1 *vertex-stream*
           :sam *sampler*)
    (swap)))
