(in-package #:kons-9)

;;;; drawing-settings ==========================================================

(defclass drawing-settings ()
  ((monitor-scale :accessor monitor-scale :initarg :monitor-scale :initform 1.0)
   (default-font :accessor default-font :initarg :default-font :initform (asdf:system-relative-pathname "kons-9" "data/font/DejaVuSansMono.ttf"))
   (point-size :accessor point-size :initarg :point-size :initform 3.0)
   (line-thickness :accessor line-thickness :initarg :line-thickness :initform 1.0)
   (fg-color :accessor fg-color :initarg :fg-color :initform (c! 0 0 0))
   (bg-color :accessor bg-color :initarg :bg-color :initform (c! 1 1 1))
   (sel-color :accessor sel-color :initarg :sel-color :initform (c! 1 0 0))
   (shading-color :accessor shading-color :initarg :shading-color :initform (c! 1 1 1))
   (light-color :accessor light-color :initarg :light-color :initform (c! 2 2 2))
   (axes-size :accessor axes-size :initarg :axes-size :initform 3.0)
   (axes-thickness :accessor axes-thickness :initarg :axes-thickness :initform 3.0)
   (ground-plane-size :accessor ground-plane-size :initarg :ground-plane-size :initform 10.0)
   (ground-plane-segments :accessor ground-plane-segments :initarg :ground-plane-segments :initform 10)
   (ground-plane-thickness :accessor ground-plane-thickness :initarg :ground-plane-thickness :initform 1.0)
   (ground-plane-color :accessor ground-plane-color :initarg :ground-plane-color :initform (c! .8 .8 .8))
   (secondary-line-thickness :accessor secondary-line-thickness :initarg :secondary-line-thickness :initform 1.0)))

;;(defparameter *drawing-settings* (make-instance 'drawing-settings))

(defun set-lines-thin ()
  (let* ((drawing-settings (application-drawing-settings *app*))
	 (monitor-scale (monitor-scale drawing-settings)))
    (setf (point-size drawing-settings) (* 3.0 monitor-scale))
    (setf (line-thickness drawing-settings) (* 1.0 monitor-scale))
    (setf (axes-thickness drawing-settings) (* 3.0 monitor-scale))
    (setf (secondary-line-thickness drawing-settings) (* 0.5 monitor-scale))))

(defun set-lines-thick ()
  (let* ((drawing-settings (application-drawing-settings *app*))
	 (monitor-scale (monitor-scale drawing-settings)))
    (setf (point-size drawing-settings) (* 6.0 monitor-scale))
    (setf (line-thickness drawing-settings) (* 2.0 monitor-scale))
    (setf (axes-thickness drawing-settings) (* 5.0 monitor-scale))
    (setf (secondary-line-thickness drawing-settings) (* 1.0 monitor-scale))))

(defun set-theme-bright ()
  (let* ((drawing-settings (application-drawing-settings *app*)))
    (setf (fg-color drawing-settings) (c! 0 0 0))
    (setf (bg-color drawing-settings) (c! 1 1 1))
    (set-ground-plane-bright)))

(defun set-theme-dark ()
  (let* ((drawing-settings (application-drawing-settings *app*)))
    (setf (fg-color drawing-settings) (c! 1 1 1))
    (setf (bg-color drawing-settings) (c! 0 0 0))
    (set-ground-plane-dark)))

(defun set-ground-plane-bright ()
  (setf (ground-plane-color (application-drawing-settings *app*)) (c! .8 .8 .8)))

(defun set-ground-plane-dark ()
  (setf (ground-plane-color (application-drawing-settings *app*)) (c! .2 .2 .2)))

(defun set-ground-plane-sparse ()
  (setf (ground-plane-segments (application-drawing-settings *app*)) 10))

(defun set-ground-plane-dense ()
  (setf (ground-plane-segments (application-drawing-settings *app*)) 40))

#| test changing appearance

(set-lines-thin)
(set-lines-thick)
(set-theme-bright)
(set-theme-dark)
(set-ground-plane-bright)
(set-ground-plane-dark)
(set-ground-plane-sparse)
(set-ground-plane-dense)

|#

;;;; utils =====================================================================
#-krma
(defmacro with-gl-enable (flag &body body)
  `(progn
     (gl:enable ,flag)
     (let ((result (progn ,@body)))
       (gl:disable ,flag)
       result)))

#-krma
(defmacro with-gl-disable (flag &body body)
  `(progn
     (gl:disable ,flag)
     (let ((result (progn ,@body)))
       (gl:enable ,flag)
       result)))

#-krma
(defun gl-set-color (col)
  (gl:color (c-red col) (c-green col) (c-blue col)))

#+krma
(defun gl-set-color (col)
  (declare (ignore col))
  (values))

#-krma
(defun gl-set-fg-color ()
  (gl-set-color (fg-color (application-drawing-settings *app*))))

#+krma
(defun gl-set-fg-color ()
  (values))

#-krma
(defun gl-set-sel-color ()
  (gl-set-color (sel-color (application-drawing-settings *app*))))

#+krma
(defun gl-set-sel-color ()
  (values))

;;;; graphics ==================================================================

#-krma
(defun draw-world-axes ()
  (let* ((drawing-settings (application-drawing-settings *app*))
	 (size (axes-size drawing-settings)))
    (gl:line-width (axes-thickness drawing-settings))
    (gl:begin :lines)
    (gl:color 1.0 0.0 0.0)
    (gl:vertex 0.0  0.001  0.0)
    (gl:vertex size 0.001 0.0)
    (gl:color 0.0 1.0 0.0)
    (gl:vertex 0.0 0.0 0.0 )
    (gl:vertex 0.0 size 0.0 )
    (gl:color 0.0 0.0 1.0)
    (gl:vertex 0.0  0.001  0.0)
    (gl:vertex 0.0  0.001 size)
    (gl:end)))

#-krma
(defun draw-ground-plane ()
  (let* ((drawing-settings (application-drawing-settings *app*))
	 (size (ground-plane-size drawing-settings))
	 (segs (ground-plane-segments drawing-settings))
	 (col (ground-plane-color drawing-settings))
	 (thick (ground-plane-thickness drawing-settings)))
    (gl-set-color col)
    (gl:line-width thick)
    (gl:begin :lines)
    (dotimes (i (1+ segs))
      (let* ((f (/ i segs))
             (coord (lerp f (- size) size)))
        (gl:vertex coord 0.0 (- size))
        (gl:vertex coord 0.0    size)
        (gl:vertex (- size) 0.0 coord)
        (gl:vertex    size  0.0 coord)))
    (gl:end)))

;;(defparameter *viewport-aspect-ratio* (/ 16.0 9.0))

;;(defparameter *cam-x-rot* 0.0)
;;(defparameter *cam-y-rot* 0.0)
;;(defparameter *cam-fwd-dist* 0.0)
;;(defparameter *cam-side-dist* 0.0)
;;(defparameter *cam-up-dist* 0.0)

;;(defparameter *do-lighting?* t)
;;(defparameter *display-filled?* t)
;;(defparameter *display-wireframe?* t)
;;(defparameter *display-points?* t)
;;(defparameter *do-backface-cull?* t)
;;(defparameter *do-smooth-shading?* nil)
;;(defparameter *display-ground-plane?* t)
;;(defparameter *display-axes?* t)

(defun init-view-camera ()
  (with-slots (cam-x-rot
	       cam-y-rot
	       cam-fwd-dist
	       cam-side-dist
	       cam-up-dist)
      (application-scene *app*)
    (setf cam-x-rot 15.0)
    (setf cam-y-rot -35.0)
    (setf cam-fwd-dist -10.0)
    (setf cam-side-dist 0.0)
    (setf cam-up-dist 0.0))) ;-2.0))

#-krma
(defun gl-enable-light (light-id dir &optional (color (light-color (application-drawing-settings *app*))))
  (gl:enable light-id)
  (gl:light light-id :position (vector (p:x dir) (p:y dir) (p:z dir) 0.0))
  (gl:light light-id :ambient (vector 0.25 0.25 0.25 1.0))
  (gl:light light-id :diffuse color)
  (gl:light light-id :specular color))

#+krma
(defun gl-enable-light (light-id dir &optional (color (light-color (application-drawing-settings *app*))))
  (declare (ignore light-id color))
  (setf (krma:scene-light-position (application-scene *app*)) (3d-vectors:vec3 (p:x dir) (p:y dir) (p:z dir)))
  (values))

#-krma
(defun gl-disable-light (light-id)
  (gl:disable light-id))

#+krma
(defun gl-disable-light (light-id)
  (declare (ignore light-id))
  (setf (krma:scene-light-position (application-scene *app*)) nil)
  (values))

#-krma
(defun gl-set-material (&optional (diff (shading-color (application-drawing-settings *app*))) (spec (c! 0 0 0)) (shine 0.0))
  (gl:material :front-and-back :diffuse diff)
  (gl:material :front-and-back :specular spec)
  (gl:material :front-and-back :shininess shine))

#+krma
(defun gl-set-material (&optional (diff (shading-color (application-drawing-settings *app*))) (spec (c! 0 0 0)) (shine 0.0))
  (declare (ignore diff spec shine))
  (values))

#-krma
(defun 3d-update-light-settings ()
  (with-slots (do-backface-cull?
		do-lighting?
		cam-x-rot
		cam-y-rot)
      (application-scene *app*)
    (if do-backface-cull?
	(progn
	  (gl:enable :cull-face)
	  (gl:light-model :light-model-two-side :false))
	(progn
	  (gl:disable :cull-face)
	  (gl:light-model :light-model-two-side :true)))

    (if do-lighting?
	(gl:enable :lighting)
	(gl:disable :lighting))
    (let ((mtx (matrix-multiply (make-x-rotation-matrix (- (radians cam-x-rot)))
				(make-y-rotation-matrix (- (radians cam-y-rot))))))
      (gl-enable-light :light0 (transform-point (p! 0 0 1) mtx) (c! 0.7 0.7 0.7)))))

#-krma
(defun 3d-setup-buffer ()
  (let ((bg-color (bg-color (application-drawing-settings *app*))))
    (gl:clear-color (c-red bg-color) (c-green bg-color) (c-blue bg-color) 0.0)
    (gl:clear :color-buffer-bit :depth-buffer-bit)
    (gl:enable :depth-test)
    (gl:cull-face :back)))

#+krma
(defun 3d-setup-buffer ()
  (let ((bg-color (application-drawing-settings *app*)))
    #-darwin
    (setf (vk::clear-value (vk::main-window *app*)) bg-color)))

#-krma
(defun 3d-setup-projection ()
  (with-slots (viewport-aspect-ratio
	       cam-side-dist
	       cam-up-dist
	       cam-fwd-dist
	       cam-x-rot
	       cam-y-rot)
      (application-scene *app*)
    (gl:matrix-mode :projection)
    (gl:load-identity)
    (glu:perspective 45.0d0 (coerce viewport-aspect-ratio 'double-float) 0.01d0 1000.0d0)
    (gl:matrix-mode :modelview)
    (gl:load-identity)
    (gl:translate cam-side-dist cam-up-dist cam-fwd-dist)
    (gl:rotate cam-x-rot 1.0 0.0 0.0)
    (gl:rotate cam-y-rot 0.0 1.0 0.0)))

#+krma
(defun 3d-setup-projection (&optional (scene (application-scene *app*)))
  (with-slots (viewport-aspect-ratio
	       cam-side-dist
	       cam-up-dist
	       cam-fwd-dist
	       cam-x-rot
	       cam-y-rot)
      scene
    (3d-matrices:m* (krma:mperspective-vulkan 45.0d0 (coerce viewport-aspect-ratio 'double-float) 0.01d0 1000.0d0)
		    (3d-matrices:nmrotate
		     cam-y-rot
		     (3d-matrices:nmrotate
		      cam-x-rot
		      (3d-matrices:nmtranslate (3d-matrices:meye 4)
					       (3d-vectors:vec3 cam-side-up cam-up-dist cam-fwd-dist))
		      (3d-vectors:vec3 1.0 0.0 0.0))
		     (3d-vectors:vec3 0.0 1.0 0.0)))))

#-krma
(defun 3d-cleanup-render ()
  (gl:disable :lighting))

#+krma
(defun 3d-cleanup-render ())

#-krma
(defun 3d-flush-render ()
  (gl-set-fg-color)
  (gl:flush))

#+krma
(defun 3d-flush-render ())

;;; 3d display =================================================================

#-krma
(defun 3d-translate (p)
  (gl:translate (p:x p) (p:y p) (p:z p)))

#+krma
(defun 3d-translate (p &optional (m (3d-matrices:meye 4)))
  (3d-matrices:nmtranslate m (3d-vectors:vec3 (p:x p) (p:y p) (p:z p))))


#-krma
(defun 3d-push-matrix (matrix)
  (gl:push-matrix)
  (gl:mult-matrix (matrix->vector matrix))) ;is order correct?

#+krma
(defun 3d-push-matrix (current new)
  (3d-matrices:m* new current))

#-krma
(defun 3d-draw-marker (size)
  (gl:color 1.0 1.0 0.0)
  (gl:line-width (* 2 (line-thickness (application-drawing-settings *app*))))
  (gl:begin :lines)
  (gl:vertex    size  0.0  0.0)
  (gl:vertex (- size) 0.0  0.0)
  (gl:vertex  0.0    size  0.0)
  (gl:vertex  0.0 (- size) 0.0)
  (gl:vertex  0.0  0.0    size )
  (gl:vertex  0.0  0.0 (- size))
  (gl:end))

#+krma
(defun 3d-draw-marker (size &optional (scene (application-scene *app*)))
  (let ((line-width (* 2 (line-thickness (application-drawing-settings *app*)))))
    (krma:scene-draw-3d-line scene #xffff00ff line-width size 0.0 0.0 (- size) 0.0 0.0)
    (krma:scene-draw-3d-line scene #xffff00ff line-width 0.0 size 0.0 0.0 (- size) 0.0)
    (krma:scene-draw-3d-line scene #xffff00ff line-width 0.0 0.0 size 0.0 0.0 (- size))))

#-krma
(defun 3d-draw-axis (size)
  (with-gl-disable :lighting
    (gl:line-width (line-thickness (application-drawing-settings *app*)))
    (gl:begin :lines)
    ;; x axis (red)
    (gl:color 1.0 0.0 0.0)
    (gl:vertex 0.0 0.0 0.0)
    (gl:vertex size 0.0 0.0)
    ;; y axis (green)
    (gl:color 0.0 1.0 0.0)
    (gl:vertex 0.0 0.0 0.0)
    (gl:vertex 0.0 size 0.0)
    ;; z axis (blue)
    (gl:color 0.0 0.0 1.0)
    (gl:vertex 0.0 0.0 0.0)
    (gl:vertex 0.0 0.0 size)
    (gl:end)))

#+krma
(defun 3d-draw-axis (size)
  (let ((scene (application-scene *app*))
	(line-width (line-thickness (application-drawing-settings *app*))))
    (krma:scene-draw-3d-line scene #xff0000ff line-width 0.0f0 0.0f0 0.0f0 size 0.0f0 0.0f0)
    (krma:scene-draw-3d-line scene #x00ff00ff line-width 0.0f0 0.0f0 0.0f0 0.0f0 size 0.0f0)
    (krma:scene-draw-3d-line scene #x0000ffff line-width 0.0f0 0.0f0 0.0f0 0.0f0 0.0f0 size)))

#-krma
(defun 3d-draw-bounds (lo hi color)
  (let ((drawing-settings (application-drawing-settings *app*)))
    (with-gl-disable :lighting
      (gl:line-width (* 2 (line-thickness drawing-settings))) ;otherwise not visible for cubes etc.
      (gl-set-color color)
      (when (and lo hi)
	(let ((x0 (p:x lo))
              (y0 (p:y lo))
              (z0 (p:z lo))
              (x1 (p:x hi))
              (y1 (p:y hi))
              (z1 (p:z hi)))
          (gl:begin :lines)

          (gl:vertex x0 y0 z0) (gl:vertex x1 y0 z0)
          (gl:vertex x1 y0 z0) (gl:vertex x1 y0 z1)
          (gl:vertex x1 y0 z1) (gl:vertex x0 y0 z1)
          (gl:vertex x0 y0 z1) (gl:vertex x0 y0 z0)
        
          (gl:vertex x0 y1 z0) (gl:vertex x1 y1 z0)
          (gl:vertex x1 y1 z0) (gl:vertex x1 y1 z1)
          (gl:vertex x1 y1 z1) (gl:vertex x0 y1 z1)
          (gl:vertex x0 y1 z1) (gl:vertex x0 y1 z0)
        
          (gl:vertex x0 y0 z0) (gl:vertex x0 y1 z0)
          (gl:vertex x1 y0 z0) (gl:vertex x1 y1 z0)
          (gl:vertex x1 y0 z1) (gl:vertex x1 y1 z1)
          (gl:vertex x0 y0 z1) (gl:vertex x0 y1 z1)
        
          (gl:end)))
      (gl-set-color (shading-color drawing-settings)))))    ;reset color

#+krma
(defun 3d-draw-bounds (lo hi color)
  (let* ((drawing-settings (application-drawing-settings *app*))
	 (scene (application-scene *app*))
	 (line-width (* 2 (line-thickness drawing-settings)))
	 (color (krma-canonicalize-color color)))
    (when (and lo hi)
      (let ((x0 (p:x lo))
	    (y0 (p:y lo))
	    (z0 (p:z lo))
	    (x1 (p:x hi))
	    (y1 (p:y hi))
	    (z1 (p:y hi)))

	(krma:scene-draw-3d-line scene color line-width x0 y0 x0 x1 y0 z0)
	(krma:scene-draw-3d-line scene color line-width x1 y0 z0 x1 y0 z1)
	(krma:scene-draw-3d-line scene color line-width x1 y0 z1 z0 y0 z1)
	(krma:scene-draw-3d-line scene color line-width x0 y0 z1 x0 y0 z0)

	(krma:scene-draw-3d-line scene color line-width x0 y1 z0 x1 y1 z0)
	(krma:scene-draw-3d-line scene color line-width x1 y1 z0 x1 y1 z1)
	(krma:scene-draw-3d-line scene color line-width x1 y1 z1 x0 y1 z1)
	(krma:scene-draw-3d-line scene color line-width x0 y1 z1 x0 y1 z0)

	(krma:scene-draw-3d-line scene color line-width x0 y0 z0 x0 y1 z0)
	(krma:scene-draw-3d-line scene color line-width x1 y0 z0 x1 y1 z0)
	(krma:scene-draw-3d-line scene color line-width x1 y0 z1 x1 y1 z1)
	(krma:scene-draw-3d-line scene color line-width x0 y0 z1 x0 y1 z1)))))

#-krma
(defun 3d-pop-matrix ()
  (gl:pop-matrix))

#+krma
(defun 3d-pop-matrix ())

#-krma
(defun 3d-draw-curve (points is-closed? &optional (line-width (line-thickness (application-drawing-settings *app*))))
  (with-gl-disable :lighting
    (gl-set-fg-color)
    (gl:line-width line-width)
    (if is-closed?
        (gl:begin :line-loop)
        (gl:begin :line-strip))
    (do-array (i p points)
      (gl:vertex (p:x p) (p:y p) (p:z p)))
    (gl:end)))

#+krma
(defun 3d-draw-curve (points is-closed? &optional (line-width (line-thickness (application-drawing-settings *app*))))
  (krma:scene-draw-3d-polyline-1 (application-scene *app*) is-closed? line-width
				 (fg-color (application-drawing-settings *app*))
				 points))


#-krma
(defun 3d-draw-points (points point-colors &key (highlight? nil))
  (let ((drawing-settings (application-drawing-settings *app*)))
    (with-gl-disable :lighting
      (if highlight?
          (progn
            (gl-set-sel-color)
            (gl:point-size (* 2 (point-size drawing-settings))))
          (progn
            (gl-set-fg-color)
            (gl:point-size (point-size drawing-settings))))
      (gl:begin :points)
      (cond (point-colors
             (do-array (i p points)
		       (let ((c (aref point-colors i)))
			 (gl:color (c-red c) (c-green c) (c-blue c)))
		       (gl:vertex (p:x p) (p:y p) (p:z p))))
            (t
             (do-array (i p points)
		       (gl:vertex (p:x p) (p:y p) (p:z p)))))
      (gl:end))))

#+krma
(defun 3d-draw-points (points point-colors &key (highlight? nil))
  (let* ((drawing-settings (application-drawing-settings *app*))
	 (scene (application-scene *app*))
	 (point-size (point-size drawing-settings)))
    (cond (point-colors
	   (do-array (i p points)
		     (krma:scene-draw-3d-point scene (aref point-colors i) point-size (p:x p) (p:y p) (p:z p))))
	  (t
	   (let ((color (if highlight?
			    (sel-color (application-drawing-settings *app*))
			    (fg-color (application-drawing-settings *app*)))))
	     (do-array (i p points)
		       (krma:scene-draw-3d-point scene color point-size (p:x p) (p:y p) (p:z p))))))))

#-krma
(defun 3d-draw-lines (points &key (highlight? nil))
  (let ((drawing-settings (application-drawing-settings *app*)))
    (with-gl-disable :lighting
      (gl-set-fg-color)
      (if highlight?
          (progn
            (gl-set-sel-color)
            (gl:line-width (* 2 (line-thickness drawing-settings))))
          (progn
            (gl-set-fg-color)
            (gl:line-width (line-thickness drawing-settings))))
      (gl:begin :lines)
      (dolist (p points)
	(gl:vertex (p:x p) (p:y p) (p:z p)))
      (gl:end))))

#+krma
(defun 3d-draw-lines (points &key (highlight? nil))
  (let* ((drawing-settings (application-drawing-settings *app*))
	 (scene (application-scene *app*))
	 (color (if highlight?
		    (sel-color (application-drawing-settings *app*))
		    (fg-color (application-drawing-settings *app*))))
	 (line-width (line-thickness drawing-settings)))
    
    (loop for (head tail) on points by #'cddr
	  do (krma:scene-draw-3d-line scene color line-width
				     (p:x tail) (p:y tail) (p:z tail)
				     (p:x head) (p:y head) (p:z head)))))
    

#-krma
(defun 3d-setup-lighting (&optional (scene (application-scene *app*)))
  (if (scene-do-lighting? scene)
      (gl:enable :lighting)
      (gl:disable :lighting)))

#+krma
(defun 3d-setup-lighting (&optional scene)
  (declare (ignore scene))
  (values))

#-krma
(defun 3d-draw-filled-polygons (points faces face-normals point-normals point-colors
				&optional (scene (application-scene *app*)))
  (if (scene-do-smooth-shading? scene)
      (gl:shade-model :smooth)
      (gl:shade-model :flat))
  (gl:polygon-mode :front-and-back :fill)
  (with-gl-enable :normalize
    (with-gl-enable :polygon-offset-fill
      (gl:polygon-offset 1.0 1.0)
      (3d-draw-filled-polygons-aux points faces face-normals point-normals point-colors))))

#+krma
(defun 3d-draw-filled-polygons (points faces face-normals point-normals point-colors
				&optional (scene (application-scene *app*)))
  (3d-draw-filled-polygons-aux points faces face-normals point-normals point-colors scene))

#-krma
(defmethod 3d-draw-filled-polygons-aux (points faces face-normals point-normals point-colors
					&optional (scene (application-scene *app*)))
  (let* ((col (shading-color (application-drawing-settings *app*)))
         (r (c-red col))
         (g (c-green col))
         (b (c-blue col)))
    (gl-set-material col)
    (gl:color-material :front-and-back :diffuse)
    (with-gl-enable :color-material
      (cond ((and (> (length point-colors) 0) (scene-do-smooth-shading? scene))
             (dotimes (f (length faces))
               (gl:begin :polygon)
               (dolist (pref (aref faces f))
                 (let ((p (aref points pref))
                       (n (aref point-normals pref))
                       (c (aref point-colors pref)))
                   (gl:color (c-red c) (c-green c) (c-blue c))
                   (gl:normal (p:x n) (p:y n) (p:z n))
                   (gl:vertex (p:x p) (p:y p) (p:z p))))
               (gl:end)))
            ((> (length point-colors) 0)
             (dotimes (f (length faces))
               (gl:begin :polygon)
               (let ((n (aref face-normals f)))
                 (gl:normal (p:x n) (p:y n) (p:z n)))
               (dolist (pref (aref faces f))
                 (let ((p (aref points pref))
                       (c (aref point-colors pref)))
                   (gl:color (c-red c) (c-green c) (c-blue c))
                   (gl:vertex (p:x p) (p:y p) (p:z p))))
               (gl:end)))
              ((scene-do-smooth-shading? scene)
               (dotimes (f (length faces))
                 (gl:begin :polygon)
                 (dolist (pref (aref faces f))
                   (let ((p (aref points pref))
                         (n (aref point-normals pref)))
                     (gl:normal (p:x n) (p:y n) (p:z n))
                     (gl:color r g b)
                     (gl:vertex (p:x p) (p:y p) (p:z p))))
                 (gl:end)))
              (t
               (dotimes (f (length faces))
                 (gl:begin :polygon)
                 (let ((n (aref face-normals f)))
                   (gl:normal (p:x n) (p:y n) (p:z n)))
                 (dolist (pref (aref faces f))
                   (let ((p (aref points pref)))
                     (gl:color r g b)
                     (gl:vertex (p:x p) (p:y p) (p:z p))))
                 (gl:end)))))))

#+krma
(defmethod 3d-draw-filled-polygons-aux (points faces face-normals point-normals point-colors
					&optional (scene (application-scene *app*)))
  (declare (ignore face-normals)) ;; face normal is computed by fragment shader (flat shading)
  (let* ((col (krma:canonicalize-color (shading-color (application-drawing-settings *app*))))
	 (smooth-shading? (scene-do-smooth-shading? scene)))
    (cond ((and (> (length point-colors) 0) smooth-shading?)
	   (dotimes (f (length faces))
	     (let ((vertices ()))
	       (dolist (pref (aref faces f))
		 (let ((p (aref points pref))
		       (n (aref point-normals pref))
		       (c (aref point-colors pref)))
		   (push (p:x p) vertices)
		   (push (p:y p) vertices)
		   (push (p:z p) vertices)
		   (push (p:x n) vertices)
		   (push (p:y n) vertices)
		   (push (p:z n) vertices)
		   (push c vertices)))
	       (krma:scene-draw-multicolor-3d-convex-polygon-diffuse scene (nreverse vertices)))))
	  ((length point-colors)
	   (dotimes (f (length faces))
	     (let ((vertices ())) 
	       (dolist (pref (aref faces f))
		 (let ((p (aref points pref))
		       (c (aref point-colors pref)))
		   (push (p:x p) vertices)
		   (push (p:y p) vertices)
		   (push (p:z p) vertices)
		   (push c vertices)))
	       (krma:scene-draw-multicolor-3d-convex-polygon-flat scene (nreverse vertices)))))
	  (smooth-shading?
	   (dotimes (f (length faces))
	     (let ((vertices ()))
	       (dolist (pref (aref faces f))
		 (let ((p (aref points pref))
		       (n (aref point-normals pref)))
		   (push (p:x p) vertices)
		   (push (p:y p) vertices)
		   (push (p:z p) vertices)
		   (push (p:x n) vertices)
		   (push (p:y n) vertices)
		   (push (p:z n) vertices)))
	       (krma:scene-draw-filled-3d-convex-polygon-diffuse scene col (nreverse vertices)))))
	  (t 
	   (dotimes (f (length faces))
	     (let ((vertices ()))
	       (dolist (pref (aref faces f))
		 (let ((p (aref points pref)))
		   (push (p:x p) vertices)
		   (push (p:y p) vertices)
		   (push (p:z p) vertices)))
	       (krma:scene-draw-filled-3d-convex-polygon-flat scene col (nreverse vertices))))))))
	 

#-krma
(defmethod 3d-draw-filled-polygons-aux-SAV (points faces face-normals point-normals point-colors)
  (let ((col (shading-color (application-drawing-settings *app*)))
	(do-smooth-shading? (scene-do-smooth-shading? (application-scene *app*))))
    (gl-set-material col)
    (gl:color-material :front-and-back :diffuse)
    (with-gl-enable :color-material
      (dotimes (f (length faces))
        (gl:begin :polygon)
        (when (not do-smooth-shading?)
          (let ((n (aref face-normals f)))
            (gl:normal (p:x n) (p:y n) (p:z n))))
        (dolist (pref (aref faces f))
          (if (> (length point-colors) 0)
              (let ((c (aref point-colors pref)))
                (gl:color (c-red c) (c-green c) (c-blue c)))
              (gl:color (c-red col) (c-green col) (c-blue col))) ;inefficient...
          (when do-smooth-shading?
            (let ((n (aref point-normals pref)))
              (gl:normal (p:x n) (p:y n) (p:z n))))
          (let ((p (aref points pref)))
            (gl:vertex (p:x p) (p:y p) (p:z p))))
        (gl:end)))))
#-krma
(defun 3d-draw-highlighted-polygons (points faces face-normals point-normals faces-highlighted)
  (let ((scene (application-scene *app*)))
    (if (scene-do-lighting? scene)
	(gl:enable :lighting)
	(gl:disable :lighting))
    (if (scene-do-smooth-shading? scene)
	(gl:shade-model :smooth)
	(gl:shade-model :flat))
    (gl:polygon-mode :front-and-back :fill)
    (with-gl-enable :rescale-normal
      (3d-draw-highlighted-polygons-aux points faces face-normals point-normals faces-highlighted))))

#-krma
(defmethod 3d-draw-highlighted-polygons-aux (points faces face-normals point-normals faces-highlighted
					     &optional (scene (application-scene *app*)))
  (let ((sel-col (sel-color (application-drawing-settings *app*)))
	(do-smooth-shading? (scene-do-smooth-shading? scene)))
    (gl:color-material :front-and-back :diffuse)
    (with-gl-enable :color-material
      (dotimes (f (length faces))
        (when (aref faces-highlighted f)
          (gl:begin :polygon)
          (when (not do-smooth-shading?)
            (let ((n (aref face-normals f)))
              (gl:normal (p:x n) (p:y n) (p:z n))))
          (dolist (pref (aref faces f))
            (gl:color (c-red sel-col) (c-green sel-col) (c-blue sel-col))
            (when do-smooth-shading?
              (let ((n (aref point-normals pref)))
                (gl:normal (p:x n) (p:y n) (p:z n))))
            (let ((p (aref points pref)))
              (gl:vertex (p:x p) (p:y p) (p:z p))))
          (gl:end))))))

#-krma
(defun 3d-draw-wireframe-polygons (points faces &key (closed? t))
  (gl:polygon-mode :front-and-back :line)
  (with-gl-disable :lighting
    (gl-set-fg-color)
    (gl:line-width (secondary-line-thickness (scene (application-scene *app*))))
    (dotimes (f (length faces))
      (if closed?
          (gl:begin :polygon)
          (gl:begin :line-strip))
      (dolist (pref (aref faces f))
        (let ((p (aref points pref)))
          (gl:vertex (p:x p) (p:y p) (p:z p))))
      (gl:end))))

;;; 2d display =================================================================
#-krma
(defun 2d-setup-projection (w h)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:ortho 0.0 w h 0.0 -1.0 1.0) ; y=0 at top
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  (gl:disable :depth-test)
  (gl:disable :cull-face)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:enable :blend)
  )

#+krma
(defun 2d-setup-projection (w h)
  #-darwin
  (krma:mortho-vulkan 0.0 w h 0.0 -1.0 1.0)
  #+darwin
  (krma:mortho-metal 0.0 w h 0.0 -1.0 1.0))


