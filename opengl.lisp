
;;;; C interface ========================================================

(defmacro with-c-array-1 (vec &body forms)
  `(multiple-value-bind (v vp)
       (make-heap-ivector 1 'single-float)
       (setf (aref v 0) (aref ,vec 0))
     (unwind-protect
          (progn ,@forms)
       (dispose-heap-ivector v)
       (dispose-heap-ivector vp))))

(defmacro with-c-array-3 (vec &body forms)
  `(multiple-value-bind (v vp)
       (make-heap-ivector 3 'single-float)
       (setf (aref v 0) (aref ,vec 0))
       (setf (aref v 1) (aref ,vec 1))
       (setf (aref v 2) (aref ,vec 2))
     (unwind-protect
          (progn ,@forms)
       (dispose-heap-ivector v)
       (dispose-heap-ivector vp))))

(defmacro with-c-array-4 (vec &body forms)
  `(multiple-value-bind (v vp)
       (make-heap-ivector 4 'single-float)
       (setf (aref v 0) (aref ,vec 0))
       (setf (aref v 1) (aref ,vec 1))
       (setf (aref v 2) (aref ,vec 2))
       (setf (aref v 3) (aref ,vec 3))
     (unwind-protect
          (progn ,@forms)
       (dispose-heap-ivector v)
       (dispose-heap-ivector vp))))

;;;; graphics ===========================================================

(defclass scene-view (ns:ns-opengl-view)
  ((scene :accessor scene :initarg :scene :initform nil))
  (:metaclass ns:+ns-object))

;;; draw a square outline in OpenGL
(defun draw-square ()
  (#_glColor3f 1.0 1.0 1.0)
  (#_glLineWidth 3.0)
  (#_glBegin #$GL_LINE_LOOP)
  (#_glVertex3f  0.5  0.5 -0.0)
  (#_glVertex3f  0.5 -0.5 -0.0)
  (#_glVertex3f -0.5 -0.5 -0.0)
  (#_glVertex3f -0.5  0.5 -0.0)
  (#_glEnd))

(defun draw-axes (size)
  (#_glLineWidth 3.0)
  (#_glBegin #$GL_LINES)
  (#_glColor3f 1.0 0.0 0.0)
  (#_glVertex3f 0.0  0.0  0.0)
  (#_glVertex3f size 0.0  0.0)
  (#_glColor3f 0.0 1.0 0.0)
  (#_glVertex3f 0.0 0.0  0.0 )
  (#_glVertex3f 0.0 size 0.0 )
  (#_glColor3f 0.0 0.0 1.0)
  (#_glVertex3f 0.0  0.0  0.0)
  (#_glVertex3f 0.0  0.0 size)
  (#_glEnd))

(defun draw-ground-plane (size segments)
  (#_glColor3f 0.8 0.8 0.8)
  (#_glLineWidth 1.0)
  (#_glBegin #$GL_LINES)
  (dotimes (i (+ segments 1))
    (let* ((f (/ i segments))
           (coord (lerp f (- size) size)))
      (#_glVertex3f coord 0.0 (- size))
      (#_glVertex3f coord 0.0    size)
      (#_glVertex3f (- size) 0.0 coord)
      (#_glVertex3f    size  0.0 coord)))
  (#_glEnd))

(defparameter *cam-x-rot* 0.0)
(defparameter *cam-y-rot* 0.0)
(defparameter *cam-fwd-dist* 0.0)
(defparameter *cam-side-dist* 0.0)
(defparameter *cam-up-dist* 0.0)

(defparameter *do-lighting?* t)
(defparameter *display-filled?* t)
(defparameter *display-wireframe?* t)
(defparameter *display-points?* t)
(defparameter *do-backface-cull?* t)
(defparameter *do-smooth-shading?* nil)
(defparameter *display-ground-plane?* t)
(defparameter *display-axes?* t)
;; (defparameter *light-0-on?* t)
;; (defparameter *light-1-on?* t)

;(defparameter *shading-color* (c! 0.8 0.8 0.8))
(defparameter *shading-color* (c! 1 1 1))
(defparameter *light-color* (c! 2 2 2))
;; (defparameter *fg-color* (c! 1 1 1))
;; (defparameter *bg-color* (c! 0 0 0))
(defparameter *fg-color* (c! 0 0 0))
(defparameter *bg-color* (c! 1 1 1))
(defparameter *sel-color* (c! 1 0 0))

(defun set-theme-bright ()
  (setf *fg-color* (c! 0 0 0))
  (setf *bg-color* (c! 1 1 1)))

(defun set-theme-dark ()
  (setf *fg-color* (c! 1 1 1))
  (setf *bg-color* (c! 0 0 0)))

(defun init-view-camera ()
  (setf *cam-x-rot* 15.0)
  (setf *cam-y-rot* -35.0)
  (setf *cam-fwd-dist* -10.0)
  (setf *cam-side-dist* 0.0)
  (setf *cam-up-dist* -2.0))

(defun gl-set-fg-color ()
  (#_glColor3f (c-red *fg-color*) (c-green *fg-color*) (c-blue *fg-color*)))
  
(defun gl-set-sel-color ()
  (#_glColor3f (c-red *sel-color*) (c-green *sel-color*) (c-blue *sel-color*)))

(defun gl-enable-light (light-id dir &optional (color *light-color*))
  (#_glEnable light-id)

  (with-c-array-4 (vector (x dir) (y dir) (z dir) 0.0)
    (#_glLightfv light-id #$GL_POSITION vp))

  (with-c-array-4 (vector 0.25 0.25 0.25 1.0)
    (#_glLightfv light-id #$GL_AMBIENT vp))
  (with-c-array-4 (vector (c-red color) (c-green color) (c-blue color) 1.0)
    (#_glLightfv light-id #$GL_DIFFUSE vp))
  (with-c-array-4 (vector (c-red color) (c-green color) (c-blue color) 1.0)
    (#_glLightfv light-id #$GL_SPECULAR vp))
  )

(defun gl-disable-light (light-id)
  (#_glDisable light-id))

(defun gl-set-material (&optional (diff *shading-color*) (spec (c! 0 0 0)) (shine 0.0))
  (with-c-array-4 (vector (c-red diff) (c-green diff) (c-blue diff) 1.0)
    (#_glMaterialfv #$GL_FRONT_AND_BACK #$GL_DIFFUSE vp))
  (with-c-array-4 (vector (c-red spec) (c-green spec) (c-blue spec) 1.0)
    (#_glMaterialfv #$GL_FRONT_AND_BACK #$GL_SPECULAR vp))
  (with-c-array-1 (vector shine)
    (#_glMaterialfv #$GL_FRONT_AND_BACK #$GL_SHININESS vp)))
  
(defun new-pixel-format (&rest attributes)
  ;; take a list of opengl pixel format attributes (enums and other
  ;; small ints), make an array (character array?), and create and
  ;; return an NSOpenGLPixelFormat
  (let* ((attribute-size (ccl::foreign-size #>NSOpenGLPixelFormatAttribute :bytes))
         (nattributes (length attributes)))
    (ccl::%stack-block ((objc-attributes (* attribute-size (1+ nattributes))))
      (loop for i from 0 to nattributes
            for attribute in attributes do
            (setf (ccl:paref objc-attributes (:* #>NSOpenGLPixelFormatAttribute) i) 
                  attribute) ; <- autocoerced?
            finally (setf 
                     (ccl:paref objc-attributes 
                                (:* #>NSOpenGLPixelFormatAttribute) nattributes) 0))
      (make-instance ns:ns-opengl-pixel-format :with-attributes objc-attributes))))

(defmethod initialize-instance :after ((self scene-view) &rest initargs)
  (declare (ignore initargs))
  (#/setPixelFormat: self (new-pixel-format ;#$NSOpenGLPFAOpenGLProfile 
                                            ;#$NSOpenGLProfileVersion3_2Core
                                            ;#$NSOpenGLPFADoubleBuffer
                                            #$NSOpenGLPFAColorSize 32
                                            #$NSOpenGLPFADepthSize 24))

  (#/setWantsLayer: self t)
  (#/setBorderWidth: (#/layer self) 1)
;  (#/setCornerRadius: (#/layer self) 10)

  (init-view-camera))

(defun update-light-settings ()
  (if *do-backface-cull?*
      (progn
        (#_glEnable #$GL_CULL_FACE)
        (#_glLightModeli #$GL_LIGHT_MODEL_TWO_SIDE #$GL_FALSE))
      (progn
        (#_glDisable #$GL_CULL_FACE)
        (#_glLightModeli #$GL_LIGHT_MODEL_TWO_SIDE #$GL_TRUE)))

  (if *do-lighting?*
      (#_glEnable #$GL_LIGHTING)
      (#_glDisable #$GL_LIGHTING))
  (let ((mtx (matrix-multiply (make-x-rotation-matrix (- (radians *cam-x-rot*)))
                              (make-y-rotation-matrix (- (radians *cam-y-rot*))))))
    (gl-enable-light #$GL_LIGHT0 (transform-point (p! 0 0 1) mtx) (c! 0.7 0.7 0.7))))

;; (if *light-0-on?*
  ;;     (gl-enable-light #$GL_LIGHT0 (p! 0 1 0.8) (c! 0.7 0.7 0.7))
  ;;     (gl-disable-light #$GL_LIGHT0))
  ;; (if *light-1-on?*
  ;;     (gl-enable-light #$GL_LIGHT1 (p! 1 -0.5 0) (c! 0.3 0.3 0.3))
  ;;     (gl-disable-light #$GL_LIGHT1))
  ;; (when (and (not *light-0-on?*) (not *light-1-on?*)) ;use camera light
  ;;   (let ((mtx (matrix-multiply (make-x-rotation-matrix (- (radians *cam-x-rot*)))
  ;;                               (make-y-rotation-matrix (- (radians *cam-y-rot*))))))
  ;;     (gl-enable-light #$GL_LIGHT0 (transform-point (p! 0 0 1) mtx))))
;;  )

;;; display the view
(objc:defmethod (#/drawRect: :void) ((self scene-view) (rect :<NSR>ect))
  (#_glClearColor (c-red *bg-color*) (c-green *bg-color*) (c-blue *bg-color*) 0.0)

  (#_glClear (logior #$GL_COLOR_BUFFER_BIT #$GL_DEPTH_BUFFER_BIT))
  (#_glEnable #$GL_DEPTH_TEST)
  (#_glCullFace #$GL_BACK)

  (update-light-settings)
  
  (#_glMatrixMode #$GL_PROJECTION)
  (#_glLoadIdentity)
  (#_gluPerspective 45.0d0 (/ 16.0d0 9.0d0) 0.01d0 1000.0d0)
  (#_glMatrixMode #$GL_MODELVIEW)
  (#_glLoadIdentity)

;;  (#_gluLookAt 4.0d0 3.0d0 5.0d0 0.0d0 0.0d0 0.0d0 0.0d0 1.0d0 0.0d0)

  (#_glTranslatef *cam-side-dist* *cam-up-dist* *cam-fwd-dist*)
  (#_glRotatef *cam-x-rot* 1.0 0.0 0.0)
  (#_glRotatef *cam-y-rot* 0.0 1.0 0.0)

  (when (scene self)
    (draw (scene self)))

  (#_glDisable #$GL_LIGHTING)
  (when *display-ground-plane?*
    (draw-ground-plane 10.0 10))
  (when *display-axes?*
    (draw-axes 3.0))
  (gl-set-fg-color)
  (#_glFlush))

;;; respond to first click in window
(objc:defmethod (#/acceptsFirstMouse: :<BOOL>) ((self scene-view) event)
  (declare (ignore event))
  t)

;;; request a view refresh when mouse click
(objc:defmethod (#/mouseDown: :void) ((self scene-view) event)
  (declare (ignore event))
  (#/setNeedsDisplay: self t))

;;; accept key events
(objc:defmethod (#/acceptsFirstResponder :<BOOL>) ((self scene-view))
   t)

(defun print-viewport-help ()
  (format t "Mouse drag: orbit, [option] track left/right and up/down, [command] track in/out~%~
`: toggle lighting~%~
1: toggle filled display~%~
2: toggle wireframe display~%~
3: toggle point display~%~
4: toggle backface culling~%~
5: toggle smooth shading~%~
6: toggle ground plane display~%~
7: toggle axes display~%~
z: reset camera~%~
a: init scene~%~
space: update scene (hold down for animation) ~%~
h or ?: print this help message~%"))

;; x: call GENERATE-SCENE function ~%~
;; c: update scene for 100 frames ~%~
;; v: call GENERATE-SCENE and update scene for 100 frames ~%~

;;; update scene when key pressed
(objc:defmethod (#/keyDown: :void) ((self scene-view) event)
  (let* ((str (objc:lisp-string-from-nsstring (#/charactersIgnoringModifiers event)))
         (char (char str 0))
         (scene (scene self)))
    (case char
          ;;; #\tab #\^Y 
      (#\h (print-viewport-help))
      (#\? (print-viewport-help))
      (#\a (when scene (init-scene scene)))
      (#\` (setf *do-lighting?* (not *do-lighting?*)))
      ;; (#\1 (setf *light-0-on?* (not *light-0-on?*)))
      ;; (#\2 (setf *light-1-on?* (not *light-1-on?*)))
      (#\1 (setf *display-filled?* (not *display-filled?*)))
      (#\2 (setf *display-wireframe?* (not *display-wireframe?*)))
      (#\3 (setf *display-points?* (not *display-points?*)))
      (#\4 (setf *do-backface-cull?* (not *do-backface-cull?*)))
      (#\5 (setf *do-smooth-shading?* (not *do-smooth-shading?*)))
      (#\6 (setf *display-ground-plane?* (not *display-ground-plane?*)))
      (#\7 (setf *display-axes?* (not *display-axes?*)))
      (#\z (init-view-camera))
      ;; (#\x (when (scene self) (generate-scene (scene self))))
      ;; (#\c (when (scene self) (dotimes (i 100) (update-scene (scene self)))))
      ;; (#\v (when (scene self)
      ;;        (generate-scene (scene self))
      ;;        (dotimes (i 100) (update-scene (scene self)))))
;      (#\space (when (scene self) (update-scene (scene self))))))
      (#\space (dolist (v *scene-views*) (update-scene (scene v))))))
  (redraw))

(objc:defmethod (#/mouseDragged: :void) ((self scene-view) event)
  (let ((flags (#/modifierFlags event))
        (dx (coerce (#/deltaX event) 'single-float))
        (dy (coerce (#/deltaY event) 'single-float)))
    ;;    (format t "~a, ~a~%" flags #$NSAlternateKeyMask)
    (cond ((or (= flags 524320) (= flags 524352)) ; #$NSAlternateKeyMask -- this has been deprecated
           (if (>= (abs dx) (abs dy))
               (incf *cam-side-dist* (* 0.1 dx))
               (incf *cam-up-dist* (* -0.1 dy))))
          ((or (= flags 1048584) (= flags 1048592)) ; command
           (incf *cam-fwd-dist* (* 0.1 dx)))
          (t
           (incf *cam-x-rot* dy)
           (incf *cam-y-rot* dx))))
  (redraw))



(defclass app-window (ns:ns-window)
  ()
  (:metaclass ns:+ns-object))

;;; create and display a window containing an OpeGL view
(defun show-window (scene)
  (setf *scene-views* '())
  (ns:with-ns-rect ;(frame 0 0 640 360)
    (frame 0 0 960 540)
    (let* ((w (make-instance 'app-window
                             :title "foo"
                             :with-content-rect frame
                             :style-mask (logior #$NSTitledWindowMask
                                                 #$NSClosableWindowMask
                                                 #$NSMiniaturizableWindowMask)
                             :backing #$NSBackingStoreBuffered
                             :defer t))
           (v (make-instance 'scene-view :scene scene)))
      (#/setContentView: w v)

      (push v *scene-views*)

      (redraw)
      (#/release v)
      (#/center w)
      (#/orderFront: w nil)
      w)))

;;; create and display a window containing an OpeGL view
(defparameter *window-x-size* 960)
(defparameter *window-y-size* 540)
(defun show-grid-window (grid-size)
  (setf *scene-views* '())
  (ns:with-ns-rect
    (frame 0 0 *window-x-size* *window-y-size*)
    (let* ((w (make-instance 'ns:ns-window
                             :title "foo"
                             :with-content-rect frame
                             :style-mask (logior #$NSTitledWindowMask
                                                 #$NSClosableWindowMask
                                                 #$NSMiniaturizableWindowMask)
                             :backing #$NSBackingStoreBuffered
                             :defer t))
           (dx (floor (/ *window-x-size* grid-size)))
           (dy (floor (/ *window-y-size* grid-size))))

      (dotimes (y grid-size)
        (dotimes (x grid-size)
          (let ((v (make-instance 'scene-view :scene (make-instance 'scene))))
            (#/addSubview: (#/contentView w) v)
            (#/setFrameOrigin: v (ns:make-ns-point (* x dx) (* y dy)))
            (#/setFrameSize: v (ns:make-ns-point dx dy))
            (push v *scene-views*)
            (#/release v))))

      (setf *scene-views* (reverse *scene-views*))
      
      (redraw)
      (#/center w)
      (#/orderFront: w nil)
      w)))

(defmacro with-redraw (&body body)
  `(let ((result (progn ,@body)))
     (redraw)
     result))

;;; with-redraw macro: add variant that clears scene
(defmacro with-clear-and-redraw (&body body)
  `(progn
     (clear-scene *scene*)
     (setf (init-done? *scene*) nil)
     (setf (current-frame *scene*) 0)
     (let ((_result (progn ,@body)))
       (redraw)
       _result)))

(defmacro with-grid-clear-and-redraw (&body body)
  `(progn
     (dolist (v *scene-views*)
       (clear-scene (scene v)))
     (let ((_result (progn ,@body)))
       (redraw)
       _result)))

(defmacro with-gl-enable (flag &body body)
  `(progn
     (#_glEnable ,flag)
     (let ((result (progn ,@body)))
       (#_glDisable ,flag)
       result)))

(defmacro with-gl-disable (flag &body body)
  `(progn
     (#_glDisable ,flag)
     (let ((result (progn ,@body)))
       (#_glEnable ,flag)
       result)))


;(defun pick-in-window ()
;  (#_gluUnProject 