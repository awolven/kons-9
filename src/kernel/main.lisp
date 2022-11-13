(in-package #:kons-9)

;;;; run graphics =======================================================

;;(defparameter *scene* (make-instance (krma:scene-class *app*)))

(define-symbol-macro *scene*
    (application-scene *app*))

(defclass kons-9 (#+krma krma:krma-application-mixin)
  ((vk::application-name :initform "kons-9")
   (*drawing-settings* :initform (make-instance 'drawing-settings) :reader application-drawing-settings)
   (*window-size* :initform '(960 540) :accessor application-window-size)
   (*current-mouse-pos-x* :initform 0 :accessor current-mouse-pos-x)
   (*current-mouse-pos-y* :initform 0 :accessor current-mouse-pos-y)
   (*current-mouse-modifier* :initform nil :accessor current-mouse-modifier)
   (*current-highlighted-ui-item* :initform nil :accessor current-highlighted-ui-item)
   (*ui-popup-menu-width* :initform 200 :accessor ui-popup-menu-width)
   (*ui-button-item-width* :initform 200 :accessor ui-button-item-width)
   (*ui-button-item-height* :initform 25 :accessor ui-button-item-height)
   (*ui-default-padding* :initform 20 :accessor ui-default-padding)
   (*ui-default-spacing* :initform 5  :accessor ui-default-spacing)
   (*ui-border-width* :initform 1 :accessor ui-border-width)
   (*ui-border-color* :initform (c! 0 0 0) :accessor ui-border-color)
   (*ui-highlight-border-width* :initform 4 :accessor ui-highlight-border-width)
   (*ui-highlight-border-color* :initform (c! 0 0 1) :accessor ui-highlight-border-color)
   (*ui-keyboard-focus* :initform nil :accessor ui-keyboard-focus)
   (*ui-font-width* :initform 7.3 :accessor ui-font-width)
   (*transaction-log* :initform nil :accessor application-transaction-log)))

(defmacro with-app-globals ((app) &body body)
  `(with-slots (*drawing-settings*
                *window-size*
                *current-mouse-pos-x*
                *current-mouse-pos-y*
                *current-mouse-modifier*
                *current-highlighted-ui-item*
                *ui-popup-menu-width*
                *ui-button-item-width*
                *ui-button-item-height*
                *ui-default-padding*
                *ui-default-spacing*
                *ui-border-width*
                *ui-border-color*
                *ui-highlight-border-width*
                *ui-highlight-border-color*
                *ui-keyboard-focus*
                *ui-font-width*)
       (cl:the kons-9 ,app)
     ,@body))

(defclass krma-enabled-scene (scene #+krma krma:krma-essential-scene-mixin)
  ((*viewport-aspect-ratio* :initform #.(/ 16.0 9.0) :accessor viewport-aspect-ratio)
   (*cam-x-rot* :initform 0.0 :accessor cam-x-rot)
   (*cam-y-rot* :initform 0.0 :accessor cam-y-rot)
   (*cam-fwd-dist* :initform 0.0 :accessor cam-fwd-dist)
   (*cam-side-dist* :initform 0.0 :accessor cam-side-dist)
   (*cam-up-dist* :initform 0.0 :accessor cam-up-dist)
   (*do-lighting?* :initform t :accessor scene-do-lighting?)
   (*display-filled?* :initform t :accessor scene-display-filled?)
   (*display-wireframe?* :initform t :accessor scene-display-wireframe?)
   (*display-points?* :initform t :accessor scene-display-points?)
   (*do-backface-cull?* :initform t :accessor scene-do-backface-cull?)
   (*do-smooth-shading?* :initform t :accessor scene-do-smooth-shading?)
   (*display-ground-plane?* :initform t :accessor scene-display-ground-plane?)
   (*display-axes?* :initform t :accessor scene-display-axes?)
   (*item-name-table* :initform (make-hash-table :test #'eq) :accessor scene-item-name-table)))

(defmacro with-scene-globals ((scene) &body body)
  `(with-slots (*viewport-aspect-ratio*
                *cam-x-rot*
                *cam-y-rot*
                *cam-fwd-dist*
                *cam-side-dist*
                *cam-up-dist*
                *do-lighting?*
                *display-filled?*
                *display-wireframe?*
                *display-points?*
                *do-backface-cull?*
                *do-smooth-shading?*
                *display-ground-plane?*
                *display-axes?*
                *item-name-table*)
       (cl:the krma-enabled-scene ,scene)
     ,@body))

(defmethod krma:scene-class ((app kons-9))
  'krma-enabled-scene)

krma::(defmethod main ((app kons-9::kons-9))
        (let* ((device (default-logical-device app))
               (main-window (main-window app))
               (index (queue-family-index (render-surface main-window)))
               (queue (find-queue device index))
               (command-pool (find-command-pool device index))
               (command-buffer (elt (command-buffers command-pool) 0))
               (descriptor-pool (default-descriptor-pool app))
               (sampler (create-sampler device :allocator (allocator app)))
               (texture-dsl (create-descriptor-set-layout
                             device
                             :bindings (list (make-instance 'descriptor-set-layout-binding
                                                            :type VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
                                                            :count 1
                                                            :flags VK_SHADER_STAGE_FRAGMENT_BIT
                                                            :samplers (list sampler)))))
               (kons-9::scene-view (make-instance 'kons-9::scene-view :scene (application-scene app)))
               (kons-9::drawing-settings (kons-9::application-drawing-settings app)))

          (setq glfw:*window* (h (main-window app)))

          ;; these should be put in initialize-instance of krma-test-application
          (multiple-value-bind (width height) (get-framebuffer-size main-window)
            (setf (main-window-width app) width
	              (main-window-height app) height))

          kons-9::
          (progn
            (setf *scene-view* scene-view)

            (setf (monitor-scale drawing-settings)
                  (floor (first (glfw:get-monitor-content-scale (glfw:get-primary-monitor)))))

            (set-lines-thin)

            (setf %gl:*gl-get-proc-address* #'glfw:get-proc-address)
            (glfw:set-key-callback 'key-callback)
            (glfw:set-char-callback 'char-callback)
            (glfw:set-mouse-button-callback 'mouse-callback)
            (glfw:set-cursor-position-callback 'cursor-position-callback)
            (glfw:set-window-position-callback 'window-position-callback)
            (glfw:set-window-size-callback 'window-size-callback)
            (setf (application-window-size krma::app) (glfw:get-window-size))
            (update-gl-3d-viewport)
            (update-window-title glfw:*window*)
            (update-status-bar (status-bar *scene-view*)
                               :view-width (first (application-window-size krma::app))
                               :view-height (second (application-window-size krma::app)))
            )

          (setf *sampler* sampler)

          (device-wait-idle device)

          (reset-command-pool device command-pool)

          ;; one time commands here.
          (unless (probe-file (asdf/system:system-relative-pathname :krma "acache.json"))
            (sdf-bmfont:create-bmfont #+linux "/usr/share/fonts/liberation-mono/LiberationMono-Regular.ttf"
				                      #+darwin "/System/Library/Fonts/Monaco.ttf"
                                      #+windows "C:/Windows/Fonts/Arial.ttf" "acache.json"
                                      :size 16 :mode :msdf+a :type :json :spread 8))
          #-(or linux darwin)
          (unless (probe-file (asdf/system:system-relative-pathname :krma "tcache.json"))
            (sdf-bmfont:create-bmfont "C:/Windows/Fonts/Times.ttf" "tcache.json"
                                      :size 16 :mode :msdf+a :type :json :spread 8))


          (setq *font*
                (vulkan-make-font device queue sampler texture-dsl descriptor-pool command-buffer
                                  :cache-file "acache.json"))
          #-(or linux darwin)
          (setq *font2*
                (vulkan-make-font device queue sampler texture-dsl descriptor-pool command-buffer
                                  :cache-file "tcache.json"))

          (let* ((bpp 4)
                 (bitmap (make-array bpp :element-type '(unsigned-byte 8) :initial-element #xff)))
            (setq *white-texture*
                  (make-vulkan-texture device queue sampler texture-dsl descriptor-pool command-buffer bpp bitmap 1 1)))

          ;;(test)

          (let ((image-index)
                (work-queue)
                (current-frame-cons (current-frame-cons app))
                (current-draw-data-cons (current-draw-data-cons app)))

            (with-slots (exit?) app

              (loop until (glfw:window-should-close-p)
		            with frames = 0
		            with fps
		            with time = (/ (get-internal-real-time) internal-time-units-per-second)
		            with base-time = 0
                    with last-time = time
		            do
		               (glfw:poll-events)

                       (when (< (- last-time time) #.(/ 120.0f0))
                         ;; let's not heat up the computer if we're in VK_PRESENT_MODE_IMMEDIATE or something
                         ;; can't seem to get a resolution lower than 1/60 s or so
                         ;; oh well, works ok on my PC ~60Hz
                         (sb-unix:nanosleep 0 2000000))

                       (setq last-time time)

		               (when (recreate-swapchain? main-window)
		                 (multiple-value-bind (width height) (get-framebuffer-size main-window)
		                   (recreate-swapchain main-window (swapchain main-window) width height)
		                   (setf (main-window-width app) width
			                     (main-window-height app) height)
		                   (setf (recreate-swapchain? main-window) nil)))

		               (let* ((swapchain (swapchain main-window))
			                  (frame-count (number-of-images swapchain))
			                  (current-frame (car current-frame-cons))
			                  (current-draw-data (car current-draw-data-cons))
			                  (scene (application-scene app))
			                  (frame-resource0 (elt (frame-resources swapchain) current-frame))
			                  (command-buffer (frame-command-buffer frame-resource0))
			                  (rm-draw-data (aref (rm-draw-data scene) current-draw-data)))

		                 (erase-immediate-mode-draw-data app)
		                 (setq work-queue (draw-data-work-queue rm-draw-data))

		                 (maybe-defer-debug (app)
		                   (loop with work = nil
			                     while (setq work (sb-concurrency:dequeue work-queue))
			                     do (funcall work)))

		                 (call-immediate-mode-work-functions app)

		                 (setq image-index
			                   (frame-begin swapchain (render-pass swapchain)
					                        current-frame (kons-9::c! 1 1 1 1)
					                        command-pool))

                         ;;(update-2d-camera scene)
                         ;;(update-3d-camera scene)

		                 (maybe-defer-debug (app)
		                   kons-9::(3d-setup-projection)
		                   kons-9::(destructuring-bind (w h) (application-window-size *app*)
				                     (2d-setup-projection w h))
		                   kons-9::(progn
				                     (draw-scene-view *scene-view*)
				                     (update-status-bar-for-scene)
				                     krma::(incf frames)
				                     krma::(setq time (/ (get-internal-real-time) internal-time-units-per-second))
				                     krma::(when (> (- time base-time) 1)
					                         (setq fps (float (/ frames (- time base-time))))
					                         (setq base-time time)
					                         (setq frames 0))
				   
				     
				                     (krma::draw-text (format nil "fps: ~4,0f" krma::fps) (- (car (application-window-size krma::app)) 100) 10 :color #x000000ff)
				                     ))

                         ;; render here.
		                 (render-scene scene app command-buffer rm-draw-data (im-draw-data scene))



		                 (frame-end swapchain queue current-frame)

		                 (frame-present swapchain queue current-frame image-index main-window)

                         ;; this needs to be the only thread that modifies current-frame
		                 (sb-ext:atomic-update (car current-frame-cons)
					                           #'(lambda (cf) (mod (1+ cf) frame-count)))
		                 (sb-ext:atomic-update (car current-draw-data-cons)
					                           #'(lambda (cdd) (mod (1+ cdd) 2)))))

              (shutdown-application app)))))

;;; execute code on main thread -- necessary for interacting with UI elements
(defun run ()
  ;;(trivial-main-thread:call-in-main-thread
  (funcall
   (lambda ()
     #+darwin(sb-int:set-floating-point-modes :traps nil)
     (krma:main (make-instance 'kons-9))
     #+NIL
     (kons-9::show-window kons-9::*scene*))))
