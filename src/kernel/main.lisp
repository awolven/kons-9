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

(defmethod initialize-instance :after ((instance kons-9) &rest initargs)
  (declare (ignore initargs))
  (setq *scene-view* (make-instance 'scene-view :scene (krma:application-scene instance)))
  (set-lines-thin)
  (setq glfw:*window* (vk:h (vk:main-window instance)))
  (setf (application-window-size instance) (glfw:get-window-size))
  (update-gl-3d-viewport)
  (update-window-title glfw:*window*)
  (update-status-bar (status-bar *scene-view*)
		     :view-width (first (application-window-size instance))
		     :view-height (second (application-window-size instance)))
  (destructuring-bind (width height) (glfw:get-framebuffer-size (vk:h (krma::main-window instance)))
    (setf (krma::main-window-width instance) width
	  (krma::main-window-height instance) height))
  (glfw:set-key-callback 'key-callback)
  (glfw:set-char-callback 'char-callback)
  (glfw:set-mouse-button-callback 'mouse-callback)
  (glfw:set-cursor-position-callback 'cursor-position-callback)
  (glfw:set-window-position-callback 'window-position-callback)
  (glfw:set-window-size-callback 'window-size-callback)
  (setf (monitor-scale (application-drawing-settings instance))
	(first (glfw:get-monitor-content-scale (glfw:get-primary-monitor))))
  (unless (probe-file (asdf/system:system-relative-pathname
		       :krma "submodules/krma-fonts/rm16cache.json"))
    (sdf-bmfont:create-bmfont
     (asdf/system:system-relative-pathname
      :krma "submodules/krma-fonts/Roboto_Mono/static/RobotoMono-Medium.ttf")
     (asdf/system:system-relative-pathname :krma "submodules/krma-fonts/rm16cache.json")
     :size (floor 24 (monitor-scale (application-drawing-settings instance)))
     :mode :msdf+a :type :json :spread 8))
  (values))

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

(defmethod krma::main ((app kons-9::kons-9))
  (let* ((device (vk:default-logical-device app))
	 (main-window (vk:main-window app))
	 (index (vk:queue-family-index (vk:render-surface main-window)))
	 (queue (vk:find-queue device index))
	 (command-pool (vk:find-command-pool device index))
	 (command-buffer (elt (vk:command-buffers command-pool) 0))
	 (descriptor-pool (vk:default-descriptor-pool app))
	 (sampler (vk:create-sampler device :allocator (vk:allocator app)))
	 (texture-dsl (vk:create-descriptor-set-layout
		       device
		       :bindings (list (make-instance 'vk:descriptor-set-layout-binding
						      :type %vk:VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
						      :count 1
						      :flags %vk:VK_SHADER_STAGE_FRAGMENT_BIT
						      :samplers (list sampler))))))
    
    (setf krma::*sampler* sampler)
    
    (vk:device-wait-idle device)
    
    (vk:reset-command-pool device command-pool)

    ;; one time commands here.
    
    (uiop/filesystem:with-current-directory
	((asdf/system:system-relative-pathname :krma "submodules/krma-fonts/"))
      (setf (krma::application-default-font *app*)
	    (krma::vulkan-make-font
	     device queue sampler texture-dsl descriptor-pool command-buffer
	     :cache-file "rm16cache.json")))

    (let* ((bpp 4)
	   (bitmap (make-array bpp :element-type '(unsigned-byte 8) :initial-element #xff)))
      (setq krma:*white-texture*
	    (krma::make-vulkan-texture device queue sampler texture-dsl descriptor-pool command-buffer bpp bitmap 1 1)))
    
    (let ((image-index)
          (work-queue)
          (current-frame-cons (krma::current-frame-cons app))
          (current-draw-data-cons (krma::current-draw-data-cons app)))
      
      (with-slots (exit?) app
	
        (loop until (glfw:window-should-close-p)
	      with frames = 0
	      with fps
	      with time = (/ (get-internal-real-time) internal-time-units-per-second)
	      with base-time = 0
              with last-time = time
	      do
		 (glfw:poll-events)
		 
              #-windows
	       (when (< (- last-time time) #.(/ 120.0f0))
		 ;; let's not heat up the computer if we're in VK_PRESENT_MODE_IMMEDIATE or something
		 ;; nanosleep must be calling sleep on Windows!
		 (sb-unix:nanosleep 0 2000000))

	       (setq last-time time)
	       
	       (when (vk:recreate-swapchain? main-window)
		 (destructuring-bind (width height) (glfw:get-framebuffer-size (vk:h main-window))
		   (vk:recreate-swapchain main-window (vk:swapchain main-window) width height)
		   (setf (krma::main-window-width app) width
			 (krma::main-window-height app) height)
		   (setf (vk::recreate-swapchain? main-window) nil)))

	       (let* ((swapchain (vk:swapchain main-window))
		      (frame-count (vk:number-of-images swapchain))
		      (current-frame (car current-frame-cons))
		      (current-draw-data (car current-draw-data-cons))
		      (scene (application-scene app))
		      (frame-resource (elt (vk:frame-resources swapchain) current-frame))
		      (command-buffer (vk:frame-command-buffer frame-resource))
		      (rm-draw-data (aref (krma:rm-draw-data scene) current-draw-data)))

		 (krma::erase-immediate-mode-draw-data app)
		 (setq work-queue (krma::draw-data-work-queue rm-draw-data))

		 (krma::maybe-defer-debug (app)
		   (loop with work = nil
			 while (setq work (sb-concurrency:dequeue work-queue))
			 do (funcall work)))

		 (krma::call-immediate-mode-work-functions app)

		 (setq image-index
		       (vk:frame-begin swapchain (vk:render-pass swapchain)
				    current-frame (kons-9::c! 1 1 1 1)
				    command-pool))

		 (krma::maybe-defer-debug (app)
		   (3d-setup-projection)
		   (destructuring-bind (w h) (application-window-size app)
		     (2d-setup-projection w h)
		     (progn
		       (draw-scene-view *scene-view*)
		       (update-status-bar-for-scene)
		       (incf frames)
		       (setq time (/ (get-internal-real-time) internal-time-units-per-second))
		       (when (> (- time base-time) 1)
			 (setq fps (float (/ frames (- time base-time))))
				     (setq base-time time)
				     (setq frames 0))
		       (krma:draw-text (format nil "fps: ~4,0f" fps) (- w 100) (- h 25) :color #x000000ff))))

		 ;; render here.
		 (krma:render-scene scene app command-buffer rm-draw-data (krma:im-draw-data scene))

		 (vk:frame-end swapchain queue current-frame)

		 (vk:frame-present swapchain queue current-frame image-index main-window)

		 ;; this needs to be the only thread that modifies current-frame
		 (sb-ext:atomic-update (car current-frame-cons)
				       #'(lambda (cf) (mod (1+ cf) frame-count)))
		 (sb-ext:atomic-update (car current-draw-data-cons)
				       #'(lambda (cdd) (mod (1+ cdd) 2)))))

	(vk:device-wait-idle (vk::default-logical-device app))
	(vk:destroy-image krma::*white-texture*)
	(vk:destroy-image (krma::font-atlas (krma::application-default-font app)))
	(%vk:vkDestroySampler (vk:h (vk::default-logical-device app))
			      (vk:h krma::*sampler*)
			      (vk:h (vk::allocator app)))
	(%vk::vkdestroydescriptorsetlayout (vk:h (vk::default-logical-device app))
					   (vk:h texture-dsl)
					   (vk:h (vk:allocator app)))
	(vk:shutdown-application app)
	(setq vk::*app* nil)
	(sb-ext:gc :full t)))))

(defun run-1 ()
  (krma:main (make-instance 'kons-9 :width 960 :height 540)))

;;; execute code on main thread -- necessary for interacting with UI elements
(defun run ()
  (trivial-main-thread:call-in-main-thread
   (lambda ()
     #+darwin(sb-int:set-floating-point-modes :traps nil)
     (run-1)
     #+NIL
     (kons-9::show-window kons-9::*scene*))))
