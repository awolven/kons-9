(in-package #:kons-9)

(defclass kons-9 (#+krma krma:krma-application-mixin)
  ((drawing-settings :initform (make-instance 'drawing-settings) :reader application-drawing-settings)
   (window-size :initform '(960 540) :accessor application-window-size)
   (current-mouse-pos-x :initform 0 :accessor current-mouse-pos-x)
   (current-mouse-pos-y :initform 0 :accessor current-mouse-pos-y)
   (current-mouse-modifier :initform nil :accessor current-mouse-modifier)
   (ui-popup-menu-width :initform 200 :accessor ui-popup-menu-width)
   (ui-button-item-width :initform 200 :accessor ui-button-item-width)
   (ui-button-item-height :initform 25 :accessor ui-button-item-height)
   (ui-default-padding :initform 20 :accessor ui-default-padding)
   (ui-default-spacing :initform 5  :accessor ui-default-spacing)
   (ui-border-width :initform 1 :accessor ui-border-width)
   (ui-border-color :initform (c! 0 0 0) :accessor ui-border-color)
   (ui-highlight-border-width :initform 4 :accessor ui-highlight-border-width)
   (ui-highlight-border-color :initform (c! 0 0 1) :accessor ui-highlight-border-color)
   (ui-keyboard-focus :initform nil :accessor ui-keyboard-focus)
   (ui-font-width :initform 7.3 :accessor ui-font-width)
   (transaction-log :initform nil :accessor application-transaction-log)))
   

(defclass scene (#+krma krma:krma-essential-scene-mixin)
  ((current-highlighted-ui-item :initform nil :accessor current-highlighted-ui-item)
   (viewport-aspect-ratio :initform #.(/ 16.0 9.0) :accessor viewport-aspect-ratio)
   (cam-x-rot :initform 0.0 :accessor cam-x-rot)
   (cam-y-rot :initform 0.0 :accessor cam-y-rot)
   (cam-fwd-dist :initform 0.0 :accessor cam-fwd-dist)
   (cam-side-dist :initform 0.0 :accessor cam-side-dist)
   (do-lighting? :initform t :accessor scene-do-lighting?)
   (display-filled? :initform t :accessor scene-display-filled?)
   (display-wireframe? :initform t :accessor scene-display-wireframe?)
   (display-points? :initform t :accessor scene-display-points?)
   (do-backface-cull? :initform t :accessor scene-do-backface-cull?)
   (do-smooth-shading? :initform t :accessor scene-do-smooth-shading?)
   (display-ground-plane? :initform t :accessor scene-display-ground-plane?)
   (display-axes? :initform t :accessor scene-display-axes?)
   (item-name-table :initform (make-hash-table :test #'eq) :accessor scene-item-name-table)))
   
   

   
   
