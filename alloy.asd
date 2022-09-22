#|
This file is a part of Alloy
(c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#


(pushnew (asdf/system:system-relative-pathname :kons-9 "alloy/windowing/") asdf:*central-registry* :test #'equalp)
(pushnew (asdf/system:system-relative-pathname :kons-9 "alloy/animation/") asdf:*central-registry* :test #'equalp)
(pushnew (asdf/system:system-relative-pathname :kons-9 "alloy/renderers/simple/presentations/") asdf:*central-registry* :test #'equalp)
(pushnew (asdf/system:system-relative-pathname :kons-9 "alloy/renderers/simple/") asdf:*central-registry* :test #'equalp)
(pushnew (asdf/system:system-relative-pathname :kons-9 "alloy/renderers/opengl/") asdf:*central-registry* :test #'equalp)
(pushnew (asdf/system:system-relative-pathname :kons-9 "alloy/renderers/glfw/") asdf:*central-registry* :test #'equalp)

(asdf:defsystem alloy
  :version "0.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description ""
  :homepage "https://github.com/Shirakumo/alloy"
  :serial T
  :components ((:file "experimental/alloy/package")
               (:file "experimental/alloy/conditions")
               (:file "experimental/alloy/units")
               (:file "experimental/alloy/geometry")
               (:file "experimental/alloy/events")
               (:file "experimental/alloy/observable")
               (:file "experimental/alloy/renderer")
               (:file "experimental/alloy/data")
               (:file "experimental/alloy/container")
               (:file "experimental/alloy/focus-tree")
               (:file "experimental/alloy/layout")
               (:file "experimental/alloy/component")
               (:file "experimental/alloy/structure")
               (:file "experimental/alloy/ui")
               (:module "experimental/alloy/layouts"
                :components ((:file "fixed")
                             (:file "linear")
                             (:file "grid")
                             (:file "border")
                             (:file "clip-view")
                             (:file "swap")
                             (:file "flow")
                             (:file "popup")
                             (:file "fullscreen")))
               (:module "experimental/alloy/components"
                :components ((:file "base")
                             (:file "button")
                             (:file "switch")
                             (:file "text-input")
                             (:file "slider")
                             (:file "radio")
                             (:file "combo")
                             (:file "scroll")
                             (:file "plot")
                             (:file "drag")
                             (:file "wheel")
                             (:file "symbol")
                             (:file "printable")
                             (:file "sequence")))
               (:module "experimental/alloy/structures"
                :components ((:file "query")
                             (:file "scroll-view")
                             (:file "tab-view")
                             (:file "window")
                             (:file "dialog")
                             (:file "sidebar")
                             (:file "inspector")
                             (:file "menu")))
               (:file "experimental/alloy/builder")
               (:file "experimental/alloy/widget")
               (:file "experimental/alloy/documentation"))
  :depends-on (:documentation-utils
               :array-utils
               :float-features
               :closer-mop)
  :in-order-to ((asdf:test-op (asdf:test-op :alloy-test))))

(asdf:defsystem alloy-constraint
  :version "0.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description ""
  :homepage "https://github.com/Shirakumo/alloy"
  :serial T
  :components ((:file "experimental/alloy/layouts/constraint/package")
               (:file "experimental/alloy/layouts/constraint/constraints")
               (:file "experimental/alloy/layouts/constraint/layout")
               (:file "experimental/alloy/layouts/constraint/documentation"))
  :depends-on (:documentation-utils
               :alloy
               :classowary)
  :in-order-to ((asdf:test-op (asdf:test-op :alloy-test))))

(asdf:defsystem alloy-windowing
  :version "0.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description ""
  :homepage "https://github.com/Shirakumo/alloy"
  :serial T
  :components ((:file "experimental/alloy/windowing/package")
               (:file "experimental/alloy/windowing/protocol"))
  :depends-on (:documentation-utils
               :alloy)
  :in-order-to ((asdf:test-op (asdf:test-op :alloy-test))))

(asdf:defsystem alloy-animation
  :version "0.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description ""
  :homepage "https://github.com/Shirakumo/alloy"
  :serial T
  :components ((:file "experimental/alloy/animation/package")
               (:file "experimental/alloy/animation/easing")
               (:file "experimental/alloy/animation/lerp")
               (:file "experimental/alloy/animation/animation")
               (:file "experimental/alloy/animation/change"))
  :depends-on (:documentation-utils
               :alloy
               :colored)
  :in-order-to ((asdf:test-op (asdf:test-op :alloy-test))))

(asdf:defsystem alloy-simple-presentations
  :version "0.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description ""
  :homepage "https://github.com/Shirakumo/alloy"
  :serial T
  :components ((:file "experimental/alloy/renderers/simple/presentations/package")
               (:file "experimental/alloy/renderers/simple/presentations/protocol")
               (:file "experimental/alloy/renderers/simple/presentations/default")
               (:file "experimental/alloy/renderers/simple/presentations/documentation"))
  :depends-on (:documentation-utils
               :stealth-mixin
               :colored
               :alloy-simple
               :alloy-animation)
  :in-order-to ((asdf:test-op (asdf:test-op :alloy-test))))

(asdf:defsystem alloy-simple
  :version "0.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description ""
  :homepage "https://github.com/Shirakumo/alloy"
  :serial T
  :components ((:file "experimental/alloy/renderers/simple/package")
               (:file "experimental/alloy/renderers/simple/protocol")
               (:file "experimental/alloy/renderers/simple/transforms")
               (:file "experimental/alloy/renderers/simple/defaults")
               (:file "experimental/alloy/renderers/simple/documentation"))
  :depends-on (:documentation-utils
               :colored
               :alloy)
  :in-order-to ((asdf:test-op (asdf:test-op :alloy-test))))

(asdf:defsystem alloy-opengl
  :version "0.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description ""
  :homepage "https://github.com/Shirakumo/alloy"
  :serial T
  :components ((:file "experimental/alloy/renderers/opengl/package")
               (:file "experimental/alloy/renderers/opengl/protocol")
               (:file "experimental/alloy/renderers/opengl/renderer")
               (:file "experimental/alloy/renderers/opengl/gradient")
               (:file "experimental/alloy/renderers/opengl/unmanaged")
               (:file "experimental/alloy/renderers/opengl/documentation"))
  :depends-on (:documentation-utils
               :alloy-simple
               :cl-opengl)
  :in-order-to ((asdf:test-op (asdf:test-op :alloy-test))))

(asdf:defsystem alloy-opengl-fond
  :version "0.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description ""
  :homepage "https://github.com/Shirakumo/alloy"
  :serial T
  :components ((:file "experimental/alloy/renderers/opengl/fond"))
  :depends-on (:alloy-opengl
               :font-discovery
               :cl-fond)
  :in-order-to ((asdf:test-op (asdf:test-op :alloy-test))))

(asdf:defsystem alloy-opengl-msdf
  :version "0.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description ""
  :homepage "https://github.com/Shirakumo/alloy"
  :serial T
  :components ((:file "experimental/alloy/renderers/opengl/msdf"))
  :depends-on (:alloy-opengl
               :alloy-simple-presentations
               :alloy-animation
               :font-discovery
               :uax-14
               :sdf/bmfont
               :3b-bmfont
               :3b-bmfont/json)
  :in-order-to ((asdf:test-op (asdf:test-op :alloy-test))))

(asdf:defsystem alloy-opengl-png
  :version "0.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description ""
  :homepage "https://github.com/Shirakumo/alloy"
  :serial T
  :components ((:file "experimental/alloy/renderers/opengl/png"))
  :depends-on (:alloy-opengl
               :pngload)
  :in-order-to ((asdf:test-op (asdf:test-op :alloy-test))))

(asdf:defsystem alloy-glfw
  :version "0.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description ""
  :homepage "https://github.com/Shirakumo/alloy"
  :serial T
  :components ((:file "experimental/alloy/renderers/glfw/package")
               (:file "experimental/alloy/renderers/glfw/fixup")
               (:file "experimental/alloy/renderers/glfw/renderer")
               (:file "experimental/alloy/renderers/glfw/windowing")
               (:file "experimental/alloy/renderers/glfw/documentation"))
  :depends-on (:documentation-utils
               :alloy-simple
               :alloy-simple-presentations
               :alloy-opengl
               :alloy-opengl-msdf
               :alloy-opengl-png
               :alloy-windowing
               :cl-opengl
               :cl-glfw3
               :colored)
  :in-order-to ((asdf:test-op (asdf:test-op :alloy-test))))

(asdf:defsystem alloy-examples
  :version "0.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Example programs using Alloy"
  :homepage "https://github.com/Shirakumo/alloy"
  :serial T
  :components ((:file "experimental/alloy/examples/package")
               (:file "experimental/alloy/examples/toolkit")
               (:file "experimental/alloy/examples/windows")
               (:file "experimental/alloy/examples/drop")
               (:file "experimental/alloy/examples/constraint")
               (:file "experimental/alloy/examples/animation")
               (:file "experimental/alloy/examples/menu")
               (:file "experimental/alloy/examples/font-mixing")
               (:file "experimental/alloy/examples/fonts"))
  :depends-on (:alloy-glfw
               :alloy-constraint))
