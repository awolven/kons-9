(asdf:defsystem :sdf/base
  :description "Signed distance field generator"
  :version "0.0.1"
  :author "Bart Botta <00003b at gmail.com>"
  :license "MIT"
  :depends-on (cl-vectors cl-paths cl-aa cl-aa-misc parse-number
                          float-features)
  :serial t
  :components ((:file "text/sdf/packages-base")
               (:file "text/sdf/v2")
               (:file "text/sdf/geometry")
               (:file "text/sdf/quadratic-intersect-common")
               (:file "text/sdf/quadratic-intersect-geometry")
               (:file "text/sdf/quadratic-intersect")
               (:file "text/sdf/shape")
               (:file "text/sdf/edit-shape")
               (:file "text/sdf/shapedesc")
               (:file "text/sdf/shape-ops")
               (:file "text/sdf/sdf-base")
               (:file "text/sdf/edge-list")
               (:file "text/sdf/rb")
               (:file "text/sdf/df-queue")
               (:file "text/sdf/ff-queue")
               (:file "text/sdf/clean-shape")
               (:file "text/sdf/msdfec")
               (:file "text/sdf/msdf")
               (:file "text/sdf/sdf")))

(asdf:defsystem :sdf/ttf
  :description "utilities for SDF font generation"
  :version "0.0.1"
  :author "Bart Botta <00003b at gmail.com>"
  :license "MIT"
  :depends-on (sdf/base zpb-ttf cl-paths-ttf)
  :serial t
  :components ((:file "text/sdf/packages-ttf")
               (:file "text/sdf/metrics")
               (:file "text/sdf/ttf")))

(asdf:defsystem :sdf
  :description "Signed distance field font glyph atlas generator."
  :version "0.0.1"
  :author "Bart Botta <00003b at gmail.com>, Pavel Korolev <dev@borodust.org>"
  :license "MIT"
  :depends-on (sdf/base sdf/ttf opticl binpack/2)
  :serial t
  :components ((:file "text/sdf/packages")
               (:file "text/sdf/api")))

(asdf:defsystem :sdf/bmfont
  :description "Convert an SDF atlas to a bmfont structure"
  :version "0.0.1"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :license "MIT"
  :depends-on (3b-bmfont sdf pathname-utils)
  :serial t
  :components ((:file "text/sdf/bmfont")))

(defsystem sdf/test
  :depends-on (sdf parachute md5 float-features)
  :serial t
  :perform
  (asdf:test-op (op c) (uiop:symbol-call :parachute :test :sdf/test))
  :components ((:file "text/sdf/test-package")
               ;; automated tests, (parachute:run :sdf/test)
               (:file "text/sdf/tests")
               (:file "text/sdf/edge-test")
               (:file "text/sdf/clean-regression")
               (:file "text/sdf/intersect-regression")

               ;; utilities/manual tests etc
               (:file "text/sdf/leak-check")
               (:file "text/sdf/clean-test")))
