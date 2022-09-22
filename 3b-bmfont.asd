
(defsystem 3b-bmfont
  :description "BMFont file format readers/writers"
  :version "0.0.1"
  :author "Bart Botta <00003b at gmail.com>"
  :license "MIT"
  ;; currently only load text backend by default, will use others
  ;; if loaded
  :depends-on (3b-bmfont/text)
  :components ((:file "text/3b-bmfont/package")
               (:file "text/3b-bmfont/bmfont")))

(defsystem 3b-bmfont/common
  :depends-on (alexandria split-sequence)
  :components ((:file "text/3b-bmfont/package")
               (:file "text/3b-bmfont/common")))

(defsystem 3b-bmfont/text
  :description "Load/Save BMFont text format"
  :depends-on (3b-bmfont/common)
  :components ((:file "text/3b-bmfont/package")
               (:file "text/3b-bmfont/bmfont-text")))

(defsystem 3b-bmfont/xml
  :description "Load/Save BMFont xml format"
  :depends-on (3b-bmfont/common cxml split-sequence flexi-streams)
  :components ((:file "text/3b-bmfont/package")
               (:file "text/3b-bmfont/bmfont-xml")))

(defsystem 3b-bmfont/json
  :description "Load/Save BMFont-like json format"
  :depends-on (3b-bmfont/common jsown)
  :components ((:file "text/3b-bmfont/package")
               (:file "text/3b-bmfont/bmfont-json")))
