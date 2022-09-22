;; $Id: zpb-ttf.asd,v 1.5 2006/03/24 20:47:27 xach Exp $

(defpackage #:zpb-ttf-system
  (:use #:cl #:asdf))

(in-package #:zpb-ttf-system)

(defsystem #:zpb-ttf
  :version "1.0.5"
  :author "Zach Beane <xach@xach.com>"
  :description "Access TrueType font metrics and outlines from Common Lisp"
  :license "BSD"
  :components ((:file "text/zpb-ttf/package")
               (:file "text/zpb-ttf/util"
                      :depends-on ("text/zpb-ttf/package"))
               (:file "text/zpb-ttf/conditions"
                      :depends-on ("text/zpb-ttf/package"))
               (:file "text/zpb-ttf/bounding-box"
                      :depends-on ("text/zpb-ttf/package"))
               (:file "text/zpb-ttf/font-loader"
                      :depends-on ("text/zpb-ttf/package"
                                   "text/zpb-ttf/util"
                                   "text/zpb-ttf/bounding-box"))
               (:file "text/zpb-ttf/maxp"
                      :depends-on ("text/zpb-ttf/package"
                                   "text/zpb-ttf/util"
                                   "text/zpb-ttf/font-loader"))
               (:file "text/zpb-ttf/head"
                      :depends-on ("text/zpb-ttf/package"
                                   "text/zpb-ttf/util"
                                   "text/zpb-ttf/conditions"
                                   "text/zpb-ttf/font-loader"))
               (:file "text/zpb-ttf/kern"
                      :depends-on ("text/zpb-ttf/package"
                                   "text/zpb-ttf/util"
                                   "text/zpb-ttf/conditions"
                                   "text/zpb-ttf/font-loader"))
               (:file "text/zpb-ttf/loca"
                      :depends-on ("text/zpb-ttf/package"
                                   "text/zpb-ttf/util"
                                   "text/zpb-ttf/font-loader"))
               (:file "text/zpb-ttf/name"
                      :depends-on ("text/zpb-ttf/package"
                                   "text/zpb-ttf/util"
                                   "text/zpb-ttf/conditions"
                                   "text/zpb-ttf/font-loader"))
               (:file "text/zpb-ttf/cmap"
                      :depends-on ("text/zpb-ttf/package"
                                   "text/zpb-ttf/util"
                                   "text/zpb-ttf/name"
                                   "text/zpb-ttf/font-loader"))
               (:file "text/zpb-ttf/post"
                      :depends-on ("text/zpb-ttf/package"
                                   "text/zpb-ttf/util"
                                   "text/zpb-ttf/conditions"
                                   "text/zpb-ttf/font-loader"))
               (:file "text/zpb-ttf/hhea"
                      :depends-on ("text/zpb-ttf/package"
                                   "text/zpb-ttf/util"
                                   "text/zpb-ttf/font-loader"))
               (:file "text/zpb-ttf/hmtx"
                      :depends-on ("text/zpb-ttf/package"
                                   "text/zpb-ttf/util"
                                   "text/zpb-ttf/font-loader"
                                   "text/zpb-ttf/hhea"))
               (:file "text/zpb-ttf/glyf"
                      :depends-on ("text/zpb-ttf/package"
                                   "text/zpb-ttf/util"
                                   "text/zpb-ttf/loca"
                                   "text/zpb-ttf/font-loader"))
               (:file "text/zpb-ttf/glyph"
                      :depends-on ("text/zpb-ttf/package"
                                   "text/zpb-ttf/util"
                                   "text/zpb-ttf/font-loader"
                                   "text/zpb-ttf/bounding-box"
                                   "text/zpb-ttf/glyf"
                                   "text/zpb-ttf/kern"
                                   "text/zpb-ttf/loca"))
               (:file "text/zpb-ttf/font-loader-interface"
                      :depends-on ("text/zpb-ttf/package"
                                   "text/zpb-ttf/util"
                                   "text/zpb-ttf/conditions"
                                   "text/zpb-ttf/font-loader"
                                   "text/zpb-ttf/maxp"
                                   "text/zpb-ttf/head"
                                   "text/zpb-ttf/kern"
                                   "text/zpb-ttf/loca"
                                   "text/zpb-ttf/name"
                                   "text/zpb-ttf/cmap"
                                   "text/zpb-ttf/post"
                                   "text/zpb-ttf/hhea"
                                   "text/zpb-ttf/hmtx"))))
