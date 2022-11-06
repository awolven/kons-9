(defpackage #:kons-9
  (:use #:common-lisp)
  (:local-nicknames (#:p #:origin.vec3))
  (:export
   #:*scene*
   #:run
   #:show-window)
  (:import-from #:vk
		#:*app*)
  (:import-from #:krma
		#:application-scene))
