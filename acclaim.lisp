;;; -*- Mode: Lisp; -*-
;;; --------------------------------------------------------------------
;;;     Title: Acclaim - A Simple Presentation Program
;;;   Authors: Daniel Barlow, Max-Gerd Retzlaff
;;;   Version: 0.25
;;;    Date of this version: Jun, 19th 2004, 03:16:12 CEST
;;;
;;;   First version by Daniel Barlow <dan@telent.net>
;;;   (see http://www.linux.org.uk/~dan/linux2003/viewer.lisp)
;;;   Changes by Max-Gerd Retzlaff <m.retzlaff@gmx.net>
;;;
;;;   You need CLX and a modified version of image-reader.lisp of the 
;;;   Eclipse window manager that you should have gotten with this file.
;;;
;;;   There is a homepage for this program at:
;;;        http://www.bl0rg.net/~mgr/acclaim.html
;;;     or http://team.vantronix.net/~mgr/acclaim.html
;;;
;;;   History:
;;;   0.10 - 0.11: page numeration
;;;   0.11 - 0.12: li*, br, *draw-background-image* 
;;;   0.12 - 0.13: inline image support (with all test code)
;;;   0.13 - 0.14: inline image support (test code removed)
;;;   0.14 - 0.15: cosmetic changes of the source coded
;;;   0.15 - 0.16: renamed from exhibitionist to acclaim
;;;   0.16 - 0.17: new function load-bg-image
;;;   0.17 - 0.18: only minimal changes
;;;   0.18 - 0.19: - make-element is generic function now.
;;;                - Inline images are preloaded if *preload-images* is set.
;;;                - The background image (or solid color) is to be defined
;;;                  in a configure-block of the slide definition file.
;;;                - If *slides-pathname* is set to a valid pathname the slides
;;;                  (and its images) will be loaded as acclaim itself is loaded.
;;;   0.19 - 0.20: - Now you can also specify fonts and their colors in the
;;;                  configure-blog. Right now it is a crude hack and you
;;;                  can only define the default font and the element "title".
;;;                  In the next release I will problably remove the macro
;;;                  make-render-element=around again, replacing it by the
;;;                  metaclass-foo I had in a previous version of acclaim.
;;;                - Note to toggling of *preload-images*:
;;;                  Use (load-slides pathname :preload-images nil) to switch
;;;                  it off and t for enabling, respectively.
;;;   0.20 - 0.21: - New metaclass element-class; the elements have now
;;;                  class slots for fontname, color, and shift-value
;;;                  (as well as additional slots for their default values).
;;;                  The macro make-render-element=around is replaced by
;;;                  a more generic method that uses the new class slots.
;;;                  These changes make the code far more complex but
;;;                  on the other hand the configuration of fonts, colors,
;;;                  etc. in the slide definition files works smoothly now.
;;;                - All variables defining the look of acclaim's output
;;;                  are now restored before a new slide definition is loaded.
;;;                - Inline images can be positioned by :x-align and :y-align.
;;;                - A new function (change-host hostname).
;;;   0.21 - 0.22: - New variable *debug-do-not-load-bg-image*
;;;                - Set *font* in #'load-slides if *display* is bound.
;;;                - Set *last-foil* to 0 in #'restore-defaults.
;;;                - button-press-event removed; #\Space is enough.
;;;   0.22 - 0.23: - line wrapping support
;;;                  (The code itself is very ugly and the algorithm is really
;;;                  inefficient but it works (also in some more complicated
;;;                  situations like some different font enviroments in one
;;;                  line) and as there is normally only a handful of lines per
;;;                  slide the inefficiency is really not a very big problem.)
;;;                - Every text that is not in a pre enviroment (or a subclass
;;;                  of it, i.e. smallpre) is now wrapped.
;;;                - Apart from the *offset* the *line-begin* of a new line of
;;;                  text is now to be remembered, in case that a line wrap
;;;                  occurs.
;;;                - Inline images can be positioned by :x-align and :y-align.
;;;   0.23 - 0.24: - (Not really new, exiting features; mainly refactoring.)
;;;                - Pseudo package my-pcl for importing of mop functions
;;;                - Global variables for the default color, fontname, bg-color,
;;;                  and bg-clx-image removed and replaced by _class_ slots of
;;;                  the element slide.
;;;                - fontname, color, shift-value, and bg-color (the latter
;;;                  only for a slide) are now instance specific;
;;;                  Hence you can write ((tt :color "white") "foo") to get a
;;;                  white tt, or ((slide :bg-color "blue" :color "green") ..)
;;;                  to get a blue slide with a default textcolor of green.
;;;                - New elements slideset, font and page-number.
;;;                - The page-numbers are now added to the slides themselves
;;;                  when the slides file is loaded (in load-slides) and not
;;;                  when the slide is displayed (in render-slide).
;;;                - The slides of a slideset are now grouped as the content
;;;                  of a slideset element, as in:
;;;                  (slideset (configure ..) (slide ..) (slide ..))
;;;                  It would be possible to define several slidesets in one
;;;                  file but right now only the first one is actually used.
;;;                - New method element-property (and aliases element-fontname,
;;;                  element-color, etc.). It tries to get the property's value
;;;                  for the instance, if that's not set then for the class of
;;;                  the instance, then for the superclasses up to the element
;;;                  class itself.  So if the class pre has the color "white",
;;;                  the subclass smallpre will also be white.
;;;                - Due to the above changes the make-element configure method
;;;                  got much simpler. The format of a configure block is also
;;;                  easier: No keywords just a list of element setting blocks.
;;;                - wm-properties also set to Acclaim and not to Exhibition now.
;;;                - Hotkey #\s (for screenshot) added; the shots will be
;;;                  placed in the directory of the slides definition file. The
;;;                  pathname "/usr/bin/scrot" for the screen capture program
;;;                  "scrot" is hardcoded. Go to http://linuxbrit.co.uk/scrot/
;;;                  to get scrot (or do "apt-get install scrot" on Debian).
;;;   0.24 - 0.25: - (Another mainly-refactoring-release, although there are some
;;;                  perhaps interesting changes.)
;;;                - No use of the meta object protocol (MOP) (no meta-class
;;;                  element) anymore. It is replaced by a model based on
;;;                  generic methods. (An idea of Manuel Odendahl. Thank you.)
;;;                  This removes quite some complexity in my humble opinion.
;;;                - Thus the program defaults are stored as generic methods for
;;;                  each element now, whilst the slideset defaults are an
;;;                  association list in the slot :configuration of the
;;;                  particular slideset instance. (This also means that you can
;;;                  have more than one slideset with different configurations
;;;                  at a time. Perhaps useless, but who know..)
;;;                - Because of the above mentioned there is no configure block
;;;                  anymore, instead the settings are defined as a alist of the
;;;                  slideset's :configuration slot as in:
;;;                  ((slideset :configuration ((slide :bg-image "Yummy_Pi.pnm")))
;;;                     (slide ..) (slide ..))
;;;                - As there is no configure block anymore, there is no
;;;                  make-element method for it, either. A slightly drawback of
;;;                  this is that there are also no "Changing element ~a: .."-
;;;                  messages during slide loading anymore.
;;;                - And finally as the configuration is not stored in class
;;;                  slots of the elements anymore there is no need for a
;;;                  function restore-defaults. An advantage of the new model.
;;;                - New exported function change-slideset to switch between the
;;;                  loaded slidesets (that are stored in *slidesets* by the way).
;;;                - The exported functions start and go-on are merged, and the
;;;                  resulting single function is called run. Thus the
;;;                  presentation will always start with the current slide
;;;                  defined by *last-foil* which is normally (see below) reset
;;;                  to zero in *load-slides*. If you want to start at a
;;;                  different slide call something like (acclaim:run :foil 2).
;;;                - New element no-wrap. It is useful to define blocks whose
;;;                  text is not wrapped. The element pre is defined as a
;;;                  subclass of it now.
;;;                - Slides can have individual background images now. But while
;;;                  the slideset's default background image is preloaded in an
;;;                  X image pixmap on program start (in the run function), a
;;;                  slide's individual background image is displayed using
;;;                  xlib:put-image (just as an inline image) which is slower.
;;;                  (I don't want to encourage the use of many different
;;;                  background images but if you really need this then drop me
;;;                  a line. A drawback would be that the creation of X image
;;;                  pixmaps for these additional background images would slow
;;;                  down the start of the presentation (the function run).)
;;;                - The function render-slide is changed because of the
;;;                  individual background images. The order of places that are
;;;                  checked to get the actually used background is:
;;;                  1) The slide's background image, 2) the slide's background
;;;                  color, 3) the slideset's default background image,
;;;                  4) the slideset's default background color, 5) the program's
;;;                  default background color (i.e. "midnightblue" at present).
;;;                - Global variable *show-bg-image* renamed to
;;;                  *debug-show-bg-image*, *debug-do-not-load-bg-image* renamed
;;;                  to *debug-do-not-load-bg-images*. New global variables
;;;                  *debug-do-not-reset-*last-foil** and *slideset-number*.
;;;                - The loading of background images is not influenced by the
;;;                  variable *preload-images*. They are preloaded unless
;;;                  *debug-do-not-load-bg-images* is true, and in that case
;;;                  they are not displayed at all (a slightly drawback of the
;;;                  new configuration model).
;;;
;;; --------------------------------------------------------------------

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; package definition
(defpackage #:my-pcl
  (:use #+sbcl #:SB-PCL
	#+openmcl #:CCL
	#+cmu #:PCL
	#+sbcl #:sb-ext)
  (:export #:class-direct-superclasses
	   #:run-program))

(defpackage #:acclaim
  (:use #:cl #:my-pcl)
;;(:require #:xlib #:ppm) ;; only pseudo code.. use asdf
  (:export #:run #:load-slides #:change-host #:change-slideset))

(in-package :acclaim)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; variables you might want to change
;;

;; Set this variable to non-nil if the slides should be loaded directly at load-time:
(defvar *slides-pathname*
  #p"/home/mgr/daten/coding/lisp/acclaim/slides/lisp-ist-toll.slides")

;;;;;;;;;;;;;;;;;;;;;;;
;; Far too many variables..
;; But normally you do not have to change them:
;;   *preload-images* is to be set via (load-slides pathname :preload-images nil),
;;   *host* should be changed via (change-host "hostname"), and
;;   *slideset-number* using (change-slideset 0).

(defvar *preload-images* t
  "t: load images while loading the slides (using load-slides), or
nil: load images when they are actually put on the screen (using render-element)")
  
(defvar *host* ""
  "The hostname of the computer on whose default display the slides should be displayed.
Please use (change-host \"name\") to change this variable.")
(defvar *default-display-depth* ;; has to be before (defvar *bg-clx-image* ..)
                                ;; and after (defvar *host* ..)
  (ppm:initialize-host-default-display *host*))

(defvar *slideset-number* 0
  "The number of the slideset that should be displayed.
Please use (change-slideset 0) to change it.")
(defvar *slidesets* nil)
(defvar *offset* (complex 0 0))
(defvar *line-begin* 0)
(defvar *last-string-skip* nil)
(defvar *wrap-lines* t)
(defvar *main-x-border* 50)
(defvar *main-y-border* 20)
(defvar *last-foil* 0)

(DEFVAR *DISPLAY*)
(DEFVAR *SCREEN*)
(DEFVAR *SCREEN-width*)
(DEFVAR *SCREEN-height*)
(DEFVAR *COLORMAP*)
(DEFVAR *WIN*)

(defvar *font*)
(defvar *fg-color*)
(defvar *fg-color-gcontext*)
(defvar *bg-color-gcontext*)
(defvar *foreground-pixel*)
(defvar *background-pixel*)
(defvar *bg-image-pixmap*)
(defvar *bg-image-gcontext*)

;;;;;;;;;;;;;;;;;;;;;;;
;; debug variables
(defvar *debug-do-not-reset-*last-foil** nil)
(defvar *debug-boxes* nil)
(defvar *debug-show-bg-image* t)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *debug-do-not-load-bg-images* nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; class definitions and defclass macro

(eval-when (:compile-toplevel :load-toplevel :execute) ;; <-- !!!
  ;; this goes for slightly more than 100 lines!
  
(defmacro read-format (control-string &rest format-arguments)
  `(read-from-string (format nil ,control-string ,@format-arguments)))

(defun create-name (part1 part2)
  (read-format "~a-~a" part1 part2))

(defun create-keyword (word)
  (read-format ":~a" word))
  
(defclass element ()
  ((content :initarg :content :accessor element-content :initform nil)
   (parent :initarg :parent :accessor element-parent :initform nil)
   (slideset :initarg :slideset :accessor element-slideset :initform nil)

   (fontname :initarg :fontname :accessor element-fontname :initform nil)
   (color :initarg :color :accessor element-color :initform nil)
   (shift-value :initarg :shift-value :accessor element-shift-value :initform nil)))

(defgeneric element-fontname-default (element))
(defgeneric element-color-default (element))
(defgeneric element-shift-value-default (element))
(defgeneric element-bg-color-default (element))
(defgeneric element-bg-clx-image-default (element))
(defmethod element-fontname-default ((element element)) nil)  
(defmethod element-color-default ((element element)) nil)
(defmethod element-shift-value-default ((element element)) nil)
(defmethod element-bg-color-default ((element element)) nil)
(defmethod element-bg-clx-image-default ((element element)) nil)

(defmacro def-element-class (class-name (&key fontname color shift-value bg-color)
					&rest class-def)
  `(prog1
     (defclass ,class-name ,@class-def)
     ,@(remove nil
	 (mapcar (lambda (slotset)
		   (let ((slotname (car slotset))
			 (value (cadr slotset)))
		     (when value
		       (let ((method-name (read-format "element-~a-default" slotname)))
			 `(defmethod ,method-name ((element ,class-name))
			    ,value)))))
	       `((fontname ,fontname)
		 (color ,color)
		 (shift-value ,shift-value)
		 (bg-color ,bg-color))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; element class definitions

(def-element-class slideset () (element)
  ((configuration :initarg :configuration :accessor slideset-configuration :initform nil)))

(def-element-class horizontal-element () (element) ())
(def-element-class i
  (:fontname "-*-helvetica-medium-o-*-*-*-240-*-*-*-*-*-*")
  (horizontal-element) ())
(def-element-class tt
  (:fontname "-*-courier-medium-r-*-*-*-240-*-*-*-*-*-*")
  (horizontal-element) ())
(def-element-class b
  (:fontname "-*-helvetica-bold-r-*-*-*-240-*-*-*-*-*-*")
  (horizontal-element) ())
(def-element-class center () (horizontal-element) ())
(def-element-class font () (horizontal-element) ())

(def-element-class vertical-element () (element) ())
(def-element-class slide 
  (:fontname "-*-helvetica-medium-r-*-*-*-240-*-*-*-*-*-*"
   :color "yellow"
   :shift-value #c(0 0)
   :bg-color "midnightblue")
  (vertical-element)
  ((bg-color :initarg :bg-color :accessor slide-bg-color :initform nil)
   (bg-clx-image :initarg :bg-clx-image :accessor slide-bg-clx-image :initform nil)
;;   (bg-image-pixmap :initarg :bg-image-pixmap :accessor slide-bg-image-pixmap :initform nil)
;;   (bg-image-gcontext :initarg :bg-image-gcontext :accessor slide-bg-image-gcontext :initform nil)
   ))
(def-element-class title
  (:fontname "-*-helvetica-medium-r-*-*-*-480-*-*-*-*-*-*"
   :color "white"
   :shift-value #c(0 40))
  (vertical-element) ())
(def-element-class ul () (vertical-element) ())
(def-element-class no-wrap () (vertical-element) ())
(def-element-class pre
  (:fontname "-*-courier-medium-r-*-*-*-240-*-*-*-*-*-*"
   :color "white")
  (no-wrap) ())
(def-element-class smallpre
  (:fontname "-*-courier-medium-r-*-*-*-180-*-*-*-*-*-*")
  (pre) ())
(def-element-class line () (vertical-element) ())
(def-element-class p () (vertical-element) ())
(def-element-class li () (vertical-element) ())
(def-element-class li* () (vertical-element) ())
(def-element-class br () (vertical-element) ())
(def-element-class image () (vertical-element)
  ((clx-image :initarg :clx-image)))
(def-element-class page-number () (vertical-element) ())

) ;;   <----- !!!!!!   ) of (eval-when..   !!!!!


(defun element-property-default (element property-name)
  (let ((*package*  #.*package*))
    (funcall (read-format "element-~a-default" property-name) element)))

(defun element-property (element property-name &key no-recursion)
  (let* ((class (class-of element))
	 (element-name (class-name class))
	 (slideset (element-slideset element))
	 (element-config
	  (when slideset
	    (cdr (assoc element-name (slideset-configuration slideset)))))
	 (property (or (slot-value element property-name)
		       (getf element-config (create-keyword property-name))
		       (element-property-default element property-name))))
    ;; Recursion: Check the superclasses up to the class element.
    (if property
	property
      (if (or (eql element-name 'element)
	      no-recursion)
	  nil ;; Yes, this is the right thing!
	;; Note that only the first of direct-superclasses is used!
	;; Right now I can't think of an important case to use multiple 
	;; inheritence to define an element, so this is enough for now.
	(element-property (make-instance (car (class-direct-superclasses class))) property-name)))))
     
(defgeneric element-fontname (element))
(defmethod element-fontname ((element element))
  (element-property element 'fontname))
(defgeneric element-color (element))
(defmethod element-color ((element element))
  (element-property element 'color))
(defgeneric element-shift-value (element))
(defmethod element-shift-value ((element element))
  (element-property element 'shift-value))
(defgeneric element-bg-color (element))
(defmethod element-bg-color ((element slide))
  (element-property element 'bg-color :no-recursion t))
(defgeneric element-bg-clx-image (element))
(defmethod element-bg-clx-image ((element slide))
  (element-property element 'bg-clx-image :no-recursion t))

(defun default-bg-clx-image-for-slideset (&optional (slideset-number *slideset-number*))
  (element-bg-clx-image (make-instance 'slide :slideset (nth slideset-number *slidesets*))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; methods and functions

;;;;;;;;;;;;;;;;;;;;;;;
;;; miscellaneous functions


(defun change-host (hostname)
  "Set the hostname to get output on the default display of a different host computer."
  (format *trace-output* "Note: If the new host's default display has a different color depth you have to reload the slides.~%")
  (setf *host* hostname)
  (setf *default-display-depth*
	(ppm:initialize-host-default-display *host*)))

(defun change-slideset (slideset-number)
  (unless *debug-do-not-reset-*last-foil**
    (setf *last-foil* 0))
  (setf *slideset-number* slideset-number))

(defun load-image (filename &optional (images-dir *slides-pathname*))
  "Load a pnm file (given by filename) and return the resulting clx-image;
   filename is relative to images-dir (default *slides-pathname*)."
  (ppm:load-ppm-into-clx-image_depth
   (merge-pathnames filename images-dir)
   *default-display-depth*))

(defun get-font (f) (xlib:open-font *display* f))

(defun colorname-to-color (colorname)
  (xlib:alloc-color *colormap*
		    (xlib:lookup-color *colormap* colorname)))

(defun get-slides (&optional (slideset-number *slideset-number*))
  (element-content (nth slideset-number *slidesets*)))

;;;;;;;;;;;;;;;;;;;;;;;
;;; render-element methods

(defgeneric render-element (e))

(defmethod render-element :around ((element element))
  (let* ((fontname (element-fontname element))
	 (color (element-color element))
	 (shift-value (or (element-shift-value element)
			  #c(0 0)))
	 (*font* (if fontname
		     (get-font fontname)
		   *font*))
	 (*fg-color* (if color
			(colorname-to-color color)
		      *fg-color*)))
    (if (or fontname color)
	(xlib:with-gcontext (*fg-color-gcontext*
			     :font *font*
			     :foreground *fg-color*)
			    (+ shift-value (call-next-method)))
	(+ shift-value (call-next-method)))))

(defmethod render-element :around ((element no-wrap))
  (let ((*wrap-lines* nil))
    (call-next-method)))

(defmethod render-element :around ((element vertical-element))
  (let ((*line-begin* (realpart *offset*)))
    (call-next-method)))

(defmethod render-element :around ((element page-number))
  (let ((*wrap-lines* nil)
	(*offset* (complex (- *screen-width* 85)
			   (- *screen-height* 60))))
    (call-next-method)))

(defmethod render-element :around ((element br))
  (setf *offset* (complex *line-begin* (imagpart *offset*)))
  (call-next-method))

(defmethod render-element :around ((element t))
;;  (declare (optimize (debug 3)))
;;    (format *trace-output* "font: ~a, element-class: ~a~%" *font* (class-of element))
  (let ((before *offset*)
	(size (call-next-method)))
    (when (and *debug-boxes* before size)
      (xlib:draw-rectangle *win* *fg-color-gcontext*
			   (floor (realpart before))
			   (floor (imagpart before))
			   (floor (realpart size))
			   (floor (imagpart size))))
    size))

(defmethod render-element ((e center))
  (multiple-value-bind (w a d l r ascent descent) 
      (xlib:text-extents *font* (car (element-content e)))
    (declare (ignore a d l r ascent descent))
    (let* ((new-offset (- (/ (- (xlib:drawable-width *win*) w) 2)
			  (realpart *offset*)))
	   (*offset* (+ *offset* new-offset)))
      (+ new-offset (call-next-method)))))
    
(defmethod render-element ((e string))
  (let (i
	start
	line  
	(width 0)
	(height 0)
	(line-begin (floor (realpart *offset*)))
	(no-whitespace-magic 0)
	wrap-occured)
    (when *wrap-lines*
      ;; hm, should this also slurp the whitespace following the newline?
      (setf e (substitute #\Space #\Newline e)))
    (loop
;;      (format *trace-output* "a: start: ~a, i: ~a~%" start i)
;;      (force-output)
     (tagbody loop-start
     (setf start (if i (1+ i) 0)
	   i (unless *wrap-lines*
	       (position #\Newline e :start (if i (1+ i) 0)))
	   line (subseq e (or start 0) (or i (length e))))
;;      (format *trace-output* "b: start: ~a, i: ~a~%" start i)
;;      (force-output)
     (tagbody make-fit ;; Yes, the performance of this algorithm is _impressive_!
       (multiple-value-bind (line-width a d l r ascent descent) 
	   (xlib:text-extents *font* e
			      :start (or start 0)
			      :end  (or i (length e)))
	 (declare (ignore a d l r))
	 (when (and (> line-width (- *screen-width*
				     line-begin
				     *main-x-border*))
		    *wrap-lines*)
;; 	   (format *trace-output* "too long: ~a~%" line)
;; 	   (force-output)
	   (setf wrap-occured t)
	   (setf i (position-if (lambda (char)
				  (position char '(#\Space #\Newline #\Tab)))
				e :start (or start 0)
				  :end (or i (length e))
				  :from-end t)
		 ;; to do: Do some magic if i is nil.
		 line (subseq e (or start 0) (or i (length e))))
;; 	   (format *trace-output* "new: ~a~%" line)
;; 	   (force-output)
	   (if i ;; <- This is the formidable magic if the line is 
	         ;;    too long and it doesn't contain any whitespace.
	       (go make-fit)
	     (progn
	       (setf e (concatenate 'string " " e))
	       (setf no-whitespace-magic 
		     (or *last-string-skip* (+ ascent descent)))
	       (go loop-start))))
       (xlib:draw-glyphs *win* *fg-color-gcontext*
			 line-begin
			 (floor (+ height ascent (imagpart *offset*)))
			 line)
       (setf width line-width ;(max width line-width)
	     height (+ ascent descent height)
	     *last-string-skip* (+ ascent descent)
	     line-begin *line-begin*)
       (unless i (return))))))
    (complex (if wrap-occured
		 ;; subtract original difference of *offset* to *line-begin*
		 (- width (- (realpart *offset*) *line-begin*))
	       width)
	     (+ height no-whitespace-magic))))

(defmethod render-element ((e element))
  (let ((size 0)
	(*offset* *offset*)
	(kids (element-content e))
	outer-wrap-correction)
;;    (pprint kids *trace-output*)
    (loop
     (let ((kid (car kids)))
       (setf kids (cdr kids))
       (unless kid (return (+ size (if outer-wrap-correction
				       outer-wrap-correction
				     0))))
       (let ((kid-size (render-element kid)))
	 (when (typep kid 'vertical-element)
	     (setf size (complex (max (realpart size) (realpart kid-size))
				 (+ (imagpart kid-size) (imagpart size)))
		   *offset* (+ *offset* (complex 0 (imagpart kid-size)))))
	 (when (typep kid 'horizontal-element)
	     (setf size (complex (+ (realpart size) (realpart kid-size))
				 (max (imagpart kid-size) (imagpart size)))
		   *offset* (+ *offset* (realpart kid-size))))
	 (when (typep kid 'string)
	   (let ((wrap-correction (if *last-string-skip*
				      (complex 0 (- *last-string-skip*))
				    0)))
	     (when *last-string-skip*
	       (if outer-wrap-correction
		   (incf outer-wrap-correction wrap-correction)
		 (setf outer-wrap-correction 0)))
	     (setf *last-string-skip* nil)
	     (setf size (complex (+ (realpart size) (realpart kid-size))
				 (+ (imagpart kid-size) (imagpart size)))
		   *offset* (+ *offset* kid-size wrap-correction)))))))))

	     

(defmethod render-element ((e ul))
  (let ((*offset* (+ *offset* #c(20 15)))
	(*line-begin* (+ *line-begin* 20)))
    (+ #c(20 30) (call-next-method))))

(defmethod render-element ((e li))
  (let ((*offset* (+ *offset* 30))
	(*line-begin* (+ *line-begin* 30)))
    (let ((size (call-next-method)))
      (xlib:draw-rectangle *win* *fg-color-gcontext*
			   (floor (- (realpart *offset*) 20))
			   (floor (+ (imagpart *offset*) 10))
			   10 10 :fill-p)
      (+ (complex (realpart size) (+ 20 (imagpart size))) 30))))

(defmethod render-element ((e li*))
  (let ((*offset* (+ *offset* 30))
	(*line-begin* (+ *line-begin* 30)))
    (let ((size (call-next-method)))
      (+ (complex (realpart size) (+ 20 (imagpart size))) 30))))

(defmethod render-element ((e p))
  (multiple-value-bind (w a d l r asc desc)
      (xlib:text-extents *font* "J") ;; <- Why exactly "J", Dan? :)
    (declare (ignore w a d l r))
    (let ((n (call-next-method)))
      (complex (realpart n) (+ asc desc (imagpart n))))))

(defmethod render-element ((e br))
;;  (+ #c(0 30)))
  (multiple-value-bind (w a d l r asc desc)
      (xlib:text-extents *font* "J") ;; <- Why exactly "J", Dan? :)
    (declare (ignore w a d l r))
    (complex 0 (+ asc desc))))

(defmethod render-element ((e image))
  "syntax: (image filename &key x y width height x-align y-align (ignore-text nil))
Normally :x and :y, respectively, should _not_ be used.
:x-align can be \"left\", \"center\", or \"right\".
:y-align can be \"top\", \"center\", or \"bottom\".
:ignore-text means that *offset* will be returned unchanged."
  (destructuring-bind
      (filename &key x y width height x-align y-align (ignore-text nil))
      (element-content e)
    
  (let* ((clx-image (or (slot-value e 'clx-image)
			(load-image filename)))
	 (image-gcontext (xlib:create-gcontext :drawable *win*))
	 (width (or width (xlib:image-width clx-image)))
	 (height (or height (xlib:image-height clx-image)))
	 (x-pos (cond
		 ((equal x-align "center")
		  (/ (- (xlib:drawable-width *win*) width) 2))
		 ((equal x-align "right")
		  (- (xlib:drawable-width *win*)
		     *main-x-border*
		     width))
		 ((equal x-align "left")
		  *main-x-border*)
		 (t (realpart *offset*))))
	 (y-pos (cond
		 ((equal y-align "center")
		  (/ (- (xlib:drawable-height *win*) height) 2))
		 ((equal y-align "bottom")
		  (- (xlib:drawable-height *win*)
		     *main-y-border*
		     height))
		 ((equal y-align "top")
		  *main-y-border*)
		 (t (imagpart *offset*)))))
    (xlib:put-image *win* image-gcontext clx-image
		    :x (or x (round x-pos))
		    :y (or y (round y-pos))
		    :width width :height height)

    (if ignore-text
	#c(0 0)
	(+ (complex 0 (+ 20 height)))))))


;;;;;;;;;;;;;;;;;;;;;;;
;;; render-slide function

(defun render-slide (slide)
  (let ((slide-bg-clx-image (slide-bg-clx-image slide)))
    (if (and *debug-show-bg-image*
	     slide-bg-clx-image)
	(xlib:put-image *win* *bg-image-gcontext* slide-bg-clx-image :x 0 :y 0)
      (xlib:with-gcontext (*bg-color-gcontext*
			   :foreground (colorname-to-color (element-bg-color slide)))
        (xlib:draw-rectangle *win* ;; much faster bg-image code for default bg-image
			     (if (and *debug-show-bg-image*
				      (default-bg-clx-image-for-slideset)
				      (not (slide-bg-color slide)))
				 *bg-image-gcontext*
			       *bg-color-gcontext*)
			     0 0
			     (xlib:drawable-width *win*)
			     (xlib:drawable-height *win*)
			     :fill-p))))
  (let ((*offset* (complex *main-x-border* *main-y-border*))
	(*line-begin* *main-y-border*))
    (render-element slide))
  (xlib:display-force-output *display*))

;;;;;;;;;;;;;;;;;;;;;;;
;;; make-element methods

(defgeneric make-element (slideset parent class &rest content))

(defmethod make-element (slideset parent (class string) &rest content)
  (declare (ignore parent content))
  class)

(defmethod make-element (slideset parent (class (eql 'image)) &rest content)
  (let* ((filename (car content))
	 (pathname (merge-pathnames filename *slides-pathname*))
	 (clx-image (progn
		      (format *trace-output* "~:[Not l~;L~]oading inline image ~a.~%"
			      *preload-images* filename)
		      (force-output)
		      (when *preload-images*
			(ppm:load-ppm-into-clx-image_depth pathname *default-display-depth*)))))
    (make-instance class :slideset slideset :parent parent :content content
		   :clx-image clx-image)))

(defun replace-bg-image-by-bg-clx-image (slot-specs)
  (let ((bg-image (getf slot-specs :bg-image)))
    (when bg-image
      (remf slot-specs :bg-image)
      (if *debug-do-not-load-bg-images*
	  (format *trace-output* "Not loading background image ~a.~%" bg-image)
	(progn
	  (format *trace-output* "Loading background image ~a.~%" bg-image)
	  (force-output)
	  (setf (getf slot-specs :bg-clx-image)
		(load-image bg-image))))))
  slot-specs)

(defun replace-bg-image-by-bg-clx-image/of-the-slide-config-in-the-configuration-slot (slot-specs)
  (let ((configuration (getf slot-specs :configuration)))
    (when configuration
      (let ((slide-config (assoc 'slide configuration)))
	(when slide-config
	  (rplacd (assoc 'slide configuration)
		  (replace-bg-image-by-bg-clx-image (cdr slide-config)))))))
    slot-specs)

(defun with-loaded-bg-images (slot-specs)
  (replace-bg-image-by-bg-clx-image/of-the-slide-config-in-the-configuration-slot
   (replace-bg-image-by-bg-clx-image slot-specs)))
		       

(defmethod make-element (slideset parent (class t) &rest content)
  (let* ((instance (if (consp class)
		       (apply #'make-instance (car class)
			      :slideset slideset :parent parent
			      (with-loaded-bg-images (cdr class)))
		     (make-instance class :slideset slideset :parent parent)))
	;; if this is a new slideset set slideset to its instance
	 (slidesetp (eql (if (consp class)
			     (car class)
			   class)
			 'slideset))
	 (slideset (if slidesetp instance slideset)))
    (setf (element-content instance)
	  (remove nil 
		  (mapcar (lambda (c)
			    (apply #'make-element slideset instance
				   (if (listp c)
				       c
				     (list c))))
			  content)))
    instance))

;;;;;;;;;;;;;;;;;;;;;;;
;;; add page numbers
(defun add-page-number-to-slide (number slide)
  (let ((string (princ-to-string number)))
    (setf (element-content slide)
	  (append (element-content slide)
		  (list (make-instance 'page-number :content (list string)))))))

(defun add-page-numbers-to-slideset (slideset)
  (do* ((i 2 (1+ i)) ;; do not number the front page, and start with 2
	(slides (cdr (element-content slideset))
		(cdr slides))
	(slide (car slides) (car slides)))
      ((null slides) slideset)
;    (format t "i: ~a, slide: ~a~%" i slide)
    (add-page-number-to-slide i slide)))


;;;;;;;;;;;;;;;;;;;;;;;
;;; load-slides function

(defun load-slides (&optional (pathname *slides-pathname* new-pathname)
		    &key      (preload-images *preload-images* preload-images-p))
  "Yes, I do know that using both &optinal and &key can be confusing.
Think of (load-slides) without any parameter as syntactic sugar while you are creating a slide set. :)
Be aware that :preload-images toggles the preloading of images *globally* and not only for this invokation of load-slides!"
  (when new-pathname
    (setf *slides-pathname* (merge-pathnames pathname (or *slides-pathname* *default-pathname-defaults*))))
  (when preload-images-p
    (setf *preload-images* preload-images))
  (if (and *slides-pathname*
	   (probe-file *slides-pathname*))
      (progn
	(unless *debug-do-not-reset-*last-foil**
	  (setf *last-foil* 0))
	(format *trace-output* "Loading slides definition in file ~a.~%" *slides-pathname*)
	(setf *slidesets*
	      (remove nil
		  (with-open-file (slides-file *slides-pathname* :direction :input)
	            (let ((*package*  #.*package*))
		      (loop for form = (read slides-file nil nil)
			    while form
			    collect (apply #'make-element nil nil form))))))
	;; add page numbers to the slidesets
	(mapcar #'add-page-numbers-to-slideset *slidesets*))
    (format *trace-output* "Could not load slides definition as the file ~a is not existing.~%Please load one using (load-slide pathname) before starting the slide show with (run)."
	    *slides-pathname*)))

(load-slides) ;; <---- !!!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; go-on, start, and run-core

(defmacro special-let* (variables &rest body)
  `(let* ,variables
     (declare (special ,@(mapcar (lambda (var)
				   (if (symbolp var)
				       var
				     (car var)))
				 variables)))
     ,@body))

(defun run (&key (foil *last-foil*)
		   width
		   height
		   (host *host*))
  "Start the slide show at foil *last-foil*, or at the foil specified by the key-parameter :foil.
   control keys:
   -------------
       Space: next slide
   Backspace: previous slide
           q: quit slide show
           r: reload slide definition file and repaint slide
           s: put a screen shot in the directory of the slides definition file"
  (unless *slidesets*
    (load-slides *slides-pathname*))
  (special-let*
   ((*display* (xlib:open-display host))
    (*screen* (xlib:display-default-screen *display*))
    (*default-display-depth* (xlib:screen-root-depth *screen*))
    (*colormap* (ppm:initialize	;; <- ugly side effect!
		 (xlib:screen-default-colormap *screen*)))
    (*screen-width* (or width (xlib:screen-width *screen*)))
    (*screen-height* (or height (xlib:screen-height *screen*)))
    (*win*
     (xlib:create-window
      :parent (xlib:screen-root *screen*)
      :x 0
      :y 0
      :width *screen-width*
      :height *screen-height*
      :event-mask '(:exposure :button-press :key-release)
      ;;	  :background *bg-image-pixmap*
      ;;  	  :background (colorname-to-color "midnightblue")))
      ))
    (*foreground-pixel* (xlib:screen-black-pixel *screen*)) ; default-foreground-pixel
    (*background-pixel* (xlib:screen-white-pixel *screen*)) ; default-background-pixel
    (*fg-color-gcontext*
     (xlib:create-gcontext
      :cache-p nil
      :drawable *win*
      :fill-style :solid
      :background *background-pixel*))
    (*bg-color-gcontext*
     (xlib:create-gcontext 
      :drawable *win*
      :fill-style :solid
      :background *background-pixel*))
    (*bg-image-pixmap*
     (when (and (not *debug-do-not-load-bg-images*)
		(default-bg-clx-image-for-slideset))
       (xlib:image-pixmap
	(xlib:screen-root *screen*)
	(default-bg-clx-image-for-slideset))))
    (*bg-image-gcontext*
     (xlib:create-gcontext 
      :drawable *win*
      :tile *bg-image-pixmap*
      :fill-style :tiled)))
   (unwind-protect
       (progn
	 (xlib:set-wm-properties *win*
				 :name '|Acclaim|
				 :icon-name "Acclaim"
				 :resource-name "Acclaim"
				 :resource-class '|Acclaim|
				 ;; 				     :command (list* 'hello-world host args)
				 ;; 				     :x x :y y :width width :height height
				 ;; 				     :min-width width :min-height height
				 ;; 				     :input :off :initial-state :normal
				 )
	 (run-core foil))
     ;; close screen
     (xlib:close-display *display*))))

(defun run-core (&optional (number 0))
  (xlib:map-window *win*)
  (labels ((repaint ()
	      (if (< number (length (get-slides)))
		  (progn (render-slide (elt (get-slides) number))
			 nil)
		t)))
;;    ;; assure that number is not out of range
;;    (if (>= number (length *slides*))
;; 	(setf number (1- (length *slides*)))
;;       (if (< number 0)
;; 	  (setf number 0)))
;;    (repaint) ;;superfluous
    (xlib:event-case
       (*display* :discard-p t :force-output-p t)
       (exposure 
	 (window count)
	 (when (zerop count) ;; Ignore all but the last exposure event
	   (xlib:with-state (window)
	     (repaint)))
	 nil)
;;       (button-press ()
;;  	   (when (< number (1- (length (get-slides))))
;;	     (incf number))
;;	   (repaint))
       (key-release
	 (code state)
	 (multiple-value-prog1 
	  (case (xlib:keycode->character *display* code state)
	    (#\Space (when (< number (1- (length (get-slides))))
		       (incf number))
	             (repaint))
	    (#\Backspace (when (> number 0)
			   (decf number))
	                 (repaint))
	    (#\r (load-slides) (repaint))
            ;; bash hack for generating a pdf out of the screenshots:
	    ;; for i in slide-*.pnm; do pnmtops $i ; done | ps2pdf - > all.pdf	    
	    (#\s (run-program "/usr/bin/scrot"
		    (list (format nil "~aslide-~3,'0d.pnm"
				  (directory-namestring *slides-pathname*)
				  (1+ number))))
	          nil)
	    (#\q t)
	    (t nil))
	  (setf *last-foil* number)))))
  *last-foil*)
