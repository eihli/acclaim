
(defpackage "ACCLAIM"
  (:use "CL" "XLIB"))

(in-package :acclaim)

(defclass element ()
  ((content :initarg :content :accessor element-content)
   (parent :initarg :parent :accessor element-parent)))

(defclass horizontal-element (element) ())
(defclass i (horizontal-element) ())
(defclass tt (horizontal-element) ())
(defclass b (horizontal-element) ())
(defclass center (horizontal-element) ())

(defclass vertical-element (element) ())
(defclass slide (vertical-element) ())
(defclass title (vertical-element) ())
(defclass ul (vertical-element) ())
(defclass pre (vertical-element) ())
(defclass smallpre (vertical-element) ())
(defclass line (vertical-element) ())
(defclass p (vertical-element) ())
(defclass li (vertical-element) ())

(defvar *display* (xlib:open-default-display))
(defvar *screen* (xlib:display-default-screen *display*))
(defvar *colormap* (xlib:screen-default-colormap *screen*))
(defvar *yellow* 
  (xlib:alloc-color *colormap* (xlib:lookup-color *colormap* "yellow")))

(defvar *win*
  (xlib:create-window
   :parent (xlib:screen-root *screen*)
   :x 0
   :y 0
   :width 1024
   :height 768
   :event-mask '(:exposure :button-press :key-release)
   :background (xlib:alloc-color *colormap*
                 (xlib:lookup-color *colormap*
                            "midnightblue"))))

(defun get-font (f) (open-font *display* f))
(defvar *font* (get-font "-*-helvetica-medium-r-*-*-*-240-*-*-*-*-*-*"))

(defvar *gcontext*
  (create-gcontext
   :cache-p nil
   :drawable *win*
   :fill-style :solid
   :background (xlib:screen-white-pixel *screen*)
   :foreground *yellow*
   :font *font*))

(defvar *background* (xlib:create-gcontext
              :drawable *win*
              :fill-style :solid
              :background (xlib:screen-white-pixel *screen*)
              :foreground (xlib:alloc-color *colormap*
                   (xlib:lookup-color *colormap*
                              "midnightblue"))
              :font "fixed"))
(defvar *palette* nil)
(defvar *black* (xlib:screen-black-pixel *screen*))

(defun make-element (parent class &rest content)
  (let ((r (make-instance class :parent parent)))
    (setf (element-content r)
      (loop for c in content
        if (stringp c) collect c
        else collect (apply #'make-element r c)))
    r))

(defmethod render-element :around ((element title))
  (let ((*font* (get-font "-*-helvetica-medium-r-*-*-*-480-*-*-*-*-*-*")))
    (with-gcontext (*gcontext*
            :foreground (xlib:screen-white-pixel *screen*)
            :font *font*)
      (+ (call-next-method) #c(0 50)))))

(defmethod render-element :around ((element tt))
  (let ((*font* (get-font "-*-courier-medium-r-*-*-*-240-*-*-*-*-*-*")))
    (with-gcontext (*gcontext*
            :font *font*)
      (call-next-method))))

(defmethod render-element :around ((element pre))
  (let ((*font* (get-font "-*-courier-medium-r-*-*-*-240-*-*-*-*-*-*")))
    (with-gcontext (*gcontext*
            :foreground (xlib:screen-white-pixel *screen*)
            :font *font*)
      (call-next-method))))

(defmethod render-element :around ((element smallpre))
  (let ((*font* (get-font "-*-courier-medium-r-*-*-*-180-*-*-*-*-*-*")))
    (with-gcontext (*gcontext*
            :foreground (xlib:screen-white-pixel *screen*)
            :font *font*)
      (call-next-method))))

(defmethod render-element :around ((element b))
  (let ((*font* (get-font "-*-helvetica-bold-r-*-*-*-240-*-*-*-*-*-*")))
    (with-gcontext (*gcontext* :font *font*)
      (call-next-method))))

(defmethod render-element :around ((element i))
  (let ((*font* (get-font "-*-helvetica-medium-o-*-*-*-240-*-*-*-*-*-*")))
    (with-gcontext (*gcontext* :font *font*)
      (call-next-method))))

(defvar *debug-boxes* nil)

(defmethod render-element :around ((element t))
  (declare (optimize (debug 3)))
  (let ((before *offset*)
    (size (call-next-method)))
    (when (and *debug-boxes* before size)
      (xlib:draw-rectangle *win* *gcontext*
               (floor (realpart before))
               (floor (imagpart before))
               (floor (realpart size))
               (floor (imagpart size))))
    size))

(defvar *offset* (complex 0 0))

(defmethod render-element ((e center))
  (multiple-value-bind (w a d l r ascent descent) 
      (xlib:text-extents *font* (car (element-content e)))
    (let* ((new-offset (- (/ (- (drawable-width *win*) w) 2)
              (realpart *offset*)))
       (*offset* (+ *offset* new-offset)))
      (+ new-offset (call-next-method)))))
    

(defmethod render-element ((e string))
  (let (i start
    (width 0)
    (height 0))
    (loop
     (setf start (if i (1+ i) 0)
       i (position #\Newline e :start (if i (1+ i) 0)))
     (multiple-value-bind (w a d l r ascent descent) 
     (xlib:text-extents *font* e :start (or start 0)
                :end (or i (length e)))
       (xlib:draw-glyphs *win* *gcontext*
             (floor (realpart *offset*))
             (floor (+ height ascent (imagpart *offset*))) e
             :start (or start 0)
             :end (or i (length e)))
       (setf width (max width w)
         height (+ ascent descent height))
       (unless i (return))))
    (complex width height)))

(defmethod render-element ((e element))
  (let ((size 0)
    (*offset* *offset*)
    (kids (element-content e)))
    (loop
     (let ((kid (car kids)))
       (setf kids (cdr kids))
       (unless kid (return size))
       (let ((kid-size (render-element kid)))
     (if (typep kid 'vertical-element)
         (setf size (complex (max (realpart size) (realpart kid-size))
                 (+ (imagpart kid-size) (imagpart size)))
           *offset* (+ *offset* (complex 0 (imagpart kid-size))))
         (setf size (complex (+ (realpart size) (realpart kid-size))
                 (max (imagpart kid-size) (imagpart size)))
           *offset* (+ *offset* (realpart kid-size)))))))))

(defmethod render-element ((e ul))
  (let ((*offset* (+ *offset* #c(20 15))))
    (+ #c(20 30) (call-next-method))))

(defmethod render-element ((e li))
  (let ((*offset* (+ *offset* 30)))
    (let ((size (call-next-method)))
      (xlib:draw-rectangle *win* *gcontext*
               (floor (- (realpart *offset*) 20))
               (floor (+ (imagpart *offset*) 10))
               10 10 :fill-p)
      (+ (complex (realpart size) (+ 20 (imagpart size))) 30))))

(defmethod render-element ((e p))
  (multiple-value-bind (w a d l r asc desc) (text-extents *font* "J")
    (let ((n (call-next-method)))
      (complex (realpart n) (+ asc desc (imagpart n))))))
      
(defun render-slide (slide)
  (xlib:draw-rectangle *win* *background*  0 0
               (xlib:drawable-width *win*)
               (xlib:drawable-height *win*)
               :fill-p)
  (let ((*offset* #c(50 20)))
    (render-element slide))
  (xlib:display-force-output *display*))

(defun run (&optional (number 0))
  (xlib:map-window *win*)
  (let (slides)
    (labels ((reload ()
           (setf slides
             (with-open-file
             (slides "ukuug-slides.lisp"  :direction :input)
               (loop for form = (read slides nil nil)
                 while form
                 collect (apply #'make-element nil form)))))
         (repaint ()
           (if (< number (length slides))
           (progn (render-slide (elt slides number)) nil)
           t)))
      (reload)
      (repaint)
      (event-case
       (*display* :discard-p t :force-output-p t)
       (exposure 
    (window count)
    (when (zerop count) ;; Ignore all but the last exposure event
      (with-state (window)
        (render-slide (elt slides number))))
    nil)
       (button-press
    ()
    (incf number)
    (repaint))
       (key-release
    (code state)
    (case (keycode->character *display* code state)
      (#\Space (incf number) (repaint))
      (#\Backspace (when (> number 0) (decf number)) (repaint))
      (#\r (reload) (repaint))
      (#\q t)
      (t nil)))))))
