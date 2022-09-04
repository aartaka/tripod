;;;; tripod.lisp

(in-package #:tripod)

(defclass element ()
  ((text :initform ""
         :type string
         :initarg :text
         :accessor text
         :documentation "The contents of the `elem'."))
  (:documentation "Basic element."))

(defclass heading (element)
  ((level :initform 1
          :type (integer 1 3)
          :initarg :level
          :accessor level
          :documentation "The level of the heading from one to three."))
  (:documentation "A top-level heading."))

(defclass paragraph (element)
  ()
  (:documentation "A paragraph element with text wrapping."))

(defmethod initialize-instance :after ((para paragraph) &key)
  (setf (text para)
        (string-trim
         '(#\Space) (cl-ppcre:regex-replace-all
                     "\\s{2,}" (substitute #\Space #\Newline (text para)) " "))))

(defclass items (element)
  ((items :initform '()
             :type list
             :initarg :elements
             :accessor elements
             :documentation "A list of strings as elements."))
  (:documentation "Unordered list."))

(defclass link (element)
  ((href :initform (quri:uri "")
         :type quri:uri
         :initarg :href
         :accessor href
         :documentation "The proper URI to some (HTTP, Gemini, Gopher) resource."))
  (:documentation "A link to somewhere."))

(defclass blockquote (element)
  ()
  (:documentation "A quote element."))


(defclass preformatted (element)
  ((alt :initform ""
        :type string
        :initarg :alt
        :accessor alt
        :documentation "The alt text for the text below.
Can be, but is not guaranteed to be, a caption, source language, content warning etc."))
  (:documentation "Pre-formatted text."))
