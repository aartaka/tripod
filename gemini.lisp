;;;; gopher.lisp

(in-package #:tripod)

(defvar +gemini+ :gemini)
(defvar +gemtext+ :gemtext)

(setf (gethash "gmi" mimes:*mime-db*)                  "text/gemini"
      (gethash "text/gemini" mimes::*reverse-mime-db*) "gmi"
      (gethash "text/gemini" *mime->backend*)          :gemini
      (gethash :gemini *backend->mime*)                "text/gemini")

;;; Backend to Tripod conversion.

(defvar %current-ul% nil
  "Contains the ul currently being converted from the Gemini list, if any.")

(defmethod backend->tripod ((object t) (backend (eql +gemtext+)) &key)
  (backend->tripod object +gemini+))

(defmethod backend->tripod ((elements list) (backend (eql +gemini+)) &key)
  (loop for element in elements
        append (backend->tripod element +gemini+)
          into result
        finally (let ((ul %current-ul%))
                  (setf %current-ul% nil)
                  (return (append result (when ul (list ul)))))))

(defmethod backend->tripod ((p gemtext:paragraph) (backend (eql +gemini+)) &key)
  (list (make-instance 'paragraph :text (gemtext:text p))))

(defmethod backend->tripod ((q gemtext:blockquote) (backend (eql +gemini+)) &key)
  (list (make-instance 'blockquote :text (gemtext:text q))))

(defmethod backend->tripod ((pre gemtext:verbatim) (backend (eql +gemini+)) &key)
  (list (make-instance 'preformatted :text (gemtext:text pre) :alt (gemtext:alt pre))))

(defmethod backend->tripod ((title gemtext:title) (backend (eql +gemini+)) &key)
  (list (make-instance 'heading :text (gemtext:text title) :level (gemtext:level title))))

(defmethod backend->tripod ((a gemtext:link) (backend (eql +gemini+)) &key)
  (list (make-instance 'link :text (gemtext:text a) :href (gemtext:url a))))

(defmethod backend->tripod ((i gemtext:item) (backend (eql +gemini+)) &key)
  (if %current-ul%
      (setf (elements %current-ul%)
            (append (elements %current-ul%) (list (gemtext:text i))))
      (setf %current-ul% (make-instance 'items :elements (list (gemtext:text i)))))
  nil)

(defmethod backend->tripod :around ((e gemtext:element) (backend (eql +gemini+)) &key)
  (let ((result (call-next-method)))
    (cond
      ((typep e 'gemtext:item)
       result)
      ((and %current-ul% (not (typep e 'gemtext:item)))
       (prog1
           (append (list %current-ul%) result)
         (setf %current-ul% nil)))
      (t result))))

;;; Tripod to Backend conversion.

(defmethod tripod->backend ((nodes list) (backend (eql +gemtext+)) &key)
  (tripod->backend nodes +gemini+))

(defmethod tripod->backend :around ((nodes list) (backend (eql +gemini+)) &key)
  (phos/gemtext:unparse (call-next-method) nil))

(defmethod tripod->backend ((nodes list) (backend (eql +gemini+)) &key)
  (loop for node in nodes
        when (typep node 'items)
          append (loop for element in (elements node)
                       collect (make-instance 'gemtext:item :text element))
            into elements
        else when (typep node 'heading)
               collect (make-instance 'gemtext:title
                                      :level (level node) :text (text node))
                 into elements
        else when (and (typep node 'link)
                       (href node))
               collect (make-instance 'gemtext:link
                                      :url (href node) :text (text node))
                 into elements
        else when (typep node 'blockquote)
               collect (make-instance 'gemtext:blockquote :text (text node))
                 into elements
        else when (typep node 'preformatted)
               collect (make-instance 'gemtext:verbatim :alt (alt node) :text (text node))
                 into elements
        else
          collect (make-instance 'gemtext:paragraph :text (text node))
            into elements
        finally (return elements)))

;;; File to Tripod reading

(defmethod file->tripod ((file pathname) (backend (eql +gemtext+)) &key)
  (file->tripod file +gemini+))

(defmethod file->tripod ((file pathname) (backend (eql +gemini+)) &key)
  (ignore-errors
   (uiop:with-input-file (in file)
     (backend->tripod (gemtext:parse in) +gemini+))))

;;; Gemini Acceptor

(defclass gemini-acceptor (hunchentoot:ssl-acceptor)
  ()
  (:default-initargs
   :address "127.0.0.1"
   :port 1965))

(defmethod hunchentoot:process-connection ((acceptor gemini-acceptor) socket)
  (let* ((url (quri:uri (loop with stream = (usocket:socket-stream socket)
                              with vec = (make-array 0 :element-type '(unsigned-byte 8)
                                                     :adjustable t :fill-pointer 0)
                              for char = (read-byte stream) and prev = char
                              until (and (= char (char-code #\newline))
                                         (= prev (char-code #\return)))
                              do (vector-push-extend char vec)
                              finally (let ((path (progn
                                                    (vector-pop vec)
                                                    (flex:octets-to-string vec :external-format :utf-8))))
                                        (hunchentoot:log-message* :info "Got a gemini url: ~a" path)
                                        (return path)))))
         (path (resolve-path (quri:uri-path url)))
         (mime-type (mimes:mime path))
         (tripod (ignore-errors (file->tripod path (path-backend path)))))
    (hunchentoot:log-message* :info "Got a gemini request: ~a" url)
    (flet ((write-to-bytes (string)
             (write-sequence (flex:string-to-octets string)
                             (usocket:socket-stream socket))))
      (cond
        ((and path tripod)
         (write-to-bytes (format nil "20 text/gemini~c~c" #\return #\newline))
         (write-to-bytes
          (phos/gemtext:unparse
           (when (resolve-path "header.gmi")
             (alexandria:with-input-from-file (f (resolve-path "header.gmi"))
               (phos/gemtext:parse f)))
           nil))
         (write-to-bytes (path->backend path :gemini))
         (write-to-bytes
          (phos/gemtext:unparse
           (when (resolve-path "footer.gmi")
             (alexandria:with-input-from-file (f (resolve-path "footer.gmi"))
               (phos/gemtext:parse f)))
           nil))
         (force-output (usocket:socket-stream socket))
         (usocket:socket-close socket))
        (path
         (write-to-bytes (format nil "20 ~a~c~c" mime-type #\return #\newline))
         (write-sequence
          (alexandria:read-file-into-byte-vector path)
          (usocket:socket-stream socket))
         (force-output (usocket:socket-stream socket))
         (usocket:socket-close socket))
        (t (usocket:socket-close socket))))))
