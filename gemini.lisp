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

(defmethod hunchentoot:start-listening ((acceptor gemini-acceptor))
  (when (hunchentoot::acceptor-listen-socket acceptor)
    (hunchentoot:hunchentoot-error "acceptor ~A is already listening" acceptor))
  (let ((hunchentoot:*acceptor* acceptor))
    (hunchentoot:log-message* :info "Connecting to socket listening at port ~a"
                              (hunchentoot:acceptor-port acceptor)))
  (setf (hunchentoot::acceptor-listen-socket acceptor)
        (usocket:socket-listen (or (hunchentoot:acceptor-address acceptor)
                                   usocket:*wildcard-host*)
                               (hunchentoot:acceptor-port acceptor)
                               :reuseaddress t
                               :backlog (hunchentoot:acceptor-listen-backlog acceptor)))
  (values))

(defmethod hunchentoot:process-connection ((acceptor gemini-acceptor) socket
                                           &aux (hunchentoot:*acceptor* acceptor))
  (hunchentoot:log-message* :info "Starting gemini request processing...")
  (let* ((url (quri:uri (read-line (usocket:socket-stream socket) nil nil)))
         (path (resolve-path (quri:uri-path url)))
         (mime-type (mimes:mime path))
         (tripod (ignore-errors (file->tripod path (path-backend path)))))
    (hunchentoot:log-message* :info "Got a gemini request: ~a" url)
    (cond
      ((and path tripod)
       (write-sequence (format nil "20 text/gemini~c~c" #\return #\newline)
                       (usocket:socket-stream socket))
       (write-sequence
        (phos/gemtext:unparse
         (when (resolve-path "header.gmi")
           (alexandria:with-input-from-file (f (resolve-path "header.gmi"))
             (phos/gemtext:parse f)))
         nil)
        (usocket:socket-stream socket))
       (write-sequence (path->backend path :gemini)
                       (usocket:socket-stream socket))
       (write-sequence
        (phos/gemtext:unparse
         (when (resolve-path "footer.gmi")
           (alexandria:with-input-from-file (f (resolve-path "footer.gmi"))
             (phos/gemtext:parse f)))
         nil)
        (usocket:socket-stream socket))
       (write-char #\newline (usocket:socket-stream socket))
       (force-output (usocket:socket-stream socket))
       (usocket:socket-close socket))
      (path
       (write-sequence (format nil "20 ~a~c~c" mime-type #\return #\newline)
                       (usocket:socket-stream socket))
       (write-sequence
        (uiop:read-file-string path)
        (usocket:socket-stream socket))
       (force-output (usocket:socket-stream socket))
       (usocket:socket-close socket))
      (t (usocket:socket-close socket)))))
