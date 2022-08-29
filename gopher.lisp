;;;; gopher.lisp

(in-package #:tripod)

(defvar +gopher+ :gopher)

(setf (gethash "gopher" mimes:*mime-db*)               "text/gopher"
      (gethash "text/gopher" mimes::*reverse-mime-db*) "gopher"
      (gethash "text/gopher" *mime->backend*)          :gopher
      (gethash :gopher *backend->mime*)                "text/gopher")

;;; Tripod to Backend conversion

(defmethod tripod->backend :around ((nodes list) (backend (eql +gopher+)) &key)
  (apply #'concatenate 'string
         (mapcar (lambda (l) (cl-gopher:write-gopher-line l :stream nil))
                 (call-next-method))))

(defmethod tripod->backend ((nodes list) (backend (eql +gopher+)) &key)
  (alexandria:mappend #'(lambda (n) (tripod->backend n +gopher+)) nodes))

(defmethod tripod->backend ((node element) (backend (eql +gopher+)) &key)
  (list (make-instance 'cl-gopher:info-message :display-string (text node))))

(defmethod tripod->backend ((node heading) (backend (eql +gopher+)) &key)
  (list (make-instance
         'cl-gopher:info-message
         :display-string (uiop:strcat
                          (make-string (level node) :initial-element #\#) (text node)))))

(defmethod tripod->backend ((node items) (backend (eql +gopher+)) &key)
  (mapcar (lambda (e)
            (make-instance
             'cl-gopher:info-message
             :display-string (uiop:strcat "* " (text e) #\newline)))
          (elements node)))

(defmethod tripod->backend ((node link) (backend (eql +gopher+)) &key)
  (let ((mime (mimes:mime (quri:uri-path (href node))))
        (absolute-url (quri:uri-scheme (href node))))
    (cond
      ((and (not absolute-url)
            (not mime))
       (make-instance 'cl-gopher:submenu
                      :display-string (text node)
                      :selector (quri:uri-path (href node))))
      ((and absolute-url
            (uiop:string-prefix-p "text/html" mime))
       (make-instance 'cl-gopher:html-file
                      :display-string (text node)
                      :selector (quri:render-uri (href node))))
      ((uiop:string-prefix-p "image/gif" mime)
       (make-instance 'cl-gopher:gif
                      :display-string (text node)
                      :selector (quri:uri-path (href node))))
      ((uiop:string-prefix-p "image/png" mime)
       (make-instance 'cl-gopher:png
                      :display-string (text node)
                      :selector (quri:uri-path (href node))))
      ((uiop:string-prefix-p "image/" mime)
       (make-instance 'cl-gopher:image
                      :display-string (text node)
                      :selector (quri:uri-path (href node))))
      ((uiop:string-prefix-p "audio/" mime)
       (make-instance 'cl-gopher:sound-file
                      :display-string (text node)
                      :selector (quri:uri-path (href node))))
      ((uiop:string-prefix-p "text/" mime)
       (make-instance 'cl-gopher:text-file
                      :display-string (text node)
                      :selector (quri:uri-path (href node))))
      ((uiop:string-prefix-p "binary/" mime)
       (make-instance 'cl-gopher:binary-file
                      :display-string (text node)
                      :selector (quri:uri-path (href node))))
      (t
       (make-instance 'cl-gopher:unknown :display-string (text node))))))

;;; Acceptor to serve Gopher content

(defclass gopher-acceptor (hunchentoot:acceptor)
  ()
  (:default-initargs
   :address "127.0.0.1"
   :port 70))

(defmethod hunchentoot:start-listening ((acceptor gopher-acceptor))
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

;; Fix selectors so that files are serveable as submenus/textfiles.
(defmethod hunchentoot:process-connection ((acceptor gopher-acceptor) socket)
  (flet ((read-path ()
           (loop with stream = (usocket:socket-stream socket)
                 with vec = (make-array 0 :element-type 'character
                                          :adjustable t :fill-pointer 0)
                 for char = (read-char stream)
                 when (eql char #\newline)
                   do (return (coerce (subseq vec 0 (1- (length vec))) 'string))
                 else do (vector-push-extend char vec))))
    (handler-case
        (loop
          (let* ((path (read-path))
                 (path (resolve-path path))
                 (tripod (ignore-errors (path->tripod* path (path-backend path))))
                 (text (when tripod
                          (tripod->backend tripod :gopher))))
            (hunchentoot:log-message* :info "Gopher path: ~a" path)
            (write-sequence text (usocket:socket-stream socket))
            (write-line "." (usocket:socket-stream socket))
            (force-output (usocket:socket-stream socket))))
      (error ()
        (force-output (usocket:socket-stream socket))
        (usocket:socket-close socket)))))
