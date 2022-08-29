;;;; gopher.lisp

(in-package #:tripod)

(defvar +gopher+ :gopher)

(setf (gethash "gopher" mimes:*mime-db*)               "text/gopher"
      (gethash "text/gopher" mimes::*reverse-mime-db*) "gopher"
      (gethash "text/gopher" *mime->backend*)          :gopher
      (gethash :gopher *backend->mime*)                "text/gopher")

;;; Tripod to Backend conversion

(defvar *address* nil "Current address to resolve selectors against.")
(defvar *port* nil "Current port to resolve selectors against.")

(defmethod tripod->backend :around ((nodes list) (backend (eql +gopher+)) &key)
  (apply #'concatenate 'string
         (mapcar (lambda (l) (cl-gopher:write-gopher-line l :stream nil))
                 (call-next-method))))

(defmethod tripod->backend ((nodes list) (backend (eql +gopher+)) &key)
  (alexandria:mappend #'(lambda (n) (tripod->backend n +gopher+)) nodes))

(defmethod tripod->backend ((node element) (backend (eql +gopher+)) &key)
  (list (make-instance 'cl-gopher:info-message
                       :display-string (text node)
                       :hostname *address*
                       :port *port*)))

(defmethod tripod->backend ((node heading) (backend (eql +gopher+)) &key)
  (list (make-instance
         'cl-gopher:info-message
         :display-string (uiop:strcat
                          (make-string (level node) :initial-element #\#) " " (text node))
         :hostname *address*
         :port *port*)))

(defmethod tripod->backend ((node items) (backend (eql +gopher+)) &key)
  (mapcar (lambda (e)
            (make-instance
             'cl-gopher:info-message
             :display-string (uiop:strcat "* " e #\newline)
             :hostname *address*
             :port *port*))
          (elements node)))

(defmethod tripod->backend ((node link) (backend (eql +gopher+)) &key)
  (let ((mime (mimes:mime (quri:uri-path (href node))))
        (absolute-url (quri:uri-scheme (href node))))
    (list
     (cond
       ((not absolute-url)
        (make-instance 'cl-gopher:submenu
                       :display-string (text node)
                       :selector (quri:uri-path (href node))
                       :hostname *address*
                       :port *port*))
       ((and absolute-url
             (uiop:string-prefix-p "text/html" mime))
        (make-instance 'cl-gopher:html-file
                       :display-string (text node)
                       :selector (quri:render-uri (href node))
                       :hostname *address*
                       :port *port*))
       ((uiop:string-prefix-p "image/gif" mime)
        (make-instance 'cl-gopher:gif
                       :display-string (text node)
                       :selector (quri:uri-path (href node))
                       :hostname *address*
                       :port *port*))
       ((uiop:string-prefix-p "image/png" mime)
        (make-instance 'cl-gopher:png
                       :display-string (text node)
                       :selector (quri:uri-path (href node))
                       :hostname *address*
                       :port *port*))
       ((uiop:string-prefix-p "image/" mime)
        (make-instance 'cl-gopher:image
                       :display-string (text node)
                       :selector (quri:uri-path (href node))
                       :hostname *address*
                       :port *port*))
       ((uiop:string-prefix-p "audio/" mime)
        (make-instance 'cl-gopher:sound-file
                       :display-string (text node)
                       :selector (quri:uri-path (href node))
                       :hostname *address*
                       :port *port*))
       ((uiop:string-prefix-p "text/" mime)
        (make-instance 'cl-gopher:text-file
                       :display-string (text node)
                       :selector (quri:uri-path (href node))
                       :hostname *address*
                       :port *port*))
       ((uiop:string-prefix-p "binary/" mime)
        (make-instance 'cl-gopher:binary-file
                       :display-string (text node)
                       :selector (quri:uri-path (href node))
                       :hostname *address*
                       :port *port*))
       (t
        (make-instance 'cl-gopher:unknown
                       :display-string (text node)
                       :hostname *address*
                       :port *port*))))))

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

(defmethod hunchentoot:accept-connections :before ((acceptor gopher-acceptor))
  (let ((hunchentoot:*acceptor* acceptor))
    (hunchentoot:log-message* :info "Accepting gopher connection.")))

(defmethod hunchentoot:process-connection :before ((acceptor gopher-acceptor) socket)
  (let ((hunchentoot:*acceptor* acceptor))
    (hunchentoot:log-message* :info "Starting gopher request processing.")))

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
        (let* ((path (read-path))
               (path (resolve-path path))
               (*address* (hunchentoot:acceptor-address acceptor))
               (*port* (hunchentoot:acceptor-port acceptor))
               (text (path->backend path :gopher)))
          (hunchentoot:log-message* :info "Gopher path: ~a" path)
          (write-sequence text (usocket:socket-stream socket))
          (write-line "." (usocket:socket-stream socket))
          (write-char #\newline (usocket:socket-stream socket))
          (force-output (usocket:socket-stream socket)))
      (error (e)
        (hunchentoot:log-message* :info "Error while processing gopher request: ~a" e)
        (force-output (usocket:socket-stream socket))
        (usocket:socket-close socket)))))
