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

(defun split-for-terminal (string &optional (width 80))
  "Splits the STRING into a list of strings.
Every string in the resulting list is guaranteed to be under WIDTH in length.
ALWAYS returns a list, even if there's just one string in it."
  (if (< (length string) width)
      (list string)
      (loop with last = (1- (length string))
            for start = 0 then end
            for end = (position #\Space string :start start :end (min last (+ start width)) :from-end t)
            until (>= (+ start width) last)
            collect (subseq string start end) into lines
            finally (return (mapcar (alexandria:curry #'string-trim " ")
                                    (append lines (list (subseq string start))))))))

(defun mkline (class display-string &optional selector)
  (apply #'make-instance
         class
         :display-string display-string
         :hostname *address*
         :port *port*
         (when selector
           (list :selector selector))))

(defmethod tripod->backend ((nodes list) (backend (eql +gopher+)) &key)
  (alexandria:mappend #'(lambda (n) (tripod->backend n +gopher+)) nodes))

(defmethod tripod->backend ((node element) (backend (eql +gopher+)) &key)
  (mapcar (lambda (l) (mkline 'cl-gopher:info-message l))
          (split-for-terminal (text node))))

(defmethod tripod->backend ((node paragraph) (backend (eql +gopher+)) &key)
  (append
   (mapcar (lambda (l) (mkline 'cl-gopher:info-message l))
           (split-for-terminal (text node)))
   (list (mkline 'cl-gopher:info-message ""))))

(defmethod tripod->backend ((node blockquote) (backend (eql +gopher+)) &key)
  (append
   (mapcar (lambda (line)
             (mkline 'cl-gopher:info-message (uiop:strcat "> " line)))
           (split-for-terminal (text node) 77))
   (list (mkline 'cl-gopher:info-message ""))))

(defmethod tripod->backend ((node preformatted) (backend (eql +gopher+)) &key)
  (append
   (list (mkline 'cl-gopher:info-message (uiop:strcat "``` " (alt node))))
   (mapcar (lambda (line)
             (mkline 'cl-gopher:info-message line))
           (butlast (uiop:split-string (text node) :separator '(#\Newline))))
   (list (mkline 'cl-gopher:info-message (uiop:strcat "``` " (alt node)))
         (mkline 'cl-gopher:info-message ""))))

(defmethod tripod->backend ((node heading) (backend (eql +gopher+)) &key)
  (list
   (mkline 'cl-gopher:info-message
           (uiop:strcat (make-string (level node) :initial-element #\#) " " (text node)))
   (mkline 'cl-gopher:info-message "")))

(defmethod tripod->backend ((node items) (backend (eql +gopher+)) &key)
  (append
   (alexandria:mappend
    (lambda (e)
      (let ((terminal-lines (split-for-terminal e 77)))
        (cons (mkline 'cl-gopher:info-message (uiop:strcat "- " (first terminal-lines)))
              (mapcar (lambda (l)
                        (mkline 'cl-gopher:info-message (uiop:strcat "  " l)))
                      (rest terminal-lines)))))
    (elements node))
   (list (mkline 'cl-gopher:info-message ""))))

(defmethod tripod->backend ((node link) (backend (eql +gopher+)) &key)
  (let ((mime (mimes:mime (quri:uri-path (href node))))
        (absolute-url (quri:uri-scheme (href node))))
    (list
     (cond
       ((not absolute-url)
        (mkline 'cl-gopher:submenu (text node) (quri:uri-path (href node))))
       ((and absolute-url
             (member (quri:uri-scheme (href node)) '("http" "https") :test #'string=))
        (mkline 'cl-gopher:html-file (text node) (quri:render-uri (href node))))
       ((uiop:string-prefix-p "image/gif" mime)
        (mkline 'cl-gopher:gif (text node) (quri:uri-path (href node))))
       ((uiop:string-prefix-p "image/png" mime)
        (mkline 'cl-gopher:png (text node) (quri:uri-path (href node))))
       ((uiop:string-prefix-p "image/" mime)
        (mkline 'cl-gopher:image (text node) (quri:uri-path (href node))))
       ((uiop:string-prefix-p "audio/" mime)
        (mkline 'cl-gopher:sound-file (text node) (quri:uri-path (href node))))
       ((uiop:string-prefix-p "text/" mime)
        (mkline 'cl-gopher:text-file (text node) (quri:uri-path (href node))))
       ((uiop:string-prefix-p "binary/" mime)
        (mkline 'cl-gopher:binary-file (text node) (quri:uri-path (href node))))
       (t
        (mkline 'cl-gopher:unknown (text node)))))))

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
