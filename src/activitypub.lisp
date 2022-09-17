;;;; html.lisp

(in-package #:tripod)

(defvar +ap+ :ap)

;; TODO: Maybe also modify the MIME hash table to associate JSON with ActivityStreams?
(setf (gethash "application/json" *mime->backend*) +ap+)
(setf (gethash "application/ld+json" *mime->backend*) +ap+)
(setf (gethash "application/ld+json; profile=\"https://www.w3.org/ns/activitystreams\"" *mime->backend*) +ap+)
(setf (gethash +ap+ *backend->mime*) "application/ld+json; profile=\"https://www.w3.org/ns/activitystreams\"")

(defun file->ap (path)
  (let* ((modification-time (file-write-date path))
         (timestring (local-time:format-timestring
                      nil (local-time:universal-to-timestamp modification-time)))
         (title (text (title path)))
         (description (text (first-paragraph path))))
    (make-instance 'nactivitypub:article
                   :name title
                   :summary description
                   :published timestring
                   :updated timestring
                   :to '("https://www.w3.org/ns/activitystreams#Public")
                   :content (plump:serialize
                             (elt (clss:select "body" (path->backend path :html)) 0)
                             nil)
                   :id (let ((json-path (uiop:make-pathname*
                                         :name (pathname-name path)
                                         :type "json"
                                         :defaults (relative-path path))))
                         (quri:render-uri
                          (quri:copy-uri
                           (quri:uri (hunchentoot:request-uri*))
                           :path (namestring json-path)))))))

(defun directory->ap (path)
  (let* ((subdirs (uiop:subdirectories path))
         (files (uiop:directory-files path))
         (modification-time (file-write-date path))
         (timestring (local-time:format-timestring
                      nil (local-time:universal-to-timestamp modification-time)))
         (title (text (title path)))
         (description (text (first-paragraph path))))
    (make-instance 'nactivitypub:collection
                   :name title
                   :summary description
                   :published timestring
                   :updated timestring
                   :to '("https://www.w3.org/ns/activitystreams#Public")
                   :total-items (+ (length subdirs)
                                   (length files))
                   :items (mapcar (lambda (p) (path->backend p :ap))
                                  (append subdirs files))
                   :id (let* ((relative (namestring (relative-path path)))
                              (relative (if (uiop:string-suffix-p relative "/")
                                            (subseq relative 0 (1- (length relative)))
                                            relative)))
                         (quri:render-uri
                          (quri:copy-uri
                           (quri:uri (hunchentoot:request-uri*))
                           :path (concatenate 'string relative ".json")))))))

(defmethod path->backend ((path pathname) (backend (eql +ap+)) &key &allow-other-keys)
  (if (uiop:directory-pathname-p path)
      (directory->ap path)
      (file->ap path)))

;;; Acceptor (using the http(s) one)

(hunchentoot:define-easy-handler
    (serve-ap-content
     :uri (lambda (request)
            (and
             (eq :get (hunchentoot:request-method request))
             (or
              (string-equal "json" (pathname-type (hunchentoot:script-name request)))
              (member
               (hunchentoot:header-in "Accept" request)
               '("application/json"
                 "application/ld+json"
                 "application/ld+json; profile=\"https://www.w3.org/ns/activitystreams\"")
               :test #'string-equal))))
     :default-request-type :get)
    ()
  (hunchentoot:log-message* "Got an ActivityPub request for ~a" (hunchentoot:script-name*))
  (alexandria:when-let* ((path (ignore-errors (resolve-path (hunchentoot:script-name*)))))
    (setf (hunchentoot:content-type*)
          "application/ld+json; profile=\"https://www.w3.org/ns/activitystreams\"")
    (let ((out (hunchentoot:send-headers))
          (content (path->backend path :ap)))
      (write-sequence (flex:string-to-octets content :external-format :utf8) out))
    t))
