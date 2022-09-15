;;;; parse.lisp

(in-package #:tripod)

(defvar *tripod-directory* nil
  "The directory to resolve relative pathnames against.")

(defvar *current-backend* nil
  "The backend that currently processing the request.")

(defvar *current-path* nil
  "The path Tripod currently processes for serving.")

(defun tripod-directory ()
  (or *tripod-directory* (uiop:getcwd)))

(defun relative-path (pathname &optional (relative-to (tripod-directory)))
  (let ((relative-to (uiop:directory-exists-p relative-to))
        (pathname (if (uiop:directory-pathname-p pathname)
                             (uiop:directory-exists-p pathname)
                             (uiop:file-exists-p pathname))))
    (when (and relative-to (zerop (search (namestring relative-to) (namestring pathname))))
      (pathname (subseq (namestring pathname) (length (namestring relative-to)))))))

(defvar *mime->backend* (make-hash-table :test #'equal)
  "The map from backend-specific MIME type to the backend that processes it.")

(defvar *backend->mime* (make-hash-table :test #'equal)
  "A reverse `*mime->backend*'.")

(defun path-backend (path)
  (when (uiop:file-pathname-p path)
    (or (gethash (mimes:mime path) *mime->backend*) :plain)))

(defgeneric backend->tripod (object backend &key &allow-other-keys)
  (:method ((object t) (backend t) &key &allow-other-keys)
    "Simply return nil for the convenience of all the other methods."
    nil)
  (:method :around ((object t) (backend t) &key &allow-other-keys)
    (let* ((elements (call-next-method))
           (non-elem (find-if-not (lambda (e) (or (typep e 'element) (typep e 'items))) elements)))
      (when non-elem
        (error "Some of the elements are not Tripod-native elements: ~a" non-elem))
      elements))
  (:documentation "Transforms the backend-specific object into a list of Tripod nodes.
Returns a flat list of `elem's.

BACKEND should be eql-specified, like (eql +gemini+), where +gemini+
is some eql keyword variable."))

(defun backend->tripod* (object &optional (backend *current-backend*))
  "Transforms the backend-specific object into a list of Tripod nodes.
The backend to use, if not provided, is inferred based on the
`*current-backend*'."
  (backend->tripod object backend))

(defgeneric tripod->backend (nodes backend &key &allow-other-keys)
  (:documentation "Transform the list of Tripod NODES to the specified BACKEND-specific format.

BACKEND should be eql-specified, like (eql +gemini+), where +gemini+
is some eql keyword variable."))

(defun tripod->backend* (nodes &optional (backend *current-backend*))
  "Transform the list of Tripod NODES to the specified BACKEND-specific format.
The backend to use, if not provided, is inferred based on the
`*current-backend*'."
  (tripod->backend (uiop:ensure-list nodes) backend))

(defgeneric file->tripod (file backend &key &allow-other-keys)
  (:method :around ((file t) (backend t) &key &allow-other-keys)
    (let ((*current-path* file))
      (call-next-method)))
  (:method ((file t) (backend t) &key &allow-other-keys)
    nil)
  (:method ((file t) (backend null) &key &allow-other-keys)
    (file->tripod file (path-backend file)))
  (:method ((file string) (backend t) &key &allow-other-keys)
    (file->tripod (uiop:parse-native-namestring file) backend))
  (:documentation "Get the contents of a file as a list of tripod nodes.

BACKEND should be eql-specified, like (eql +gemini+), where +gemini+
is some eql keyword variable.

If BACKEND is passed in as nil, it's being guessed automatically.

The only thing left for backends to define is how the data is fetched
based on the backend."))

(defun file->tripod* (file &optional (backend *current-backend*))
  "Get the contents of a file as a list of tripod nodes.
The backend to use, if not provided, is inferred based on the
`*current-backend*'."
  (file->tripod file backend))

(defgeneric directory->tripod (directory backend &key &allow-other-keys)
  (:method :around ((directory t) (backend t) &key &allow-other-keys)
    (let ((*current-path* directory))
      (call-next-method)))
  (:method ((directory t) (backend t) &key &allow-other-keys)
    nil)
  (:method ((directory string) (backend t) &key &allow-other-keys)
    (directory->tripod (uiop:parse-native-namestring directory) backend))
  (:method ((directory pathname) (backend t) &key &allow-other-keys)
    (flet ((directory-name (dir)
             (alexandria:lastcar
              (remove-if #'uiop:emptyp
                         (uiop:split-string
                          (directory-namestring dir)
                          :separator "/")))))
      (append
       (list (make-instance 'heading
                            :level 1
                            :text (format nil "~:(~a~)" (or (directory-name directory) "root"))))
       (if (or (uiop:subdirectories directory)
               (uiop:directory-files directory))
           (append
            (loop for dir in (sort (uiop:subdirectories directory)
                                   #'< :key #'uiop:safe-file-write-date)
                  collect (make-instance 'link
                                         :href (quri:uri (uiop:strcat (directory-name dir) "/"))
                                         :text (directory-name dir)))
            (loop for file in (sort (uiop:directory-files directory)
                                    #'< :key #'uiop:safe-file-write-date)
                  collect (make-instance 'link
                                         :href (quri:uri (pathname-name file))
                                         :text (or (ignore-errors
                                                    (text (find-if (lambda (n) (and (eq (type-of n) 'heading)
                                                                                    (= 1 (level n))))
                                                                   (file->tripod file nil))))
                                                   (pathname-name file)))))
           (list (make-instance 'paragraph :text "This directory is empty..."))))))
  (:documentation "Get the contents of a directory as a list of tripod nodes."))

(defun directory->tripod* (directory &optional (backend *current-backend*))
  "Get the contents of the DIRECTORY as a list of tripod nodes.
The backend to use, if not provided, is inferred based on the
`*current-backend*'."
  (directory->tripod directory backend))

(defun path->tripod* (path &optional (backend *current-backend*))
  "Get the contents of the PATH (file or directory) as a list of tripod nodes.
The backend to use, if not provided, is inferred based on the
`*current-backend*'."
  (if (uiop:directory-pathname-p path)
      (directory->tripod path backend)
      (file->tripod path backend)))

(defgeneric path->backend (path backend &key &allow-other-keys)
  (:method ((path pathname) backend &key &allow-other-keys)
    (tripod->backend (path->tripod* path (path-backend path)) backend))
  (:documentation "Transforms the PATH to the the BACKEND-friendly format.

The default action is the most sensible (it transforms the file to
Tripod and the to the BACKEND-specific format), you should only
redefine it if you need to skip the Tripod transformation step."))

(defun path->backend* (path &optional (backend *current-backend*))
  (let ((*current-path* path))
    (if (eq (path-backend path) backend)
        (uiop:read-file-string path)
        (path->backend path backend))))

(defmethod resolve-path ((file pathname) &optional (current-path (tripod-directory)))
  (let* ((current-path (if (uiop:absolute-pathname-p current-path)
                           current-path
                           (uiop:merge-pathnames* current-path (tripod-directory))))
         (name (cond
                 ((uiop:directory-pathname-p file)
                  nil)
                 ((uiop:emptyp (pathname-name file))
                  "index")
                 (t (pathname-name file))))
         (file (if name
                   (find-if
                    (lambda (f)
                      (and (string-equal (pathname-name f) name)
                           (if (pathname-type file)
                               (string-equal (pathname-type f)
                                             (pathname-type file))
                               t)))
                    (uiop:directory-files
                     (uiop:merge-pathnames*
                      (uiop:pathname-directory-pathname file)
                      current-path)))
                   ;; Directory
                   (when (uiop:directory-exists-p (uiop:merge-pathnames* file current-path))
                     (uiop:merge-pathnames* file current-path)))))
    file))

(defmethod resolve-path ((path string) &optional (current-path (tripod-directory)))
  (declare (ignorable current-path))
  (if (member path '("/" "" "index") :test #'string-equal)
      (resolve-path (pathname "index"))
      (let ((clean-path
              (if (char= #\/ (elt path 0))
                  (subseq path 1)
                  path)))
        (or (resolve-path (pathname clean-path) current-path)
            (resolve-path (pathname (uiop:strcat clean-path "/")) current-path)))))

(defmethod title ((nodes list))
  (find-if (lambda (n) (and (eq (type-of n) 'heading)
                            (= 1 (level n))))
           nodes))

(defmethod title ((path pathname))
  (title (path->tripod* path (path-backend path))))

(defmethod first-paragraph ((nodes list))
  (find-if (lambda (node)
             (and (typep node 'paragraph)
                  (not (uiop:emptyp (text node)))))
           nodes))

(defmethod first-paragraph ((path pathname))
  (first-paragraph (path->tripod* path (path-backend path))))
