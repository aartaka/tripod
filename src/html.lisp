;;;; html.lisp

(in-package #:tripod)

(defvar +html+ :html)

(setf (gethash "text/html" *mime->backend*) :html
      (gethash :html *backend->mime*)       "text/html")

;;; Backend to Tripod conversion.

(defun process-text (text)
  (when text
    (let ((whitespace '(#\Space #\Tab #\Linefeed #\Return #\Newline #\Page #\Vt)))
      (string-right-trim
       whitespace
       (apply #'concatenate 'string
              (mapcar (lambda (line)
                        (concatenate 'string
                                     (string-left-trim whitespace line) " "))
                      (uiop:split-string text :separator '(#\newline))))))))

(defun processed-text (element)
  (process-text (plump:text element)))

(defmethod label-of ((element plump:element))
  (let ((alt (process-text (plump:get-attribute element "alt")))
        (labels
         (when (plump:get-attribute element "name")
           (clss:select
            (format nil "label[name=\"~a\"], figure > figcaption"
                    (plump:get-attribute element "name"))
            ;; Try to get higher in the hierarchy first, as a precaution.
            (or (ignore-errors (plump:parent (plump:parent element)))
                (plump:parent element))))))
    (or alt
        (and labels
             (not (uiop:emptyp labels))
             (processed-text (elt labels 0))))))

(defmethod backend->tripod ((element plump:element) (backend (eql +html+)) &key)
  (let ((tag (plump:tag-name element)))
    (cond
      ((string-equal tag "p")
       (list (make-instance 'paragraph :text (processed-text element))))
      ((string-equal tag "br")
       (list (make-instance 'paragraph :text "")))
      ((member tag '("img" "video" "audio") :test #'string-equal)
       (let ((src (plump:get-attribute element "src")))
         (when (and src (not (uiop:emptyp (process-text src))))
           (list (make-instance 'link :href src :text (label-of element))))))
      ((string-equal tag "a")
       (let ((href (plump:get-attribute element "href")))
         (when (and href (not (uiop:emptyp (process-text href))))
           (list (make-instance 'link
                                :text (processed-text element)
                                :href href)))))
      ((member tag '("h1" "h2" "h3") :test #'string-equal)
       (list (make-instance 'heading
                            :text (processed-text element)
                            :level (parse-integer (plump:tag-name element) :start 1))))
      ((string-equal tag "q")
       (list (make-instance 'blockquote :text (processed-text element))))
      ((string-equal tag "pre")
       (list (make-instance 'preformatted :alt (label-of element) :text (processed-text element))))
      ((member tag '("h4" "h5" "h6") :test #'string-equal)
       (warn "Tripod does not support heading levels beyond 3."))
      (t nil))))

(defmethod backend->tripod ((object plump:root) (backend (eql +html+)) &key)
  (alexandria:mappend (lambda (e) (backend->tripod e +html+))
                      (coerce (clss:select "*" object) 'list)))

;;; Tripod to Backend conversion.

(defmethod tripod->backend ((nodes list) (backend (eql +html+)) &key)
  (flet ((read-of-create (template parent element)
           (if template
               (elt (clss:select element parent) 0)
               (plump:make-element parent element)))
         (make-with-text (parent tag text)
           (plump:make-text-node (plump:make-element parent tag) text))
         (make-meta (head name content)
           (let ((meta (plump:make-element head "meta")))
             (plump:set-attribute meta "name" name)
             (plump:set-attribute meta "content" content))))
    (let* ((template (ignore-errors (plump:parse (resolve-path "template.html"))))
           (root (or template (plump:make-root)))
           (head (read-of-create template root "head"))
           (body (read-of-create template root "body"))
           (header (read-of-create template body "header"))
           (header-contents (ignore-errors (plump:parse (resolve-path "header.html"))))
           (main (read-of-create template body "main"))
           (footer (read-of-create template body "footer"))
           (footer-contents (ignore-errors (plump:parse (resolve-path "footer.html"))))
           (first-paragraph (first-paragraph nodes))
           (title (title nodes)))
      (when title
        (plump:make-text-node (plump:make-element head "title") (text title)))
      (make-meta head "viewport" "width=device-width, initial-scale=1.0")
      (when *current-path*
        (let ((date-string
                (local-time:format-timestring
                 nil (local-time:universal-to-timestamp (file-write-date *current-path*)))))
          (make-meta head "dcterms.modified" date-string)
          (make-meta head "dcterms.available" date-string)
          (make-meta head "dcterms.issued" date-string)))
      (when first-paragraph
        (make-meta head "description" (text first-paragraph)))
      (when header-contents
        (loop for header-elem across (plump:children header-contents)
              do (plump:append-child header header-elem)))
      (dolist (node nodes)
        ;; FIXME: Generate elements in separate `tripod->backend' methods?
        (typecase node
          (paragraph (if (uiop:emptyp (text node))
                         (plump:make-element main "br")
                         (make-with-text main "p" (text node))))
          (blockquote (make-with-text main "q" (text node)))
          (heading (let ((heading (plump:make-element
                                   main (case (level node) (1 "h1") (2 "h2") (3 "h3")))))
                     (plump:make-text-node heading (text node))))
          (items (let ((ul (plump:make-element main "ul")))
                   (dolist (element (elements node))
                     (make-with-text ul "li" element))))
          (link (let ((a (plump:make-element main "a")))
                  (plump:make-text-node a (text node))
                  (plump:set-attribute a "href" (quri:render-uri (href node)))))
          (preformatted (make-with-text main "pre" (text node)))))
      (when footer-contents
        (loop for footer-elem across (plump:children footer-contents)
              do (plump:append-child footer footer-elem)))
      (plump:serialize root nil))))

;;; File to Tripod reading

(defmethod file->tripod ((file pathname) (backend (eql +html+)) &key)
  (ignore-errors (backend->tripod (plump:parse file) +html+)))

;;; Acceptor

(defclass http-acceptor (hunchentoot:easy-acceptor)
  ()
  (:default-initargs
   :address "127.0.0.1"
   :port 80))

(defclass https-acceptor (hunchentoot:easy-ssl-acceptor)
  ()
  (:default-initargs
   :address "127.0.0.1"
   :port 443))

(defun respond-regular-page (script-name)
  (alexandria:when-let* ((path (ignore-errors (resolve-path script-name))))
    (setf (hunchentoot:content-type*) "text/html;charset=utf8")
    (let ((out (hunchentoot:send-headers))
          (content (path->backend* path :html)))
      (write-sequence (flex:string-to-octets content :external-format :utf8) out))
    t))

(defun respond-404-page ()
  (setf (hunchentoot:content-type*) "text/html;charset=utf8"
        (hunchentoot:return-code*) hunchentoot:+http-not-found+)
  (let* ((out (hunchentoot:send-headers))
         (404-path (resolve-path "404"))
         (content (if 404-path
                      (path->backend* 404-path :html)
                      (tripod->backend
                       (list (make-instance 'heading :level 1 :text "Sorry, there's no such page")
                             (make-instance
                              'paragraph
                              :text "It seems the page you're looking for is not there.
Please, check the address again."))
                       :html))))
    (write-sequence (flex:string-to-octets content :external-format :utf8) out)))

(hunchentoot:define-easy-handler
    (process-files
     :uri (lambda (request)
            (uiop:emptyp (pathname-type (hunchentoot:script-name request))))
     :default-request-type :get)
    ()
  (or
   (respond-regular-page (hunchentoot:script-name hunchentoot:*request*))
   (respond-404-page)))
