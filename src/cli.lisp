(in-package #:tripod)

(defun get-cli-arg-or-env (&key arg long-arg env default)
  (let ((args (uiop:command-line-arguments)))
    (or (second (member arg args :test #'string=))
        (not (not (member arg args :test #'string=)))
        (when long-arg
          (or (second (member long-arg args :test #'string=))
              (not (not (member long-arg args :test #'string=)))))
        (when env
          (uiop:getenv env))
        default)))

(defun entry-point ()
  (let* ((help-p (get-cli-arg-or-env
                  :arg "-h" :long-arg "--help"))
         (address (get-cli-arg-or-env
                   :arg "-a" :long-arg "--addr"))
         (tripod-directory (get-cli-arg-or-env
                            :arg "-d" :long-arg "--dir"
                            :env "TRIPOD_DIR"))
         (log-file (get-cli-arg-or-env
                    :arg "-l" :long-arg "--log"
                    :env "TRIPOD_LOG_FILE"))
         (gemini-port (parse-integer
                       (or (get-cli-arg-or-env
                            :arg "-m" :long-arg "--gemini"
                            :env "TRIPOD_GEMINI_PORT")
                           "")
                       :junk-allowed t))
         (gopher-port (parse-integer
                       (or (get-cli-arg-or-env
                            :arg "-p" :long-arg "--gopher"
                            :env "TRIPOD_GOPHER_PORT")
                           "")
                       :junk-allowed t))
         (http-port (parse-integer
                     (or (get-cli-arg-or-env
                          :arg "-t" :long-arg "--http"
                          :env "TRIPOD_HTTP_PORT")
                         "")
                     :junk-allowed t))
         (https-port (parse-integer
                      (or (get-cli-arg-or-env
                           :arg "-s" :long-arg "--https"
                           :env "TRIPOD_HTTPS_PORT")
                          "")
                      :junk-allowed t))
         (certificate-file (get-cli-arg-or-env
                            :arg "-c" :long-arg "--cert"))
         (key-file (get-cli-arg-or-env
                    :arg "-k" :long-arg "--key"))
         (acceptors nil))
    (when log-file
      (uiop:ensure-all-directories-exist (list (uiop:pathname-directory-pathname log-file))))
    (when help-p
      (format t "Tripod is a minimalist/absolutist blog engine.

It allows you to host HTTP(S)/HTML, Gemini/Gemtext, and Gopher
websites (with possibly more to come) from directories of
plaintext (.gmi/.html, potentially .gopher/.txt/.md/.org) files.

Styling, dynamic applications, form processing, and anything
else... is not included, for purpose. The only thing that Tripod does
is hosting your precious plaintext files in the most accessible
selection of protocols/formats.

Usage:
./tripod [-h/--help]
         [-d/--dir DIR]
         [-p/--gopher PORT] [-m/--gemini PORT] [-t/--http PORT] [-s/--https PORT]
         [-c/--cert FILE] [-k/--key FILE]

Flag~13tArg~20tEnvironment~40tDescription
--help/-h~40tShow this help message.
--dir/-d~13tDIR~20tTRIPOD_DIR~40tDirectory with the website content.
--address/-a~13tIP~40tThe IP address of the server website is hosted on.
--gopher/-p~13tPORT~20tTRIPOD_GOPHER_PORT~40tThe port to host Gopher content on.
--gemini/-m~13tPORT~20tTRIPOD_GEMINI_PORT~40tThe port for Gemini content.
--http/-t~13tPORT~20tTRIPOD_HTTP_PORT~40tThe HTTP port.
--https/-s~13tPORT~20tTRIPOD_HTTPS_PORT~40tThe HTTPS port.
--cert/-c~13tFILE~40tThe location of the SSL certificate file.
--key/-k~13tFILE~40tThe location of the SSL key file.

Special files:

- template.html: An HTML template. Specific to HTML backend, as HTML
   is much more content-full than Gemini/Gopher and thus needs a bit
   more styling, fonting, etc. SHOULD have a <head> and <body>, MAY
   have a <header> and <footer> (if you use footer.XXX and
   header.XXX), the rest is up to you.
- footer.XXX: Footer contents. Only the first file found is used for all ports.
- header.XXX: Header contents. Only the first file found is used for all ports.~%")
      (uiop:quit 0))
    (when tripod-directory
      (setf *tripod-directory* (uiop:parse-native-namestring tripod-directory)))
    (when gopher-port
      (format t "Starting Gopher handler on port ~d~%" gopher-port)
      (push (hunchentoot:start (make-instance 'gopher-acceptor
                                              :port gopher-port
                                              :address address
                                              :message-log-destination (or log-file *standard-output*)
                                              :access-log-destination (or log-file *standard-output*)))
            acceptors))
    (when http-port
      (format t "Starting HTTP handler on port ~d~%" http-port)
      (push (hunchentoot:start (make-instance 'http-acceptor
                                              :port http-port
                                              :address address
                                              :message-log-destination (or log-file *standard-output*)
                                              :access-log-destination (or log-file *standard-output*)))
            acceptors))
    (when (and https-port key-file certificate-file)
      (push (hunchentoot:start (make-instance 'https-acceptor
                                              :address address
                                              :port https-port
                                              :ssl-privatekey-file key-file
                                              :ssl-certificate-file certificate-file
                                              :message-log-destination (or log-file *standard-output*)
                                              :access-log-destination (or log-file *standard-output*)))
            acceptors))
    (when (and gemini-port key-file certificate-file)
      (push (hunchentoot:start (make-instance 'gemini-acceptor
                                              :address address
                                              :port gemini-port
                                              :ssl-privatekey-file key-file
                                              :ssl-certificate-file certificate-file
                                              :message-log-destination (or log-file *standard-output*)
                                              :access-log-destination (or log-file *standard-output*)))
            acceptors))
    (handler-case (bt:join-thread (find-if (lambda (th)
                                             (search "hunchentoot" (bt:thread-name th)))
                                           (bt:all-threads)))
      (#+sbcl sb-sys:interactive-interrupt
       #+ccl  ccl:interrupt-signal-condition
       #+clisp system::simple-interrupt-condition
       #+ecl ext:interactive-interrupt
       #+allegro excl:interrupt-signal
       ()
        (format *error-output* "Aborting.~&")
        (mapcar #'hunchentoot:stop acceptors)
        (uiop:quit))
      (error (c)
        (when (find-restart 'continue)
          (invoke-restart 'continue))
        (format t "An unknown error occured:~&~a~&" c)
        (uiop:print-backtrace :condition c)))))
