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

(defun start-https (https-port certificate-file key-file)
  (if (and certificate-file key-file)
      (progn
        (format t "Starting HTTPS handler on port ~d~%" https-port)
        (hunchentoot:start (make-instance 'https-acceptor
                                          :port https-port
                                          :ssl-privatekey-file key-file
                                          :ssl-certificate-file certificate-file)))
      (error "Both cert file and key file are required for Tripod HTTPS handler")))

(defun start-gemini (gemini-port certificate-file key-file)
  (if (and certificate-file key-file)
      (progn
        (format t "Starting Gemini handler on port ~d~%" gemini-port)
        (hunchentoot:start (make-instance 'gemini-acceptor
                                          :port gemini-port
                                          :ssl-privatekey-file key-file
                                          :ssl-certificate-file certificate-file)))
      (error "Both cert file and key file are required for Tripod Gemini handler")))

(defun entry-point ()
  (let* ((help-p (get-cli-arg-or-env
                  :arg "-h" :long-arg "--help"))
         (tripod-directory (get-cli-arg-or-env
                            :arg "-d" :long-arg "--dir"
                            :env "TRIPOD_DIR"))
         (gopher-port (ignore-errors
                       (parse-integer
                        (get-cli-arg-or-env
                         :arg "-p" :long-arg "--gopher"
                         :env "TRIPOD_GOPHER_PORT")
                        :junk-allowed nil)))
         (gemini-port (ignore-errors
                       (parse-integer
                        (get-cli-arg-or-env
                         :arg "-m" :long-arg "--gemini"
                         :env "TRIPOD_GEMINI_PORT")
                        :junk-allowed nil)))
         (http-port (ignore-errors
                     (parse-integer
                      (get-cli-arg-or-env
                       :arg "-t" :long-arg "--http"
                       :env "TRIPOD_HTTP_PORT")
                      :junk-allowed nil)))
         (https-port (ignore-errors
                      (parse-integer
                       (get-cli-arg-or-env
                        :arg "-s" :long-arg "--https"
                        :env "TRIPOD_HTTPS_PORT")
                       :junk-allowed nil)))
         (certificate-file (get-cli-arg-or-env
                            :arg "-c" :long-arg "--cert"))
         (key-file (get-cli-arg-or-env
                    :arg "-k" :long-arg "--key")))
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
    (when (and gopher-port (numberp gopher-port) (not (zerop gopher-port)))
      (format t "Starting Gopher handler on port ~d~%" gopher-port)
      (hunchentoot:start (make-instance 'gopher-acceptor :port gopher-port)))
    (when (and http-port (numberp http-port) (not (zerop http-port)))
      (format t "Starting HTTP handler on port ~d~%" http-port)
      (hunchentoot:start (make-instance 'http-acceptor :port http-port)))
    (when (and https-port (numberp https-port) (not (zerop https-port)))
      (start-https https-port certificate-file key-file))
    (when (and gemini-port (numberp gemini-port) (not (zerop gemini-port)))
      (start-gemini gemini-port certificate-file key-file))
    (loop)))
