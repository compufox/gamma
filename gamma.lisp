;;;; gamma.lisp

(in-package #:gamma)

(defvar *config* "site.conf"
  "default config name")
(defvar *output-dir* "_site"
  "default output directory")
(defvar *templates* (make-hash-table :test 'equal)
  "hash table containing all compiled templates keyed off of filename")
(defvar *default-template* (cl-template:compile-template "<html><body>(@ content)</body></html>")
  "a basic HTML template")
(defvar *static-dir* nil
  "folder that holds static assets - set via config file")

(opts:define-opts
    (:name :help
     :description "prints this help text"
     :short #\h
     :long "help")
    (:name :config-file
     :description "config file to load (defaults to site.conf)"
     :short #\c
     :long "config"
     :arg-parse #'identity)
    (:name :version
     :description "prints the application version"
     :long "version")
    (:name :output-dir
     :description "DIRECTORY to output html into (defaults to _site)"
     :short #\o
     :long "out"
     :arg-parse #'identity
     :meta-var "DIRECTORY"))

(defun main ()
  (multiple-value-bind (options free-args) (opts:get-opts)
    (when (getf options :help)
      (opts:describe :prefix "generates static HTML from markdown files"
                     :usage-of "gamma")
      (opts:exit 0))

    (when (getf options :version)
      (format t "gamma v~A~&" #.(asdf:component-version (asdf:find-system :gamma))))

    (when (getf options :config)
      (setf *config* (getf options :config)))
    
    (when (getf options :output-dir)
      (setf *output-dir* (getf options :output-dir))))
  
  (load-config *config*)

  (setf *static-dir* (conf:config :static-dir)
        *output-dir* (pathify *output-dir*))
  
  ;; loads and compiles all of our tempaltes
  (when (config :template-dir)
    (loop :for f :in (uiop:directory-files (config :template-dir))
          :do (setf (gethash (pathname-name f) *templates*)
                    (cl-template:compile-template (str:from-file f)))))

  ;; create our output directory
  (ensure-directories-exist *output-dir*)

  (let* ((dirs (list* "." (site-dirs "." :ignore (list* *output-dir*
                                                        *static-dir*
                                                        (conf:config :template-dir)
                                                        (conf:config :ignore-dirs)))))
         (files (loop :for dir :in dirs
                      :do (ensure-directories-exist (merge-pathnames (make-relative-path dir)
                                                                     *output-dir*))
                      :collect (uiop:directory-files dir "*.md"))))
    
    (loop :for file :in files
          :do (let* ((parsed-output (parse-markdown file))
                     (output-path (merge-pathnames (change-extension (make-relative-path file) "html")
                                                   *output-dir*))
                     (templ-func (gethash (getf parsed-output :template) *templates* *default-template*)))
                (str:to-file output-path (funcall templ-func parsed-output)
                             :if-exists :overwrite))))

  ;; now that our static files are created, lets copy over our static
  ;;  assets if they exist
  (when (and *static-dir* (uiop:directory-exists-p *static-dir*))
    (copy-directory *static-dir* *output-dir*)))

