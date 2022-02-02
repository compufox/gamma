;;;; gamma.lisp

(in-package #:gamma)

(defvar *root-dir* "."
  "default folder to look for generating files")
(defvar *config* "site.conf"
  "default config name")
(defvar *template-dir* "_templates"
  "default directory for templates")
(defvar *output-dir* "_site"
  "default output directory")
(defvar *templates* (make-hash-table :test 'equal)
  "hash table containing all compiled templates keyed off of filename")
(defvar *default-template* (cl-template:compile-template "<html><body><%= @ content %></body></html>")
  "a basic HTML template")
(defvar *static-dir* nil
  "folder that holds static assets - set via config file")

(opts:define-opts
    (:name :help
     :description "prints this help text"
     :short #\h
     :long "help")
    (:name :config-file
     :description "config file FILE to load (defaults to site.conf)"
     :short #\c
     :long "config"
     :meta-var "FILE"
     :arg-parser #'identity)
    (:name :version
     :description "prints the application version"
     :long "version")
    (:name :output-dir
     :description "directory DIR to output html into (defaults to _site)"
     :short #\o
     :long "out"
     :arg-parser #'identity
     :meta-var "DIR")
  (:name :root-dir         
     :description "root directory DIR for the site"
     :short #\r
     :long "root"
     :arg-parser #'identity
     :meta-var "DIR"))

(defun walk-directory-and-generate-html (path)
  (uiop:with-current-directory ((pathify path))
    (conf:load-config *config*)

    (setf *static-dir* (conf:config :static-dir)
          *output-dir* (pathify *output-dir*))
  
    ;; loads and compiles all of our tempaltes
    (when (or (conf:config :template-dir) (uiop:directory-exists-p *template-dir*))
      (loop :for f :in (uiop:directory-files (conf:config :template-dir *template-dir*))
            :do (setf (gethash (pathname-name f) *templates*)
                      (cl-template:compile-template (str:from-file f)))))

    ;; create our output directory
    (ensure-directories-exist *output-dir*)

    (let* ((dirs (list* "." (site-dirs "." :ignore (remove-if #'null
                                                              (list* *output-dir*
                                                                     *static-dir*
                                                                     (conf:config :template-dir *template-dir*)
                                                                     (conf:config :ignore-dirs))))))
           (files (alexandria:flatten (loop :for dir :in dirs
                                            :do (ensure-directories-exist (merge-pathnames (make-relative-path dir)
                                                                                           *output-dir*))
                                            :collect (uiop:directory-files dir "*.md")))))
      (loop :for file :in files
            :do (let* ((parsed-output (parse-markdown file))
                       (output-path (merge-pathnames (change-extension (make-relative-path file) "html")
                                                     *output-dir*))
                       (templ-func (gethash (getf parsed-output :template) *templates* *default-template*)))
                  (str:to-file output-path (funcall templ-func parsed-output)
                               :if-exists :overwrite))))

    ;; now that our site files are created, lets copy over our static
    ;;  assets if they exist
    (when (and *static-dir* (uiop:directory-exists-p *static-dir*))
      (copy-directory *static-dir* *output-dir*))))

(defun main ()
  (multiple-value-bind (options) (opts:get-opts)
    (when (getf options :help)
      (opts:describe :prefix "generates static HTML from markdown files"
                     :usage-of "gamma")
      (opts:exit 0))

    (when (getf options :version)
      (format t "gamma v~A~&" #.(asdf:component-version (asdf:find-system :gamma)))
      (opts:exit 0))

    (when (getf options :config-file)
      (setf *config* (getf options :config-file)))

    (when (getf options :root-dir)
      (setf *root-dir* (getf options :root-dir)))
    
    (when (getf options :output-dir)
      (setf *output-dir* (getf options :output-dir))))
  
  (walk-directory-and-generate-html *root-dir*))

