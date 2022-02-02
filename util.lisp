(in-package :gamma)

(defun make-relative-path (absolute-path &key (root-dir "."))
  "removes the current working directory from ABSOLUTE-PATH"
  (uiop:with-current-directory ((pathify root-dir))
      (let ((cwd (namestring (uiop:getcwd))))
        (str:replace-first cwd "" (namestring absolute-path)))))

(defun pathify (str)
  (if (str:ends-with-p "/" str)
      str
      (str:join "" (list str "/"))))

(defun change-extension (path new-ext)
  (let* ((pathstr (namestring path))
         (pos (search "." pathstr :from-end t :test #'string=)))
    (pathname (str:join "." (list (subseq pathstr 0 pos) new-ext)))))

(defun copy-directory (from to)
  (loop :for dir :in (site-dirs from)
        :do (let ((relative-path (merge-pathnames (make-relative-path dir :root-dir from) (pathify to))))
              (ensure-directories-exist relative-path)
              (loop :for file :in (uiop:directory-files dir)
                    :do (uiop:copy-file file (merge-pathnames (make-relative-path file :root-dir from) (pathify to))))))
  (loop :for file :in (uiop:directory-files (pathify from))
        :do (uiop:copy-file file (merge-pathnames (make-relative-path file :root-dir from) (pathify to)))))

(defun site-dirs (root-dir &key ignore)
  "returns a list of all subdirectories under ROOT-DIR. does not return any folder starting with _

IGNORE is a list of strings containing directories that the walker should NOT catalog"
  (let ((dirs (remove-if #'(lambda (x) (or (member (make-relative-path x) (mapcar #'pathify ignore) :test #'string=)
                                           (and (not *check-dirs-with-underscores*)
                                                (str:starts-with-p "_" (namestring (make-relative-path x :root-dir root-dir))))))
                         (uiop:subdirectories root-dir))))
    (alexandria:flatten (list dirs (mapcar #'(lambda (d) (site-dirs d :ignore ignore)) dirs)))))

(defun parse-markdown-vars (str)
  (alexandria:flatten
   (mapcar #'(lambda (l)
               (let ((values (mapcar #'str:trim (str:split ":" (str:trim l) :omit-nulls t))))
                 (list (intern (string-upcase (first values)) :keyword)
                       (second values))))
           (str:lines (str:trim str) :omit-nulls t))))

(defun parse-markdown (file)
  (let* ((file-str (uiop:read-file-string file))
         (vars (first (remove-if #'str:blankp (ppcre:split "---" file-str)))))
    (list* :content (markdown.cl:parse (str:replace-first (format nil "---~A---" vars) "" file-str))
           (when (str:starts-with-p "---" file-str)
             (parse-markdown-vars vars)))))

