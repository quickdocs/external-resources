(defpackage #:external-resources
  (:nicknames #:external-resources/main)
  (:use #:cl)
  (:import-from #:external-resources/cliki
                #:generate-cliki-topics)
  (:import-from #:dexador)
  (:import-from #:yason)
  (:export #:main))
(in-package #:external-resources)

(defparameter *info-json-url*
  "https://storage.googleapis.com/quickdocs-dist/quicklisp/info.json")

(defun latest-version-info-url ()
  (let* ((body (dex:get *info-json-url*))
         (json (yason:parse body)))
    (gethash "latest_version_info_url" json)))

(defun latest-dist-info ()
  (let ((url (latest-version-info-url)))
    (yason:parse (dex:get url))))

(defun release-names (dist-info)
  (let* ((url (gethash "provided_releases_url" dist-info))
         (body (dex:get url))
         (json (yason:parse body)))
    (loop for key being the hash-keys of json
          for count from 1
          collect key into release-names
          finally (return (make-array count
                                      :element-type 'string
                                      :initial-contents release-names)))))

(defvar *output-dir*
  (asdf:system-relative-pathname :external-resources #P"output/"))

(defun today ()
  (multiple-value-bind (sec min hour date month year)
      (decode-universal-time (get-universal-time) 0)
    (declare (ignore sec min hour))
    (format nil "~4,'0D-~2,'0D-~2,'0D" year month date)))

(defun now ()
  (multiple-value-bind (sec min hour date month year)
      (decode-universal-time (get-universal-time) 0)
    (format nil "~4,'0D-~2,'0D-~2,'0DT:~2,'0D:~2,'0D:~2,'0DZ"
            year month date hour min sec)))

(defun write-index-json (index-file source version)
  (ensure-directories-exist index-file)
  (uiop:with-output-file (out index-file
                              :if-exists :supersede
                              :if-does-not-exist :create)
    (format out "~&{
  \"source\": \"~A\",
  \"version\": \"~A\",
  \"latest_url\": ~:*~:*\"https://storage.googleapis.com/quickdocs-resources/~A/~A.json\",
  \"generated_at\": \"~A\"
}~%" source version (now))))

(defun cliki (version releases)
  (let* ((output-dir (merge-pathnames "cliki/" *output-dir*))
         (file
           (merge-pathnames (format nil "~A.json" version) output-dir))
         (index-file (merge-pathnames "index.json" output-dir)))

    ;; output/cliki/{date}.json
    (format t "~&Writing to '~A'.~2%" file)
    (ensure-directories-exist file)
    (uiop:with-output-file (out file
                                :if-exists :supersede
                                :if-does-not-exist :create)
      (generate-cliki-topics out releases))

    ;; output/cliki/index.json
    (write-index-json index-file "cliki" version)
    file))

(defun main ()
  (let ((releases (release-names (latest-dist-info)))
        (version (today)))
    (cliki version releases)))
