(defpackage #:external-resources/cliki
  (:use #:cl)
  (:import-from #:dexador)
  (:import-from #:quri)
  (:import-from #:lquery)
  (:export #:generate-cliki-topics))
(in-package #:external-resources/cliki)

(defparameter *info-json-url*
  "https://storage.googleapis.com/quickdocs-dist/quicklisp/info.json")

(defvar *excluded-words*)

(defun cliki.net (page)
  (format nil "https://cliki.net/~A"
          (quri:url-encode page)))

(defun cliki-topics-under (topic)
  (let ((body (dex:get (cliki.net topic))))
    (coerce
      (lquery:$ (initialize body)
        "#article li a:first-child.internal"
        (text))
      'list)))

(defun excluded-words ()
  (if (boundp '*excluded-words*)
      *excluded-words*
      (setf *excluded-words*
            (append
              (cliki-topics-under "license")
              (cliki-topics-under "person")
              (list "public domain"
                    "Debian" "Debian package" "Quicklisp" "clbuild"
                    "Linux" "Unix" "FreeBSD" "Darwin" "MacOS X" "Cygwin" "Windows" "NetBSD" "Solaris"
                    "RFC" "free" "Free Software" "ANSI" "LISP" "Library")))))

(defun excluded-word-p (word)
  (check-type word string)
  (and (member word (excluded-words) :test #'string-equal)
       t))

(defun cliki-html (name)
  (let ((url (cliki.net (if (string= name "cl+ssl")
                            "cl-plus-ssl"
                            name))))
    (handler-case (dex:get url)
      (dex:http-request-not-found () nil))))

(defun cliki-topics-of (name)
  (let ((html (cliki-html name)))
    (when html
      (remove-duplicates
        (map 'vector
             #'string-downcase
             (remove-if
               (lambda (word)
                 (or (excluded-word-p word)
                     (string-equal word name)
                     (= 0 (length word))))
               (lquery:$ (initialize html)
                 "#content #article .category" (text))))
        :test #'string=))))

(defun generate-cliki-topics (out release-names &key (interval 5))
  (let ((all-count (length release-names)))
    (format out "~&{~%")
    (loop for i from 1
          for name across release-names
          do
          (format t "~&[~D / ~D] ~A..." i all-count name)
          (let ((topics (cliki-topics-of name)))
            (cond
              ((null topics)
               (format t " not found"))
              (t
               (format t " done")
               (when (/= 0 (length topics))
                 (format out "~A~&  ~S: [~{~S~^, ~}]"
                         (if (= i 1) "" ",")
                         name
                         (coerce topics 'list))
                 (force-output out)))))
          (fresh-line)
          (sleep interval))
    (format out "~&}~%")
    (force-output out))
  (values))
