;; test-helper.el --- Test helpers for Timonier

;; Copyright (C) 2016 Nicolas Lamirault <nicolas.lamirault@gmail.com>

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;;; Commentary:

;;; Code:

(require 'ansi)
(require 'cl) ;; http://emacs.stackexchange.com/questions/2864/symbols-function-definition-is-void-cl-macroexpand-all-when-trying-to-instal
(require 'ert)
(require 'f)
(require 'undercover)

(setq debugger-batch-max-lines (+ 50 max-lisp-eval-depth)
      debug-on-error t)

(defvar timonier--username (getenv "HOME"))

(defconst timonier-testsuite-dir
  (f-parent (f-this-file))
  "The testsuite directory.")

(defconst timonier-source-dir
  (f-parent timonier-testsuite-dir)
  "The timonier.el source directory.")

(defconst timonier-sandbox-path
  (f-expand "sandbox" timonier-testsuite-dir)
  "The sandbox path for timonier.")

(defun cleanup-load-path ()
  "Remove home directory from 'load-path."
  (message (ansi-green "[timonier] Cleanup path"))
  (mapc #'(lambda (path)
            (when (string-match (s-concat timonier--username "/.emacs.d") path)
              (message (ansi-yellow "Suppression path %s" path))
              (setq load-path (delete path load-path))))
        load-path)
  (add-to-list 'load-path timonier-source-dir))

(defun load-unit-tests (path)
  "Load all unit test from PATH."
  (message (ansi-green "[timonier] Execute unit tests %s"
                       path))
  (dolist (test-file (or argv (directory-files path t "-test.el$")))
    (load test-file nil t)))


(defun load-library (file)
  "Load current library from FILE."
  (let ((path (s-concat timonier-source-dir file)))
    (message (ansi-yellow "[timonier] Load library from %s" path))
    (undercover "*.el" (:exclude "*-test.el"))
    (require 'timonier path)))


(defmacro with-test-sandbox (&rest body)
  "Evaluate BODY in an empty sandbox directory."
  `(unwind-protect
       (condition-case nil ;ex
           (let ((default-directory timonier-source-dir))
             (cleanup-load-path)
             ;; (message "Load path: %s" load-path)
             (load-library "/timonier.el")
             ,@body))))


;;; test-helper.el ends here
