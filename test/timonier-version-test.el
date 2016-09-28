;;; timonier-version-test.el --- Tests for version information

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

(require 'pkg-info)

(setq current-version "0.2.0")


(ert-deftest test-timonier-library-version ()
  :tags '(version)
  (with-test-sandbox
   (should (string= current-version (timonier--library-version)))))

(ert-deftest test-timonier-version ()
  :tags '(version)
  (with-test-sandbox
   (should (string= current-version (timonier-version)))))


(provide 'timonier-version-test)
;;; timonier-version-test.el ends here
