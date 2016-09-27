;;; timonier-mode.el --- Mode for timonier

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

(require 'widget)
(require 'cl-lib)

(require 'f)
(require 's)


(require 'timonier-custom)
(require 'timonier-k8s)


;; ------------------
;; Customization
;; ------------------

(defgroup timonier-k8s-mode nil
  "Customization group for `timonier-k8s-mode'."
  :prefix "timonier-k8s-mode-"
  :tag "Timonier Kubernetes Mode"
  :group 'timonier)

(defcustom timonier-k8s-mode-buffer "*timonier-k8s*"
  "The Timonier kubernetes buffer name."
  :type 'string
  :group 'timonier-k8s-mode)

;; ------------------
;; Faces
;; ------------------

(defgroup timonier-k8s-mode-faces '((timonier-k8s-mode custom-group))
  "Customization group for the faces of `timonier-k8s-mode'."
  :prefix "timonier-k8s-mode-"
  :tag "Timonier Kubernetes mode faces"
  :group 'timonier-k8s-mode)

(defface timonier-k8s-mode-pod
  '((t :weight bold :inherit font-lock-warning-name-face))
  "Face used in the Timonier buffer."
  :group 'timonier-k8s-mode-faces)

(defface timonier-k8s-mode-service
  '((t :weight bold :inherit font-lock-warning-name-face))
  "Face used in the Timonier buffer."
  :group 'timonier-k8s-mode-faces)

(defface timonier-k8s-mode-node
  '((t :weight bold :inherit font-lock-warning-name-face))
  "Face used in the Timonier buffer."
  :group 'timonier-k8s-mode-faces)

(defface timonier-k8s-mode-namespace
  '((t :inherit font-lock-comment-face))
  "Face used in the Timonier buffer"
  :group 'timonier-k8s-mode-faces)

(defface timonier-k8s-mode-pod-status
  '((t :inherit font-lock-string-face))
  "Face used in the Timonier buffer"
  :group 'timonier-k8s-mode-faces)


(defvar timonier-k8s-mode-padding 2
  "The number of columns used for padding on the left side of the buffer.")


;; ------------------
;; I/O
;; ------------------


(defun timonier-k8s-mode-quit ()
  "Timonier exit."
  (interactive)
  (kill-buffer timonier-k8s-mode-buffer))


;; ------------------
;; Mode
;; ------------------

(defmacro timonier-k8s-mode-with-widget (title &rest body)
  `(progn
     (set-buffer (get-buffer-create timonier-k8s-mode-buffer))
     (switch-to-buffer-other-window timonier-k8s-mode-buffer)
     (kill-all-local-variables)
     (let ((inhibit-read-only t))
       (erase-buffer)
       (remove-overlays)
       (widget-insert (format "\n[%s]\n\n" ,title))
       ,@body)
     (use-local-map widget-keymap)
     (widget-setup)
     (timonier-k8s-mode)
     (widget-minor-mode)
     (goto-char 0)))

(defvar timonier-k8s-mode-hook nil)


(defvar timonier-k8s-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "q") 'timonier-k8s-mode-quit)
    map)
  "Keymap for `timonier-k8s-mode' major mode.")


(define-derived-mode timonier-k8s-mode tabulated-list-mode
  "Timonier Kubernetes mode"
  "Major mode for Timonier."
  :group 'dionysos
  )


(defun timonier--k8s-mode-render-pod (pod)
  "Render a Kubernetes `POD' to the Timonier buffer."
  ;; (message "Pod: %s" pod)
  (let* ((pod-data (timonier--k8s-extract-pod-informations pod)))
    (insert (all-the-icons-octicon "package"))
    (widget-insert
     (format " %s %s"
             (propertize (plist-get pod-data 'name)
                         'face 'timonier-k8s-mode-pod)
             (propertize (plist-get pod-data 'status)
                         'face 'timonier-k8s-mode-pod-status))
     (format  "\n   Namespace: %s\n\n"
             (propertize (plist-get pod-data 'namespace)
                         'face 'timonier-k8s-mode-namespace)))))


(defun timonier--k8s-mode-render-pods (pods)
  "Render Kubernetes `PODS'."
  (widget-insert (format "\n== PODS ==\n\n"))
  (let ((start (point)))
    (dotimes (i (length pods))
      (let ((pod (elt pods i)))
        (timonier--k8s-mode-render-pod pod)
        (put-text-property start (point) :k8s-pod pod)))
    (widget-insert "\n")))


(defun timonier--k8s-mode-render-service (service)
  "Render a Kubernetes `SERVICE' to the Timonier buffer."
  (let* ((service-data (timonier--k8s-extract-service-informations service)))
    (insert (all-the-icons-octicon "link-external"))
    (widget-insert
     (format " %s"
             (propertize (plist-get service-data 'name)
                         'face 'timonier-k8s-mode-service))
     (format "\n  Namespace: %s"
             (propertize (plist-get service-data 'namespace)
                         'face 'timonier-k8s-mode-namespace))
     (format "\n  ClusterIP: %s" (plist-get service-data 'cluster-ip))
     (format "\n  Endpoints: %s"
             (propertize
              (s-join ":" (mapcar (lambda (elt)
                                    (format "%s" elt))
                                  (plist-get service-data 'ports)))))
     (format "\n  Labels: %s\n\n"
             (propertize
              (s-join " " (plist-get service-data 'labels)))))))


(defun timonier--k8s-mode-render-services (services)
  "Render Kubernetes `SERVICES'."
  (widget-insert (format "\n== SERVICES ==\n\n"))
  (let ((start (point)))
    (dotimes (i (length services))
      (let ((service (elt services i)))
        (timonier--k8s-mode-render-service service)
        (put-text-property start (point) :k8s-service service)))
    (widget-insert "\n")))


(defun timonier--k8s-mode-render-node (node)
  "Render a Kubernetes `NODE' to the Timonier buffer."
  (let* ((node-data (timonier--k8s-extract-node-informations node)))
    (insert (all-the-icons-octicon "server"))
    (widget-insert
     (format " %s %s"
             (propertize (plist-get node-data 'name)
                         'face 'timonier-k8s-mode-service)
             (propertize (plist-get node-data 'creation)
                         'face 'timonier-k8s-mode-namespace))
     (format "\n  Labels: %s\n\n"
             (propertize
              (s-join " " (plist-get node-data 'labels)))))))


(defun timonier--k8s-mode-render-nodes (nodes)
  "Render Kubernetes `NODES'."
  (widget-insert (format "\n== NODES ==\n\n"))
  (let ((start (point)))
    (dotimes (i (length nodes))
      (let ((node (elt nodes i)))
        (timonier--k8s-mode-render-node node)
        (put-text-property start (point) :k8s-node node)))
    (widget-insert "\n")))


;; ------------------
;; API
;; ------------------

(defvar timonier-k8s-mode-history nil)



;;;###autoload
(defun timonier-k8s-pods ()
  (interactive
   (list (read-from-minibuffer "Kubernetes Proxy : "
                               (car timonier-k8s-mode-history)
                               nil
                               nil
                               'timonier-k8s-mode-history)))
  (timonier-k8s-mode-with-widget
   (propertize "Kubernetes")
   timonier--k8s-mode-render-pods (timonier--k8s-get-pods)))



;;;###autoload
(defun timonier-k8s ()
  "Display informations about the Kubernetes cluster."
  (interactive)
  (timonier-k8s-mode-with-widget
   "Kubernetes"
   (let ((nodes (timonier--assoc-cdr 'items (timonier--k8s-get-nodes)))
         (services (timonier--assoc-cdr 'items (timonier--k8s-get-services)))
         (pods (timonier--assoc-cdr 'items (timonier--k8s-get-pods))))
     (timonier--k8s-mode-render-nodes nodes)
     (timonier--k8s-mode-render-services services)
     (timonier--k8s-mode-render-pods pods)
     )))





(provide 'timonier-mode)
;;; timonier-mode.el ends here
