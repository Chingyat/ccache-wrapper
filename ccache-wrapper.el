;;; ccache-wrapper.el --- Minor mode for compilation with ccache  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Fu Zhengyi

;; Author: Fu Zhengyi <tsingyat@outlook.com>
;; Keywords: convenience, c

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Usage:
;; Add the following code to `init.el' file.
;; #+begin_src elisp
;; (use-package ccache-wrapper
;;  :demand t
;;  :straight (ccache-wrapper :type git :host github :repo "fuzy112/ccache-wrapper"
;;                            :files ("ccache-wrapper.c"
;;                                    "ccache-wrapper.el"))
;;  :commands (ccache-wrapper-mode)
;;  :config
;;  (ccache-wrapper-mode +1))
;; #+end_src
;;

;;; Code:

(require 'compile)

(defvar ccache-wrapper-debug nil)

(defun ccache-wrapper--file-time (file)
  (let* ((attr (file-attributes file))
         (time (file-attribute-modification-time attr))
         (integer-time (time-convert time 'integer)))
    integer-time))

(defun ccache-wrapper--file-newer-p (file other)
  (> (ccache-wrapper--file-time file)
     (ccache-wrapper--file-time other)))

(defun ccache-wrapper-path ()
  (let ((default-directory (file-name-directory (locate-library "ccache-wrapper.el"))))
    (unless (and (file-exists-p "ccache-wrapper-lib.so")
                 (ccache-wrapper--file-newer-p "ccache-wrapper-lib.so" (file-truename "ccache-wrapper.c")))
      (shell-command "cc -shared -fPIC -O2 -o ccache-wrapper-lib.so ccache-wrapper.c -ldl")
      (message "Compiled ccache-wrapper-lib.so"))
    (expand-file-name "ccache-wrapper-lib.so")))

(defun ccache-wrapper-environment ()
  (cons (format "LD_PRELOAD=%s" (ccache-wrapper-path))
        (if ccache-wrapper-debug
            (list "CCACHE_WRAPPER_DEBUG=1")
          nil)))

(define-minor-mode ccache-wrapper-mode
  "Minor mode for compilation with `ccache'."
  :lighter " ccache"
  :global nil
  :group 'compilation)


(define-minor-mode global-ccache-wrapper-mode
  "Global minor mode for compilation with `ccache'."
  :global t
  :group 'compilation)


(defun ccache-wrapper--compilation-start (&rest args)
  (let ((process-environment (append (if global-ccache-wrapper-mode
                                         (ccache-wrapper-environment))
                                     process-environment)))
    (apply args)))

(advice-add 'compilation-start :around #'ccache-wrapper--compilation-start)

(defun ccache-wrapper--compilation-setup (&rest _)
  (if global-ccache-wrapper-mode
      (ccache-wrapper-mode +1)))

(defun ccache-wrapper--compilation-unsetup (&rest _)
  (ccache-wrapper-mode -1))

(advice-add 'compilation-setup :after #'ccache-wrapper--compilation-setup)
(advice-add 'compilation--unsetup :before #'ccache-wrapper--compilation-unsetup)

(provide 'ccache-wrapper)
;;; ccache-wrapper.el ends here
