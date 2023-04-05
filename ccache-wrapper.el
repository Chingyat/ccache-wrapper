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
;; #+end_src elisp
;;

;;; Code:

(require 'compile)

(defvar ccache-wrapper-debug nil)

(defvar ccache-wrapper-path
  (let ((default-directory (file-name-directory (locate-library "ccache-wrapper.el"))))
    (unless (and (file-exists-p "ccache-wrapper-lib.so")
                 (f-newer-p "ccache-wrapper-lib.so" (file-truename "ccache-wrapper.c")))
      (shell-command "cc -shared -fPIC -O2 -o ccache-wrapper-lib.so ccache-wrapper.c")
      (message "Compiled ccache-wrapper-lib.so"))
    (expand-file-name "ccache-wrapper-lib.so")))

(defconst ccache-wrapper-compilation-modes '(compilation-mode comint-mode))

(defun ccache-wrapper-environment ()
  (cons (format "LD_PRELOAD=%s" (expand-file-name ccache-wrapper-path))
        (if ccache-wrapper-debug
            (list "CCACHE_WRAPPER_DEBUG=1")
          nil)))

(define-minor-mode ccache-wrapper-mode
  "Minor mode for compilation with `ccache'."
  :lighter " ccache"
  :global nil
  :group 'comilation
  (if ccache-wrapper-mode
      (setq-default compilation-environment (append (ccache-wrapper-environment)
                                                    compilation-environment))
    (kill-local-variable 'compilation-environment)))


(define-minor-mode global-ccache-wrapper-mode
  "Global minor mode for compilation with `ccache'."
  :global t
  :group 'compilation
  (if global-ccache-wrapper-mode
      (dolist (mode ccache-wrapper-compilation-modes)
        (add-hook (intern (format "%s-hook" mode)) #'ccache-wrapper-mode))
    (dolist (mode ccache-wrapper-compilation-modes)
      (remove-hook (intern (format "%s-hook" mode)) #'ccache-wrapper-mode))))

(provide 'ccache-wrapper)
;;; ccache-wrapper.el ends here
