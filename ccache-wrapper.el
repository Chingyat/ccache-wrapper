;;; ccache-wrapper.el --- Minor mode for compilation with ccache -*- lexical-binding: t; -*-

;;; Commentary:
;; Usage:
;; Add the following code to `init.el' file.
;; #+begin_src elisp
;; (use-package ccache-wrapper
;;  :demand t
;;  :straight (ccache-wrapper :type git :host github :repo "fuzy112/ccache-wrapper"
;;                            :pre-build "make"
;;                            :files ("ccache-wrapper.so"
;;                                    "ccache-wrapper.el"))
;;  :commands (ccache-wrapper-mode)
;;  :config
;;  (ccache-wrapper-mode +1))
;; #+end_src elisp
;;

;;; Code:
(require 'compile)

(defvar ccache-wrapper-path (locate-library "ccache-wrapper.so"))

(define-minor-mode ccache-wrapper-mode
  "Global minor mode for compilation with `ccache'."
  :global t
  :group 'compilation
  (if ccache-wrapper-mode
      (add-to-list 'compilation-environment (format "LD_PRELOAD=%s" (expand-file-name ccache-wrapper-path)))
    (setq compilation-environment (delete (format "LD_PRELOAD=%s" (expand-file-name ccache-wrapper-path)) compilation-environment))))

(provide 'ccache-wrapper)
;;; ccache-wrapper.el ends here
