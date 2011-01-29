;; All the customizations have been split into multiple files
;; They are located in ~/.emacs.d/lisp/
;; All the external lisp code is in ~/.emacs.d/lisp

;; Load paths
(let* ((my-lisp-dir "~/.emacs.d/")
       (default-directory my-lisp-dir)
       (orig-load-path load-path))
  (setq load-path (cons my-lisp-dir nil))
  (normal-top-level-add-subdirs-to-load-path)
  (nconc load-path orig-load-path))

;; Personal customizations
(require 'elisp-load-dir)
(elisp-load-dir "~/.emacs.d/lisp-personal")
