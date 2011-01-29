;; Org mode

(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(add-hook 'org-mode-hook 'turn-on-font-lock)  ; Org buffers only
(add-hook 'org-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode-hook
          (lambda ()
            (flyspell-mode 1)))

;; requite org-latex so that the following variables are defined
(require 'org-latex)

;; tell org to use listings
(setq org-export-latex-listings t)

;; you must include the listings package
(add-to-list 'org-export-latex-packages-alist '("" "listings"))

;; if you want colored source code then you need to include the color package
(add-to-list 'org-export-latex-packages-alist '("" "color"))

;-------------------------------------------------------------------------
;; inline images
;;-------------------------------------------------------------------------
(require 'iimage)
(add-to-list 'iimage-mode-image-regex-alist
             (cons (concat "\\[\\[file:\\(~?" iimage-mode-image-filename-regex
                           "\\)\\]") 1))
;;(define-key org-mode-map [(control c) ?i] 'iimage-mode) ; C-c i for image
;;(add-hook 'org-mode-hook (lambda ()
;; (local-set-key "\M-I" 'org-toggle-iimage-in-org)))


(defun org-toggle-iimage-in-org ()
  "display images in your org file"
  (interactive)
  (if (face-underline-p 'org-link)
      (set-face-underline-p 'org-link nil)
    (set-face-underline-p 'org-link t))
  (iimage-mode))

(define-key org-mode-map [(control c) ?i] 'org-toggle-iimage-in-org)


;; allow for export=>beamer by placing
;; #+LaTeX_CLASS: beamer in org files

(unless (boundp 'org-export-latex-classes)
  (setq org-export-latex-classes nil))
;; (add-to-list 'org-export-latex-classes
;;   ;; beamer class, for presentations
;;   '("beamer"
;;      "\\documentclass[11pt]{beamer}\n
;;       \\mode<{{{beamermode}}}>\n
;;       \\usetheme{{{{beamertheme}}}}\n
;;       \\usecolortheme{{{{beamercolortheme}}}}\n
;;       \\beamertemplateballitem\n
;;       \\setbeameroption{show notes}
;;       \\usepackage[utf8]{inputenc}\n
;;       \\usepackage[T1]{fontenc}\n
;;       \\usepackage{hyperref}\n
;;       \\usepackage{color}
;;       \\usepackage{listings}
;;       \\lstset{numbers=none,language=[ISO]C++,tabsize=4,
;;   frame=single,
;;   basicstyle=\\small,
;;   showspaces=false,showstringspaces=false,
;;   showtabs=false,
;;   keywordstyle=\\color{blue}\\bfseries,
;;   commentstyle=\\color{red},
;;   }\n
;;       \\usepackage{verbatim}\n
;;       \\institute{{{{beamerinstitute}}}}\n          
;;        \\subject{{{{beamersubject}}}}\n"

;;      ("\\section{%s}" . "\\section*{%s}")
     
;;      ("\\begin{frame}[fragile]\\frametitle{%s}"
;;        "\\end{frame}"
;;        "\\begin{frame}[fragile]\\frametitle{%s}"
;;        "\\end{frame}")))

  ;; letter class, for formal letters

(add-to-list 'org-export-latex-classes
  '("letter"
     "\\documentclass[11pt]{letter}\n
      \\usepackage[utf8]{inputenc}\n
      \\usepackage[T1]{fontenc}\n
      \\usepackage{color}"
     
     ("\\section{%s}" . "\\section*{%s}")
     ("\\subsection{%s}" . "\\subsection*{%s}")
     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
     ("\\paragraph{%s}" . "\\paragraph*{%s}")
     ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))


(add-to-list 'org-export-latex-classes
  '("tkkthesis"
    "\\documentclass[12pt,a4paper,oneside]{report}\n"
    ("\\chapter{%s}" . "\\chapter*{%s}")
    ("\\section{%s}" . "\\section*{%s}")
    ("\\subsection{%s}" . "\\subsection*{%s}")
    ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))

;; Mail related
;;(add-hook 'mail-mode-hook 'turn-on-orgtbl)
(add-hook 'mail-mode-hook 'turn-on-orgstruct)

(global-set-key (kbd "C-c f") 'org-footnote-action)

;; Put README files in orgmode automatically
;; (add-to-list 'auto-mode-alist '("README$" . org-mode))

;; Use ido for completion
(setq org-completion-use-ido t)

;; Ditaa
(setq org-ditaa-jar-path "~/.emacs.d/contrib/ditaa.jar")

;; reftex
(defun org-mode-reftex-setup ()
  (load-library "reftex")
  (define-key org-mode-map (kbd "C-c )") 'reftex-citation))

;; (add-hook 'org-mode-hook
;;           (lambda ()
;;             (message "hello123 %s" org-export-latex-options-plist)))
;; (and (buffer-file-name)
;;      (file-exists-p (buffer-file-name))
;;      (reftex-parse-all))

(require 'org-publish)
(setq org-publish-project-alist
      '(
        ("org-notes"
         :base-directory "~/Documents/Bally/gamecore/"
         :base-extension "org"
         :publishing-directory "~/public_html/"
         :recursive t
         :publishing-function org-publish-org-to-html
         :headline-levels 4             ; Just the default for this project.
         :auto-preamble t
         )
        
        ("org-static"
         :base-directory "~/Documents/Bally/gamecore/"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
         :publishing-directory "~/public_html/"
         :recursive t
         :publishing-function org-publish-attachment
         )

        ("org" :components ("org-notes" "org-static"))
        
        )
      )
(provide 'sk-orgmode)
