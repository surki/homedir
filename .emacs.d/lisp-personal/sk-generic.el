(setq inhibit-splash-screen t)

;; (if (>= emacs-major-version 23)
;;     (set-default-font "Monospace-10.5"))

;; make all the frames have default font
;; (add-to-list 'default-frame-alist '(font . "Monospace-10.5"))

;; theme
(require 'color-theme)
(color-theme-initialize)
;; (require 'zenburn)
;; (color-theme-zenburn)
(color-theme-arjen)

;; Save desktop on exit
(desktop-save-mode 1)
(setq desktop-load-locked-desktop 't)

;; "y or n" instead of "yes or no"
(fset 'yes-or-no-p 'y-or-n-p)

;; Manual entry
(global-set-key [f1] 'manual-entry)

;; indentation
(setq-default indent-tabs-mode nil);
(setq default-tab-width 4);

;; default coding style
(setq c-default-style
      '((java-mode . "java") (awk-mode . "awk") (other . "stroustrup")))

;; Display line and column numbers
(setq line-number-mode    t)
(setq column-number-mode  t)

;; Enable narrowing
(put 'narrow-to-defun 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; show parenthesis
(show-paren-mode 1)

;;open read only files in view mode
(setq view-read-only t)

;; Disable menu
(tool-bar-mode -1)

;; Scrollbar right side
(set-scroll-bar-mode 'right)

;; colored terminal
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; customize output goes here
(setq custom-file "~/.emacs.d/lisp-personal/sk-custom.el")

(setq semanticdb-default-save-directory "~/.semantic")
'(semanticdb-persistent-path nil)

;; Browse the URLs using conkeror
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "conkeror")

;; ediff
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

;; backup directory
(setq
 backup-by-copying t      ; don't clobber symlinks
 backup-directory-alist
 '(("." . "~/.emacs.d/.saves"))    ; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)       ; use versioned backups

;; c-mode
(add-hook 'c-mode-common-hook
          (function (lambda ()
                      (define-key c-mode-base-map [f5] 'surki-compile)
                      (define-key c-mode-base-map [f6] 'next-error)
                      (define-key c-mode-base-map [f7] 'previous-error))))

(require 'windmove)
(windmove-default-keybindings 'meta)

(global-set-key (kbd "ESC <left>") 'windmove-left)          ; move to left windnow
(global-set-key (kbd "ESC <right>") 'windmove-right)        ; move to right window
(global-set-key (kbd "ESC <up>") 'windmove-up)              ; move to upper window
(global-set-key (kbd "ESC <down>") 'windmove-down)         

;; newline automatically indents
(global-set-key (kbd "RET") 'reindent-then-newline-and-indent)

(setq-default save-place t)
(require 'saveplace)

(require 'midnight)
(setq-default midnight-mode t)

;; --------------------------------------------------------------------------------

;; gpg
(require 'epa)
(epa-file-enable)

;; --------------------------------------------------------------------------------

;; If emacs is run in a terminal, the clipboard- functions have no
;; effect. Instead, we use of xsel.
;; http://hugoheden.wordpress.com/2009/03/08/copypaste-with-emacs-in-terminal/
(unless window-system

  ;; Callback for when user cuts
  (defun xsel-cut-function (text &optional push)
    ;; Insert text to temp-buffer, and "send" content to xsel stdin
    (with-temp-buffer
      (insert text)
      (call-process-region (point-min) (point-max) "xsel" nil 0 nil "--clipboard" "--input")))

  ;; Call back for when user pastes
  (defun xsel-paste-function()
    ;; Find out what is current selection by xsel. If it is different
    ;; from the top of the kill-ring (car kill-ring), then return
    ;; it. Else, nil is returned, so whatever is in the top of the
    ;; kill-ring will be used.
    (let ((xsel-output (shell-command-to-string "xsel --clipboard --output")))
      (unless (string= (car kill-ring) xsel-output)
        xsel-output )))

  (setq interprogram-cut-function 'xsel-cut-function)
  (setq interprogram-paste-function 'xsel-paste-function))


;; --------------------------------------------------------------------------------

;; Support for marking a rectangle of text with highlighting.
(setq cua-enable-cua-keys nil)
(cua-mode t)
;; TODO remove rect-mark.el
;; (require 'rect-mark)
;; (define-key ctl-x-map "r\C-@" 'rm-set-mark)
;; (define-key ctl-x-map [?r ?\C-\ ] 'rm-set-mark)
;; (define-key global-map [S-down-mouse-1] 'rm-mouse-drag-region)

;; (add-hook 'picture-mode-hook 'rm-example-picture-mode-bindings)

;; (global-set-key
;;  (kbd "C-w")
;;  '(lambda(b e) (interactive "r")
;;     (if rm-mark-active
;;         (rm-kill-region b e)
;;       (clipboard-kill-region b e))))
;; (global-set-key
;;  (kbd "M-w")
;;  '(lambda(b e) (interactive "r")
;;     (if rm-mark-active
;;         (rm-kill-ring-save b e) (clipboard-kill-ring-save b e))))
;; (global-set-key
;;  (kbd "C-x C-x")
;;  '(lambda(&optional p) (interactive "p")
;;     (if rm-mark-active
;;         (rm-exchange-point-and-mark p) (exchange-point-and-mark p))))

;; --------------------------------------------------------------------------------

;; Org mode

(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(add-hook 'org-mode-hook 'turn-on-font-lock)  ; Org buffers only

;; --------------------------------------------------------------------------------

;; mutt
(defun surkis-mail-mode-hook ()
  (turn-on-auto-fill) ;;; Auto-Fill is necessary for mails
  (turn-on-font-lock) ;;; Font-Lock is always cool *g*
  (flush-lines "^\\(> \n\\)*> -- \n\\(\n?> .*\\)*") ;;; Kills quoted sigs.
  (not-modified) ;;; We haven't changed the buffer, haven't we? *g*
  (mail-text) ;;; Jumps to the beginning of the mail text
  (setq make-backup-files nil) ;;; No backups necessary.
)
(or (assoc "mutt-" auto-mode-alist)
    (setq auto-mode-alist (cons '("mutt-" . mail-mode) auto-mode-alist)))
(add-hook 'mail-mode-hook 'surkis-mail-mode-hook)

;; --------------------------------------------------------------------------------

;; ido mode

(ido-mode 1)
(setq ;; Use it for many file dialogs
 ido-everywhere t
 ;; Don’t be case sensitive
 ido-case-fold t
 ;; If the file at point exists, use that
 ido-use-filename-at-point t
 ;; Or if it is an URL…
 ido-use-url-at-point t
 ;; Even if TAB completes uniquely,
 ;; still wait for RET
 ido-confirm-unique-completion t
 ;; If the input does not exist,
 ;; don’t look in unexpected places.
 ;; I probably want a new file.
 ido-auto-merge-work-directories-length -1)
(setq ido-enable-flex-matching t)
(add-hook 'ido-setup-hook
          (lambda ()
            (define-key ido-completion-map
              (kbd "C-w")
              'ido-delete-backward-updir)))

;; --------------------------------------------------------------------------------

;; Translation services
(autoload 'babel "babel"
  "Use a web translation service to translate the message MSG." t)
(autoload 'babel-region "babel"
  "Use a web translation service to translate the current region." t)
(autoload 'babel-as-string "babel"
  "Use a web translation service to translate MSG, returning a string." t)
(autoload 'babel-buffer "babel"
  "Use a web translation service to translate the current buffer." t)

;; We want the translated temporary buffer to appear in the current window
(add-to-list 'same-window-buffer-names "*babel*")

;; --------------------------------------------------------------------------------

;; gtags stuff
;; http://www.mail-archive.com/help-gnu-emacs@gnu.org/msg04410.html

;; (when (locate-library "gtags")
;;   (autoload 'gtags-mode "gtags" nil t)

;;   (when (executable-find "global")

;;     (defadvice gtags-visit-rootdir (after make-complete-list activate)
;;       "Rebuilds completion list when changing GLOBAL database rootdir."
;;       (gtags-make-complete-list))

;;     (defun gtags-global-dir-p (dir) 
;;       "Return non-nil if directory DIR contains a GLOBAL database."  
;;       (and (file-exists-p (expand-file-name "GPATH" dir))
;;            (file-exists-p (expand-file-name "GRTAGS" dir))
;;            (file-exists-p (expand-file-name "GSYMS" dir))
;;            (file-exists-p (expand-file-name "GTAGS" dir))))

;;     (defun gtags-global-dir (&optional dir)
;;       "Return the nearest super directory that contains a GLOBAL database."
;;       (interactive) 
;;       (when (null dir) 
;;         (setq dir default-directory))
;;       (cond ((gtags-global-dir-p dir) dir)
;;             ((equal (file-truename dir) 
;;                     (file-truename "/")) nil) 
;;             (t (gtags-global-dir 
;;                 (file-name-as-directory
;;                  (expand-file-name ".."  dir))))))

;;     (defvar gtags-global-complete-list-obsolete-flag nil
;;       "When non-nil, the GLOBAL complete list should be rebuilt.")

;;     (defun gtags-global-update () 
;;       "If current directory is part of a GLOBAL database update it." 
;;       (interactive) 
;;       (when (gtags-global-dir)
;;         (if (equal (call-process "global" nil nil nil "-vu") 0)
;;             (setq gtags-global-complete-list-obsolete-flag t)
;;           (error "global database update failed"))))

;;     (defun gtags-global-complete-list-maybe ()
;;       "Rebuild the GLOBAL complete list when indicated.  See
;; 'gtags-global-complete-list-obsolete-flag'."
;;       (interactive)
;;       (when gtags-global-complete-list-obsolete-flag
;;         (gtags-make-complete-list) 
;;         (setq gtags-global-complete-list-obsolete-flag nil)))

;;     (add-hook 'gtags-mode-hook 
;;               (lambda () 
;;                 (add-hook 'after-save-hook 'gtags-global-update nil t) 

;;                 (defadvice gtags-find-tag
;;                   (before gtags-global-complete-list-maybe activate)
;;                   (gtags-global-complete-list-maybe))

;;                 (defadvice gtags-find-rtag 
;;                   (before gtags-global-complete-list-maybe activate)
;;                   (gtags-global-complete-list-maybe))

;;                 (defadvice gtags-find-symbol 
;;                   (before gtags-global-complete-list-maybe activate)
;;                   (gtags-global-complete-list-maybe))

;;                 (defadvice gtags-find-pattern
;;                   (before gtags-global-complete-list-maybe activate)
;;                   (gtags-global-complete-list-maybe))

;;                 (defadvice gtags-find-with-grep 
;;                   (before gtags-global-complete-list-maybe activate)
;;                   (gtags-global-complete-list-maybe))

;;                 (defadvice gtags-find-with-idutils
;;                   (before gtags-global-complete-list-maybe activate)
;;                   (gtags-global-complete-list-maybe))

;;                 (defadvice gtags-find-file
;;                   (before gtags-global-complete-list-maybe activate)
;;                   (gtags-global-complete-list-maybe))

;;                 (defadvice gtags-parse-file
;;                   (before gtags-global-complete-list-maybe activate)
;;                   (gtags-global-complete-list-maybe))

;;                 (defadvice gtags-find-tag-from-here 
;;                   (before gtags-global-complete-list-maybe activate)
;;                   (gtags-global-complete-list-maybe))
;;                 )  ; (lambda () ...)
;;               )  ; (add-hook 'gtags-mode-hook ...)
;;     )  ; (when (executable-find "global") ...)

;;   ;; Use gtags in all modes for now.  
;;   (gtags-mode 1)
;;   ) ; (when (locate-library "gtags") ...)

;; (defun surki-gtags-update ()
;;   "create the gnu global tag file"
;;   (interactive)
;;   (if (= 0 (call-process "global" nil nil nil " -p")) ; tagfile doesn't exist?
;;     (shell-command "global -u && echo 'updated tagfile'")))


(defun surki-gtags-update ()
  "create the gnu global tag file"
  (interactive)
  (if (= 0 (call-process "global" nil nil nil " -p")) ; tagfile doesn't exist?
    (start-process "gtags" "*Messages*" "global" "-u")))

;; (defun surki-gtags-global-update ()
;;   "If current directory is part of a GLOBAL database update it."
;;   (interactive) 
;;   (when (surki-gtags-global-dir)
;;     (if (equal (call-process "global" nil nil nil "-vu") 0)
;;         (setq gtags-global-complete-list-obsolete-flag t)
;;       (error "global database update failed"))))

;; (defun surki-gtags-global-dir-p (dir)
;;   "Return non-nil if directory DIR contains a GLOBAL database."
;;   (and (file-exists-p (expand-file-name "GPATH" dir))
;;        (file-exists-p (expand-file-name "GRTAGS" dir))
;;        (file-exists-p (expand-file-name "GSYMS" dir))
;;        (file-exists-p (expand-file-name "GTAGS" dir))))

;; (defun surki-gtags-global-dir (&optional dir)
;;   "Return the nearest super directory that contains a GLOBAL database."
;;   (interactive) 
;;   (when (null dir) 
;;     (setq dir default-directory))
;;   (cond ((surki-gtags-global-dir-p dir) dir)
;;         ((equal (file-truename dir) 
;;                 (file-truename "/")) nil) 
;;         (t (surki-gtags-global-dir 
;;             (file-name-as-directory
;;              (expand-file-name ".."  dir))))))

(defun surki-gtags-next-gtag ()
  "Find next matching tag, for GTAGS."
  (interactive)
  (let ((latest-gtags-buffer
         (car (delq nil  (mapcar (lambda (x) (and (string-match "GTAGS SELECT" (buffer-name x)) (buffer-name x)) )
                                 (buffer-list)) ))))
    (cond (latest-gtags-buffer
           (switch-to-buffer latest-gtags-buffer)
           (next-line)
           (gtags-select-it nil))
          ) ))
 
(add-hook 'gtags-mode-hook 
  (lambda()
    (local-set-key (kbd "M-.") 'gtags-find-tag)   ; find a tag, also M-.
    (local-set-key (kbd "M-,") 'gtags-find-rtag)  ; reverse tag
    (local-set-key (kbd "C-M-,") 'gtags-find-pattern)  ; reverse tag
    (local-set-key "C-M-;" 'surki-gtags-next-gtag)   ;; M-; cycles to next result, after doing M-. C-M-. or C-M-,
    (local-set-key "\M-." 'gtags-find-tag) ;; M-. finds tag
    ))

(add-hook 'gtags-mode-hook 
          (lambda () 
            (add-hook 'after-save-hook 'surki-gtags-update nil t)))

(add-hook 'c-mode-common-hook
  (lambda ()
    (require 'gtags)
    (gtags-mode t)))

(add-hook 'asm-mode-hook
  (lambda ()
    (require 'gtags)
    (gtags-mode t)))

;; --------------------------------------------------------------------------------

;; (load "escreen")
;; (escreen-install)

;; in screen, some of the keys need to be translated
;; (when (null window-system)
;;   (when (string-match "\\`screen" (getenv "TERM"))
;;     (mapc (lambda (x)
;;             (define-key function-key-map (car x) (cdr x)))
;;           (nconc
;;            ;; rxvt-unicode
;;            '(("\eOa" . [C-up])
;;              ("\eOb" . [C-down])
;;              ("\eOc" . [C-right])
;;              ("\eOd" . [C-left])
;;              ("\e[a" . [S-up])
;;              ("\e[b" . [S-down])
;;              ("\e[c" . [S-right])
;;              ("\e[d" . [S-left])
;;              ("\e[Z" . [S-iso-lefttab]))))))
;;This is mainly for the urxvt here, and a bit for the vt510.
;; (mapc (lambda (map)
;;         (define-key function-key-map
;;           (read-kbd-macro (cadr map))
;;           (read-kbd-macro (car map))))
;;       '(("<S-tab>"     "M-[ Z")
;;         ("<S-up>"      "M-[ a")
;;         ("<S-down>"    "M-[ b")
;;         ("<S-right>"   "M-[ c")
;;         ("<S-left>"    "M-[ d")
;;         ("<M-up>"      "ESC M-O A")
;;         ("<M-down>"    "ESC M-O B")
;;         ("<M-right>"   "ESC M-O C")
;;         ("<M-left>"    "ESC M-O D")
;;         ("<C-up>"      "M-O a")
;;         ("<C-down>"    "M-O b")
;;         ("<C-right>"   "M-O c")
;;         ("<C-left>"    "M-O d")
;;         ("<C-M-up>"    "ESC M-O a")
;;         ("<C-M-down>"  "ESC M-O b")
;;         ("<C-M-right>" "ESC M-O c")
;;         ("<C-M-left>"  "ESC M-O d")
;;         ("<M-S-up>"    "ESC M-[ a")
;;         ("<M-S-down>"  "ESC M-[ b")
;;         ("<M-S-right>" "ESC M-[ c")
;;         ("<M-S-left>"  "ESC M-[ d")
;;         ))
;; (mapc (lambda (map)
;;         (define-key function-key-map
;;           (read-kbd-macro (cadr map))
;;           (make-string 1 (decode-char 'ucs (car map)))))
;;       '((197 "C-x 8 a A")
;;         (229 "C-x 8 a a")
;;         ))

;; start the server
;; (server-start)

;; (require 'jabber-autoloads)
;; (setq jabber-roster-show-bindings nil)
;; (setq jabber-account-list 
;;       '(("sureshkumar.pp@gmail.com"
;;          (:network-server . "talk.google.com")
;;          (:connection-type . ssl))))


(load "elscreen" "ElScreen" )
(load "elscreen-gf" "ElScreen-GF" t)

;; F9 creates a new elscreen, shift-F9 kills it
(global-set-key (kbd "<f9>"    ) 'elscreen-create)
(global-set-key (kbd "S-<f9>"  ) 'elscreen-kill)  
;; Windowskey+PgUP/PgDown switches between elscreens
(global-set-key (kbd "<s-prior>") 'elscreen-previous) 
(global-set-key (kbd "<s-next>")  'elscreen-next) 

;; --------------------------------------------------------------------------------

;; http://emacs-fu.blogspot.com/2009/11/showing-pop-ups.html

(defun surki-popup (title msg &optional icon sound)
  "Show a popup if we're on X, or echo it otherwise; TITLE is the title
of the message, MSG is the context. Optionally, you can provide an ICON and
a sound to be played"

  (interactive)
  (when sound (shell-command
                (concat "mplayer -really-quiet " sound " 2> /dev/null")))
  (shell-command (concat "notify-send "
                         (if icon (concat "-i " icon) "")
                         " '" title "' '" msg "'"))
  )

;; --------------------------------------------------------------------------------

(add-to-list 'load-path "~/src/slime")
(require 'slime)
(add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
(add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))
;; Optionally, specify the lisp program you are using. Default is "lisp"
(setq inferior-lisp-program "/usr/local/bin/sbcl") 

;; --------------------------------------------------------------------------------

;; Prefer GIT over other VCs

(defun swap-elements ( the-list a b)
  (rotatef (car (member a the-list))
          (car (member b the-list)))) 

(setq vc-handled-backends '(Git RCS CVS SVN SCCS Bzr Hg Mtn Arch))

(autoload 'magit-status "magit" nil t)

(setq ido-use-filename-at-point nil)

;; --------------------------------------------------------------------------------

(require 'autopair)
(autopair-global-mode 1)
(setq autopair-autowrap t)
;; Use electric-pair-mode which is part of emacs 24
;; (electric-pair-mode)

;; --------------------------------------------------------------------------------

;;; integrate ido with artist-mode
(defun artist-ido-select-operation (type)
  "Use ido to select a drawing operation in artist-mode"
  (interactive (list (ido-completing-read "Drawing operation: " 
                                          (list "Pen" "Pen Line" "line" "straight line" "rectangle" 
                                                "square" "poly-line" "straight poly-line" "ellipse" 
                                                "circle" "text see-thru" "text-overwrite" "spray-can" 
                                                "erase char" "erase rectangle" "vaporize line" "vaporize lines" 
                                                "cut rectangle" "cut square" "copy rectangle" "copy square" 
                                                "paste" "flood-fill"))))
  (artist-select-operation type))

(defun artist-ido-select-settings (type)
  "Use ido to select a setting to change in artist-mode"
  (interactive (list (ido-completing-read "Setting: " 
                                          (list "Set Fill" "Set Line" "Set Erase" "Spray-size" "Spray-chars" 
                                                "Rubber-banding" "Trimming" "Borders"))))
  (if (equal type "Spray-size") 
      (artist-select-operation "spray set size")
    (call-interactively (artist-fc-get-fn-from-symbol 
                         (cdr (assoc type '(("Set Fill" . set-fill)
                                            ("Set Line" . set-line)
                                            ("Set Erase" . set-erase)
                                            ("Rubber-banding" . rubber-band)
                                            ("Trimming" . trimming)
                                            ("Borders" . borders)
                                            ("Spray-chars" . spray-chars))))))))

(add-hook 'artist-mode-init-hook 
          (lambda ()
            (define-key artist-mode-map (kbd "C-c C-a C-o") 'artist-ido-select-operation)
            (define-key artist-mode-map (kbd "C-c C-a C-c") 'artist-ido-select-settings)))

;; --------------------------------------------------------------------------------

;; nhtml stuff
;; http://www.emacswiki.org/cgi-bin/wiki/NxmlModeForXHTML
;; (when (string-match "\\.\\(x?html\\|php[34]?\\)$"
;;                     (file-name-sans-versions (buffer-file-name)))
;;   (my-xhtml-extras))

;; (defun my-xhtml-extras ()
;;     (make-local-variable 'outline-regexp)
;;     (setq outline-regexp "\\s *<\\([h][1-6]\\|html\\|body\\|head\\)\\b")
;;     (make-local-variable 'outline-level)
;;     (setq outline-level 'my-xhtml-outline-level)
;;     (outline-minor-mode 1)
;;     (hs-minor-mode 1))

;; (defun my-xhtml-outline-level ()
;;   (save-excursion (re-search-forward html-outline-level))
;;     (let ((tag (buffer-substring (match-beginning 1) (match-end
;;     1))))
;;       (if (eq (length tag) 2)
;;           (- (aref tag 1) ?0)
;;         0)))


;; --------------------------------------------------------------------------------

;nXml - make it hs-minor-mode friendly

(add-to-list 'hs-special-modes-alist
             '(nxml-mode "<!--\\|<[^/>]>\\|<[^/][^>]*[^/]>"
                         ""
                         "<!--" ;; won't work on its own; uses syntax table
                         (lambda (arg) (my-nxml-forward-element))
                         nil))

(defun my-nxml-forward-element ()
  (let ((nxml-sexp-element-flag))
    (setq nxml-sexp-element-flag (not (looking-at "<!--")))
    (unless (looking-at outline-regexp)
      (condition-case nil
          (nxml-forward-balanced-item 1)
        (error nil)))))

(defun my-nxml-mode-hook ()
  "Functions to run when in nxml mode."
  (setq nxml-sexp-element-flag t)
  (hs-minor-mode 1))

(add-hook 'nxml-mode-hook 'my-nxml-mode-hook)

(eval-after-load "hideshow.el"
  (let ((nxml-mode-hs-info '(nxml-mode ("^\\s-*\\(<[^/].*>\\)\\s-*$" 1)
                                       "^\\s-*</.*>\\s-*$")))
    (when (not (member nxml-mode-hs-info hs-special-modes-alist))
      (setq hs-special-modes-alist
            (cons nxml-mode-hs-info hs-special-modes-alist)))))

;; --------------------------------------------------------------------------------

;; hideshow related

;; http://www.emacswiki.org/emacs/HideShow
;; Universal code folding set-selective-display is a simple, universal
;; function which hides code according to its indentation level. It
;; can be used as a fall-back for hs-toggle-hiding.
(defun toggle-selective-display (column)
  (interactive "P")
  (set-selective-display
   (or column
       (unless selective-display
         (1+ (current-column))))))

(defun toggle-hiding (column)
      (interactive "P")
      (if hs-minor-mode
          (if (condition-case nil
                  (hs-toggle-hiding)
                (error t))
              (hs-show-all))
        (toggle-selective-display column)))

(add-hook 'c-mode-common-hook   'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'java-mode-hook       'hs-minor-mode)
(add-hook 'lisp-mode-hook       'hs-minor-mode)
(add-hook 'perl-mode-hook       'hs-minor-mode)
(add-hook 'sh-mode-hook         'hs-minor-mode)

;; --------------------------------------------------------------------------------

;; http://github.com/alexott/emacs-configs/blob/master/rc/emacs-rc-cedet.el

;;; ede customization

(global-ede-mode t)

;; gamecore project definition
(setq gamecore-project
      (ede-cpp-root-project "gamecore" 
                            :file "/gamedev/gamecore/Makefile"
                            :local-variables (list
                                              (cons 'compile-command 'surki-gamecore-compile-string)
                                              )
                            ))
;; TODO Handle various targets
(defun surki-gamecore-compile-string ()
  "Generates compile string for compiling gamecore project"
  (let* ((current-dir (file-name-directory
                       (or (buffer-file-name (current-buffer)) default-directory)))
         (prj (ede-current-project current-dir))
         (root-dir (ede-project-root-directory prj))
         )
    (concat "sudo /gamedev/sandbox/tools/go2sandbox /sandbox.4.01.00.001 \"\" \"cd /agp/gamecore && make debug_allgames\"")))


;; gamecore project definition
(setq linux-kernel
      (ede-cpp-root-project "kernel" 
                            :file "~/src/linux-2.6/Makefile"
                            :local-variables (list
                                              (cons 'compile-command 'surki-kernel-compile-string)
                                              )
                            ))

;; TODO Handle various targets
(defun surki-kernel-compile-string ()
  "Generates compile string for compiling gamecore project"
  (let* ((current-dir (file-name-directory
                       (or (buffer-file-name (current-buffer)) default-directory)))
         (prj (ede-current-project current-dir))
         (root-dir (ede-project-root-directory prj))
         )
    (concat "cd " root-dir "; make -j2")))

;; my functions for EDE
(defun surki-ede-get-local-var (fname var)
  "fetch given variable var from :local-variables of project of file fname"
  (let* ((current-dir (file-name-directory fname))
         (prj (ede-current-project current-dir)))
    (when prj
      (let* ((ov (oref prj local-variables))
            (lst (assoc var ov)))
        (when lst
          (cdr lst))))))

;; setup compile package
(require 'compile)
(setq compilation-disable-input nil)
(setq compilation-scroll-output t)
(setq mode-compile-always-save-buffer-p t)
(defun surki-compile (&optional prefix)
  "Saves all unsaved buffers, and runs 'compile'."
  (interactive "P")
  (let* ((r (surki-ede-get-local-var
             (or (buffer-file-name (current-buffer)) default-directory)
             'compile-command))
         (cmd (if (functionp r) (funcall r) r)))
    (message "AA: %s" prefix)
    (set (make-local-variable 'compile-command) (or cmd compile-command))
 
   (if (consp prefix)
        (set (make-local-variable 'compilation-read-command) t)
      (set (make-local-variable 'compilation-read-command) nil)
      )

    (call-interactively 'compile))
  )
;; --------------------------------------------------------------------------------

(require 'switch-window)

;; --------------------------------------------------------------------------------

;; Zap-upto-char
(defadvice zap-to-char (after my-zap-to-char-advice (arg char) activate)
  "Kill up to the ARG'th occurence of CHAR, and leave CHAR.
  The CHAR is replaced and the point is put before CHAR."
    (insert char)
    (forward-char -1))

;; --------------------------------------------------------------------------------

;; this file provides
(provide 'sk-generic)
