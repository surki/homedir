;; Autoload wanderlust on "wl"
(autoload 'wl "wl" "Wanderlust" t)
(autoload 'wl-other-frame "wl" "Wanderlust on new frame." t)
(autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)
(autoload 'wl-user-agent-compose "wl-draft" "Compose with Wanderlust." t)
;;(autoload 'wl-user-agent-compose "wl-draft" nil t)

;; --------------------------------------------------------------------------------

;; Basic settings

(setq wl-plugged t
      elmo-imap4-use-modified-utf7 t
      elmo-imap4-use-cache t
      elmo-nntp-use-cache t
      elmo-pop3-use-cache t
      wl-ask-range nil
      wl-insert-message-id nil
      wl-message-id-use-wl-from t
      wl-default-spec "%"

      ;; Need a smaller user agent string
      wl-generate-mailer-string-function 'wl-generate-user-agent-string-1
      elmo-message-fetch-confirm t
      elmo-message-fetch-threshold 250000
      wl-fcc-force-as-read t

      ;; Signature
      signature-insert-at-eof t
      signature-delete-blank-lines-at-eof t

      wl-draft-always-delete-myself  t
      wl-draft-reply-buffer-style 'keep
      wl-interactive-send t
      wl-interactive-exit t

      ;; Windows and decoration
      wl-folder-use-frame nil
      wl-highlight-body-too t
      wl-use-highlight-mouse-line nil
      wl-show-plug-status-on-modeline t
      wl-message-window-size '(1 . 4)
      )

;; Use wanderlust for default compose-mail
(if (boundp 'mail-user-agent)
    (setq mail-user-agent 'wl-user-agent))
(if (fboundp 'define-mail-user-agent)
    (define-mail-user-agent
      'wl-user-agent
      'wl-user-agent-compose
      'wl-draft-send
      'wl-draft-kill
      'mail-send-hook))

;; --------------------------------------------------------------------------------

;;; Folders
(setq wl-stay-folder-window t
      wl-folder-window-width 30
      wl-folder-desktop-name "Email"
      ;; wl-trash-folder ".Trash"
      wl-interactive-save-folders t

      wl-use-petname t
      wl-folder-petname-alist nil
      wl-fldmgr-make-backup  t
      wl-fldmgr-sort-group-first  t

      elmo-folder-update-confirm t
      elmo-folder-update-threshold 1000

      wl-folder-check-async  t
      ;; FIX ME
      ;; wl-auto-check-folder-name 'none
      ;; wl-auto-check-folder-list '("^\\.")
      ;; wl-auto-uncheck-folder-list nil

      wl-folder-notify-deleted t
      wl-fldmgr-add-complete-with-current-folder-list t
      wl-folder-info-save t
      wl-folder-many-unsync-threshold  100
      wl-highlight-folder-by-numbers 1
      )

;; --------------------------------------------------------------------------------

;;; Summary
(setq wl-auto-select-next 'unread
      wl-summary-width nil
      wl-summary-weekday-name-lang "en"
      ;;wl-summary-showto-folder-regexp ".Sent.*"
      ;;wl-summary-line-format "%n%T%P%M/%D(%W)%h:%m %t%[%17(%c %f%) %] %s"
      wl-summary-line-format "%n%T%P%M/%D(%W)%h:%m %[ %17f %]%[%1@%] %t%C%s"

      ;; Summary threads
      wl-thread-insert-opened t
      wl-thread-open-reading-thread t
      )

;; --------------------------------------------------------------------------------

;; Message
(setq mime-view-mailcap-files '("~/.mailcap")
      wl-forward-subject-prefix "Fwd: "
      wl-message-ignored-field-list '("^.*:")
      wl-message-visible-field-list
      '("^\\(To\\|Cc\\):"
        "^Subject:"
        "^\\(From\\|Reply-To\\):"
        "^Organization:"
        "^X-Attribution:"
        "^\\(Posted\\|Date\\):"
        "^X-Mailer:"
        "^User-Agent:"
        )

      wl-message-sort-field-list
      '("^From"
        "^Organization:"
        "^X-Attribution:"
        "^Subject"
        "^Date"
        "^To"
        "^Cc")
      
      nobreak-char-display nil
      
      ;; ;; Invert behaviour of with and without argument replies.
      ;; ;; just the author
      ;; wl-draft-reply-without-argument-list
      ;; '(("Reply-To" ("Reply-To") nil nil)
      ;;   ("Mail-Reply-To" ("Mail-Reply-To") nil nil)
      ;;   ("From" ("From") nil nil))

      ;; ;; bombard the world
      ;; wl-draft-reply-with-argument-list
      ;; '(("Followup-To" nil nil ("Followup-To"))
      ;;   ("Mail-Followup-To" ("Mail-Followup-To") nil ("Newsgroups"))
      ;;   ("Reply-To" ("Reply-To") ("To" "Cc" "From") ("Newsgroups"))
      ;;   ("From" ("From") ("To" "Cc") ("Newsgroups")))
)

(eval-after-load "mime-view"
  '(progn
     (ctree-set-calist-strictly
      'mime-acting-condition
      '((mode . "play")
        (type . application)(subtype . pdf)
        (method . my-mime-save-content-find-file)))))

;; --------------------------------------------------------------------------------

;;; Refiling:
;; See also `bbdb-wl-refile-alist' and `wl-init-hook'

;; (defcustom wl-general-refile-rule-alist nil
;;   "General rule alist which may be extended to include the `From' `folder'
;; entries defined in the BBDB by `bbdb-wl-refile-alist'.
;; e.g.
;; '((\"From\"
;;    (\"teranisi@isl.ntt.co.jp\" . \"+teranisi\"))
;;   (\"x-ml-name\"
;;    (\"^Wanderlust\"    . \"+wl\")
;;    (\"^Elips\" . \"+elips\")))"
;;   :type '(repeat (list (string :tag "Field")
;;                        (repeat :inline t
;;                                (cons (regexp :tag "Value")
;;                                      (string :tag "Folder")))))
;;   :group 'wl-pref)

;; Set the default value of wl-refile-rule-alist
;; (setq wl-refile-rule-alist wl-general-refile-rule-alist)

;; --------------------------------------------------------------------------------

;; Configure BBDB to manage Email addresses

(require 'bbdb-wl)
(bbdb-wl-setup)

(setq
      bbdb-use-pop-up t ;; Allow pop-ups
      bbdb-pop-up-target-lines 2

      ;; auto collection
      bbdb/mail-auto-create-p t

      bbdb-auto-notes-alist '(("X-ML-Name" (".*$" ML 0)))

      ;; get addresses only from these folders
      bbdb-wl-folder-regexp ".*Inbox.*\\|.*Sent.*|.*TKK.*"
      ;;bbdb-wl-ignore-folder-regexp "^@" ;; folders without auto collection

      ;; FIX ME
      ;; bbdb-north-american-phone-numbers-p nil
      ;; bbdb-auto-notes-alist '(("X-ML-Name" (".*$" ML 0)))
      ;; bbdb-dwim-net-address-allow-redundancy t

      ;; shows the name of bbdb in the summary

      ;; Not with wl-summary-showto-folder-regexp
      ;;wl-summary-from-function 'bbdb-wl-from-func
      ;; Use the default:
      wl-summary-from-function 'wl-summary-default-from

      ;; Using BBDB for pet names is OK
      wl-summary-get-petname-function 'bbdb-wl-get-petname
      )

;; --------------------------------------------------------------------------------

;;; Sending
;;; Select from address and smtp server based on the to address if replying

;; Don't split large messages
(setq mime-edit-split-message nil
      wl-draft-config-matchone t)

;; --------------------------------------------------------------------------------

;;; Add hooks

(add-hook
 'wl-init-hook
 '(lambda ()
    (run-with-idle-timer 30 t 'my-wl-auto-save-draft-buffers)
    ))

(add-hook
 'wl-folder-mode-hook
 '(lambda ()
    (hl-line-mode t)
    ))

(add-hook
 'wl-summary-mode-hook
 '(lambda ()
    (hl-line-mode t)

    ;; Key bindings
    (local-set-key "D" 'wl-thread-delete)
    (local-set-key "b" 'wl-summary-resend-bounced-mail)
    ;; (local-set-key "\C-d" 'my-wl-summary-delete-and-move-prev)
    ;; (local-set-key "\C-cQ" 'my-wl-delete-whole-folder)
    ;; (local-set-key "\C-cb" 'my-bbdb-wl-refile-alist)
    (local-set-key "\C-a"
                   '(lambda ()
                      (interactive)
                      (wl-summary-reply-with-citation 1)))
    ;; (local-set-key "\M-m" 'mairix-search)
    ))

(add-hook
 'wl-summary-exec-hook
 '(lambda ()
    ;; Synchronise the folder with the server after executing the summary
    ;; operation
    (wl-summary-sync-update)
    ))

(add-hook
 'wl-message-buffer-created-hook
 '(lambda ()
    (setq truncate-lines nil) ;; Fold over-length lines
    ))

(add-hook
 'wl-draft-mode-hook
 '(lambda ()
    ;; Key bindings
    ;; (local-set-key "\C-c\C-k" 'my-wl-draft-kill-force)
    (local-set-key (kbd "<backtab>") 'bbdb-complete-name)
    ;; (define-key wl-draft-mode-map (kbd "<backtab>") 'bbdb-complete-name)))
    ))

;; Check mail for subject and attachment before sending
(add-hook 'wl-mail-send-pre-hook 'my-wl-draft-subject-check)
(add-hook 'wl-mail-send-pre-hook 'my-wl-draft-attachment-check)
;; (add-hook 'wl-biff-notify-hook 'my-wl-mail-notification-hook)

;; Add lots of goodies to the mail setup
(add-hook 'wl-mail-setup-hook 'my-mail-setup)

(add-hook
 'mime-view-mode-hook
 '(lambda ()
    "Change [mouse-2] to drag-scroll rather than follow link.
Set [(return)] to execute the mime-button.
Set the `f' key to run `find-file' on the attached entity.
Set the `C-f' key to run `find-file-at-point'.
Set the `w' key to run `wget'.
Set the `j' key to run `mime-preview-quit'."
    ;; Key bindings
    (local-set-key [down-mouse-2] 'mouse-drag-drag)
    (local-set-key [(return)] 'my-mime-button-exec)
    (local-set-key [?f] 'my-mime-find-file-current-entity)
    (local-set-key [(control ?f)] 'find-file-at-point)
    (local-set-key [?w] 'wget)
    (local-set-key [?o] 'wget-open)
    (local-set-key [?j] 'mime-preview-quit)
    (local-set-key [?s] '(lambda ()
                           (interactive)
                           (mime-preview-quit)
                           (wl-summary-sync)))
    (local-set-key [?t] 'babel-buffer)
    ))

;; (add-hook
;;  'wl-biff-notify-hook
;;  '(lambda ()
;;     (my-wl-update-current-summaries)
;;     ))

;; Automatically add mailing list fields
;; (add-hook 'bbdb-notice-hook 'bbdb-auto-notes-hook)

;; Smilies
(add-hook
 'wl-message-redisplay-hook
 '(lambda () (smiley-region (point-min) (point-max))
    ))

(add-hook
 'wl-draft-cited-hook
 '(lambda ()
     (and (featurep 'smiley-mule)
          (smiley-toggle-buffer -1))
     ))

;; --------------------------------------------------------------------------------

;; Extension Functions

;; (defun my-bbdb-wl-refile-alist ()
;;   "Add the `From To' refile to `folder' entries from the BBDB to the
;; `wl-refile-rule-alist'."
;;   (interactive)
;;   (let ((from-rule-alist (list '("From" "To")))
;;         (records (bbdb-records))
;;         (record))
;;     (while records
;;       (setq record (car records))
;;       (let ((email-addrs (bbdb-record-net record))
;;             (folder (bbdb-record-getprop record 'folder))
;;             (email-addr))
;;         (if folder
;;             (progn
;;               (while email-addrs
;;                 (setq email-addr (car email-addrs))
;;                 (setq from-rule-alist
;;                       (append from-rule-alist (list (cons email-addr folder))))
;;                 (setq email-addrs (cdr email-addrs))))))
;;       (setq records (cdr records)))
;;     (setq wl-refile-rule-alist
;;           (append wl-general-refile-rule-alist (list from-rule-alist)))
;;     ))

;; --------------------------------------------------------------------------------

;;; User Functions

;;; TODO Check and get rid of these

(defun my-wl-draft-kill-force ()
  (interactive)
  (wl-draft-kill t))

;; (defun my-wl-delete-whole-folder ()
;;   (interactive)
;;   (wl-summary-target-mark-all)
;;   (wl-summary-target-mark-delete)
;;   (wl-summary-exec)
;;   (wl-summary-exit))

(defun my-wl-check-mail-primary ()
  (interactive)
  (unless (get-buffer wl-folder-buffer-name)
    (wl))
  (delete-other-windows)
  (switch-to-buffer wl-folder-buffer-name)
  (goto-char (point-min))
  (next-line 1)
  (wl-folder-jump-to-current-entity))

(defun my-wl-auto-save-draft-buffers ()
  (let ((buffers (wl-collect-draft)))
    (save-excursion
      (while buffers
        (set-buffer (car buffers))
        (if (buffer-modified-p) (wl-draft-save))
        (setq buffers (cdr buffers))))))

(defun my-wl-update-current-summaries ()
  (let ((buffers (wl-collect-summary)))
    (while buffers
      (with-current-buffer (car buffers)
        (save-excursion
          (wl-summary-sync-update)))
      (setq buffers (cdr buffers)))))

;; (defun my-wl-summary-delete-and-move-prev ()
;;   (interactive)
;;   (let (wl-summary-move-direction-downward)
;;     (call-interactively 'wl-summary-delete)))

(defun wl-rehilight ()
  "Re-highlight message."
  (let ((beg (point-min))
        (end (point-max)))
    (put-text-property beg end 'face nil)
    (wl-highlight-message beg end t)))

(defun my-mail-setup ()
  "Set up appropriate modes for writing Email and clean-up citation for replies."
  (interactive)

  ;; Fold over-length lines
  (setq truncate-lines nil)
  (turn-on-auto-fill)
  (flyspell-mode t)

  ;; Apply template based on from address
  (unless wl-draft-reedit ; don't apply when reedit.
    (wl-draft-config-exec wl-draft-config-alist))

  (remove-hook 'wl-draft-send-hook 'wl-draft-config-exec)

  ;; Switch on the completion selection mode
  ;; and set the default completion-selection to bbdb
  ;; (completion-selection-mode t)
  ;; (completion-selection-set 'complete-bbdb)

  ;; Clean up reply citation
  (save-excursion
    ;; Goto the beginning of the message body
    (mail-text)
    ))

(defun my-mime-save-content-find-file (entity &optional situation)
  "Save the attached mime ENTITY and load it with `find-file-other-frame'
so that the appropriate emacs mode is selected according to the file extension."
  (let* ((name (or (mime-entity-safe-filename entity)
                   (format "%s" (mime-entity-media-type entity))))
         (dir (if (eq t mime-save-directory)
                  default-directory
                mime-save-directory))
         (filename (expand-file-name
                    (file-name-nondirectory name) temporary-file-directory)))
    (mime-write-entity-content entity filename)
    (select-frame (make-frame))
    (find-file filename)
    ))

(defun my-mime-view-emacs-mode (entity &optional situation)
  "Internal method for mime-view to display the mime ENTITY in a buffer with an
appropriate emacs mode."
  (let ((buf (get-buffer-create
              (format "%s-%s" (buffer-name) (mime-entity-number entity)))))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (mime-insert-text-content entity)
      ;;(mule-caesar-region (point-min) (point-max))
      ;; Set emacs mode here
      (set-buffer-modified-p nil)
      )
    (let ((win (get-buffer-window (current-buffer))))
      (or (eq (selected-window) win)
          (select-window (or win (get-largest-window)))
          ))
    (view-buffer buf)
    (goto-char (point-min))
    ))


(defun my-mime-find-file-current-entity ()
  "Save the current mime entity and load it with `find-file-other-frame'
so that the appropriate emacs mode is selected according to the file extension."
  (interactive)
  (let ((entity (get-text-property (point) 'mime-view-entity)))
    (if entity
        (my-mime-save-content-find-file entity)))
  )

(defun my-wl-draft-subject-check ()
  "Check whether the message has a subject before sending"
  (if (and (< (length (std11-field-body "Subject")) 1)
           (null (y-or-n-p "No subject! Send current draft?")))
      (error "Abort.")))


;; note, this check could cause some false positives; anyway, better
;; safe than sorry...
(defun my-wl-draft-attachment-check ()
  "if attachment is mention but none included, warn the the user"
  (save-excursion
    (goto-char 0)
    (unless ;; don't we have an attachment?

        (re-search-forward "^Content-Disposition: attachment" nil t)
      (when ;; no attachment; did we mention an attachment?
          (re-search-forward "attach" nil t)
        (unless (y-or-n-p "Possibly missing an attachment. Send current draft?")
          (error "Abort."))))))

(defun my-wl-mail-notification-hook ()
  "Update /tmp/surki-mails on new mail arrival"
  (interactive)
  (shell-command "echo New Mail > /tmp/surki-mails")
  )


  ;; (with-open-file (stream  "/tmp/surki-mails"
  ;;                          :direction :output
  ;;                          :if-exists :overwrite
  ;;                          :if-does-not-exist :create )
  ;;   (format stream "New Mail"))

(require 'elmo)

;; --------------------------------------------------------------------------------

;; Key-bindings

;; (global-set-key "\C-xm" 'my-wl-check-mail-primary)
;; (setq my-wl-tky-smtp-server 'my-wl-tky-smtp-server)

;; --------------------------------------------------------------------------------

;; Box quoting
(require 'boxquote)

;; --------------------------------------------------------------------------------

;; Biff: Check for new mail
(setq 
      wl-biff-check-interval 10
      wl-biff-use-idle-timer t
      wl-biff-notify-hook nil)

(add-hook 'wl-biff-notify-hook
          (lambda()
            (surki-popup "Wanderlust" "You have new mail!"
                         "/usr/share/icons/gnome/32x32/status/mail-unread.png")))

;; --------------------------------------------------------------------------------

;; from wl-en / Katsumi Yamaoka <yamaoka@jpl.org>
(defun my-mime-preview-play-current-entity-with-doc-view ()
  "Play part using DocView."
  (interactive)
  (let ((entity (get-text-property (point) 'mime-view-entity))
	name)
    (when entity
      (if (setq name (mime-entity-safe-filename entity))
	  (setq name (file-name-nondirectory (eword-decode-string name)))
	(setq name (make-temp-name "doc-view-")))
      (let ((pop-up-frames t))
	(pop-to-buffer (generate-new-buffer name)))
      (set-buffer-multibyte nil)
      (insert (mime-entity-content entity))
      (set-buffer-modified-p nil)
      (setq buffer-file-name name)
      (condition-case err
	  (doc-view-mode)
	(error (message "%s" (error-message-string err))))
      (use-local-map (copy-keymap doc-view-mode-map))
      (local-set-key
       "q"
       (lambda ()
	 (interactive)
	 (delete-frame (prog1
			   (selected-frame)
			 (quit-window 'kill))))))))

(add-hook
 'mime-view-mode-hook
 (lambda ()
   (local-set-key
    "V"
    'my-mime-preview-play-current-entity-with-doc-view)))

;; --------------------------------------------------------------------------------

;; (require 'tls)
;; (set-alist 'elmo-network-stream-type-alist "!" '(ssl ssl open-tls-stream))
;; (setq ssl-program-name "gnutls-cli")
;; (setq ssl-program-arguments '("-p" service host))

(require 'ssl)
(setq ssl-program-name "openssl")
(setq ssl-program-arguments '("s_client" "-quiet" "-host" host "-port" service))

;; --------------------------------------------------------------------------------

(require 'mime-w3m)

;; For the guys who use html
(setq mime-setup-enable-inline-html t)
(eval-after-load "mime-view"
  '(progn
     (ctree-set-calist-strictly
      'mime-preview-condition
      '((type . text)
	(subtype . html)
	(body . visible)
	(body-presentation-method . mime-display-text/plain)))
     (set-alist 'mime-view-type-subtype-score-alist
		'(text . html) 0)
     ;;
     ))


;; --------------------------------------------------------------------------------

;; from a WL mailing list post by Per b. Sederber
;; Re-fill messages that arrive poorly formatted

(require 'filladapt)

(defun wl-summary-refill-message (all)
  (interactive "P")
  (if (and wl-message-buffer (get-buffer-window wl-message-buffer))
      (progn
        (wl-summary-toggle-disp-msg 'on)
        (save-excursion
          (set-buffer wl-message-buffer)
          (goto-char (point-min))
          (re-search-forward "^$")
          (while (or (looking-at "^\\[[1-9]") (looking-at "^$"))
            (forward-line 1))
          (let* ((buffer-read-only nil)
                 (find (lambda (regexp)
                         (save-excursion
                           (if (re-search-forward regexp nil t)
                               (match-beginning 0)
                             (point-max)))))
                 (start (point))
                 (end (if all
                          (point-max)
                        (min (funcall find "^[^>\n]* wrote:[ \n]+")
                             (funcall find "^>>>>>")
                             (funcall find "^ *>.*\n *>")
                             (funcall find "^-----Original Message-----")))))
            (save-restriction
              (narrow-to-region start end)
              (filladapt-mode 1)
              (fill-region (point-min) (point-max)))))
        (message "Message re-filled"))
    (message "No message to re-fill")))

(define-key wl-summary-mode-map "\M-q" 'wl-summary-refill-message)

;; --------------------------------------------------------------------------------

(provide 'sk-wanderlust)
