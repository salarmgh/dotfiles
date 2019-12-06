;; Melpa repo
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

;; Disable syntax highlighting
(global-font-lock-mode 0)

;; Remove startup message
(setq inhibit-startup-message t)

;; Disable tabs
(setq-default indent-tabs-mode nil)

;; Enable auto pair
(electric-pair-mode 1)

;; Show paran matching
(require 'paren)
(setq show-paren-style 'parenthesis)
(show-paren-mode +1)

; Display lines
(global-display-line-numbers-mode)

; Disable menu
(menu-bar-mode -1)

;; Functions
; Insert line above and bellow
; Insert above line
(defun open-line-above ()
  "Insert a newline above the current line and put point at beginning."
  (interactive)
  (unless (bolp)
    (beginning-of-line))
  (newline)
  (forward-line -1)
  (indent-according-to-mode))

; Insert line below
(defun open-line-below ()
  "Insert a newline below the current line and put point at beginning."
  (interactive)
  (unless (eolp)
    (end-of-line))
  (newline-and-indent))

; def abovep for open below
(defun open-below (&optional abovep)
  "Insert a newline below the current line and put point at beginning.
With a prefix argument, insert a newline above the current line."
  (interactive "P")
  (open-line-below))

; def abovep for open above
(defun open-above (&optional abovep)
  "Insert a newline below the current line and put point at beginning.
With a prefix argument, insert a newline above the current line."
  (interactive "P")
  (open-line-above))

; Clipboard management
; copy to clipboard function
(defun copy-to-clipboard ()
"Copies selection to x-clipboard."
(interactive)
(if (display-graphic-p)
    (progn
      (message "Yanked region to x-clipboard!")
      (call-interactively 'clipboard-kill-ring-save)
      )
  (if (region-active-p)
      (progn
        (shell-command-on-region (region-beginning) (region-end) "xsel -i -b")
        (message "Yanked region to clipboard!")
        (deactivate-mark))
    (message "No region active; can't yank to clipboard!")))
)

; paste from clipboard
(defun paste-from-clipboard ()
"Pastes from x-clipboard."
(interactive)
(if (display-graphic-p)
    (progn
      (clipboard-yank)
      (message "graphics active")
      )
  (insert (shell-command-to-string "xsel -o -b"))
  )
)
;; Shortcuts
; (meta n) for insert line below
(define-key global-map [(meta n)] 'open-below)

; (meta shift n) for insert line above
(define-key global-map [(meta shift n)] 'open-above)

; (meta [) for comment region
(define-key global-map [(meta \[)] 'comment-region)

; (meta ]) for uncomment region
(define-key global-map [(meta \])] 'uncomment-region)

; (m p) paste from clipboard
(global-set-key (kbd "M-p") 'paste-from-clipboard)
; (m c) copy to clipboard
(global-set-key (kbd "M-c") 'copy-to-clipboard)

; rectangle select mode
(global-set-key (kbd "C-x v") 'rectangle-mark-mode)

(set-face-foreground 'mode-line "green")
(set-face-background 'mode-line "black")
