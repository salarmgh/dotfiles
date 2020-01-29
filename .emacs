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

;; flycheck
(require 'flycheck)

;; tide
(defun setup-tide-mode ()
  (interactive)
  (setq typescript-indent-level 2)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))
(require 'tide)
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))

;; enable typescript-tslint checker
(flycheck-add-mode 'typescript-tslint 'web-mode)

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)
;; javascript checker
(add-hook 'js2-mode-hook #'setup-tide-mode)
;; configure javascript-tide checker to run after your default javascript checker
(flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append)

;; jsx checker
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "jsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))
;; configure jsx-tide checker to run after your default jsx checker
(flycheck-add-mode 'javascript-eslint 'web-mode)
(flycheck-add-next-checker 'javascript-eslint 'jsx-tide 'append)

;;python
(elpy-enable)
;; Enable Flycheck
(when (require 'flycheck nil t)
(setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
(add-hook 'elpy-mode-hook 'flycheck-mode))

;; Enable autopep8
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

(setq py-autopep8-options '("--max-line-length=80"))

;; Disable syntax highlighting
;(global-font-lock-mode 0)

;; delete inside selection when typing
(delete-selection-mode 1)

;; Remove startup message
(setq inhibit-startup-message t)

;; Disable tabs
(setq-default indent-tabs-mode nil)

;; Indent size
(setq-default tab-width 2)

;; JS indent
 (setq js-indent-level 2)

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

; Disable indentation view
(highlight-indentation-mode -1)

;; Functions

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

; rectangle select mode
(global-set-key (kbd "C-x r") 'string-rectangle)

;(set-face-foreground 'mode-line "black")
;(set-face-background 'mode-line "#f8f8f8")
;;(setq make-backup-files nil)
;;(setq auto-save-default nil)
;;(setq backup-by-copying t)


; Re-create ci" ca"...
(defun seek-backward-to-char (chr)
  "Seek backwards to a character"
  (interactive "cSeek back to char: ")
  (while (not (= (char-after) chr))
    (forward-char -1)))

(setq char-pairs
      '(( ?\" . ?\" )
        ( ?\' . ?\' )
        ( ?\( . ?\) )
        ( ?\[ . ?\] )
        ( ?\{ . ?\} )
        ( ?<  . ?>  )))

(defun get-char-pair (chr)
  (let ((result ()))
    (dolist (x char-pairs)
      (setq start (car x))
      (setq end (cdr x))
      (when (or (= chr start) (= chr end))
        (setq result x)))
      result))

(defun get-start-char (chr)
  (car (get-char-pair chr)))
(defun get-end-char (chr)
  (cdr (get-char-pair chr)))

(defun seek-to-matching-char (start end count)
  (while (> count 0)
    (if (= (following-char) end)
        (setq count (- count 1))
      (if (= (following-char) start)
          (setq count (+ count 1))))
    (forward-char 1)))

(defun seek-backward-to-matching-char (start end count)
  (if (= (following-char) end)
      (forward-char -1))
  (while (> count 0)
    (if (= (following-char) start)
        (setq count (- count 1))
      (if (= (following-char) end)
          (setq count (+ count 1))))
    (if (> count 0)
        (forward-char -1))))

(defun delete-between-pair (char)
  "Delete in between the given pair"
  (interactive "cDelete between char: ")
  (seek-backward-to-matching-char (get-start-char char) (get-end-char char) 1)
  (forward-char 1)
  (setq mark (point))
  (seek-to-matching-char (get-start-char char) (get-end-char char) 1)
  (forward-char -1)
  (kill-region mark (point)))

(defun delete-all-pair (char)
  "Delete in between the given pair and the characters"
  (interactive "cDelete all char: ")
  (seek-backward-to-matching-char (get-start-char char) (get-end-char char) 1)
  (setq mark (point))
  (forward-char 1)
  (seek-to-matching-char (get-start-char char) (get-end-char char) 1)
  (kill-region mark (point)))

(global-set-key (kbd "C-c i") 'delete-between-pair)
(global-set-key (kbd "C-c a") 'delete-all-pair)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-enabled-themes (quote (darkburn)))
 '(custom-safe-themes
   (quote
    ("c7f10959cb1bc7a36ee355c765a1768d48929ec55dde137da51077ac7f899521" default)))
 '(elpy-modules
   (quote
    (elpy-module-company elpy-module-eldoc elpy-module-pyvenv elpy-module-yasnippet elpy-module-django elpy-module-sane-defaults)))
 '(package-selected-packages
   (quote
    (smex ido-vertical-mode git-gutter undo-fu php-mode docker-compose-mode yaml-mode blacken py-autopep8 elpy darkburn-theme ## company flycheck-package web-mode tide))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq typescript-indent-level 2)
(setq js2-basic-offset 2)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq indent-line-function 'insert-tab)
(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
)
(add-hook 'web-mode-hook  'my-web-mode-hook)


(setq require-final-newline t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq backup-directory-alist `(("." . "~/.emacs-saves")))
(setq backup-by-copying t)
(setq js-indent-level 2)
(setq css-indent-offset 2)

(setq web-mode-markup-indent-offset 2)
(setq web-mode-attr-indent-offset 2)
(setq web-mode-attr-value-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-enable-current-column-highlight 1)
(setq web-mode-enable-current-element-highlight 1)
(setq web-mode-block-padding 0)
(setq web-mode-script-padding 2)
(setq web-mode-style-padding 2)
(add-to-list 'auto-mode-alist '("\\.htm.*$" . web-mode))


; change undo key binding
;(global-set-key (kbd "C-q") 'undo)
;(global-set-key (kbd "C-\\") 'redo)
(global-set-key (kbd "C-q") 'undo-fu-only-undo)
(global-set-key (kbd "C-\\") 'undo-fu-only-redo)



(setq scroll-step 1)

(set-display-table-slot standard-display-table 'wrap ?\ )

(tool-bar-mode -1)
(setq inhibit-startup-screen t)
(global-git-gutter-mode +1)


; vim like open above line
(defun vi-open-line-above ()
  "Insert a newline above the current line and put point at beginning."
  (interactive)
  (unless (bolp)
    (beginning-of-line))
  (newline)
  (forward-line -1)
  (indent-according-to-mode))

; vim like open line below
(defun vi-open-line-below ()
  "Insert a newline below the current line and put point at beginning."
  (interactive)
  (unless (eolp)
    (end-of-line))
  (newline-and-indent))

; def abovep for open below
(defun vi-open-below (&optional abovep)
  "Insert a newline below the current line and put point at beginning.
With a prefix argument, insert a newline above the current line."
  (interactive "P")
  (vi-open-line-below))

; def abovep for open above
(defun vi-open-above (&optional abovep)
  "Insert a newline below the current line and put point at beginning.
With a prefix argument, insert a newline above the current line."
  (interactive "P")
  (vi-open-line-above))

; (meta n) for insert line below
(define-key global-map [(meta n)] 'vi-open-below)
; (meta shift n) for insert line above
(define-key global-map [(meta shift n)] 'vi-open-above)

(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)

(global-set-key (kbd "C-x \"") 'split-window-below)
(global-set-key (kbd "C-x |") 'split-window-right)

(global-set-key (kbd "M-.") 'next-buffer)
(global-set-key (kbd "M-,") 'previous-buffer)

(setq ido-enable-flex-matching nil)
(setq ido-create-new-buffer 'always)
(setq ido-everywhere t)
(ido-mode 1)
(ido-vertical-mode 1)
;; (use-package ido-vertical-mode
;;              :ensure t
;;              :init
;;              (ido-vertical-mode 1))
(setq ido-vertical-define-keys 'C-n-and-C-p-only)
(global-set-key (kbd "C-x b") 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ido-switch-buffer)
(setq ibuffer-expert t)
(global-set-key (kbd "M-x") 'smex)
