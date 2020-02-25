;;; Package intialize
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;;; Install use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


;; Appearance
;;; Remove unnecessary components
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-message t)

;;; Display line numbers
(display-line-numbers-mode 1)

;;; Modeline line and column number
(line-number-mode 1)
(column-number-mode 1)

;;; Disable scroll jumps
(setq scroll-conservatively 100)


;; Syntax helpers
;;; Highlight closing paranteses
(show-paren-mode 1)

;;; Auto close pair
(setq electric-pair-pairs '(
                           (?\{ . ?\})
                           (?\( . ?\))
                           (?\[ . ?\])
                           (?\" . ?\")
                           ))
(electric-pair-mode t)

;;; Highlight current line
(when window-system (global-hl-line-mode t))


;; Emacs options
;;; Disable annoying sounds
(setq ring-bell-function 'ignore)

;;; delete inside selection when typing
(delete-selection-mode 1)

;;; Disable tabs
(setq-default indent-tabs-mode nil)

;;; Rectangle select mode
(global-set-key (kbd "C-x v") 'rectangle-mark-mode)

;;; Rectangle replace mode
(global-set-key (kbd "C-x r") 'string-rectangle)

;;; Comment region
(define-key global-map (kbd "C-x /" 'comment-region)

;;; Uncomment region
(define-key global-map (kbd "C-x \\") 'uncomment-region)

;;; Delete trailing whitespaces
(setq require-final-newline t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)  

;;; Buffer navigation
(global-set-key (kbd "M-.") 'next-buffer)
(global-set-key (kbd "M-,") 'previous-buffer)


;; Emacs backup management
;;; Disable Emacs backup
(setq make-backup-file nil)
(setq auto-save-default nil)


;; Aliases
;;; Shortcut for yes or no
(defalias 'yes-or-no-p 'y-or-n-p)


;; Variables
;;; Ansi term as default terminal emulator
(defvar term-shell "/bin/bash")
(defadvice ansi-term (before force-bash)
  (interactive (list term-shell)))
(ad-activate 'ansi-term)
(global-set-key (kbd "C-x t") 'ansi-term)


;;; Dockerfile support
(use-package dockerfile-mode
  :ensure t)

;;; docker-compose support
(use-package docker-compose-mode
  :ensure t)

;;; yaml support
(use-package undo-fu
  :ensure t)
(global-set-key (kbd "C-q") 'undo-fu-only-undo)
(global-set-key (kbd "C-.") 'undo-fu-only-redo)

;;; markdown support
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode)
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

;;; Doom themes
(use-package doom-themes
  :ensure t)

;;; smex for Smarter command executor
(use-package smex
  :ensure t
  :init (smex-initialize)
  :bind
  ("M-x" . smex))

;;; Ido interactive buffer manager
(use-package ido-vertical-mode
  :ensure t
  :init
  (ido-vertical-mode 1))
(setq ido-vertical-define-keys 'C-n-and-C-p-only)
(setq ido-create-new-buffer 'always)
(setq ido-everywhere t)
(ido-mode 1)

;;; Ibuffer as buffer manager
(global-set-key (kbd "C-x C-b") 'ibuffer)

;;; Avy shortcut jump to char
(use-package avy
  :ensure t
  :bind
  ("M-s" . avy-goto-char))

;;; Highlight color code to it's color
(use-package rainbow-mode
  :ensure t
  :init (rainbow-mode 1))

;;; Different color for different paranteses
(use-package rainbow-delimiters
  :ensure t
  :init
    (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;;; Window switch for ease of window navigation
(use-package switch-window
  :ensure t
  :config
    (setq switch-window-input-style 'minibuffer)
    (setq switch-window-increase 4)
    (setq switch-window-threshold 2)
    (setq switch-window-shortcut-style 'qwerty)
    (setq switch-window-qwerty-shortcuts
        '("a" "s" "d" "f" "j" "k" "l" "i" "o"))
  :bind
  ([remap other-window] . switch-window))

;;; Multi marker
(use-package mark-multiple
  :ensure t
  :bind ("C-c n" . 'mark-next-like-this))

(use-package mark-multiple
  :ensure t
  :bind ("C-c a" . 'mark-all-like-this))

;;; Expand region
(use-package expand-region
  :ensure t
  :bind ("C-q" . er/expand-region))

;;; Yank history menu
(use-package popup-kill-ring
  :ensure t
  :bind ("M-y" . popup-kill-ring))

;;; Smart search
(use-package swiper
  :ensure t
  :bind ("C-s" . 'swiper))


;; Custom functions
;;; Copy whole line
(defun daedreth/copy-whole-line ()
  "Copies a line without regard for cursor position."
  (interactive)
  (save-excursion
    (kill-new
     (buffer-substring
      (point-at-bol)
      (point-at-eol)))))
(global-set-key (kbd "C-x w") 'daedreth/copy-whole-line)

;;; Change focus on new window
(defun split-and-follow-hor ()
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 2") 'split-and-follow-hor)

(defun split-and-follow-ver ()
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 3") 'split-and-follow-ver)

;;; Delete inner word
(defun kill-inner-word ()
  (interactive)
  (forward-char 1)
  (backward-word)
  (kill-word 1))
(global-set-key (kbd "C-c w") 'kill-inner-word)

;;; Copy to clipboard function
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
(global-set-key (kbd "C-x c") 'copy-to-clipboard)

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

;;; vim like open above line
(defun vi-open-line-above ()
  "Insert a newline above the current line and put point at beginning."
  (interactive)
  (unless (bolp)
    (beginning-of-line))
  (newline)
  (forward-line -1)
  (indent-according-to-mode))

;;; vim like open line below
(defun vi-open-line-below ()
  "Insert a newline below the current line and put point at beginning."
  (interactive)
  (unless (eolp)
    (end-of-line))
  (newline-and-indent))

(defun vi-open-below (&optional abovep)
  "Insert a newline below the current line and put point at beginning.
With a prefix argument, insert a newline above the current line."
  (interactive "P")
  (vi-open-line-below))

(defun vi-open-above (&optional abovep)
  "Insert a newline below the current line and put point at beginning.
With a prefix argument, insert a newline above the current line."
  (interactive "P")
  (vi-open-line-above))

(define-key global-map [(meta n)] 'vi-open-below)
(define-key global-map [(meta shift n)] 'vi-open-above)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (doom-one)))
 '(custom-safe-themes
   (quote
    ("e1ecb0536abec692b5a5e845067d75273fe36f24d01210bf0aa5842f2a7e029f" "fa3bdd59ea708164e7821574822ab82a3c51e262d419df941f26d64d015c90ee" "e1ef2d5b8091f4953fe17b4ca3dd143d476c106e221d92ded38614266cea3c8b" default)))
 '(package-selected-packages
   (quote
    (swiper popup-kill-ring expand-region mark-multiple dashboard rainbow-delimiters switch-window rainbow-mode avy smex ido-vertical-mode doom-themes use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 150 :width normal :family "Hack")))))
