(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package doom-themes
  :ensure t)

(use-package ido-vertical-mode
  :ensure t
  :init
  (ido-vertical-mode 1))
(setq ido-vertical-define-keys 'C-n-and-C-p-only)

(use-package smex
  :ensure t
  :init (smex-initialize)
  :bind
  ("M-x" . smex))

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(setq inhibit-startup-message t)

(setq scroll-conservatively 100)

(setq ring-bell-function 'ignore)

(setq make-backup-file nil)
(setq auto-save-default nil)

(when window-system (global-hl-line-mode t))

(defalias 'yes-or-no-p 'y-or-n-p)

(defvar term-shell "/bin/bash")
(defadvice ansi-term (before force-bash)
  (interactive (list term-shell)))
(ad-activate 'ansi-term)

(global-set-key (kbd "C-x t") 'ansi-term)

(setq ido-create-new-buffer 'always)
(setq ido-everywhere t)
(ido-mode 1)

(global-set-key (kbd "C-x C-b") 'ibuffer)


(use-package avy
  :ensure t
  :bind
  ("M-s" . avy-goto-char))

(use-package rainbow-mode
  :ensure t
  :init (rainbow-mode 1))

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

(setq electric-pair-pairs '(
                           (?\{ . ?\})
                           (?\( . ?\))
                           (?\[ . ?\])
                           (?\" . ?\")
                           ))
(electric-pair-mode t)

(defun kill-inner-word ()
  (interactive)
  (forward-char 1)
  (backward-word)
  (kill-word 1))
(global-set-key (kbd "C-c w") 'kill-inner-word)

(defun kill-current-buffer ()
  "Kills the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))
(global-set-key (kbd "C-x k") 'kill-current-buffer)

(defun daedreth/copy-whole-line ()
  "Copies a line without regard for cursor position."
  (interactive)
  (save-excursion
    (kill-new
     (buffer-substring
      (point-at-bol)
      (point-at-eol)))))
(global-set-key (kbd "C-x w") 'daedreth/copy-whole-line)

(show-paren-mode 1)

(use-package rainbow-delimiters
  :ensure t
  :init
    (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package dashboard
  :ensure t
  :config
    (dashboard-setup-startup-hook)
    (setq dashboard-startup-banner "~/.emacs.d/img/dashLogo.png")
    (setq dashboard-items '((recents  . 5)
                            (projects . 5)))
    (setq dashboard-banner-logo-title "Hello Salar!"))

(use-package mark-multiple
  :ensure t
  :bind ("C-c n" . 'mark-next-like-this))

(use-package mark-multiple
  :ensure t
  :bind ("C-c a" . 'mark-all-like-this))


(use-package expand-region
  :ensure t
  :bind ("C-q" . er/expand-region))

(use-package popup-kill-ring
  :ensure t
  :bind ("M-y" . popup-kill-ring))

(use-package swiper
  :ensure t
  :bind ("C-s" . 'swiper))

(line-number-mode 1)
(column-number-mode 1)


;New movie
(global-visual-line-mode t)


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
