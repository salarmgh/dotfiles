(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
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
(global-linum-mode)

;;; Modeline line and column number
(line-number-mode 1)
(column-number-mode 1)

;;; Disable scroll jumps
(setq scroll-conservatively 100)


;; Syntax helpers
;;; Highlight closing paranteses
(show-paren-mode 1)

;;; Auto close pair
(electric-pair-mode 1)

;;; Highlight current line
(when window-system (global-hl-line-mode t))

;; Disable tabs
(setq-default indent-tabs-mode nil)

;; Indent size
(setq-default tab-width 4)

;; Emacs options
;;; Disable annoying sounds
(setq ring-bell-function 'ignore)

;;; delete inside selection when typing
(delete-selection-mode 1)

;;; Disable tabs
(setq-default indent-tabs-mode nil)

;; Emacs env path
(setenv "PATH" (concat (getenv "PATH") ":/home/salarmgh/.local/go/bin:/home/salarmgh/Workspace/go/bin"))
(setq exec-path (append '("/home/salarmgh/.local/go/bin" "/home/salarmgh/Workspace/go/bin")
                        exec-path))


;; Emacs backup management
;;; Disable Emacs backup
;; make backup to a designated dir, mirroring the full path
(defun my-backup-file-name (fpath)
  "Return a new file path of a given file path.
If the new path's directories does not exist, create them."
  (let* (
        (backupRootDir "~/.emacs.d/backup/")
        (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath )) ; remove Windows driver letter in path, for example, “C:”
        (backupFilePath (replace-regexp-in-string "//" "/" (concat backupRootDir filePath "~") ))
        )
    (make-directory (file-name-directory backupFilePath) (file-name-directory backupFilePath))
    backupFilePath
  )
)

(setq make-backup-file-name-function 'my-backup-file-name)
(setq auto-save-default nil)


;; Aliases
;;; Shortcut for yes or no
(defalias 'yes-or-no-p 'y-or-n-p)

; rectangle select mode
(global-set-key (kbd "C-x v") 'rectangle-mark-mode)

; rectangle select mode
(global-set-key (kbd "C-x r") 'string-rectangle)

;; Theme
(use-package solarized-theme
  :ensure t)

;; Packages
;;; Dockerfile support
(use-package dockerfile-mode
  :ensure t)

;;; docker-compose support
(use-package docker-compose-mode
  :ensure t)

;;; Git side support
(use-package git-gutter
  :ensure t)
(global-git-gutter-mode +1)

;;; Smart search
(use-package swiper
  :ensure t
  :bind ("C-s" . 'swiper))

;;; Undo
(use-package undo-fu
  :ensure t)
(global-set-key (kbd "C-q") 'undo-fu-only-undo)
(global-set-key (kbd "C-\\") 'undo-fu-only-redo)

;;; Projectile
(use-package projectile
  :ensure t)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(setq projectile-project-search-path '("~/Workspace/"))

;; Ido
(use-package ido-vertical-mode
  :ensure t)
(setq ido-enable-flex-matching nil)
(setq ido-create-new-buffer 'always)
(setq ido-everywhere t)
(ido-mode 1)
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)
(global-set-key (kbd "C-x C-b") 'ido-switch-buffer)

(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)

(global-set-key (kbd "C-x \"") 'split-window-below)
(global-set-key (kbd "C-x |") 'split-window-right)

;(global-set-key (kbd "M-.") 'next-buffer)
;(global-set-key (kbd "M-,") 'previous-buffer)

;; Languages
(use-package lsp-mode
  :ensure t)

(use-package company-lsp
  :ensure t)

(use-package flycheck
  :ensure t)

(use-package go-mode
  :ensure t)
(setenv "GOPATH" "/home/salarmgh/Workspace/go")
(autoload 'go-mode "go-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
(defun my-go-mode-hook ()
  ; Use goimports instead of go-fmt
  (setq gofmt-command "goimports")
  ; Call Gofmt before saving                                                    
  (add-hook 'before-save-hook 'gofmt-before-save)
  ; Godef jump key binding                                                      
  (local-set-key (kbd "C-\]") 'godef-jump)
  (local-set-key (kbd "C-\[") 'pop-tag-mark)
  (local-set-key (kbd "C-x o") 'godoc-at-point)
  (add-hook 'go-mode-hook 'lsp-deferred)
  )
(add-hook 'go-mode-hook 'my-go-mode-hook)

;; Set default font
(set-face-attribute 'default nil
                    :family "DejaVu Sans Mono Book"
                    :height 120
                    :weight 'normal
                    :width 'normal)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (solarized-light-high-contrast)))
 '(custom-safe-themes
   (quote
    ("00445e6f15d31e9afaa23ed0d765850e9cd5e929be5e8e63b114a3346236c44c" "c433c87bd4b64b8ba9890e8ed64597ea0f8eb0396f4c9a9e01bd20a04d15d358" default)))
 '(package-selected-packages
   (quote
    (lsp-mode solarized-theme r swiper git-gutter docker-compose-mode dockerfile-mode use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
