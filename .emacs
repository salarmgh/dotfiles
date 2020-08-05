(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
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

;(setq-default cursor-type 'bar) 

;;; Disable scroll jumps
(setq scroll-conservatively 100)


;; Syntax helpers
;;; Highlight closing paranteses
;(show-paren-mode 1)

;;; Auto close pair
(electric-pair-mode 1)


;; Show paran matching
(require 'paren)
(setq show-paren-style 'parenthesis)
(show-paren-mode +1)

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

;; Theme
(use-package one-themes
  :ensure t)

(use-package smex
  :ensure t)
(global-set-key (kbd "M-x") 'smex)

;; Packages
(use-package magit
  :ensure t)

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
(use-package undo-tree
  :ensure t)
(global-undo-tree-mode)

(use-package goto-chg
  :ensure t)

(use-package cl-lib
  :ensure t)


(use-package evil
  :ensure t)
(evil-mode 1)

;;; Projectile
(use-package projectile
  :ensure t)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-x p") 'projectile-switch-project)
(define-key projectile-mode-map (kbd "C-x w") 'projectile-kill-buffers)
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
(use-package flycheck-golangci-lint
  :hook (go-mode . flycheck-golangci-lint-setup))


(use-package doom-themes
  :ensure t)

;; Set default font
(set-face-attribute 'default nil
                    :family "Hack"
                    :height 160
                    :weight 'normal
                    :width 'normal)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#f4eedb" "#cc1f24" "#778c00" "#a67c00" "#007ec4" "#c42475" "#11948b" "#88999b"])
 '(company-quickhelp-color-background "#D0D0D0")
 '(company-quickhelp-color-foreground "#494B53")
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#11948b")
 '(cua-normal-cursor-color "#596e76")
 '(cua-overwrite-cursor-color "#a67c00")
 '(cua-read-only-cursor-color "#778c00")
 '(custom-enabled-themes (quote (doom-one)))
 '(custom-safe-themes
   (quote
    ("2f1518e906a8b60fac943d02ad415f1d8b3933a5a7f75e307e6e9a26ef5bf570" "37144b437478e4c235824f0e94afa740ee2c7d16952e69ac3c5ed4352209eefb" "99ea831ca79a916f1bd789de366b639d09811501e8c092c85b2cb7d697777f93" "1d78d6d05d98ad5b95205670fe6022d15dabf8d131fe087752cc55df03d88595" "c0a0c2f40c110b5b212eb4f2dad6ac9cac07eb70380631151fa75556b0100063" "21055a064d6d673f666baaed35a69519841134829982cbbb76960575f43424db" "b73a23e836b3122637563ad37ae8c7533121c2ac2c8f7c87b381dd7322714cd0" "0dd2666921bd4c651c7f8a724b3416e95228a13fca1aa27dc0022f4e023bf197" "00445e6f15d31e9afaa23ed0d765850e9cd5e929be5e8e63b114a3346236c44c" "c433c87bd4b64b8ba9890e8ed64597ea0f8eb0396f4c9a9e01bd20a04d15d358" default)))
 '(fci-rule-color "#f4eedb")
 '(highlight-changes-colors (quote ("#c42475" "#5e65b6")))
 '(highlight-symbol-colors
   (quote
    ("#ecdab1" "#cce1d0" "#fbcab3" "#d8d3dc" "#dedeb2" "#f6ccae" "#d0dae0")))
 '(highlight-symbol-foreground-color "#5d737a")
 '(highlight-tail-colors
   (quote
    (("#f4eedb" . 0)
     ("#a8b84b" . 20)
     ("#66c1b3" . 30)
     ("#6fa5e7" . 50)
     ("#d6a549" . 60)
     ("#ed6e3e" . 70)
     ("#f46495" . 85)
     ("#f4eedb" . 100))))
 '(hl-bg-colors
   (quote
    ("#d6a549" "#ed6e3e" "#ff6243" "#f46495" "#837bdf" "#6fa5e7" "#66c1b3" "#a8b84b")))
 '(hl-fg-colors
   (quote
    ("#fffce9" "#fffce9" "#fffce9" "#fffce9" "#fffce9" "#fffce9" "#fffce9" "#fffce9")))
 '(hl-paren-colors (quote ("#11948b" "#a67c00" "#007ec4" "#5e65b6" "#778c00")))
 '(lsp-ui-doc-border "#5d737a")
 '(nrepl-message-colors
   (quote
    ("#cc1f24" "#bb3e06" "#a67c00" "#4f6600" "#a8b84b" "#005797" "#11948b" "#c42475" "#5e65b6")))
 '(package-selected-packages
   (quote
    (doom-themes goto-chg github-theme ayu-theme one-themes lsp-mode solarized-theme r swiper git-gutter docker-compose-mode dockerfile-mode use-package)))
 '(pdf-view-midnight-colors (quote ("#969896" . "#f8eec7")))
 '(pos-tip-background-color "#f4eedb")
 '(pos-tip-foreground-color "#5d737a")
 '(smartrep-mode-line-active-bg (solarized-color-blend "#778c00" "#f4eedb" 0.2))
 '(term-default-bg-color "#fffce9")
 '(term-default-fg-color "#596e76")
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#cc1f24")
     (40 . "#bb5918")
     (60 . "#b16b0f")
     (80 . "#a67c00")
     (100 . "#978100")
     (120 . "#8f8400")
     (140 . "#878700")
     (160 . "#7f8900")
     (180 . "#778c00")
     (200 . "#698e3f")
     (220 . "#5f8f53")
     (240 . "#519166")
     (260 . "#3d9278")
     (280 . "#11948b")
     (300 . "#1a8ba1")
     (320 . "#1986ad")
     (340 . "#1282b8")
     (360 . "#007ec4"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#fffce9" "#f4eedb" "#990001" "#cc1f24" "#4f6600" "#778c00" "#785700" "#a67c00" "#005797" "#007ec4" "#93004d" "#c42475" "#006d68" "#11948b" "#596e76" "#88999b")))
 '(xterm-color-names
   ["#f4eedb" "#cc1f24" "#778c00" "#a67c00" "#007ec4" "#c42475" "#11948b" "#002b37"])
 '(xterm-color-names-bright
   ["#fffce9" "#bb3e06" "#98a6a6" "#88999b" "#596e76" "#5e65b6" "#5d737a" "#00212b"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
