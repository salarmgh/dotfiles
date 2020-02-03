;; Melpa repository
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
2. Remove this warning from your init file so you won't see it again."
          ))
  (add-to-list 'package-archives (
    cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    (add-to-list 'package-archives (
      cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

;; Install packages
; Packages list
(setq package-list '(web-mode flycheck tide elpy py-autopep8
                              git-gutter ido-vertical-mode
                              darkburn-theme magit smex
                              fill-column-indicator undo-fu yaml-mode
                              dockerfile-mode docker-compose-mode))
; Update repo
(unless package-archive-contents
  (package-refresh-contents))

; Install packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

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
 '(custom-enabled-themes (quote (doom-tomorrow-night)))
 '(custom-safe-themes
   (quote
    ("cb96a06ed8f47b07c014e8637bd0fd0e6c555364171504680ac41930cfe5e11e" "fa3bdd59ea708164e7821574822ab82a3c51e262d419df941f26d64d015c90ee" "e1ef2d5b8091f4953fe17b4ca3dd143d476c106e221d92ded38614266cea3c8b" "1d50bd38eed63d8de5fcfce37c4bb2f660a02d3dff9cbfd807a309db671ff1af" "615123f602c56139c8170c153208406bf467804785007cdc11ba73d18c3a248b" "07e3a1323eb29844e0de052b05e21e03ae2f55695c11f5d68d61fb5fed722dd2" "229c5cf9c9bd4012be621d271320036c69a14758f70e60385e87880b46d60780" "51956e440cec75ba7e4cff6c79f4f8c884a50b220e78e5e05145386f5b381f7b" "72fda75af7caddec17ba9b49d2f99703c20a5f5f5c4dcec641d34a0b83569e88" "c83c095dd01cde64b631fb0fe5980587deec3834dc55144a6e78ff91ebc80b19" "d5d2ab76985738c142adbe6a35dc51c8d15baf612fdf6745c901856457650314" "2cdc13ef8c76a22daa0f46370011f54e79bae00d5736340a5ddfe656a767fddf" "7f791f743870983b9bb90c8285e1e0ba1bf1ea6e9c9a02c60335899ba20f3c94" "d5f8099d98174116cba9912fe2a0c3196a7cd405d12fa6b9375c55fc510988b5" "7c4cfa4eb784539d6e09ecc118428cd8125d6aa3053d8e8413f31a7293d43169" "99ea831ca79a916f1bd789de366b639d09811501e8c092c85b2cb7d697777f93" "e074be1c799b509f52870ee596a5977b519f6d269455b84ed998666cf6fc802a" "774aa2e67af37a26625f8b8c86f4557edb0bac5426ae061991a7a1a4b1c7e375" "be9645aaa8c11f76a10bcf36aaf83f54f4587ced1b9b679b55639c87404e2499" "0ad7f1c71fd0289f7549f0454c9b12005eddf9b76b7ead32a24d9cb1d16cbcbd" "6231254e74298a1cf8a5fee7ca64352943de4b495e615c449e9bb27e2ccae709" "6de37d6d573e18138aa948683c8ff0e72b89e90d1cdbf683787ea72f8e6295ab" "845103fcb9b091b0958171653a4413ccfad35552bc39697d448941bcbe5a660d" "6bacece4cf10ea7dd5eae5bfc1019888f0cb62059ff905f37b33eec145a6a430" "777a3a89c0b7436e37f6fa8f350cbbff80bcc1255f0c16ab7c1e82041b06fccd" "a339f231e63aab2a17740e5b3965469e8c0b85eccdfb1f9dbd58a30bdad8562b" "d71aabbbd692b54b6263bfe016607f93553ea214bc1435d17de98894a5c3a086" "e801830e362310e0654d7931bb10607463dec414d82107e94be224c76d5c5877" "001e4dbbdb8d01bb299c0244c489504d51ef5939ace24049079b377294786f7c" "e1ecb0536abec692b5a5e845067d75273fe36f24d01210bf0aa5842f2a7e029f" "1ed5c8b7478d505a358f578c00b58b430dde379b856fbcb60ed8d345fc95594e" "1c8171893a9a0ce55cb7706766e57707787962e43330d7b0b6b0754ed5283cda" "bc836bf29eab22d7e5b4c142d201bcce351806b7c1f94955ccafab8ce5b20208" "c7f10959cb1bc7a36ee355c765a1768d48929ec55dde137da51077ac7f899521" default)))
 '(elpy-modules
   (quote
    (elpy-module-company elpy-module-eldoc elpy-module-pyvenv elpy-module-yasnippet elpy-module-django elpy-module-sane-defaults)))
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(package-selected-packages
   (quote
    (doom-themes magit smex ido-vertical-mode git-gutter undo-fu php-mode docker-compose-mode yaml-mode blacken py-autopep8 elpy darkburn-theme ## company flycheck-package web-mode tide))))
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
;(global-set-key (kbd "C-x b") 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ido-switch-buffer)
(setq ibuffer-expert t)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-l") 'magit-log-current)
(global-set-key (kbd "C-x M-d") 'magit-diff-unstaged)
(global-set-key (kbd "C-x C-b") 'magit-blame-addition)
(global-set-key (kbd "C-x M-b") 'magit-blame-quit)
(global-set-key (kbd "C-x M-p") 'magit-pull-from-upstream)
(global-set-key (kbd "C-x M-u") 'magit-push-current-to-upstream)





(require 'fill-column-indicator)
(add-hook 'emacs-lisp-mode-hook (lambda ()
    (fci-mode 1)
  ))
