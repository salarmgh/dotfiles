;;; melpa --- Add Melpa repository
;;; Commentary:
;;; Add melpa for web-mode, flycheck, emmet mode
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
;;; Code:
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

; list of packages
(setq package-list '(web-mode flycheck emmet-mode prettier-js add-node-modules-path darkburn-theme better-defaults elpy py-autopep8 blacken git-gutter))

; update repo
(unless package-archive-contents
  (package-refresh-contents))

; install packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; the startup message
(setq inhibit-startup-message t)

;; disable tabs
(setq-default indent-tabs-mode nil)

;; Enable auto pair
(electric-pair-mode 1)

;; Enable autopep8
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

;; Enable pair highlighting
(require 'paren)
(setq show-paren-style 'parenthesis)
(show-paren-mode +1)

;;; web-mode --- syntax highlighitng for web files
;;; Commentary:
;;; Add support for syntax highlighting for web files
(setq-default indent-tabs-mode nil)
(require 'web-mode)
(defun web-mode-init-hook ()
  "Hooks for Web mode.  Adjust indent."
  (setq web-mode-markup-indent-offset 2)) ; set indentation for web-mode
(add-hook 'web-mode-hook  'web-mode-init-hook) ; enable web-mode
;; set paddings
(setq web-mode-script-padding 2)
(setq web-mode-style-padding 2)
(setq web-mode-block-padding 2)
(add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode)) ; enable js syntax highlighting
(setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'"))) ; enable jsx highlighting inside js files
(setq web-mode-enable-auto-closing t) ; enable auto closing tag
(local-set-key (kbd "RET") 'newline-and-indent) ; set indent on new line
(setq web-mode-code-indent-offset 2) ; set indent

; enable elpy
(elpy-enable)
(with-eval-after-load 'python
  (defun python-shell-completion-native-try ()
    "Return non-nil if can trigger native completion."
    (let ((python-shell-completion-native-enable t)
          (python-shell-completion-native-output-timeout
           python-shell-completion-native-try-output-timeout))
      (python-shell-completion-native-get-completions
       (get-buffer-process (current-buffer))
       nil "_"))))

;;; flycheck --- lang lint
;;; Commentary:
;;; Enable syntax checking for javascript and python
(require 'flycheck)
; disable default syntax checker
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint json-jsonlist)))
(setq elpy-modules (delq 'elpy-module-flymake elpy-modules))

(flycheck-add-mode 'javascript-eslint 'web-mode) ; enable syntax checker for web-mode
(add-hook 'after-init-hook #'global-flycheck-mode) ; globally enable syntax checker
(add-hook 'flycheck-mode-hook 'add-node-modules-path) ; use modules path for local eslint
(add-hook 'elpy-mode-hook 'flycheck-mode)

; prettier for web-mode
(defun web-mode-init-prettier-hook ()
  (add-node-modules-path)
  (prettier-js-mode))

; enable prettier for web-mode
(add-hook 'web-mode-hook  'web-mode-init-prettier-hook)

; enable emmet-mode
(add-hook 'web-mode-hook  'emmet-mode)

; enable git gutter
(global-git-gutter-mode +1)
 (custom-set-variables
 '(git-gutter:modified-sign "~")
 '(git-gutter:added-sign "+")
 '(git-gutter:deleted-sign "-"))

(set-face-foreground 'git-gutter:modified "purple")
(set-face-background 'git-gutter:modified "black")
(set-face-foreground 'git-gutter:added "green")
(set-face-foreground 'git-gutter:deleted "red")

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

; display lines
(global-display-line-numbers-mode)

; (meta n) for insert line below
(define-key global-map [(meta n)] 'vi-open-below)
; (meta shift n) for insert line above
(define-key global-map [(meta shift n)] 'vi-open-above)
; (meta [) for comment region
(define-key global-map [(meta \[)] 'comment-region)
; (meta ]) for uncomment region
(define-key global-map [(meta \])] 'uncomment-region)

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
; (m p) paste from clipboard
(global-set-key (kbd "M-p") 'paste-from-clipboard)
; (m c) copy to clipboard
(global-set-key (kbd "M-c") 'copy-to-clipboard)

; theme setup
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(custom-enabled-themes (quote (darkburn)))
 '(custom-safe-themes
   (quote
    ("c7f10959cb1bc7a36ee355c765a1768d48929ec55dde137da51077ac7f899521" "0eccc893d77f889322d6299bec0f2263bffb6d3ecc79ccef76f1a2988859419e" "cdb4ffdecc682978da78700a461cdc77456c3a6df1c1803ae2dd55c59fa703e3" "a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a" "732b807b0543855541743429c9979ebfb363e27ec91e82f463c91e68c772f6e3" default)))
 '(fci-rule-color "#383838")
 '(hl-sexp-background-color "#121212")
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(package-selected-packages
   (quote
    (git-gutter darkburn-theme zeno-theme zenburn-theme material-theme ## web-mode prettier-js json-mode js2-mode flycheck-color-mode-line exec-path-from-shell emmet-mode add-node-modules-path)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;; .emacs ends here




