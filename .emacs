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


;;; web-mode --- syntax highlighitng for web files
;;; Commentary:
;;; Add support for syntax highlighting for web files
(require 'web-mode)
(defun web-mode-init-hook ()
  "Hooks for Web mode.  Adjust indent."
  (setq web-mode-markup-indent-offset 2)) ; set indentation for web-mode
(add-hook 'web-mode-hook  'web-mode-init-hook) ; enable web-mode
(add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode)) ; enable js syntax highlighting
(setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'"))) ; enable jsx highlighting inside js files
(setq web-mode-enable-auto-closing t) ; enable auto closing tag


;;; flycheck --- lang lint
;;; Commentary:
;;; Enable syntax checking for javascript and python
(require 'flycheck)
; disable default syntax checker
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint json-jsonlist)))

(flycheck-add-mode 'javascript-eslint 'web-mode) ; enable syntax checker for web-mode
(add-hook 'after-init-hook #'global-flycheck-mode) ; globally enable syntax checker
(add-hook 'flycheck-mode-hook 'add-node-modules-path) ; use modules path for local eslint

; prettier for web-mode
(defun web-mode-init-prettier-hook ()
  (add-node-modules-path)
  (prettier-js-mode))

; enable prettier for web-mode
(add-hook 'web-mode-hook  'web-mode-init-prettier-hook)

; enable emmet-mode
(add-hook 'web-mode-hook  'emmet-mode)

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
 '(custom-enabled-themes (quote (wombat))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;; .emacs ends here
