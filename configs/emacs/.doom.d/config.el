;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; (defvar my-message-log-file "/tmp/emacs.log"
;;   "Path to the log file where messages are appended.")

;; (defun my-log-message (log-message)
;;   "Append LOG-MESSAGE to the log file."
;;   (when (and my-message-log-file (stringp my-message-log-file))
;;     (with-temp-buffer
;;       (insert log-message "\n")
;;       (append-to-file (point-min) (point-max) my-message-log-file))))

;; (defadvice message (after my-redirect activate)
;;   (let ((log-message (apply #'format (ad-get-args 0))))
;;     (my-log-message log-message)))

;; (ad-activate 'message)


;; Place your private configuration here! Remember, you do not need to run 'doom
;; refresh' after modifying this file!

(use-package! benchmark-init
  :ensure t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

;; Set garbace collection threshold to 100MB
(setq gc-cons-threshold 100000000)

;; Set frame title
(setq frame-title-format "%b - Emacs")

;; Don't confirm killing emacs
(setq confirm-kill-emacs nil)

;; Auto unbind overridden keys
(general-auto-unbind-keys)

;; Disable dashboard banner
(defun doom-dashboard-widget-banner ())

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Steven Braun"
      user-mail-address "steven.braun.mz@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (if (eq system-type 'darwin)
;; (setq doom-font (font-spec :family "Hack" :size 14.0))  ;; MacOS
;; (setq doom-font (font-spec :family "Hack" :size 12))  ;; Linux
(setq doom-font (font-spec :family "CommitMono" :size 13.5))

;;   )
;; (setq doom-font (font-spec :family "Consolas" :size 12.0))
;; (setq doom-font (font-spec :family "Inconsolata" :size 13.0))
;; (setq doom-font (font-spec :family "Iosevka" :size 20))
;; (setq doom-font (font-spec :family "DejaVu Sans Mono" :size 19))
;; (setq doom-font (font-spec :family "IBM Plex Mono" :size 22))
;; (setq doom-font (font-spec :family "DroidSansMono Nerd Font" :size 20))
;; (setq doom-variable-pitch-font (font-spec :family "DejaVu Serif" :size 25 :weight 'semi-light))
;; (setq doom-variable-pitch-font (font-spec :family "Source Sans Pro" :size 28 :weight 'semi-light))

;; Add some more space between the lines
;; (setq line-spacing 0.17)


;; If you want to change the style of line numbers, change this to `relative' or
;; `nil' to disable it:
(setq display-line-numbers-type nil)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', where Emacs
;;   looks when you load packages with `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

;; Set local leader to ","
(setq doom-localleader-key ",")

;; Enable reload of buffers when they change on disk
(global-auto-revert-mode t)

;; Enable highlight line
(global-hl-line-mode 1)
(add-hook! 'rainbow-mode-hook
  (hl-line-mode (if rainbow-mode -1 +1)))

;; Enable word wrap mode
(+global-word-wrap-mode)

;; Emacs config location
(setq emacs-dir (file-name-as-directory "~/.doom.d"))

;; Better scrolling
(setq scroll-margin 3)

;; Set which-key delay
(setq which-key-idle-delay 0.75)
(setq which-key-idle-secondary-delay 0.5)

;; Doom modeline
;; NOTE: Issues with emacsclient
(setq doom-modeline-major-mode-icon t)

;; Keybindings
(load! "+keybindings")

;; Load custom functions
(load! "+functions")

(use-package! doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package! ssh-agency
  :after magit)


(use-package! powerthesaurus
  :after tex)

(after! magit
  ;; Set magit log margin
  (setq magit-log-margin '(t "%Y-%m-%d" magit-log-margin-width t 18))

  ;; Set magit status margin
  (setq magit-status-margin '(t age magit-log-margin-width t 18)))

(use-package! magit-todos
  :after magit
  :config (magit-todos-mode 1))

(use-package! vertico
  :config
  (setq vertico-posframe-width 160))


(use-package! vertico-directory
  :after vertico
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word)))

;; Disable loading all known projects when opening a single project. If set to true, this may greatly reduce performance.
(setq lsp-pyright-multi-root nil)


(after! (projectile conda)
  (add-hook 'projectile-after-switch-project-hook #'activate-project-conda-env-maybe))



(use-package! avy
  :defer t
  :config
  ;; Avy settings
  (setq avy-orders-alist
        '((avy-goto-char . avy-order-closest)
          (avy-goto-word-0 . avy-order-closest)
          (avy-goto-char-2 . avy-order-closest)
          (avy-goto-char-timer . avy-order-closest)))
  (setq avy-single-candidate-jump t)
  (custom-set-faces!
    `(avy-lead-face :weight bold :foreground "red" :background ,(face-attribute 'default :background))
    `(avy-lead-face-0 :weight bold :foreground "dark orange" :background ,(face-attribute 'default :background))
    `(avy-lead-face-1 :weight bold :foreground "orange" :background ,(face-attribute 'default :background))
    `(avy-lead-face-2 :weight bold :foreground "gold" :background ,(face-attribute 'default :background))
    `(avy-lead-face-3 :weight bold :foreground "yellow" :background ,(face-attribute 'default :background)))
  )

;; Corfu setup
(use-package! corfu
    :config
    (setq corfu-auto-delay 0.0)
    (setq corfu-auto-prefix 3)
    (setq corfu-preselect 'prompt))

;; Orderless setup
;; (use-package! orderless
;;     :config
;;     (setq completion-styles '(orderless-flex basic)))


;; accept completion from copilot
(use-package! copilot
    :defer t
    :hook (prog-mode . copilot-mode)
    :bind (:map copilot-completion-map
            ;; ("C-f" . 'copilot-accept-completion)
            ("<tab>" . 'copilot-accept-completion)
            ("TAB" . 'copilot-accept-completion)
            ;; ("C-TAB" . 'copilot-accept-completion-by-word)
            ;; ("C-<tab>" . 'copilot-accept-completion-by-word)
            )
    :config (setq copilot-indent-offset-warning-disable t)
    )

(use-package! copilot-chat
  :after (request org markdown-mode shell-maker))


(use-package! gptel
 :config
 ;; (setq! gptel-api-key "your key")
;; OPTIONAL configuration
;; OPTIONAL configuration
        (setq
        gptel-model 'gemini-exp-1206
        gptel-backend (gptel-make-gemini "Gemini"
                        :key (lambda ()
                        (with-temp-buffer
                          (insert-file-contents "~/.gemini-api-key")
                          (buffer-string)))
                        :stream t))
 )

(use-package! transient
  :config

(defun resize-repeatable (func amount)
  "Call FUNC with AMOUNT and keep transient open."
  (funcall func amount)
  (transient-setup 'transient-resize-window))

(transient-define-prefix transient-resize-window ()
  "Transient menu for resizing the current window."
  [["Resize Window"
    ("<" "Decrease width" (lambda () (interactive) (resize-repeatable #'evil-window-decrease-width 5)))
    (">" "Increase width" (lambda () (interactive) (resize-repeatable #'evil-window-increase-width 5)))
    ("-" "Decrease width" (lambda () (interactive) (resize-repeatable #'evil-window-decrease-height 5)))
    ("+" "Increase width" (lambda () (interactive) (resize-repeatable #'evil-window-increase-height 5)))
    ("q" "Quit" transient-quit-one)]])
)


(use-package! ultra-scroll
  :init
  (setq scroll-conservatively 101 ; important!
        scroll-margin 0)
        :config
        (ultra-scroll-mode 1))


;; If pressing tab to complete sometimes doesn't work you might want to bind completion to another key or try:
;; (after! (evil copilot)
;;   ;; Define the custom function that either accepts the completion or does the default behavior
;;   (defun my/copilot-tab-or-default ()
;;     (interactive)
;;     (if (and (bound-and-true-p copilot-mode)
;;              ;; Add any other conditions to check for active copilot suggestions if necessary
;;              )
;;         (copilot-accept-completion)
;;       (evil-insert 1))) ; Default action to insert a tab. Adjust as needed.

;;   ;; Bind the custom function to <tab> in Evil's insert state
;;   (evil-define-key 'insert 'global (kbd "<tab>") 'my/copilot-tab-or-default))


;; Load private modules
(dolist (file (directory-files "~/.doom.d/private/" t directory-files-no-dot-files-regexp))
  (if (file-directory-p file)
      (load! (concat file "/config.el"))))

