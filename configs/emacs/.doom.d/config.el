;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; refresh' after modifying this file!

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
(if (eq system-type 'darwin)
    (setq doom-font (font-spec :family "Hack" :size 14.0))  ;; MacOS
  (setq doom-font (font-spec :family "Hack" :size 11.0))  ;; Linux
  )
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

;; Company config
;; (after! company
;;   (setq
;;    company-minimum-prefix-length 3
;;    company-idle-delay 0.25
;;    company-tooltip-idle-delay 1.0)
;;   ;; Set text mode backends to yasnippet only (removes company-dabbrev and company-ispell since they spam the completion list)
;;   (setf (cdr (assoc 'text-mode +company-backend-alist))
;;         '(:separate company-yasnippet)))


(use-package! corfu
  :config
  (global-corfu-mode)
  (setq corfu-auto-delay 0.1))

;; (use-package! marginalia
;;   :config
;;   (setq marginalia-field-width 80))

;; (after! lsp
;;   ;; Fix for confusing yasnippet results in completion
;;   (setq! +lsp-company-backends
;;          (if (modulep! :editor snippets)
;;              '(:separate company-capf company-yasnippet)
;;            'company-capf)))

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
(load!  "+keybindings")

;; Load custom functions
(load! "+functions")


(after! emacs-everywhere
  (add-to-list 'emacs-everywhere-markdown-windows "Mattermost"))

;; ;; Make avy faces like vim-easymotion: no background, red to yellow foreground
;; (after! avy)


(use-package! doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  ;; (doom-themes-visual-bell-config)

  ;; Enable custom neotree theme (all-the-icons must be installed!)
  ;; (doom-themes-neotree-config)
  ;; or for treemacs users
  ;; (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  ;; (doom-themes-treemacs-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package! ssh-agency :after magit)

;; (use-package! powerthesaurus)

(use-package! powerthesaurus :after tex)

(after! magit
  ;; Set magit log margin
  (setq magit-log-margin '(t "%Y-%m-%d" magit-log-margin-width t 18))

  ;; Set magit status margin
  (setq magit-status-margin '(t age magit-log-margin-width t 18)))


(use-package! vertico-directory
  :after vertico
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word)))

;; Disable loading all known projects when opening a single project. If set to true, this may greatly reduce performance.
(setq lsp-pyright-multi-root nil)


(after! (projectile)
  (add-hook 'projectile-after-switch-project-hook #'activate-project-conda-env-maybe))


;; Typst
(use-package! typst-ts-mode
  :custom
  (typst-ts-mode-watch-options "--open"))

(use-package! avy
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

(after! lsp-mode
  ;; https://github.com/emacs-lsp/lsp-mode/issues/3577#issuecomment-1709232622
  (delete 'lsp-terraform lsp-client-packages))


;; If pressing tab to complete sometimes doesn't work you might want to bind completion to another key or try:
(after! (evil copilot)
  ;; Define the custom function that either accepts the completion or does the default behavior
  (defun my/copilot-tab-or-default ()
    (interactive)
    (if (and (bound-and-true-p copilot-mode)
             ;; Add any other conditions to check for active copilot suggestions if necessary
             )
        (copilot-accept-completion)
      (evil-insert 1))) ; Default action to insert a tab. Adjust as needed.

  ;; Bind the custom function to <tab> in Evil's insert state
  (evil-define-key 'insert 'global (kbd "<tab>") 'my/copilot-tab-or-default))

;; Load private modules
(dolist (file (directory-files "~/.doom.d/private/" t directory-files-no-dot-files-regexp))
  (if (file-directory-p file)
      (load! (concat file "/config.el"))))

