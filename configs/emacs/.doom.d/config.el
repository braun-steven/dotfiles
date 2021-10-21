;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; refresh' after modifying this file!

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
(setq user-full-name "Steven Lang"
      user-mail-address "steven.lang.mz@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "Hack" :size 19))
;; (setq doom-font (font-spec :family "Iosevka" :size 20))
;; (setq doom-font (font-spec :family "DejaVu Sans Mono" :size 19))
;; (setq doom-font (font-spec :family "IBM Plex Mono" :size 19))
;; (setq doom-font (font-spec :family "DroidSansMono Nerd Font" :size 20))
;; (setq doom-variable-pitch-font (font-spec :family "DejaVu Serif" :size 25 :weight 'semi-light))
;; (setq doom-variable-pitch-font (font-spec :family "Source Sans Pro" :size 28 :weight 'semi-light))

;; Add some more space between the lines
;; (setq line-spacing 0.17)


;; If you want to change the style of line numbers, change this to `relative' or
;; `nil' to disable it:
(setq display-line-numbers-type t)


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
(setq
 company-minimum-prefix-length 3
 company-idle-delay 0.1
 company-tooltip-idle-delay 1.0)

;; Emacs config location
(setq emacs-dir (file-name-as-directory "~/.doom.d"))


;; Better scrolling
(setq scroll-margin 3)

;; Set which-key delay
(setq which-key-idle-delay 1.0)
(setq which-key-idle-secondary-delay 0.5)

;; Doom modeline
;; NOTE: Issues with emacsclient
 ;; (setq doom-modeline-major-mode-icon t)

;; Keybindings
(load!  "+keybindings")

;; Load custom functions
(load! "+functions")

;; Langtool
(setq langtool-java-classpath
      "/usr/share/languagetool:/usr/share/java/languagetool/*")

;; Enable rainbow delimiters in prog mode
(use-package! rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(after! emacs-everywhere

  (add-to-list 'emacs-everywhere-markdown-windows "Mattermost"))


;; ;; Fix doom modeline icons (only issue on arch emacs binary)
;; (custom-set-faces!
;;   ;; Flycheck check symbol and sim-card symbol (right-hand side)
;;   '(doom-modeline-warning :inherit warning)
;;   '(doom-modeline-debug :inherit font-lock-doc-face :slant normal)

;;   ;; Insert/normal state (left-hand side)
;;   '(doom-modeline-evil-emacs-state :inherit font-lock-builtin-face)
;;   '(doom-modeline-evil-insert-state :inherit font-lock-keyword-face)
;;   '(doom-modeline-info :inherit success))


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

(use-package! ssh-agency)

(use-package! powerthesaurus
  :after tex)

(after! emacs-everywhere
  (push "Mattermost" emacs-everywhere-markdown-apps)
  (push "GitHub" emacs-everywhere-markdown-windows))

(after! magit
  ;; Set magit log margin
  (setq magit-log-margin '(t age magit-log-margin-width t 18))

  ;; Set magit status margin
  (setq magit-status-margin '(t age magit-log-margin-width t 18)))



;; Enable auto-fill-mode everywhere
;; TODO: Choose some sane defaults for specific modes?
(auto-fill-mode 1)

(use-package vertico-directory
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word)))

;; Disable loading all known projects when opening a single project. If set to true, this may greatly reduce performance.
(setq lsp-pyright-multi-root nil)


;; Load private modules
(dolist (file (directory-files "~/.doom.d/private/" t directory-files-no-dot-files-regexp))
        (load! (concat file "/config.el")))
