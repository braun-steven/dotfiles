;;; ../dotfiles/configs/emacs/.doom.d/ui/+theme.el -*- lexical-binding: t; -*-
(load! "+functions")
(load! "+keybindings")

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. These are the defaults.
(setq slang/theme-light 'modus-operandi)
(setq slang/theme-dark 'doom-one)
(setq doom-theme slang/theme-dark)


(use-package! modus-themes
  :init
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs t
        modus-themes-subtle-line-numbers nil
        modus-themes-syntax '(green-strings yellow-comments)
        modus-themes-markup '(background)
        ;; modus-themes-syntax '(faint green-strings yellow-comments)
        modus-themes-bold-constructs nil)

  ;; Load the theme files before enabling a theme
  (modus-themes-load-themes)
  :config
  ;; Load the theme of your choice:
  ;; :bind ("<f5>" . modus-themes-toggle)
  )


;; ;; Theme based on daytime/long/lat
;; (use-package! circadian
;;         :config
;;         (setq calendar-latitude 49.9)
;;         (setq calendar-longitude 8.2)
;;         (setq circadian-themes `((:sunrise . ,slang/theme-light)
;;                                 (:sunset  . ,slang/theme-dark)))
;;         ;; ;; Add pdf view mode hook to enable pdf midnight mode on theme change
;;         ;; (add-hook 'circadian-after-load-theme-hook
;;         ;;         #'(lambda (theme)
;;         ;;                 (if (eq theme slang/theme-dark )
;;         ;;                 (add-hook 'pdf-view-mode-hook 'slang/enable-pdf-view-midnight-minor-mode)
;;         ;;                 (remove-hook 'pdf-view-mode-hook 'slang/enable-pdf-view-midnight-minor-mode))
;;         ;;                 ))

;;         ;; ;; Set a global variable to the active theme set by circadian el
;;         ;; (add-hook 'circadian-after-load-theme-hook
;;         ;;         #'(lambda (theme)
;;         ;;                 (setq slang/global-active-theme theme)))


;;         (circadian-setup))
