;;; ../dotfiles/configs/emacs/.doom.d/ui/+theme.el -*- lexical-binding: t; -*-
(load! "+functions")
(load! "+keybindings")

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. These are the defaults.
;; (setq sbraun/theme-light 'modus-operandi)
(setq sbraun/theme-dark 'doom-nord)
(setq doom-theme sbraun/theme-dark)

(use-package! modus-themes
  :init
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs nil
        modus-themes-common-palette-overrides '((comment yellow-cooler)
                                                (string green-cooler))
        )
  )
