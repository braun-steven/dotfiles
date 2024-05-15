;;; ../dotfiles/configs/emacs/.doom.d/ui/+theme.el -*- lexical-binding: t; -*-
(load! "+functions")
(load! "+keybindings")

(setq modus-themes-italic-constructs t
      modus-themes-bold-constructs nil
      modus-themes-common-palette-overrides '(
                                        ;; (string green)
                                        )
      )

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. These are the defaults.
(setq sbraun/theme-dark 'doom-nord)
(setq sbraun/theme-light 'modus-operandi)
(setq doom-theme sbraun/theme-dark)

(use-package! circadian
  :config
  (setq calendar-latitude 49.992)
  (setq calendar-longitude 8.247)
  (setq circadian-themes `((:sunrise . ,sbraun/theme-light)
                           (:sunset  . ,sbraun/theme-dark)))
  (circadian-setup))


;; Add set-frame-opacity to doom-switch-frame-hook
;; Necessary for new emacsclient frames
;; (setq! opacity 90)
;; (doom/set-frame-opacity opacity)
;; (add-hook! 'doom-switch-frame-hook
;;   (doom/set-frame-opacity opacity))
