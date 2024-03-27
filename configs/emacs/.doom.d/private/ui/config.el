;;; ../dotfiles/configs/emacs/.doom.d/ui/+theme.el -*- lexical-binding: t; -*-
(load! "+functions")
(load! "+keybindings")

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. These are the defaults.
;; (setq sbraun/theme-light 'modus-operandi)
(setq sbraun/theme-dark 'doom-one)
(setq doom-theme sbraun/theme-dark)

(defun set-doom-theme-based-on-file ()
  "Sets the Doom theme based on the content of the /tmp/audamo_current-theme file."
  (let ((theme-file "/tmp/audamo_current-theme"))
    (when (file-exists-p theme-file)
      (with-temp-buffer
        (insert-file-contents theme-file)
        (let ((content (string-trim (buffer-string))))
          (setq doom-theme (if (string= content "light") 'modus-operandi 'modus-vivendi)))))))

;; (set-doom-theme-based-on-file)

(use-package! circadian
  :ensure t
  :config
  (setq calendar-latitude 49.992)
  (setq calendar-longitude 8.247)
  (setq circadian-themes '((:sunrise . modus-operandi)
                           (:sunset  . modus-vivendi)))
  (circadian-setup))


;; Add set-frame-opacity to doom-switch-frame-hook
;; Necessary for new emacsclient frames
;; (setq! opacity 90)
;; (doom/set-frame-opacity opacity)
;; (add-hook! 'doom-switch-frame-hook
;;   (doom/set-frame-opacity opacity))

(use-package! modus-themes
  :init
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs nil
        modus-themes-common-palette-overrides '(
                                                ;; (comment yellow-cooler)
                                                (string green-cooler)
                                                )
        )
  )
