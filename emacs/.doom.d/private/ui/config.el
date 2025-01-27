;;; ../dotfiles/configs/emacs/.doom.d/ui/+theme.el -*- lexical-binding: t; -*-
(load! "+functions")
(load! "+keybindings")

(setq modus-themes-italic-constructs t
      modus-themes-bold-constructs nil
      modus-themes-common-palette-overrides '(
                                        ;; (string green)  ;; Note: default blue has better contrast to other black chars
                                        )
      )

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. These are the defaults.

(defun set-doom-theme-based-on-darkman ()
  "Set the doom theme based on the output of `darkman get`."
  (interactive)
  (let ((theme-output (string-trim (shell-command-to-string "darkman get"))))
    (if (string= theme-output "dark")
        (setq doom-theme sbraun/theme-dark)
      (setq doom-theme sbraun/theme-light))
    (load-theme doom-theme t)))

(defun check-and-update-theme ()
  "Check the current theme using `darkman get` and update if necessary."
  (let ((current-theme (if (string= (string-trim (shell-command-to-string "darkman get")) "dark")
                           sbraun/theme-dark
                         sbraun/theme-light)))
    (unless (eq doom-theme current-theme)
      (setq doom-theme current-theme)
      (load-theme doom-theme t))))

;; Define the themes
(setq sbraun/theme-dark 'doom-one)
(setq sbraun/theme-light 'modus-operandi)

;; Initially set the theme based on the current theme
;; (set-doom-theme-based-on-darkman)
(setq doom-theme 'doom-one)

;; Set a timer to check every 5 minutes (300 seconds)
;; (run-at-time "1 min" 60 'check-and-update-theme)

;; Frame transparency
(doom/set-frame-opacity 100)
