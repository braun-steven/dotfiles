;;; ../dotfiles/configs/emacs/.doom.d/+modules/ui/theme/functions.el -*- lexical-binding: t; -*-

(defun slang/load-doom-theme (theme)
  "Disable active themes and load a Doom theme.
   Source: https://www.reddit.com/r/emacs/comments/ezetx0/doomthemes_screenshots_updated_good_time_to_go/"
  (interactive (list (intern (completing-read "Theme: "
                                              (->> (custom-available-themes)
                                                   (-map #'symbol-name)
                                                   (--select (string-prefix-p "doom-" it)))))))
  (ap/load-theme theme))

(defun slang/load-theme (theme)
  "Disable active themes and load THEME.
   Source: https://www.reddit.com/r/emacs/comments/ezetx0/doomthemes_screenshots_updated_good_time_to_go/"
  (interactive (list (intern (completing-read "Theme: "
                                              (->> (custom-available-themes)
                                                   (-map #'symbol-name))))))
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme 'no-confirm))


(defun slang/load-theme-light ()
  "Load the light theme saved in the slang/theme-light variable."
  (interactive)
  (load-theme slang/theme-light))

(defun slang/load-theme-dark ()
  "Load the light theme saved in the slang/theme-light variable."
  (interactive)
  (load-theme slang/theme-dark))
