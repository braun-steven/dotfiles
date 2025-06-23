;;; ../dotfiles/configs/emacs/.doom.d/+modules/ui/theme/functions.el -*- lexical-binding: t; -*-

(defun sbraun/load-doom-theme (theme)
  "Disable active themes and load a Doom theme.
   Source: https://www.reddit.com/r/emacs/comments/ezetx0/doomthemes_screenshots_updated_good_time_to_go/"
  (interactive (list (intern (completing-read "Theme: "
                                              (->> (custom-available-themes)
                                                   (-map #'symbol-name)
                                                   (--select (string-prefix-p "doom-" it)))))))
  (sbraun/load-theme theme))

(defun sbraun/load-theme (theme)
  "Disable active themes and load THEME.
   Source: https://www.reddit.com/r/emacs/comments/ezetx0/doomthemes_screenshots_updated_good_time_to_go/"
  (interactive (list (intern (completing-read "Theme: "
                                              (->> (custom-available-themes)
                                                   (-map #'symbol-name))))))
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme 'no-confirm))


(defun sbraun/load-theme-light ()
  "Load the light theme saved in the sbraun/theme-light variable."
  (interactive)
  (load-theme sbraun/theme-light))

(defun sbraun/load-theme-dark ()
  "Load the light theme saved in the sbraun/theme-light variable."
  (interactive)
  (load-theme sbraun/theme-dark))

(defun sbraun/decrease-font-size (&optional increment)
  "Shrinks the font size across the current and child frames."
  (interactive "p")
  (doom-adjust-font-size (* (- 0.5) (or increment doom-font-increment))))


(defun sbraun/increase-font-size (&optional increment)
  "Shrinks the font size across the current and child frames."
  (interactive "p")
  (doom-adjust-font-size (* (+ 0.5) (or increment doom-font-increment))))
