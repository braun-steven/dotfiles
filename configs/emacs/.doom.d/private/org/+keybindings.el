;;; ../dotfiles/configs/emacs/.doom.d/+modules/lang/org/keybindings.el -*- lexical-binding: t; -*-

(map! :after org
      :map org-mode-map
      :desc "Export Repeat" "<f5>" #'sbraun/org-export-dispatch-repeat-last-action)
