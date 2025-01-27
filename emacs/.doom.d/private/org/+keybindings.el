;;; ../dotfiles/configs/emacs/.doom.d/+modules/lang/org/keybindings.el -*- lexical-binding: t; -*-

(map! :after org
      :map org-mode-map
      :desc "Export Repeat" "<f5>" #'sbraun/org-export-dispatch-repeat-last-action)

(map! :localleader
      :map org-mode-map
      (:prefix ("t" "text markup")
               :desc "italic" "i" #'(lambda () (interactive) (sbraun/org-emphasize-dwim ?/))
               :desc "bold"   "b" #'(lambda () (interactive) (sbraun/org-emphasize-dwim ?*))
               :desc "code"   "c" #'(lambda () (interactive) (sbraun/org-emphasize-dwim ?=))
               :desc "strike" "s" #'(lambda () (interactive) (sbraun/org-emphasize-dwim ?+))))
