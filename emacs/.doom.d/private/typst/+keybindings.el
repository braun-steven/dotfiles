;;; ../dotfiles/configs/emacs/.doom.d/+modules/typst/keybindings.el -*- lexical-binding: t; -*-

(map! :map typst-ts-mode-map

      ;; :nv "," nil

      :nv "C-<return>" #'typst-ts-editing-meta-return

      :localleader

      :desc "View" "v"     #'typst-ts-preview
      :desc "Watch" "w"     #'typst-ts-watch-mode
      :desc "Compile" "c"     #'typst-ts-mode-compile-and-preview
      :desc "Format" ","    #'sbraun/typstfmt-current-buffer

      (:prefix ("i" . "Insert")
               "b" #'sbraun/insert-bibtex-key-at-point
               "l" #'sbraun/insert-label-from-buffer
               )
      )
