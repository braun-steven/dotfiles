;;; ../dotfiles/configs/emacs/.doom.d/+modules/lang/latex/keybindings.el -*- lexical-binding: t; -*-

(map! :map typst-ts-mode-map

      :nv "," nil

      :localleader

      :desc "View" "v"     #'typst-ts-mode-preview
      :desc "Watch" "w"     #'typst-ts-mode-watch-toggle
      :desc "Compile" "c"     #'typst-ts-mode-compile-and-preview)
