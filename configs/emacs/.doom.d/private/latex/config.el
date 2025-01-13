;;; ../dotfiles/configs/emacs/.doom.d/lang/+latex.el -*- lexical-binding: t; -*-
(after! tex
  (load! "+functions")
  (load! "+keybindings")

  ;; Make latex sections have a larger font
  (setq font-latex-fontify-sectioning 1.1)

  ;; Add auto-fillmode (auto line brake after 80 chars)
  ;; (add-hook 'TeX-mode-hook #'auto-fill-mode)
  (add-hook 'LaTeX-mode-hook (lambda () (setq TeX-command-default "LaTeXmk")))


 ;;  (custom-set-variables
 ;; '(tex-font-script-display (quote (-0.0 0.0)))
 ;; '(tex-suscript-height-ratio 1.0)
 ;;   )

  ;; Also fontify commonly custom commands defined by \newcomand
  ;; (add-hook 'TeX-mode-hook #'(lambda () (push '("citeauthorandref" "[{") font-latex-match-reference-keywords)))

  ;; Set latex viewer
  (setq +latex-viewers '(pdf-tools))
  (setq TeX-view-program-selection '(
        ;; (output-pdf "vince")
        ;; (output-pdf "Zathura")
        (output-pdf "PDF Tools")
        ((output-dvi has-no-display-manager)
        "dvi2tty")
        ((output-dvi style-pstricks)
        "dvips and gv")
        (output-dvi "xdvi")
        (output-html "xdg-open")
        ;; (output-pdf "preview-pane")
        ))
  )
