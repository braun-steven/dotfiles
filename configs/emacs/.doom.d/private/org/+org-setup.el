;; Hide emphasis markers like /foo/ and *bar*
(setq org-hide-emphasis-markers t)

;; ;; Bigger latex fragments
;; (plist-put org-format-latex-options :scale 2)


;; ;; Org-Mode todo keywords
(setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "STARTED(s)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c)")))

;; ;; Org-ref setup
;; ;; (use-package! org-ref
;; ;;   :after org
;; ;;   :config
;; ;;   (setq reftex-default-bibliography '("~/org/bib/refereces.bib")
;; ;;         org-ref-bibliography-notes "~/org/bib/notes.org"
;; ;;         org-ref-default-bibliography '("~/org/bib/references.bib")
;; ;;         bibtex-completion-bibliography "~/org/bib/references.bib"
;; ;;         bibtex-completion-library-path "~/org/bib/bibtex-pdfs"
;; ;;         bibtex-completion-notes-path "~/org/bib/helm-bibtex-notes"))



;; ;; ox-latex
;; (use-package! ox-latex
;;   :after org
;;   :config
;;   (add-to-list 'org-latex-classes
;;                '("tudabeamer"
;;                  "\\documentclass\[presentation\]\{tudabeamer\}"
;;                  ("\\section\{%s\}" . "\\section*\{%s\}")
;;                  ("\\subsection\{%s\}" . "\\subsection*\{%s\}")
;;                  ("\\subsubsection\{%s\}" . "\\subsubsection*\{%s\}")))
;;   (add-to-list 'org-latex-packages-alist '("" "listings"))
;;   (add-to-list 'org-latex-packages-alist '("" "color"))
;;   (add-to-list 'org-latex-packages-alist '("" "tikz"))
;;   (setq org-latex-pdf-process '("latexmk -f -pdf %f -output-directory=%o"))
;;   (setq org-latex-listings t))

;; ;; ox-beamer translates *bold* to \alert{}, the following function reverts this
;; (defun my-beamer-bold (contents backend info)
;;     (when (eq backend 'beamer)
;;     (replace-regexp-in-string "\\`\\\\[A-Za-z0-9]+" "\\\\textbf" contents)))

;; (add-to-list 'org-export-filter-bold-functions 'my-beamer-bold)

;; ;; Make exports async by default
;; ;; (setq org-export-in-background t)


;; ;; Make TAB cycle all subtrees
(after! evil-org
  (remove-hook 'org-tab-first-hook #'+org-cycle-only-current-subtree-h))

;; Toggle latex fragments automatically with pointer
;; (add-hook 'org-mode-hook 'org-fragtog-mode)

;; ;;;;;;;;;;;;;;;;;;; Org-Roam START
;; (use-package! deft
;;   :after org
;;   :defer
;;   :config
;;   ;; (setq deft-recursive t)
;;   ;; (setq deft-use-filter-string-for-filename t)
;;   ;; (setq deft-default-extension "org")
;;   (setq deft-use-filename-as-title t))

;; (use-package! org-roam
;;   :after org
;;   :defer
;;   :config
;;   (setq org-roam-directory deft-directory))
;; ;;Org-Roam END



;; ;; Enable latex previews at startup
;; ;; (setq org-startup-with-latex-preview nil)

;; ;; Latex tikz
;; (eval-after-load "preview"
;;   '(add-to-list 'preview-default-preamble "\\PreviewEnvironment{tikzpicture}" t))
;; (setq org-preview-latex-default-process 'imagemagick)


;; (setq slang/doom-one-colors-alist '((red . "#ff6c6b")
;;                                     (green . "#98be65")
;;                                     (yellow . "#ECBE7B")
;;                                     (blue . "#51afef")
;;                                     (magenta . "#c678dd")
;;                                     (cyan . "#46D9FF")))


(setq org-roam-capture-templates
      '(
        ("d" "default" plain "%?" :target
         (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
         :unnarrowed t)
        ("m" "meeting" plain (file "~/org/notes/phd/meetings/template.org")
        :target (file+head "~/org/notes/phd/meetings/misc.org" "#+title: ${title}\n")
         :unnarrowed t)
        ))
