;;; ../dotfiles/configs/emacs/.doom.d/lang/+org.el -*- lexical-binding: t; -*-

;; Set directioreis of org, org-roam and deft
(setq deft-directory "~/org/notes/")
(setq org-directory "~/org/")
(setq org-roam-directory "~/org/notes")

;; Org setup
(after! org

  (load! "+functions")
  (load! "+keybindings")


   ;; Add variable-pitch mode to org-mode (for nicer writing experience)
   ;; (add-hook 'org-mode-hook 'variable-pitch-mode)

  (set-face-attribute 'org-level-1 nil :height 1.20 :background nil)
  (set-face-attribute 'org-level-2 nil :height 1.10 :background nil)
  (set-face-attribute 'org-level-3 nil :height 1.05 :background nil)
  (set-face-attribute 'org-level-4 nil :height 1.00 :background nil)
  (set-face-attribute 'org-level-5 nil :height 1.00 :background nil)
  (set-face-attribute 'org-level-6 nil :height 1.00 :background nil)
  (set-face-attribute 'org-level-7 nil :height 1.00 :background nil)
  (set-face-attribute 'org-level-8 nil :height 1.00 :background nil)


(setq org-publish-project-alist
      '(("org"
         :base-directory "~/org/notes/"
         :publishing-function org-html-publish-to-html
         :publishing-directory "~/public_html"
         :section-numbers t
         :with-toc t
         :recursive t
         :html-head "<link rel=\"stylesheet\"
                    href=\"../other/mystyle.css\"
                    type=\"text/css\"/>")))



  ;; (setq +org-roam-auto-backlinks-buffer t)
  (use-package! ox-hugo
    :after ox)

  (use-package! org-journal
    :config
    (setq org-journal-dir "~/org/journal/")
    (setq org-journal-file-type 'yearly)
    )

   ;; Hide emphasis markers like /foo/ and *bar*
   (setq org-hide-emphasis-markers t)

   ;; ;; Make TAB cycle all subtrees
   (after! evil-org
   (remove-hook 'org-tab-first-hook #'+org-cycle-only-current-subtree-h))

   ;; Enable booktabs table export
   (setq org-latex-tables-booktabs t)

   (setq org-roam-capture-templates
   '(
           ("d" "default" plain "%?"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
           :unnarrowed t)
           ("m" "meeting" entry (file "~/org/notes/templates/meetings.org")
           :target (file "~/org/notes/phd/meetings/misc.org")
           :unnarrowed t
           :empty-lines 1)
           ))

   )
