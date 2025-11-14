;;; ../dotfiles/configs/emacs/.doom.d/lang/+org.el -*- lexical-binding: t; -*-

;; Set directioreis of org, org-roam and deft
(setq deft-directory "~/org/notes/")
(setq org-directory "~/org/")
(setq org-roam-directory "~/org/notes")

;; Org setup
(after! org

  (load! "+functions")
  (load! "+keybindings")

  ;; (add-hook 'org-mode-hook 'org-appear-mode)

;; (custom-theme-set-faces
;;    'user
;;    '(variable-pitch ((t (:family "SF Pro" :height 140 :weight light))))
;;    '(fixed-pitch ((t ( :family "SF Mono" :height 140)))))

  ;; (after! org-appear
  ;;   (setq org-appear-autolinks t)
  ;;   (setq org-appear-autoentities t)
  ;;   (setq org-appear-autosubmarkers t)
  ;;   (setq org-appear-delay 0.0)
  ;;   (setq org-appear-trigger 'always))


   ;; Add variable-pitch mode to org-mode (for nicer writing experience)
  ;; (add-hook 'org-mode-hook 'variable-pitch-mode)
  ;; (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
  ;; (set-face-attribute 'org-block nil :inherit '(fixed-pitch))
  ;; (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
  ;; (set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-level-1 nil :height 1.30 :background nil)
  (set-face-attribute 'org-level-2 nil :height 1.20 :background nil)
  (set-face-attribute 'org-level-3 nil :height 1.10 :background nil)
  (set-face-attribute 'org-level-4 nil :height 1.05 :background nil)
  (set-face-attribute 'org-level-5 nil :height 1.05 :background nil)
  (set-face-attribute 'org-level-6 nil :height 1.05 :background nil)
  (set-face-attribute 'org-level-7 nil :height 1.05 :background nil)
  (set-face-attribute 'org-level-8 nil :height 1.05 :background nil)

  ;; (setq org-modern-star 'org-modern-replace-stars)
  (setq org-modern-hide-stars t)
  ;; (setq org-modern-star '("" "" "" "" ""))

   ;; (add-hook 'org-mode-hook (lambda () (setq-local line-spacing 0.5)))


   ;; Hide emphasis markers like /foo/ and *bar*
   (setq org-hide-emphasis-markers t)

   ;; ;; Make TAB cycle all subtrees
   (after! evil-org
   (remove-hook 'org-tab-first-hook #'+org-cycle-only-current-subtree-h))

   ;; ;; Enable booktabs table export
   ;; (setq org-latex-tables-booktabs t)

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
