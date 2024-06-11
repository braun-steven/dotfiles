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
        ("m" "meeting" plain (file "~/org/notes/templates/meetings.org")
         :target (file+head "~/org/notes/phd/meetings/misc.org" "#+title: ${title}\n")
         :unnarrowed t)
        ("s" "pathfinder session" plain (file "~/org/notes/templates/pathfinder-session.org")
         ;; :target (file "~/org/pathfinder-blood-lords/20231124162340-pathfinder_2e_blood_lords_session_notes.org")
         :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
         ;; :jump-to-captured
         ;; :empty-lines 1
         ;; :time-prompt
         :unnarrowed t)
        ))
