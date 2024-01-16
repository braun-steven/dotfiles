;; Hide emphasis markers like /foo/ and *bar*
(setq org-hide-emphasis-markers t)

;; ;; Make TAB cycle all subtrees
(after! evil-org
  (remove-hook 'org-tab-first-hook #'+org-cycle-only-current-subtree-h))

;; Enable booktabs table export
(setq org-latex-tables-booktabs t)

(setq org-roam-capture-templates
      '(
        ("d" "default" plain "%?" :target
         (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
         :unnarrowed t)
        ("m" "meeting" plain (file "~/org/notes/templates/meetings.org")
         :target (file+head "~/org/notes/phd/meetings/misc.org" "#+title: ${title}\n")
         :unnarrowed t)
        ("s" "pathfinder session" plain (file "~/org/notes/templates/pathfinder-session.org")
         :target (file "~/org/pathfinder-blood-lords/20231124162340-pathfinder_2e_blood_lords_session_notes.org")
         :jump-to-captured
         :empty-lines 1
         :time-prompt
         :unnarrowed t)
        ))


(after! org-roam
  (use-package! websocket
    :after org-roam)

  (use-package! org-roam-ui
    :after org-roam ;; or :after org
    ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
    ;;         a hookable mode anymore, you're advised to pick something yourself
    ;;         if you don't care about startup time, use
    ;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))


  (defcustom org-roam-graph-exclude-patterns '()
    "List of patterns to exclude from the Org-roam graph.
        Nodes with file names or titles matching any pattern in this list will be excluded."
    :type '(repeat string)
    :group 'org-roam)


  (defun org-roam-graph--dot (&optional edges all-nodes)
    "Build the graphviz given the EDGES of the graph.
        If ALL-NODES, include also nodes without edges."
    (let ((org-roam-directory-temp org-roam-directory)
          (nodes-table (make-hash-table :test #'equal))
          (seen-nodes (list))
          (excluded-nodes (list)) ;; List to store IDs of excluded nodes
          (edges (or edges (org-roam-db-query [:select :distinct [source dest type] :from links]))))

      ;; Function to check if a string matches any of the exclude patterns
      (cl-flet ((matches-exclude-pattern (s)
                  (seq-some (lambda (pattern)
                              (string-match pattern s))
                            org-roam-graph-exclude-patterns)))
        ;; First, identify nodes to exclude
        (pcase-dolist (`(,id ,file ,title)
                       (org-roam-db-query [:select [id file title] :from nodes]))
          (if (or (matches-exclude-pattern file)
                  (matches-exclude-pattern title))
              (push id excluded-nodes) ;; Add to excluded list
            (message "Excluding node: %s" title)
            (puthash id (org-roam-node-create :file file :id id :title title) nodes-table))) ;; Add to nodes table

        (with-temp-buffer
          (setq-local org-roam-directory org-roam-directory-temp)
          (insert "digraph \"org-roam\" {\n")
          (dolist (option org-roam-graph-extra-config)
            (insert (org-roam-graph--dot-option option) ";\n"))
          (insert (format " edge [%s];\n"
                          (mapconcat (lambda (var)
                                       (org-roam-graph--dot-option var nil "\""))
                                     org-roam-graph-edge-extra-config
                                     ",")))

          ;; Process edges, excluding those connected to excluded nodes
          (pcase-dolist (`(,source ,dest ,type) edges)
            (unless (or (member source excluded-nodes)
                        (member dest excluded-nodes)
                        (member type org-roam-graph-link-hidden-types))
              (insert (format "  \"%s\" -> \"%s\";\n"
                              (xml-escape-string source)
                              (xml-escape-string dest)))))

          ;; Process nodes, excluding those in the excluded list
          (maphash (lambda (id node)
                     (unless (member id excluded-nodes)
                       (insert (org-roam-graph--format-node node "id"))))
                   nodes-table)

          (insert "}")
          (buffer-string)))))
  )
