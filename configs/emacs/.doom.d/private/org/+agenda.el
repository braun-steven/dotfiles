;;; private/org/+agenda.el -*- lexical-binding: t; -*-

;; Set normal state as default for org-agenda-mode
;; TODO: This needs to be evaluated last or something?
(add-hook 'org-agenda-mode-hook #'(lambda () (evil-set-initial-state 'org-agenda-mode 'normal)))
(evil-set-initial-state 'org-agenda-mode 'normal)


;; Org agenda files: look for all files in the following directory
(setq org-agenda-files (quote ("~/org/gtd")))

;; Fix outline separator
(use-package! org-super-agenda
  :after org
  :defer
  :config
  (setq org-super-agenda-auto-group-outline-path-separator " | ")
  (org-super-agenda--def-auto-group outline-path "their outline paths"
    :key-form (org-super-agenda--when-with-marker-buffer (org-super-agenda--get-marker item)
                (concat (s-join org-super-agenda-auto-group-outline-path-separator (org-get-outline-path))
                        "\n"))))



;; Default org file when using capture (C-c c)
(setq org-default-notes-file "~/org/gtd/inbox.org")

;; Org-Capture Templates
(setq org-capture-templates
      '(;; Todo entries
        ("t"
         "Todo"
         entry
         (file+headline org-default-notes-file "Inbox")
         "** TODO %?\n:PROPERTIES:\n:CREATED:\t%u\n:END:\n"
         :empty-lines 1)
        ))

;; Set default column view headings: Task Total-Time Time-Stamp
;; (setq org-columns-default-format "%50ITEM(Task) %10CLOCKSUM %16TIMESTAMP_IA")

(setq org-agenda-window-setup 'reorganize-frame)

;; Set the org refile targets to org agenda files
(setq org-refile-targets (quote (("~/org/gtd/gtd.org" :maxlevel . 9))))

;; Save buffer on clocking in/out
;; (add-hook 'org-clock-in-hook #'save-buffer)
;; (add-hook 'org-clock-out-hook #'save-buffer)


;; (use-package! org-pomodoro
;;   :defer
;;   :config
;;   (setq org-pomodoro-play-sounds nil)
;;   ;; org-pomodoro mode hooks
;;   (setq org-pomodoro-clock-break t)  ;; Clock 30 minutes instead of 25
;;   (add-hook 'org-pomodoro-finished-hook
;;             (lambda ()
;;               (slang/notify-send "Pomodoro completed!" "Time for a break.")))

;;   (add-hook 'org-pomodoro-break-finished-hook
;;             (lambda ()
;;               (slang/notify-send "Pomodoro Short Break Finished" "Ready for Another?")))

;;   (add-hook 'org-pomodoro-long-break-finished-hook
;;             (lambda ()
;;               (slang/notify-send "Pomodoro Long Break Finished" "Ready for Another?")))

;;   (add-hook 'org-pomodoro-killed-hook
;;             (lambda ()
;;               (slang/notify-send "Pomodoro Killed" "One does not simply kill a pomodoro!")))
;;   )

;; Predefined tags
(setq org-tag-alist
      '(("@work" . ?w)
        ("@studying" . ?s)
        ("@home" . ?h)
        ("@freetime" . ?f)))

;; Disable super-agenda keymap (breaks evil up/down on headers)
(setq org-super-agenda-header-map (make-sparse-keymap))

;; Set org agenda todo view (open with <f1>)
(setq slang/org-agenda-directory "~/org/gtd/")
(setq slang/org-agenda-todo-view
      `("a" "Full Agenda"
        ;; Agenda 10-day view
        ((agenda ""
                 ((org-agenda-span '10)
                  (org-agenda-start-day "-1d")
                  ;; (org-super-agenda-groups '((:auto-parent t)))
                  (org-deadline-warning-days 365)))


         ;; To be refiled
         (org-ql-block
          ;; Query
          '(todo)
          ((org-ql-block-header "Inbox")
           (org-super-agenda-groups '((:auto-outline-path t)))
           (org-agenda-files '(,(concat slang/org-agenda-directory "inbox.org")))))

         ;; TODAY
         (org-ql-block
          ;; Query
          '(and (not (habit))
                (todo)
                (scheduled :on today))
          ((org-ql-block-header "Today")
           (org-super-agenda-groups '((:auto-outline-path t)))
           (org-agenda-files '(,(concat slang/org-agenda-directory "gtd.org")))))

         ;; TOMORROW
         (org-ql-block
          ;; Query
          '(and (not (habit))
                (todo)
                (scheduled :on +1))
          ((org-ql-block-header "Tomorrow")
           (org-super-agenda-groups '((:auto-outline-path t)))
           (org-agenda-files '(,(concat slang/org-agenda-directory "gtd.org")))))

         ;; Next Actions Category
         (org-ql-block
          ;; Query
          '(and (todo "NEXT")
                (not (scheduled :on +1))
                (not (scheduled :on today))
                )
          ;; Variables
          ((org-ql-block-header "Next Actions")
           (org-super-agenda-groups '((:auto-outline-path t)))
           (org-agenda-files '(,(concat slang/org-agenda-directory "gtd.org")))))



         ;; Missed items
         (org-ql-block
          ;; Query
          '(and (not (habit))
                (todo)
                (scheduled :to -1)) ;; Scheduled until yesterday but still todo -> missed
          ((org-ql-block-header "Missed Items")
           (org-super-agenda-groups '((:auto-outline-path t)))
           (org-agenda-files '(,(concat slang/org-agenda-directory "gtd.org")))))

         ;; Waiting Category
         (org-ql-block
          ;; Query
          '(and (todo "WAITING"))
          ;; Variables
          ((org-ql-block-header "Waiting For")
           (org-super-agenda-groups '((:auto-outline-path t)))
           (org-agenda-files '(,(concat slang/org-agenda-directory "gtd.org")))))

         ;; Projects
         (org-ql-block
          ;; Query
          '(and (not (or (tags "someday")
                         (habit)
                         (todo "WAITING")
                         (todo "NEXT")))
                (todo))
          ;; Variables
          ((org-ql-block-header "Projects")
           (org-super-agenda-groups '((:auto-outline-path t)))
           (org-agenda-files '(,(concat slang/org-agenda-directory "gtd.org")))))

         (org-ql-block
          ;; Query
          '(and (tags "someday")
                (todo))

          ;; Variables
          ((org-ql-block-header "Someday/Maybe")
           (org-super-agenda-groups '((:auto-outline-path t)))
           (org-agenda-files '(,(concat slang/org-agenda-directory "gtd.org")))))

         ;; Reference Material
         (org-ql-block
          '(or (todo)
               (todo "WAITING"))

          ;; Variables
          ((org-ql-block-header "Reference Material")
           (org-super-agenda-groups '((:auto-outline-path t)))
           (org-agenda-files '(,(concat slang/org-agenda-directory "reference-material.org"))))))
        nil
        ("/tmp/org-agenda.html")))



(defun slang/make-org-agenda-custom-view (tag key description)
    "Make a custom agenda view filtered by a specific context tag."
    `(,key ,description
        (
         ;; To be refiled
         (org-ql-block
          ;; Query
          '(todo)
          ((org-ql-block-header "Inbox")
           (org-super-agenda-groups '((:auto-outline-path t)))
           (org-agenda-files '(,(concat slang/org-agenda-directory "inbox.org")))))


         ;; TODAY
         (org-ql-block
          ;; Query
          '(and (not (habit))
                (todo)
                (tags ,tag)
                (scheduled :on today))
          ((org-ql-block-header "Today")
           (org-super-agenda-groups '((:auto-outline-path t)))
           (org-agenda-files '(,(concat slang/org-agenda-directory "gtd.org")))))

         ;; TOMORROW
         (org-ql-block
          ;; Query
          '(and (not (habit))
                (todo)
                (tags ,tag)
                (scheduled :on +1))
          ((org-ql-block-header "Tomorrow")
           (org-super-agenda-groups '((:auto-outline-path t)))
           (org-agenda-files '(,(concat slang/org-agenda-directory "gtd.org")))))

         ;; Next Actions Category
         (org-ql-block
          ;; Query
          '(and (todo "NEXT")
                (tags ,tag)
                (not (scheduled :on +1))
                (not (scheduled :on today)))
          ;; Variables
          ((org-ql-block-header "Next Actions")
           (org-super-agenda-groups '((:auto-outline-path t)))
           (org-agenda-files '(,(concat slang/org-agenda-directory "gtd.org")))))



         ;; Missed items
         (org-ql-block
          ;; Query
          '(and (not (habit))
                (todo)
                (tags ,tag)
                (scheduled :to -1)) ;; Scheduled until yesterday but still todo -> missed
          ((org-ql-block-header "Missed Items")
           (org-super-agenda-groups '((:auto-outline-path t)))
           (org-agenda-files '(,(concat slang/org-agenda-directory "gtd.org")))))

         ;; Waiting for
         (org-ql-block
          ;; Query
          '(and (not (habit))
                (todo "WAITING")
                (tags ,tag))
          ((org-ql-block-header "Waiting For")
           (org-super-agenda-groups '((:auto-outline-path t)))
           (org-agenda-files '(,(concat slang/org-agenda-directory "gtd.org")))))

         ;; Projects
         (org-ql-block
          ;; Query
          '(and (not (or (tags "someday")
                         (habit)
                         (todo "WAITING")
                         (todo "NEXT")))
                (todo)
                (tags ,tag))
          ;; Variables
          ((org-ql-block-header "Projects")
           (org-super-agenda-groups '((:auto-outline-path t)))
           (org-agenda-files '(,(concat slang/org-agenda-directory "gtd.org")))))

         (org-ql-block
          ;; Query
          '(and (tags "someday")
                (tags ,tag)
                (todo))

          ;; Variables
          ((org-ql-block-header "Someday/Maybe")
           (org-super-agenda-groups '((:auto-outline-path t)))
           (org-agenda-files '(,(concat slang/org-agenda-directory "gtd.org")))))

        )))

;; ;; Set to empty list is necessary or else org-agenda-custom-commands is not defined
(setq org-agenda-custom-commands (list))
(add-to-list 'org-agenda-custom-commands `,slang/org-agenda-todo-view)
(add-to-list 'org-agenda-custom-commands `,(slang/make-org-agenda-custom-view "phd" "cp" "PhD"))


;; Enable super agenda mode
(add-hook 'org-agenda-mode-hook (lambda () (org-super-agenda-mode t)))


;; https://emacs.stackexchange.com/questions/21754/how-to-automatically-save-all-org-files-after-marking-a-repeating-item-as-done-i
(defmacro η (fnc)
  "Return function that ignores its arguments and invokes FNC."
  `(lambda (&rest _rest)
     (funcall ,fnc)))

(advice-add 'org-agenda-deadline       :after (η #'slang/org-save-all-org-buffers-silent))
(advice-add 'org-agenda-schedule       :after (η #'slang/org-save-all-org-buffers-silent))
(advice-add 'org-store-log-note :after (η #'slang/org-save-all-org-buffers-silent))
(advice-add 'org-agenda-todo           :after (η #'slang/org-save-all-org-buffers-silent))



(setq org-agenda-prefix-format '((agenda . "  %t ")
                                 (todo . "  • ")
                                 (tags . "  • ")
                                 (search . "  • ")))
