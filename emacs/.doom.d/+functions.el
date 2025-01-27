;; https://gist.github.com/mads-hartmann/3402786
(defun toggle-maximize-buffer () "Maximize buffer"
       (interactive)
       (if (= 1 (length (window-list)))
           (jump-to-register '_)
         (progn
           (window-configuration-to-register '_)
           (delete-other-windows))))


(defun sbraun/add-all-projects ()
  "Add all projects in ~/projects to known projectile projects."
  (interactive)
  ;; Add all projects in ~/projects as known projects
  (dolist (file (directory-files "~/projects/" t directory-files-no-dot-files-regexp))
    (if (file-directory-p file)
        (projectile-add-known-project file))))

;; Functions to reload dir-locals: https://emacs.stackexchange.com/questions/13080/reloading-directory-local-variables
;; {{{
(defun my-reload-dir-locals-for-current-buffer ()
  "reload dir locals for the current buffer"
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)))

(defun my-reload-dir-locals-for-all-buffer-in-this-directory ()
  "For every buffer with the same `default-directory` as the
current buffer's, reload dir-locals."
  (interactive)
  (let ((dir default-directory))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (equal default-directory dir)
          (my-reload-dir-locals-for-current-buffer))))))

(add-hook 'emacs-lisp-mode-hook
          (defun enable-autoreload-for-dir-locals ()
            (when (and (buffer-file-name)
                       (equal dir-locals-file
                              (file-name-nondirectory (buffer-file-name))))
              (add-hook 'after-save-hook
                        'my-reload-dir-locals-for-all-buffer-in-this-directory
                        nil t))))
;; }}}


(defun sbraun/open-pathfinder-session-notes ()
  "Open the Pathfinder 2E Blood Lords session notes."
  (interactive)
  (let ((file-path (expand-file-name "org/pathfinder-blood-lords/20231124162340-pathfinder_2e_blood_lords_session_notes.org"
                                     (getenv "HOME"))))
    (find-file file-path)))

(defun sbraun/open-org-para-hub ()
  "Open the my PARA hub."
  (interactive)
  (let ((file-path (expand-file-name "org/notes/20241119070534-para_hub.org"
                                     (getenv "HOME"))))
    (find-file file-path)))

;; Projectile after switch cook
(defun activate-project-conda-env-maybe ()
  "Perform some action after switching Projectile projects."
  (message "Project changed...")
  ;; Do something interesting here...
  ;;
  ;; `projectile-current-project-files', and `projectile-current-project-dirs' can be used
  ;; to get access to the new project's files, and directories.
  (message "Project root:")
  (setq conda-env-name-candidate (nth 1 (reverse (s-split "/" (projectile-project-root)))))

  (if (member conda-env-name-candidate (conda-env-candidates))
      ;; (message "Yes")
      ;; (message "No")
      (progn
        (message (concat "Found conda environment: " conda-env-name-candidate))
        (conda-env-activate conda-env-name-candidate))
    )
  )
