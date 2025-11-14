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


(defun sbraun/open-pathfinder-blood-lords-session-notes ()
  "Open the Pathfinder 2E Blood Lords session notes."
  (interactive)
  (let ((file-path (expand-file-name "org/pathfinder-blood-lords/20231124162340-pathfinder_2e_blood_lords_session_notes.org"
                                     (getenv "HOME"))))
    (find-file file-path)))

(defun sbraun/open-pathfinder-seven-dooms-for-sandpoint-session-notes ()
  "Open the Pathfinder 2E Blood Lords session notes."
  (interactive)
  (let ((file-path (expand-file-name "~/org/pathfinder-seven-dooms-for-sandpoint/20250505174307-session_notes.org"
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


(defun my-projectile-activate-python-env-maybe ()
  "After switching Projectile projects, check for a Python environment.

1.  Look for a local '.venv' directory in the project root.
2.  Look for a local 'venv' directory in the project root.
3.  If 1 or 2 is found, activate it using 'pyvenv-activate'.
4.  If not, check if the project's directory name matches a conda
    environment and activate it using 'conda-env-activate'."
  (message "Project changed... checking for Python env.")

  (when (projectile-project-root)
    (let* ((project-root (projectile-project-root))
           ;; Check for .venv first
           (dot-venv-path (expand-file-name ".venv" project-root))
           ;; Then check for venv
           (venv-path (expand-file-name "venv" project-root))
           (found-venv-dir nil))

      ;; Determine which venv path exists, if any
      (cond
       ((file-directory-p dot-venv-path)
        (setq found-venv-dir dot-venv-path))
       ((file-directory-p venv-path)
        (setq found-venv-dir venv-path)))

      (if found-venv-dir
          ;; --- 1. Pyvenv Logic ---
          ;; Found a local .venv or venv
          (progn
            (message (concat "Found pyvenv, activating: " (file-relative-name found-venv-dir project-root)))
            (pyvenv-activate found-venv-dir))

        ;; --- 2. Conda Logic (fallback) ---
        ;; No local pyvenv found, try to find a matching conda env
        (let* (;; More robust way to get the project directory name
               (project-dir-name (file-name-nondirectory (directory-file-name project-root)))
               (conda-env-name-candidate project-dir-name))

          (message (concat "No local pyvenv found. Checking for conda env '" conda-env-name-candidate "'..."))
          
          ;; Assumes (conda-env-candidates) function exists from your setup
          (if (fboundp 'conda-env-candidates)
              (if (member conda-env-name-candidate (conda-env-candidates))
                  (progn
                    (message (concat "Found matching conda environment: " conda-env-name-candidate))
                    (conda-env-activate conda-env-name-candidate))
                (message "No matching conda env found."))
            (message "Conda function 'conda-env-candidates' not found.")))
        ))))


(defvar-local bibtex-lookup-bibfile nil
  "Path to the BibTeX file to use for key lookup.")

(defun sbraun/insert-bibtex-key-at-point ()
  "Insert a BibTeX key at point by searching keys and titles from `bibtex-lookup-bibfile`."
  (interactive)
  (unless bibtex-lookup-bibfile
    (user-error "Variable `bibtex-lookup-bibfile` is not set"))
  (let* ((python-script (expand-file-name "~/dotbin/bibkeys")) ;; update this path
         (bibfile (expand-file-name bibtex-lookup-bibfile (projectile-project-root)))
         (python-cmd (format "%s %s"
                             (shell-quote-argument python-script)
                             (shell-quote-argument bibfile)))
         (output (shell-command-to-string python-cmd))
         (lines (split-string output "\n" t))
         ;; Parse into (key . title)
         (entries
          (mapcar (lambda (line)
                    (let ((parts (split-string line " | ")))
                      (cons (car parts) (cadr parts))))
                  lines))
         ;; Calculate max key width for padding
         (max-key-len (apply #'max (mapcar (lambda (e) (length (car e))) entries)))
         ;; Prepare candidates with padded key and title separated by spaces only
         (candidates
          (mapcar (lambda (entry)
                    (let ((key (car entry))
                          (title (cdr entry)))
                      (cons (format (format "%%-%ds  %%s" max-key-len) key title) key)))
                  entries))
         (chosen (completing-read "Select BibTeX entry: " candidates nil t)))
    ;; Insert only the key part (cdr of chosen)
    (insert (cdr (assoc chosen candidates)))))

(defun sbraun/open-alacritty-in-project-root ()
  "Open a new Alacritty window in the current project's root directory."
  (interactive)
  (let ((project-root (or (when (fboundp 'project-root)
                            (when-let ((proj (project-current)))
                              (project-root proj)))
                          default-directory)))
    (if project-root
        (start-process "alacritty" nil "alacritty" "msg" "create-window" "--working-directory" project-root)
      (message "Could not determine project root."))))
