(defun sbraun/switch-to-scratch ()
  "Switch to scratch buffer."
  (interactive)
  (switch-to-buffer "*scratch*"))



;; Close compilation buffer on succeed
(defun sbraun/bury-compile-buffer-if-successful (buffer string)
  "Bury a compilation buffer if succeeded without warnings
https://stackoverflow.com/questions/11043004/emacs-compile-buffer-auto-close/11059012#11059012"
  (when (and
         (buffer-live-p buffer)
         (string-match "compilation" (buffer-name buffer))
         (string-match "finished" string)
         )
    (run-with-timer 1 nil
                    (lambda (buf)
                      (bury-buffer buf)
                      (delete-windows-on buf))
                    buffer)))


;; https://gist.github.com/mads-hartmann/3402786
(defun toggle-maximize-buffer () "Maximize buffer"
       (interactive)
       (if (= 1 (length (window-list)))
           (jump-to-register '_)
         (progn
           (window-configuration-to-register '_)
           (delete-other-windows))))


(defun sbraun/switch-other-buffer ()
  "Switch to other buffer."
  (interactive)
  (switch-to-buffer (other-buffer)))


(defun sbraun/reload-config ()
  "Reload config."
  (interactive)
  (load! "config"))

(defun sbraun/timestamp-today-string-plus-n-days (n)
  (format-time-string "%Y-%m-%d" (time-add (* 60 60 24 n) (current-time))))


(defun sbraun/notify-send (title message)
  "Call notify-send with the given title and message to send
   a notification to the desktop environment."
  (call-process "notify-send"
                nil 0 nil
                title
                message
                "--expire-time" "300000" ; 5 minutes
                "--app-name" "Emacs"
                ))

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
