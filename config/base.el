;;; -*- lexical-binding: t -*-
(add-hook 'after-init-hook #'savehist-mode)
(add-hook 'after-init-hook #'save-place-mode)
(add-hook 'text-mode-hook #'indicate-buffer-boundaries-left)
(add-hook 'after-init-hook #'column-number-mode)
(add-hook 'after-init-hook #'windmove-default-keybindings)

(setq auto-save-silent t   ; quietly save
      make-backup-files nil
      auto-save-default nil
      create-lockfiles nil
      auto-save-delete-trailing-whitespace t
      delete-auto-save-files t)

(progn ; `unset-keys'
  (global-unset-key (kbd "C-x C-o"))
  (global-unset-key (kbd "C-x f"))
  (global-unset-key (kbd "C-x C-d"))
  (global-unset-key (kbd "C-x C-k"))
  (global-unset-key (kbd "C-h C-f"))
  (global-unset-key (kbd "C-x C-p"))
  (global-unset-key (kbd "C-h C-a")))

(when (and (eq system-type 'gnu/linux)
           (string-match
            "Linux.*Microsoft.*Linux"
            (shell-command-to-string "uname -a")))
  (setq
   browse-url-generic-program  "/mnt/c/Windows/System32/cmd.exe"
   browse-url-generic-args     '("/c" "start")
   browse-url-browser-function #'browse-url-generic))

(progn ; `misc'
  (fset 'yes-or-no-p 'y-or-n-p)
  (setq visible-bell t
        ring-bell-function 'ignore)
  (setq-default cursor-type 'bar))

(provide 'base)
;;; base.el ends here
