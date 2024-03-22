;;; -*- lexical-binding: t -*-
(add-hook 'after-init-hook #'savehist-mode)
(add-hook 'after-init-hook #'save-place-mode)
;; (add-hook 'text-mode-hook #'indicate-buffer-boundaries-left)
(add-hook 'after-init-hook #'column-number-mode)
(add-hook 'after-init-hook #'windmove-default-keybindings)
(add-hook 'after-init-hook #'pixel-scroll-precision-mode)
(add-hook 'after-init-hook #'ffap-bindings)

(setq auto-save-silent t   ; quietly save
      make-backup-files nil
      auto-save-default nil
      create-lockfiles nil
      auto-save-delete-trailing-whitespace t
      delete-auto-save-files t)

(progn ; `keys'
  (global-unset-key (kbd "C-x C-o"))
  (global-unset-key (kbd "C-x f"))
  (global-unset-key (kbd "C-x C-d"))
  (global-unset-key (kbd "C-x C-k"))
  (global-unset-key (kbd "C-h C-f"))
  (global-unset-key (kbd "C-x C-p"))
  (global-unset-key (kbd "C-h C-a"))

  (global-set-key (kbd "C-x C-k") #'kill-buffer-and-window)
  (ffap-bindings))

(if win-wsl-p
    (setq browse-url-generic-program  "wslview"
          browse-url-browser-function #'browse-url-generic))

(progn ; `misc'
  (fset 'yes-or-no-p 'y-or-n-p)
  (setq visible-bell t
        ring-bell-function 'ignore)
  (setq delete-by-moving-to-trash t)
  (setq-default cursor-type 'bar)
  (setq-default cursor-in-non-selected-windows nil)
  (setq highlight-nonselected-windows nil)
  ;; Fix bug `https://emacs-china.org/t/wslg-x-display-mm-height-0/22547'
  (when win-wsl-p
    (setq display-mm-dimensions-alist '(("wayland-0" . (275 . 183))))))

(provide 'base)
;;; base.el ends here
