;;; init.el --- user-init-file                    -*- lexical-binding: t -*-
;;; Early birds
(progn ;     startup
  (defvar before-user-init-time (current-time)
    "Value of `current-time' when Emacs begins loading `user-init-file'.")
  (message "Loading Emacs...done (%.3fs)"
           (float-time (time-subtract before-user-init-time
                                      before-init-time)))
  (setq user-init-file (or load-file-name buffer-file-name))
  (setq user-emacs-directory (file-name-directory user-init-file))
  (message "Loading %s..." user-init-file)
  (when (< emacs-major-version 27)
    (setq package-enable-at-startup nil)
    ;; (package-initialize)
    (load-file (expand-file-name "early-init.el" user-emacs-directory)))
  (setq inhibit-startup-buffer-menu t)
  (setq inhibit-startup-screen t)
  (setq inhibit-startup-echo-area-message "locutus")
  (setq initial-buffer-choice t)
  (setq initial-scratch-message "")
  (when (fboundp 'scroll-bar-mode)
    (scroll-bar-mode 0))
  (when (fboundp 'tool-bar-mode)
    (tool-bar-mode 0))
  (menu-bar-mode 0))

(eval-and-compile ; `borg'
  (add-to-list 'load-path (expand-file-name "lib/borg" user-emacs-directory))
  (require 'borg)
  (borg-initialize))

(eval-and-compile ; `use-package'
  (require  'use-package)
  (setq use-package-verbose t))

(use-package dash
  :config (global-dash-fontify-mode))

(use-package auto-compile
  :config
  (setq auto-compile-display-buffer               nil)
  (setq auto-compile-mode-line-counter            t)
  (setq auto-compile-source-recreate-deletes-dest t)
  (setq auto-compile-toggle-deletes-nonlib-dest   t)
  (setq auto-compile-update-autoloads             t))

(use-package epkg
  :defer t
  :init
  (setq epkg-repository
        (expand-file-name "var/epkgs/" user-emacs-directory))
  (setq epkg-database-connector
        (if (>= emacs-major-version 29) 'sqlite-builtin 'sqlite-module)))

(use-package server
  :commands (server-running-p)
  :config (or (server-running-p) (server-mode)))

(progn ;     startup
  (message "Loading early birds...done (%.3fs)"
           (float-time (time-subtract (current-time)
                                      before-user-init-time))))

;;; Long tail
;;;; Pre
(use-package no-littering)

(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))
(use-package defs)
(use-package base)
(use-package custom
  :no-require t
  :config
  (setq custom-file (no-littering-expand-etc-file-name "custom.el"))
  (when (file-exists-p custom-file)
    (load custom-file)))



;;;; Face
(use-package font)

(use-package doom-themes
  :config
  (load-theme 'doom-one :no-confirm)
  (setq doom-themes-enable-bold t
	    doom-themes-enable-italic t))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-major-mode-icon t))

(use-package diff-hl
  :config
  (setq diff-hl-draw-borders nil)
  (global-diff-hl-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh t))

(use-package diff-mode
  :defer t
  :config
  (when (>= emacs-major-version 27)
    (set-face-attribute 'diff-refine-changed nil :extend t)
    (set-face-attribute 'diff-refine-removed nil :extend t)
    (set-face-attribute 'diff-refine-added   nil :extend t)))

;;;; Dired
(use-package dired
  :defer t
  :custom
  (dired-hide-details-hide-symlink-targets nil)
  :config
  (setq ls-lisp-dirs-first t)
  (setq dired-listing-switches "-lAhG --group-directories-first"
        dired-recursive-deletes 'always
        dired-recursive-copies 'always)
  (require 'dired+)
  (add-hook 'dired-mode-hook #'dired-hide-details-mode))

(use-package fd-dired
  :bind ("M-s D" . fd-dired)
  :commands fd-dired-dwim
  :config
  (with-eval-after-load 'dired
    (define-key dired-mode-map (kbd "r") #'fd-dired-dwim)))

(use-package diredfl
  :hook (dired-mode . diredfl-mode)
  :config
  (set-face-attribute 'diredfl-dir-name nil :bold t))

(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode)
  :init
  (with-eval-after-load 'nerd-icons
    (add-to-list 'nerd-icons-dir-icon-alist
                 '("\\.\\.?" nerd-icons-octicon "nf-oct-file_directory"))))

;;;; Completion
(use-package minibuffer
  :bind
  (
   :map minibuffer-local-completion-map
   ("M-i" . bookmark-insert-location)
   ("C-n" . minibuffer-next-completion)
   ("C-p" . minibuffer-previous-completion)
   ("C-RET" . completion:force-exit)
   ("SPC")
   :map completion-list-mode-map
   ("z" . switch-to-minibuffer))
  :custom
  (minibuffer-electric-default-mode t)
  ;; Don't insert completion at point into minibuffer
  ;; `M-<RET>' complete `minibuffer'
  (minibuffer-completion-auto-choose nil)
  ;; One frame one minibuffer.
  (minibuffer-follows-selected-frame nil)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (minibuffer-default-prompt-format " [%s]")
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt))
  (resize-mini-windows t)
  (completion-auto-help 'always)
  (completion-show-help nil)
  (completion-show-inline-help nil)
  (completion-cycle-threshold nil)
  ;; `t' `second-tab' `nil'
  (completion-auto-select 'second-tab)
  (completions-detailed t)
  ;; Ignore cases when complete
  (completion-ignore-case t)
  ;; vertical display
  (completions-format 'one-column)
  (completions-max-height 20)
  (completions-sort #'completion:list-sort)
  :config
  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t)
  ;; Do not allow the cursor in the minibuffer prompt
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  
  (defun completion:list-sort (all)
    "对 `Completions-buffer' 中的补全项进行排序"
    (let ((hist (minibuffer-history-value)))
      (thread-first all
                    (sort (lambda (c1 c2) (< (length c1) (length c2))))
                    (sort (lambda (c1 c2) (> (length (member c1 hist))
                                         (length (member c2 hist))))))))
  ;; Copy from `icomplete'
  (defun completion:force-exit (force)
    "Attempt to exit minibuffer immediately with current input.
Unless FORCE is non-nil (interactively with a prefix argument),
honor a non-nil REQUIRE-MATCH argument to `completing-read' by
trying to complete as much as possible and disallowing the exit
if that doesn't produce a completion match."
    (interactive "P")
    (if (and (not force) minibuffer--require-match)
        (minibuffer-complete-and-exit)
      (exit-minibuffer)))
  
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)
  )

(use-package vertico
  :hook (after-init . vertico-mode)
  :bind (:map vertico-map
              ("M-i" . bookmark-insert-location)))

(use-package vertico-directory
  :after vertico
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package company
  :hook
  (after-init . global-company-mode)
  :custom
  (company-backends '((company-capf :with company-yasnippet)
                      (company-keywords company-files)))
  (company-tooltip-align-annotations t)
  (company-tooltip-limit 12)
  (company-tooltip-offset-display 'line)
  :config
  (setq company-global-modes '(not message-mode
                                   help-mode
                                   eshell-mode
                                   shell-mode))
  (setq company-idle-delay 0
        company-minimum-prefix-length 2
        company-icon-margin 3
        company-require-match nil
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil)

  ;; Disable orderless for company-capf
  ;; https://www.reddit.com/r/emacs/comments/nichkl/comment/gz1jr3s/
  (defun company:completion-styles (capf-fn &rest args)
    (let ((completion-styles '(basic partial-completion)))
      (apply capf-fn args)))

  (advice-add 'company-capf :around #'company:completion-styles))

;; (use-package consult-company
;;   :bind (:map company-active-map
;;               ("C-s" . consult-comapny)))

(use-package nerd-icons-completion
  :hook (after-init . nerd-icons-completion-mode))

(use-package marginalia
  :hook (after-init . marginalia-mode)
  :config
  (setq marginalia-annotator-registry
        (assq-delete-all 'file marginalia-annotator-registry)))

;;;; Navigation
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-fd)                  ;; Alternative: consult-fd
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config
  (setq consult-buffer-filter
        (rx bos
            (? ?*)
            (or
             "Messages"
             (seq (or ?O ?o) "utput")
             "Compile-Log"
             "Completions"
             "Flymake log"
             "Warnings"
             "Flymake diagnostics"
             "Async Shell Command"
             "Async-native-compile-log"
             "Native-compile-Log"
             "Apropos"
             "Backtrace"
             "prodigy"
             "Calendar"
             "Finder"
             "Kill Ring"
             "eshell"
             "epc con"
             "shell"
             "terminal"
             "vterm"
             "quickrun"
             "elfeed-entry"
             "macro expansion"
             "Agenda Commands"
             "Org Select"
             "Capture"
             "CAPTURE-"
             "prolog"
             "rustfmt"
             "Disabled Command"
             "magit-process: "
             "magit-diff: "
             )
            (* anything)
            (? ?*)
            eos))

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-bookmark consult-buffer :preview-key "M-."
   consult-ripgrep consult-git-grep consult-grep
   consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)
  )

(use-package rg
  :bind
  ("M-s D" . rg-menu))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion)))))

;;;; Edit
(use-package edit)

(use-package avy
  :bind
  (("C-'" . avy-goto-char)
   ("M-'" . avy-goto-char-2)
   ("C-," . avy-goto-word-crt-line)
   ("M-g w" . avy-goto-word-1))
  :config
  (setq avy-background t)
  (defun avy-goto-word-crt-line ()
    "Jump to a word start on the current line only."
    (interactive)
    (avy-with avy-goto-word-1
              (avy-goto-word-0 nil (line-beginning-position) (line-end-position))))
  (with-eval-after-load 'view
    (define-keymap
      :keymap view-mode-map
      "g" #'avy-goto-line
      "f" #'avy-goto-word-crt-line)))

(use-package aggressive-indent
  :hook (after-init-hook . global-aggressive-indent-mode))

(use-package hl-todo
  :hook (after-init-hook . global-hl-todo-mode))

;;;; Help
(use-package help
  :bind (:map help-map
              ("K" . describe-keymap))
  :custom
  (help-window-select t)
  (help-window-keep-selected t)
  :config
  (temp-buffer-resize-mode))

(progn ;    `isearch'
  (setq isearch-allow-scroll t))

(use-package man
  :defer t
  :config (setq Man-width 80))

;;;; Program
;;;;; `treesit'
(use-package treesit-auto
  :if (treesit-available-p)
  :init
  (defvar treesit:load-path (no-littering-expand-var-file-name "tree-sitter")
    "The directory where install compile and load a tree-sitter language grammar library.")
  :hook
  (after-init-hook . global-treesit-auto-mode)
  :custom
  (treesit-font-lock-level 4)
  (treesit-auto-install t)
  :config
  (add-to-list 'treesit-extra-load-path treesit:load-path)
  (defun treesit:patch--path (fn &rest arg)
    (unless (nth 0 arg)
      (setf (car arg) treesit:load-path))
    (apply fn arg))
  (advice-add #'treesit--install-language-grammar-1 :around #'treesit:patch--path))

;;;;; `prog-mode'
(use-package prog-mode
  :defer
  :config
  (add-hook 'prog-mode-hook #'global-prettify-symbols-mode)
  (add-hook 'prog-mode-hook #'flymake-mode)
  ;;(add-hook 'prog-mode-hook #'eldoc-mode)
  (defun prog:indent-spaces-mode ()
    (setq indent-tabs-mode nil)))

;;;;; `elisp-mode'
(use-package elisp-mode
  :defer
  :config
  (add-hook 'emacs-lisp-mode-hook #'reveal-mode)
  (add-hook 'lisp-interaction-mode-hook #'prog:indent-spaces-mode))

;;;;; `rust-mode'
(use-package rust-ts-mode
  :mode "\\.rs\\'")

(use-package cargo
  :hook (rust-ts-mode-hook . cargo-minor-mode)
  :custom
  (compilation-ask-about-save nil)
  :config
  (define-key cargo-mode-map (kbd "C-c") 'cargo-minor-mode-command-map))

;;;;; `julia-mode'
(use-package julia-mode
  :defer)

(use-package julia-snail
  :hook julia-mode)

;;;;; `typescript-ts-mode'
(use-package typescript-ts-mode
  :mode "\\.ts\\'")

;;;;; `javascript-ts-mode'
(use-package js-ts-mode
  :mode "\\.js\\'")

;;;;; `scad-mode' to edit OpenSCAD files
(use-package scad-mode :defer)

;;;;; `shell'
(use-package ebuild-mode ;; Gentoo build script
  :defer t)

;;;;; `yaml-ts-mode'
(use-package yaml-ts-mode
  :mode "\\.ya?ml\\'")

;;;;; `yasnippet'
(use-package yasnippet
  :hook (after-init-hook . yas-global-mode))

;;;;; `eglot'
(use-package eglot
  :hook
  ((rust-ts-mode-hook . eglot-ensure)
   (typescript-ts-mode-hook . eglot-ensure)
   (js-ts-mode-hook . eglot-ensure)
   (scad-mode-hook . eglot-ensure))
  :config
  (progn ;; `deno-lsp'
    (add-to-list 'eglot-server-programs
                 '((typescript-ts-mode typescript-tsx-ts-mode) . (eglot-deno "deno" "lsp")))

    (defclass eglot-deno (eglot-lsp-server) ()
      :documentation "A custom class for deno lsp.")

    (cl-defmethod eglot-initialization-options ((server eglot-deno))
      "Passes through required deno initialization options"
      (list :deno.enable t
            :deno.lint t
            :deno.suggest.completeFunctionCalls t)))
  
  (progn ;; `openscad-lsp'
    (add-to-list 'eglot-server-programs
                 '((scad-mode) . (eglot-openscad "openscad-lsp")))
    
    (defclass eglot-openscad (eglot-lsp-server) ()
      :documentation "A custom class for openscad lsp.")
    
    (cl-defmethod eglot-initialization-options ((server eglot-openscad))
      "Passes through required openscad-lsp initialization options."
      (list :search_paths ""
            :fmt_style "file"
            :default_param t))))

;;;;; `eldoc'
(use-package eldoc-box
  :custom
  (eldoc-box-offset '(20 20 16))
  (eldoc-box-only-multi-line t)
  :config
  (add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-mode t))

;;;; `other'
(use-package recentf
  :demand t
  :config
  (add-to-list 'recentf-exclude "^/\\(?:ssh\\|su\\|sudo\\)?x?:")
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))

;;;; Tools
(use-package rime
  :bind (
         :map rime-active-mode-map
              ("TAB" . rime-inline-ascii)
         :map rime-mode-map
              ("M-j" . rime-force-enable))
  :init
  (setq rime-user-data-dir (no-littering-expand-etc-file-name "rime/"))
  :custom
  (default-input-method "rime")
  (rime-inline-ascii-holder ?a)
  (rime-cursor "|")
  ;; 与`rime-cursor'互斥 
  (rime-show-preedit 'inline)
  ;; (rime-title "✍️")
  (rime-inline-ascii-trigger 'shift-r)
  (rime-show-candidate 'minibuffer) ;; Options `message' `minibuffer' `posframe'
  (rime-posframe-properties (list :background-color "#333333"
                                  :foreground-color "#dcdccc"
                                  :internal-border-width 10))
  (rime-disable-predicates '(rime-predicate-prog-in-code-p))
  (rime-inline-predicates '(rime-predicate-space-after-cc-p
                            rime-predicate-after-ascii-char-p
                            rime-predicate-tex-math-or-command-p))
  :config
  (setq rime-translate-keybindings '("C-f" "C-b" "C-n" "C-p" "C-g" "C-v" "M-v" "," "." "S-<return>")))


(use-package vterm
  :bind (:map vterm-mode-map
              ("C-q" . vterm-send-next-key)
              ("<C-backspace>" . (lambda () (interactive) (vterm-send-key (kbd "C-w")))))
  )

(use-package magit
  :defer t
  :commands (magit-add-section-hook)
  :config
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules
                          'magit-insert-stashes
                          'append))
;;; Tequila worms

(progn ;     startup
  (message "Loading %s...done (%.3fs)" user-init-file
           (float-time (time-subtract (current-time)
                                      before-user-init-time)))
  (add-hook 'after-init-hook
            (lambda ()
              (message
               "Loading %s...done (%.3fs) [after-init]" user-init-file
               (float-time (time-subtract (current-time)
                                          before-user-init-time))))
            t))

(progn ;     personalize
  (let ((file (expand-file-name (concat (user-real-login-name) ".el")
                                user-emacs-directory)))
    (when (file-exists-p file)
      (load file))))

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; init.el ends here
