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
  (setq inhibit-startup-buffer-menu t)
  (setq inhibit-startup-screen t)
  (setq inhibit-startup-echo-area-message "locutus")
  (setq initial-buffer-choice t)
  (setq initial-scratch-message ""))

(eval-and-compile ; `use-package'
  (require  'use-package)
  (setq use-package-verbose t))

(use-package borg
  :load-path "lib/borg"
  :config
  (borg-initialize))

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
  :if nil
  :commands (server-running-p)
  :config (or (server-running-p) (server-mode)))

(progn ;     startup
  (message "Loading early birds...done (%.3fs)"
           (float-time (time-subtract (current-time)
                                      before-user-init-time))))

;;; Long tail
;;;; Pre
(use-package no-littering)

(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))
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
  (load-theme 'doom-dracula :no-confirm)
  (setq doom-themes-enable-bold t
	    doom-themes-enable-italic t))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-major-mode-icon t))

(use-package diff-hl
  :hook (after-init . global-diff-hl-mode)
  :config
  (setq diff-hl-draw-borders nil)
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
  (require 'dired+)
  (setq ls-lisp-dirs-first t)
  (setq dired-listing-switches "-lAhG --group-directories-first"
        dired-recursive-deletes 'always
        dired-recursive-copies 'always)
  (add-hook 'dired-mode-hook #'dired-hide-details-mode))

(use-package dired-x
  :hook (dired-mode . dired-omit-mode)
  :config
  (setq dired-omit-files
        (rx bos (or (seq "desktop.ini")
                    (seq ?~ (? ?$) (* (or alnum (category chinese-two-byte))) (? ".tmp"))
                    eos))))

(use-package dired-aux
  :defer
  :custom
  (dired-compress-directory-default-suffix ".7z")
  :config
  (add-to-list 'dired-compress-files-alist '("\\.7z\\'" . "7z a %o -r %i"))
  (add-to-list 'dired-compress-file-suffixes `(,(rx ?. (or "7z" "zip" "rar" "gz") eos)
                                               ""
                                               "7z x -aoa -o%o %i")))

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

(use-package vertico-posframe
  :hook (vertico-mode . vertico-posframe-mode)
  :init
  (setq vertico-posframe-parameters
        '((left-fringe  . 12)
          (right-fringe . 12))))

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

(use-package nerd-icons-completion
  :hook (after-init . nerd-icons-completion-mode))

;;;; Navigation
(use-package rg
  :bind
  ("M-s r" . rg-menu))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package tabspaces
  :hook (after-init . tabspaces-setup)
  :custom
  (tab-bar-show nil)
  (tabspaces-keymap-prefix "C-c SPC")
  (tabspaces-use-filtered-buffers-as-default t)
  (tabspaces-default-tab "Default")
  (tabspaces-remove-to-default t)
  (tabspaces-include-buffers '("*scratch*" "*Messages*"))
  (tabspaces-initialize-project-with-todo t)
  ;; sessions
  (tabspaces-session t)
  (tabspaces-session-auto-restore t)
  :config
  (define-keymap
    :keymap tabspaces-command-map
    "K" #'tabspaces-clear-buffers
    "k" #'tabspaces-remove-selected-buffer
    "d" #'tabspaces-remove-current-buffer
    "q" #'tabspaces-close-workspace
    "Q" #'tabspaces-kill-buffers-close-workspace)
  (defun tabspaces-setup ()
    "Set up tabspace at startup."
    (tabspaces-mode)
    (tab-bar-rename-tab "Home")
    (dolist (name '("*Messages*" "*scratch*"))
      (when-let ((buf (get-buffer name)))
        (set-frame-parameter nil
                             'buffer-list
                             (cons buf (frame-parameter nil 'buffer-list)))))))

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
  :hook (after-init . global-aggressive-indent-mode))

(use-package hl-todo
  :hook (after-init . global-hl-todo-mode))

;;;; Help
(use-package help
  :bind (:map help-map
              ("K" . describe-keymap))
  :custom
  (help-window-select t)
  (help-window-keep-selected t)
  :config
  (temp-buffer-resize-mode))

;;;; `isearch'
(use-package isearch-mb
  :hook (after-init . isearch-mb-mode)
  :custom
  (isearch-lazy-count t)
  (search-ring-max 200)
  (regexp-search-ring-max 200)
  (isearch-allow-scroll t)
  (isearch-regexp-lax-whitespace t)
  (search-whitespace-regexp "\\W+")
  :bind
  (("C-s" . isearch-forward-regexp)
   ("C-r" . isearch-backward-regexp))
  :config
  (add-to-list 'isearch-mb--with-buffer #'isearch-yank-word)
  (define-key isearch-mb-minibuffer-map (kbd "M-w") #'isearch-yank-word)

  (add-to-list 'isearch-mb--after-exit #'avy-isearch)
  (define-key isearch-mb-minibuffer-map (kbd "C-'") #'avy-isearch)

  (advice-add 'isearch-lazy-count-format :filter-return
              (lambda (s) "Fontification of the lazy count."
                (propertize s 'face (cond
                                     ((not isearch-success) 'error)
                                     (isearch-wrapped 'warning)
                                     (t 'success)))))
  (advice-add 'isearch-cancel :before
              (lambda () "Add search string to history also when canceling."
                (unless (string-equal "" isearch-string)
                  (isearch-update-ring isearch-string isearch-regexp))))
  )


(use-package man
  :defer t
  :config (setq Man-width 80))

;;;; Program
(use-package treesit-auto
  :init
  (defvar treesit:load-path (no-littering-expand-var-file-name "tree-sitter")
    "The directory where install compile and load a tree-sitter language grammar library.")
  :hook
  (after-init . global-treesit-auto-mode)
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

(use-package prog-mode
  :defer
  :config
  ;;(add-hook 'prog-mode-hook #'global-prettify-symbols-mode)
  (add-hook 'prog-mode-hook #'flymake-mode)
  ;;(add-hook 'prog-mode-hook #'eldoc-mode)
  (defun prog:indent-spaces-mode ()
    (setq indent-tabs-mode nil)))

(use-package elisp-mode
  :defer
  :config
  (add-hook 'emacs-lisp-mode-hook #'reveal-mode)
  (add-hook 'lisp-interaction-mode-hook #'prog:indent-spaces-mode))

(use-package rust-ts-mode
  :mode "\\.rs\\'")

(use-package cargo
  :hook (rust-ts-mode-hook . cargo-minor-mode)
  :custom
  (compilation-ask-about-save nil)
  :config
  (define-key cargo-mode-map (kbd "C-c") 'cargo-minor-mode-command-map))

(use-package julia-ts-mode :defer)

(use-package eglot-jl
  :hook (julia-ts-mode . eglot-jl-init))

(use-package julia-repl
  :hook (julia-ts-mode . julia-repl-mode)
  :config
  (when linux-p
    (julia-repl-set-terminal-backend 'vterm)))

(use-package julia-formatter
  :hook (julia-ts-mode . julia-formatter-mode))

(use-package typescript-ts-mode
  :mode "\\.ts\\'")

(use-package js-ts-mode
  :mode "\\.js\\'")

(use-package scad-mode :defer) ;;`scad-mode' to edit OpenSCAD files

(use-package ebuild-mode ;; Gentoo build script
  :defer)

(use-package yaml-ts-mode
  :mode "\\.ya?ml\\'")

(use-package yasnippet
  :hook (after-init . yas-global-mode))

(use-package eglot
  :hook
  ((rust-mode . eglot-ensure)
   (rust-ts-mode . eglot-ensure)
   (julia-mode . eglot-ensure)
   (julia-ts-mode . eglot-ensure)
   (typescript-ts-mode . eglot-ensure)
   (js-ts-mode . eglot-ensure)
   (scad-mode . eglot-ensure))
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

(use-package eldoc-box
  :custom
  (eldoc-box-offset '(20 20 16))
  (eldoc-box-only-multi-line t)
  :config
  (add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-mode t))

;;;; `Text'
(use-package text-mode
  :init
  (setq-default major-mode 'text-mode))

(use-package git-modes :defer)

(use-package csv-mode
  :if win-wsl-p
  :mode "\\.ptd\\'"
  :config
  (add-hook 'csv-mode-hook
            (lambda ()
              (when (string-match-p "\\.ptd\\'" (buffer-file-name))
                (csv-set-comment-start "!")
                (csv-set-separator (string-to-char " "))))))

(use-package conf-mode
  :if win-wsl-p
  :mode ("\\.pro\\'" . conf-space-mode))

(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode))

(use-package latex
  :mode ("\\.tex\\'" . latex-mode)
  :hook
  ((LaTeX-mode-hook . LaTeX-math-mode)
   (LaTeX-mode-hook . turn-on-reftex)
   (LaTeX-mode-hook . TeX-PDF-mode))
  :custom
  ;; use hidden dirs for auctex files
  (TeX-check-TeX nil)
  ;; Show output of Tex compilation in other window.
  ;; (TeX-show-compilation t)
  ;; Automatically save style information when saving the buffer.
  (TeX-auto-save t)
  ;; Parse file after loading it if no style hook is found for it.
  (TeX-parse-self t)
  ;; Automatically untabify when saving the buffer.
  (TeX-auto-untabify t)
  ;; If non-nil, ask user for permission to save files before starting TeX.
  (TeX-save-query nil)
  (TeX-electric-math '("$" . "$"))
  ;; view by externally program.
  (TeX-view-program-selection '((output-pdf "wslview")))
  ;; Control if server should be started for inverse search.
  (TeX-source-correlate-start-server t)
  ;; Style
  (LaTeX-default-style "standalone")
  (reftex-plug-into-AUCTeX t)
  (TeX-clean-confirm nil)
  :config
  (setq-default TeX-master nil
                TeX-engine 'xetex
                TeX-output-dir "build")
  ;; Revert the PDF-buffer after the TeX compilation has finished
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  (TeX-source-correlate-mode)

  ;; Config latemk
  (add-hook 'LaTeX-mode-hook #'latexmk-setup)
  (defun latexmk-setup ()
    (setq TeX-command-default "LaTexmk"
          TeX-command-extra-options (cond
                                     ((eq TeX-engine 'xetex)"-shell-escape")
                                     (t ""))
          LaTeX-clean-intermediate-suffixes
          (append LaTeX-clean-intermediate-suffixes '("\\.fdb_latexmk" "\\.aux.bak" "\\.fls")))
    (add-to-list 'TeX-expand-list
                 '("%(-PDF)"
                   (lambda ()
                     (cond
                      ((and (eq TeX-engine 'default)
                            TeX-PDF-mode)
                       "-pdf")
                      ((eq TeX-engine 'xetex) "-xelatex ")
                      (t "")))))
    (add-to-list 'TeX-command-list
                 '("LaTexmk"
                   "latexmk %(-PDF) %(mode) %(file-line-error) %(extraopts) %(output-dir) %S%(PDFout) %t"
                   TeX-run-format
                   nil
                   (latex-mode doctex-mode)
                   :help "Run Latexmk")))

  ;; See: https://github.com/tom-tan/auctex-latexmk/issues/28
  ;;   &  https://stackoverflow.com/questions/3124273/compile-xelatex-tex-file-with-latexmk
  (advice-add #'TeX-output-extension :before #'latexmk--TeX-output-extension)
  (defun latexmk--TeX-output-extension ()
    (when (and TeX-PDF-mode
               (eq TeX-engine 'xetex)
               (string-match-p "latexmk" TeX-command-default))
      (unless (listp TeX-output-extension)
        (setq TeX-output-extension (list TeX-output-extension)))))
  )

(use-package cdlatex
  :hook ((LaTeX-mode-hook . turn-on-cdlatex)
         (latex-mode-hook . turn-on-cdlatex))
  :custom
  (cdlatex-math-modify-alist '((?B "\\mathbb" nil t nil nil)))
  (cdlatex-math-symbol-alist '((?c ("\\(?\\)" )))))

(use-package org
  :bind
  ("C-c a" . org-agenda)
  :custom
  (org-startup-truncated t)
  (org-preview-latex-default-process 'dvisvgm)
  :config
  (require 'org-tempo)
  (setq org-M-RET-may-split-line '((default . nil)))
  (setq org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil)
  (setq org-refile-targets `((nil :level . 1)
                             ("emacs.org" :level . 1)
                             ("work.org" :level . 1)
                             ("toys.org" :level . 1)
                             ("projects.org" :level . 1))
        ;; `entry' 放置在子节点首位
        org-reverse-note-order t)
  (when (fboundp 'turn-on-org-cdlatex)
    (add-hook 'org-mode-hook #'turn-on-org-cdlatex))
  (add-hook 'org-mode-hook (lambda ()
                             (electric-pair-local-mode -1))))

(use-package org-capture
  :bind
  ("C-c c" . org-capture))

(use-package org-modern
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda)))

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
  (rime-disable-predicates '(rime-predicate-after-ascii-char-p
                             rime-predicate-prog-in-code-p
                             +rime-predicate-org-latex-mode-p
                             rime-predicate-org-in-src-block-p
                             rime-predicate-tex-math-or-command-p
                             rime-predicate-space-after-cc-p))
  (rime-inline-predicates '(rime-predicate-punctuation-line-begin-p))
  :config
  (setq rime-translate-keybindings '("C-f" "C-b" "C-n" "C-p" "C-g" "C-v" "M-v" "," "." "S-<return>"))
  (defun +rime-predicate-org-latex-mode-p ()
    "If cursor is inside an org-mode's LaTeX fragment, macro or its arguments."
    (and (derived-mode-p  'org-mode)
         (or (and (fboundp 'texmathp) (texmathp))
             (org-inside-latex-macro-p)))))

(use-package vterm
  :bind (:map vterm-mode-map
              ("C-q" . vterm-send-next-key)
              ("<C-backspace>" . (lambda () (interactive) (vterm-send-key (kbd "C-w")))))
  :config
  (add-to-list 'display-buffer-alist
               '((lambda (buffer-or-name _)
                   (let ((buffer (get-buffer buffer-or-name)))
                     (with-current-buffer buffer
                       (or (equal major-mode 'vterm-mode)
                           (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
                 (display-buffer-reuse-window display-buffer-in-side-window)
                 (side . bottom)
                 ;;(dedicated . t) ;dedicated is supported in emacs27
                 (reusable-frames . visible)
                 (window-height . 0.3))))

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
