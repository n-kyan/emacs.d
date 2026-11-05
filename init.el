;;; init.el --- My Emacs config  -*- lexical-binding: t -*-
;; Sections start with ";;; ". In this file, M-x outline-minor-mode
;; lets you fold them with TAB on a heading line.

;;; Emacs's own settings file
;; Emacs saves menu/customize settings and the installed-package list here,
;; so they don't get mixed into this file.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;;; Packages
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

;; My own elisp lives in lisp/
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Give Emacs your shell's PATH (Mac apps opened from the Dock don't get it)
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

;;; macOS
(when (eq system-type 'darwin)
  (setq mac-right-command-modifier 'meta
        mac-option-modifier 'none))

;; emacs -a command opens files in main frame instead of a new one
(setq ns-pop-up-frames nil)

;; Use GNU ls for Dired if installed (brew install coreutils); macOS's ls lacks options Dired wants
(when (executable-find "gls")
  (setq insert-directory-program "gls"))

;;; Look
(add-to-list 'default-frame-alist '(fullscreen . fullboth))
(scroll-bar-mode -1)

;; Modus theme: fringes use the normal background color (set BEFORE loading)
(setq modus-themes-common-palette-overrides
      '((fringe unspecified)))
(load-theme 'modus-vivendi t)

;;; Files: backups, auto-saves, lock files
;; Keep Emacs's safety copies in one folder instead of next to every file.
;; - Backups (file~): the previous version, saved when you first save an edit.
;; - Auto-saves (#file#): unsaved work, written every few seconds in case of a crash.
;; - Lock files (.#file): mark a file as being edited, so a second Emacs warns you.
(let ((backups (expand-file-name "backups/" user-emacs-directory))
      (autosaves (expand-file-name "auto-saves/" user-emacs-directory)))
  (make-directory backups t)
  (make-directory autosaves t)
  (setq backup-directory-alist `(("." . ,backups))
        auto-save-file-name-transforms `((".*" ,autosaves t))
        lock-file-name-transforms `((".*" ,autosaves t))))

;; Save files to disk automatically after 3 idle seconds, so the file on disk
;; always matches what you see (and outside edits, like Claude's, never conflict)
(setq auto-save-visited-interval 3)
(auto-save-visited-mode 1)

;; Reload files automatically when something else changes them (like Claude)
(global-auto-revert-mode 1)

;;; Completion
;; Live completion in the minibuffer (M-x, C-x b, C-x C-f)
(fido-vertical-mode 1)

;;; Editing
;; Can repeat commands by just pressing the last character over and over
(repeat-mode 1)

;; When a line is wider than the window, break it at a space, not mid-word
(setq-default word-wrap t)

;;; Programming: language servers
(use-package eglot
  :hook (python-mode . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode)
                 "basedpyright-langserver" "--stdio")))

;; Every Python project: use its uv .venv, less picky checking, find code in src/
(setq-default eglot-workspace-configuration
              (lambda (_server)
                `(:python (:pythonPath ,(expand-file-name ".venv/bin/python"))
                          :basedpyright (:analysis (:typeCheckingMode "standard"
								      :extraPaths ["src"])))))

;;; Programming: formatting
;; Format code on save (Python: ruff)
(use-package apheleia
  :ensure t
  :config
  (setf (alist-get 'python-mode apheleia-mode-alist) 'ruff
        (alist-get 'python-ts-mode apheleia-mode-alist) 'ruff)
  (apheleia-global-mode 1))

;;; Programming: Python
;; Python string formatting
(require 'python-strings)

;; Python TAB: smart indent on a line; shift right on a selection. C-TAB: shift left.
(defun my/python--shift (direction)
  "Shift the region, or the current line, one indent level in DIRECTION."
  (let ((beg (if (use-region-p) (region-beginning) (line-beginning-position)))
        (end (if (use-region-p) (region-end) (line-end-position))))
    (if (eq direction 'right)
        (python-indent-shift-right beg end)
      (python-indent-shift-left beg end))
    (setq deactivate-mark nil)))  ; keep the selection so you can press again

(defun my/python-tab ()
  "With a selection, shift it right one level. Otherwise, smart-indent the line."
  (interactive)
  (if (use-region-p)
      (my/python--shift 'right)
    (indent-for-tab-command)))    ; Python mode's normal smart TAB

(defun my/python-shift-left ()
  "Dedent the selected lines, or the current line, one level."
  (interactive)
  (my/python--shift 'left))

(with-eval-after-load 'python
  ;; Let repeated TAB presses cycle indent levels, as with the default TAB.
  (add-to-list 'python-indent-trigger-commands #'my/python-tab)
  (keymap-set python-base-mode-map "TAB" #'my/python-tab)
  (keymap-set python-base-mode-map "C-<tab>" #'my/python-shift-left))

;; Show marimo notebooks as cells (display only; M-n / M-p between cells, M-ret adds a new cell)
(require 'marimo-view)

;;; Git
(use-package magit
  :ensure t)

;;; Terminal and AI
;; Terminal emulator (needed for Claude Code's interface)
(use-package eat
  :ensure t)

;; Helper so Claude Code inherits Emacs's environment
(use-package inheritenv
  :vc (:url "https://github.com/purcell/inheritenv" :rev :newest))

;; Claude Code inside Emacs
(use-package claude-code
  :vc (:url "https://github.com/stevemolitor/claude-code.el" :rev :newest)
  :bind-keymap ("C-c c" . claude-code-command-map)
  :config
  (claude-code-mode))

;; Emacs help agent (C-c h)
(require 'help-agent)

;;; Prose
;; Center text in a comfortable column (prose files)
(use-package olivetti
  :ensure t
  :hook (text-mode . olivetti-mode))

;; Markdown editing (built on top of text mode)
(use-package markdown-mode
  :ensure t)

;;; Keyboard
;; Typing practice (M-x speed-type-text)
(use-package speed-type
  :ensure t)

;; Popup library (used by the Graphite cheatsheet)
(use-package posframe
  :ensure t)

;; Graphite layout popup
(require 'graphite-cheatsheet)


;; Remote Language Server
(with-eval-after-load 'tramp
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))
