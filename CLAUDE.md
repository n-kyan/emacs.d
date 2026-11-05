# Emacs config — notes for AI assistants

Owner: macOS (Apple Silicon), Emacs 31.1 (NS build). Keyboard layout: Graphite (angle mod).

## Layout
- `early-init.el` — runs before packages/GUI load. Keep tiny.
- `init.el` — the main config, one file, sections start with `;;; `.
  Add new settings to the matching section. If a section grows past ~100 lines,
  move it to `lisp/my-<topic>.el` (ending with `(provide 'my-<topic>)`) and `require` it.
- `lisp/` — the owner's own elisp. Each file ends with `(provide 'NAME)`.
- `custom.el` — written by Emacs (customize, package list). Don't hand-edit.
- `setup-checklist.org` — the setup plan we're working through.

## Rules
- NEVER run git commands that change anything (commit, push, pull, etc.) and never touch
  GitHub. Only edit files locally; the owner commits and pushes himself via Magit.
  If something needs doing on GitHub, tell him.
- Every package goes through `use-package` with `:ensure t` (or `:vc`), so a fresh
  clone installs everything on first launch.
- Comment every setting with a plain-English line saying what it does and why.
- Test before claiming something works:
  `emacs --batch -l ~/.emacs.d/init.el --eval '(message "OK")'`
  starts a separate, windowless Emacs that never touches the open one.
- The owner may have files open with unsaved edits. Don't rewrite a file the owner
  is editing; ask them to save first.
- System tools Emacs needs (basedpyright, ruff, gls, claude, …) go in `Brewfile`
  or are noted in README.md.
