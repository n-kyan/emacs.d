;;; marimo-view.el --- Read marimo notebooks as notebooks  -*- lexical-binding: t -*-

;; A display layer only: the file on disk is never changed.
;; - Hides marimo's boilerplate (file header, @app.cell + def lines, footer).
;; - Cells are separated by blank lines around each divider.
;; - Draws a divider at each cell, listing the variables the cell uses.
;; - Shrinks each cell's `return (...)' to one dim line of what it defines.
;; - M-n / M-p jump to the next / previous cell.
;; Turn it off with M-x my/marimo-view-mode to edit the hidden parts.

;; ---------- Faces (colors) ----------
(defface my/marimo-view-divider
  '((t :inherit shadow :weight bold))
  "Face for the line drawn at the start of each cell.")

(defface my/marimo-view-dim
  '((t :inherit shadow))
  "Face for the one-line summary of what a cell defines.")

;; ---------- Finding the pieces ----------
(defconst my/marimo-view--decorator
  "^@app\\.\\(cell\\|function\\|class_definition\\)\\((.*)\\)?[ \t]*$"
  "A line that starts a cell: @app.cell, @app.function or @app.class_definition.")

(defconst my/marimo-view--cell-start
  (concat my/marimo-view--decorator "\\|^with app\\.setup")
  "Where a cell begins, including the special setup cell.")

(defun my/marimo-view--names-in (beg end)
  "Return the comma-separated names between BEG and END, as one string."
  (let ((text (buffer-substring-no-properties beg end)))
    (string-join (split-string text "[(),\n \t]+" t) ", ")))

(defun my/marimo-view--fit (names room)
  "NAMES (a comma-separated string), cut to fit ROOM columns with \"+N more\"."
  (let ((parts (split-string names ", " t)) (out nil) (used 0))
    (while (and parts
                (<= (+ used (string-width (car parts)) 2 12) room))
      (setq used (+ used (string-width (car parts)) 2))
      (push (pop parts) out))
    (concat (string-join (nreverse out) ", ")
            (when parts (format "%s+%d more" (if out ", " "") (length parts))))))

(defun my/marimo-view--divider (label)
  "A full-width divider line showing LABEL."
  (let* ((head (concat "━━ " label " "))
         (width (max 10 (- (or fill-column 88) (string-width head)))))
    (propertize (concat head (make-string width ?━) "\n")
                'face 'my/marimo-view-divider)))

;; ---------- How it's drawn ----------
;; Each gap between two cells' code (the previous cell's `return', blank lines,
;; the next cell's decorator and `def' line) is covered by ONE overlay whose
;; `display' text replaces it: the summary line, a blank line, the divider,
;; and another blank line. One overlay per gap matters: when several hidden
;; pieces touch, Emacs draws only one of their texts, which dropped newlines.

(defun my/marimo-view--clear ()
  "Remove every marimo-view overlay in the buffer."
  (remove-overlays (point-min) (point-max) 'my/marimo-view t))

(defun my/marimo-view--cover (beg end text)
  "Show TEXT in place of BEG..END (the buffer itself is unchanged)."
  (when (< beg end)
    ;; front-advance t: typing at BEG lands before the overlay, i.e. visibly.
    (let ((ov (make-overlay beg end nil t nil)))
      (overlay-put ov 'my/marimo-view t)  ; tag, so we can find and delete ours
      (overlay-put ov 'evaporate t)       ; vanish if its text is deleted
      (overlay-put ov 'display text))))

(defun my/marimo-view--cell-end (start)
  "Return where the cell beginning at START ends: the next cell, or the footer."
  (save-excursion
    (goto-char start)
    (forward-line 1)
    (if (re-search-forward (concat my/marimo-view--cell-start
                                   "\\|^if __name__ == .__main__.:")
                           nil t)
        (line-beginning-position)
      (point-max))))

(defun my/marimo-view--code-end (from body)
  "Where the cell's code really ends: back from FROM over blank space.
Never earlier than the end of the first body line (at BODY), so a new,
empty cell keeps a visible line to type on."
  (save-excursion
    (goto-char from)
    (skip-chars-backward " \t\n" (save-excursion (goto-char body)
                                                 (line-end-position)))
    (point)))

(defun my/marimo-view--parse-cell (start number)
  "Describe the NUMBERth cell, whose first line begins at START.
Returns (BODY CODE-END DIVIDER SUMMARY): where its code starts and ends,
its divider text, and its \"defines\" line (or nil)."
  (save-excursion
    (goto-char start)
    (let* ((end (my/marimo-view--cell-end start))
           (kind (if (looking-at my/marimo-view--decorator) (match-string 1) "setup"))
           (hidden (looking-at ".*hide_code=True"))
           (label (format "%s %d" (pcase kind
                                    ("class_definition" "class")
                                    (_ kind))
                          number))
           (summary nil)
           body code-end)
      (forward-line 1)
      (if (and (equal kind "cell")
               (looking-at "[ \t]*def \\([[:alnum:]_]+\\)[ \t]*("))
          ;; @app.cell: the decorator and `def _(...):' line(s) get covered.
          (let* ((name (match-string 1))
                 (args-start (1- (match-end 0)))
                 (args-end (progn (goto-char args-start) (forward-sexp) (point)))
                 (uses (my/marimo-view--names-in (1+ args-start) (1- args-end)))
                 (base (concat label
                               (unless (equal name "_") (concat " · " name))
                               (when hidden " · code hidden in app")))
                 ;; Fit the "uses" list into what's left of the line.
                 (room (- (or fill-column 88) (string-width base) 20)))
            (setq label (concat base
                                (unless (string-empty-p uses)
                                  (concat " · uses "
                                          (my/marimo-view--fit uses room)))))
            (forward-line 1)
            (setq body (point))
            ;; The cell's own `return' is indented exactly 4 spaces.
            (goto-char end)
            (if (re-search-backward "^    return\\b" body t)
                (let ((ret-start (point)))
                  (forward-word 1)
                  (skip-chars-forward " \t")
                  (let* ((value-start (point))
                         (ret-end (progn (if (eq (char-after) ?\()
                                             (forward-sexp)   ; (a, b, ...)
                                           (end-of-line))
                                         (point)))
                         (defines (my/marimo-view--names-in value-start ret-end)))
                    (unless (string-empty-p defines)
                      (setq summary
                            (propertize
                             (concat "    ↳ defines "
                                     (my/marimo-view--fit
                                      defines (- (or fill-column 88) 14)))
                             'face 'my/marimo-view-dim)))
                    (setq code-end (my/marimo-view--code-end ret-start body))))
              (setq code-end (my/marimo-view--code-end end body))))
        ;; @app.function / @app.class_definition: the def is real code, so
        ;; only the decorator is covered. `with app.setup:' is covered too.
        (setq body (point)
              code-end (my/marimo-view--code-end end body)))
      (list body code-end (my/marimo-view--divider label) summary))))

(defun my/marimo-view--refresh ()
  "Rebuild the whole view from the current text."
  (save-excursion
    (save-match-data
      (my/marimo-view--clear)
      (goto-char (point-min))
      (let ((cells nil) (n 0))
        ;; Describe every cell first.
        (while (re-search-forward my/marimo-view--cell-start nil t)
          (setq n (1+ n))
          (let ((start (line-beginning-position)))
            ;; One malformed cell (e.g. mid-edit) shouldn't break the rest.
            (let ((cell (ignore-errors (my/marimo-view--parse-cell start n))))
              (when cell (push cell cells)))
            (goto-char start)
            (forward-line 1)))
        (setq cells (nreverse cells))
        (when cells
          ;; Top of file down to cell 1's code: divider, blank line.
          (my/marimo-view--cover (point-min) (nth 0 (car cells))
                                 (concat (nth 2 (car cells)) "\n"))
          ;; Between cells: end the code line, summary, blank, divider, blank.
          (let ((prev (car cells)))
            (dolist (cell (cdr cells))
              (my/marimo-view--cover
               (nth 1 prev) (nth 0 cell)
               (concat "\n"
                       (when (nth 3 prev) (concat (nth 3 prev) "\n"))
                       "\n"
                       (nth 2 cell)
                       "\n"))
              (setq prev cell))
            ;; After the last cell's code: its summary; the footer is covered.
            (my/marimo-view--cover
             (nth 1 prev) (point-max)
             (concat "\n" (when (nth 3 prev) (concat (nth 3 prev) "\n"))))))))))

;; ---------- Keep the view current while editing ----------
(defvar-local my/marimo-view--timer nil "Pending refresh, if any.")

(defun my/marimo-view--after-change (&rest _)
  "Schedule a refresh shortly after edits stop (cheap even while typing fast)."
  (unless my/marimo-view--timer
    (let ((buf (current-buffer)))
      (setq my/marimo-view--timer
            (run-with-idle-timer
             0.3 nil
             (lambda ()
               (when (buffer-live-p buf)
                 (with-current-buffer buf
                   (setq my/marimo-view--timer nil)
                   (when my/marimo-view-mode
                     (my/marimo-view--refresh))))))))))

;; ---------- Moving between cells ----------
(defun my/marimo-view--goto-body ()
  "From a cell's first line, move to its first line of code."
  (forward-line 1)
  ;; Skip the (hidden) def line(s) to the first body line.
  (when (looking-at "[ \t]*def ")
    (search-forward "(") (backward-char) (forward-sexp) (forward-line 1))
  (back-to-indentation))

(defun my/marimo-view-next-cell ()
  "Jump to the first line of code in the next cell."
  (interactive)
  (end-of-line)
  (if (re-search-forward my/marimo-view--cell-start nil t)
      (progn (beginning-of-line) (my/marimo-view--goto-body))
    (message "Last cell")))

(defun my/marimo-view-previous-cell ()
  "Jump to the first line of code in the previous cell."
  (interactive)
  ;; The first search finds the start of this cell; the second, the one before.
  (if (and (re-search-backward my/marimo-view--cell-start nil t)
           (re-search-backward my/marimo-view--cell-start nil t))
      (my/marimo-view--goto-body)
    (goto-char (point-min))
    (my/marimo-view-next-cell)
    (message "First cell")))

;; ---------- Adding a cell ----------
(defun my/marimo-new-cell ()
  "Insert an empty cell after the current one, with the cursor in it.
Just write the code: on save, `marimo check --fix' fills in the def and
return lines with the variables the cell uses and defines."
  (interactive)
  (end-of-line)
  ;; Insert just before the next cell, or before the footer if this is the last.
  (goto-char (if (re-search-forward
                  (concat my/marimo-view--cell-start "\\|^if __name__ == .__main__.:")
                  nil t)
                 (line-beginning-position)
               (point-max)))
  (insert "@app.cell\ndef _():\n    \n    return\n\n\n")
  (forward-line -4)                       ; back to the empty body line
  (end-of-line)
  ;; Redraw now: the new text landed inside a hidden gap, and Emacs would
  ;; otherwise move the cursor out of it, into the next cell.
  (my/marimo-view--refresh))

;; ---------- On save: fill in def/return lists, then format ----------
;; `marimo check --fix' rewrites each cell's `def _(...)' and `return (...)'
;; from the code. It only accepts marimo files, so it's set per buffer below.
;; Wrapped in `sh' so a notebook with errors still gets formatted by ruff:
;; marimo exits with an error code when it finds problems it can't fix.
(with-eval-after-load 'apheleia
  (setf (alist-get 'marimo-fix apheleia-formatters)
        '("sh" "-c"
          "uv run --quiet marimo check --fix \"$0\" >/dev/null 2>&1; exit 0"
          inplace)))

;; ---------- The minor mode ----------
(defvar my/marimo-view-mode-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "M-n" #'my/marimo-view-next-cell)
    (keymap-set map "M-p" #'my/marimo-view-previous-cell)
    (keymap-set map "M-RET" #'my/marimo-new-cell)
    map)
  "Keys active while viewing a marimo notebook.")

(define-minor-mode my/marimo-view-mode
  "Show a marimo notebook as cells, hiding marimo's boilerplate.
The file is never changed. Turn off to see and edit the raw text."
  :lighter " Marimo"
  (if my/marimo-view-mode
      (progn
        (my/marimo-view--refresh)
        (add-hook 'after-change-functions #'my/marimo-view--after-change nil t))
    (remove-hook 'after-change-functions #'my/marimo-view--after-change t)
    (my/marimo-view--clear)))

(defun my/marimo-view-maybe-enable ()
  "Turn on `my/marimo-view-mode' if this Python file is a marimo notebook."
  (when (save-excursion
          (goto-char (point-min))
          (re-search-forward "^app = marimo\\.App(" 5000 t))
    (my/marimo-view-mode 1)
    ;; This buffer only: marimo fills in def/return lists, then ruff formats.
    (setq-local apheleia-formatter '(marimo-fix ruff))))

(add-hook 'python-base-mode-hook #'my/marimo-view-maybe-enable)

(provide 'marimo-view)
;;; marimo-view.el ends here
