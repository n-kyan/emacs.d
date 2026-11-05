;;; python-strings.el --- Wrap docstrings and marimo markdown  -*- lexical-binding: t -*-

;; ---------- Markdown-aware paragraph rules ----------
;; Lines matching this are never filled: blank lines, headings (#),
;; table rows (|), and blockquotes / doctest lines (>).
(defconst my/md-paragraph-separate "[ \t]*$\\|[ \t]*[#|>]"
  "Lines that separate paragraphs and are left exactly as written.")

;; A list item: -, *, + or "1." / "1)" followed by a space.
(defconst my/md-list-item "[ \t]*\\([-*+]\\|[0-9]+[.)]\\)[ \t]+"
  "A line that starts a markdown list item.")

;; ---------- Which strings are safe to reflow ----------
(defun my/python--fillable-string-p ()
  "Non-nil if point is inside a docstring or a mo.md(...) string.
Only these are reflowed: their line breaks don't change what they show."
  (let ((ppss (syntax-ppss)))             ; parser state at point
    (and (nth 3 ppss)                     ; inside some string
         (or (python-info-docstring-p ppss)  ; python.el's own docstring test
             (save-excursion
               (goto-char (nth 8 ppss))   ; nth 8 = where the string starts
               (skip-chars-backward "\"'") ; back over all its quote characters
               ;; Is it the argument of mo.md( ? Allow spaces/newlines and
               ;; prefixes like r or f between the paren and the quotes.
               (looking-back "mo\\.md([ \t\n]*[rRfFbBuU]*"
                             (max (point-min) (- (point) 80))))))))

;; ---------- Never break a line inside {...} ----------
(defun my/python--inside-braces-p ()
  "Non-nil if point is inside {...} within the current string.
Used as a fill rule: Emacs won't break a line where this returns non-nil."
  (let ((start (nth 8 (syntax-ppss))))    ; where the current string begins
    (when start
      (let ((text (buffer-substring-no-properties start (point))))
        ;; More { than } since the string began means we're inside a field.
        (> (- (seq-count (lambda (c) (eq c ?{)) text)
              (seq-count (lambda (c) (eq c ?})) text))
           0)))))

;; ---------- Continuation-line indent ----------
(defun my/python--line-prefix ()
  "Indent for lines that continue the current line's paragraph.
A list item continues under its text; anything else keeps its indent."
  (save-excursion
    (beginning-of-line)
    (make-string (if (looking-at my/md-list-item)
                     (- (match-end 0) (match-beginning 0)) ; under the item's text
                   (current-indentation))
                 ?\s)))

;; ---------- Fill one string's text ----------
(defun my/python--fill-text (beg end)
  "Reflow the text between BEG and END, one markdown paragraph at a time.
Each paragraph's continuation indent is set explicitly, so Emacs doesn't guess."
  (let ((end (copy-marker end))           ; moves along as filling changes text
        (adaptive-fill-mode nil)          ; we set the indent ourselves
        (fill-nobreak-predicate (cons #'my/python--inside-braces-p
                                      fill-nobreak-predicate)))
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
        (if (and (bolp) (looking-at my/md-paragraph-separate))
            (forward-line 1)              ; blank line, heading, table row: skip
          (let* ((pstart (point))
                 (opening-line (not (bolp))) ; text on the same line as the """
                 (prefix (my/python--line-prefix)))
            ;; Extend the paragraph until a separator, a new list item, or END.
            (forward-line 1)
            (when (and opening-line (< (point) end)
                       (not (looking-at my/md-paragraph-separate)))
              (setq prefix (my/python--line-prefix))) ; indent of the 2nd line
            (while (and (< (point) end)
                        (not (looking-at my/md-paragraph-separate))
                        (not (looking-at my/md-list-item)))
              (forward-line 1))
            (let ((pend (copy-marker (min end (if (bolp) (1- (point)) (point))))))
              (let ((fill-prefix prefix))
                (fill-region-as-paragraph pstart pend))
              (goto-char pend)
              (forward-line 1))))))))

;; ---------- On save: fill docstrings and mo.md strings ----------
(defun my/python-fill-all-strings ()
  "Reflow every docstring and mo.md(...) string to `fill-column'."
  (save-excursion                         ; put the cursor back afterwards
    (goto-char (point-min))
    ;; Find each """ or '''.
    (while (re-search-forward "\"\"\"\\|'''" nil t)
      (let* ((delim (match-string 0))
             (opener-start (match-beginning 0))
             (text-start (point))
             ;; One parse, from inside. (Parsing at the quote itself first
             ;; corrupts Emacs's parse cache: that was the original bug.)
             (ppss (syntax-ppss text-start)))
        ;; An OPENING quote: we're now inside a string that began at these quotes.
        (when (and (nth 3 ppss)
                   (<= opener-start (nth 8 ppss))
                   (< (nth 8 ppss) text-start))
          (let ((fillable (my/python--fillable-string-p))) ; point is inside it
            (when (search-forward delim nil t)            ; jump to the closing quote
              (let* ((close (copy-marker (- (point) 3)))
                     ;; Stop before trailing spaces/newlines, so a closing """
                     ;; on its own line stays there.
                     (text-end (save-excursion
                                 (goto-char close)
                                 (skip-chars-backward " \t\n" text-start)
                                 (point))))
                (when (and fillable
                           (> text-end text-start)
                           ;; Skip strings that contain a code block.
                           (not (save-excursion
                                  (goto-char text-start)
                                  (search-forward "```" text-end t))))
                  (my/python--fill-text text-start text-end))
                (goto-char close)
                (forward-char 3)))))))))  ; continue after this string

;; ---------- While typing: wrap only docstrings and mo.md strings ----------
(defun my/python-auto-fill ()
  "Wrap the current line at `fill-column', but only inside docstrings or
mo.md strings, on lines that aren't headings, tables or blockquotes."
  (when (and (my/python--fillable-string-p)
             (not (save-excursion         ; not a protected markdown line
                    (beginning-of-line)
                    (looking-at my/md-paragraph-separate))))
    (let ((adaptive-fill-mode nil)
          (fill-prefix (my/python--line-prefix))
          (fill-nobreak-predicate (cons #'my/python--inside-braces-p
                                        fill-nobreak-predicate)))
      (do-auto-fill))))                   ; Emacs's normal line wrapping

;; ---------- Turn it all on for Python buffers ----------
(defun my/python-text-setup ()
  "Python buffers: 88-column wrapping for docstrings and mo.md text."
  (setq-local fill-column 88)                              ; match ruff
  (setq-local normal-auto-fill-function #'my/python-auto-fill)
  (auto-fill-mode 1)                                       ; wrap while typing
  ;; Last argument t: add the hook for this buffer only.
  (add-hook 'before-save-hook #'my/python-fill-all-strings nil t))

;; python-base-mode is the parent of both Python modes (classic and tree-sitter).
(add-hook 'python-base-mode-hook #'my/python-text-setup)

(provide 'python-strings)
;;; python-strings.el ends here
