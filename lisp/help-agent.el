;;; help-agent.el --- Persistent Emacs Q&A agent  -*- lexical-binding: t -*-

;; ---------- Settings ----------
(defvar my/help-agent-model "sonnet" "Model alias the agent uses.")

(defvar my/help-agent-reset-after (* 10 60)
  "Seconds after your last question before the agent's memory is cleared.")

(defvar my/help-agent-max-retries 2
  "How many times the agent may fix an answer that failed Emacs's checks.")

(defconst my/help-agent-prompt
  "You answer Emacs questions. Be minimal. Pick exactly ONE format below and output nothing else.
In EVERY format, wrap every key sequence and every command name in backticks, like `C-x 2` and `split-window-below`. Never use backticks inside code lines.

1. The answer is one or more commands or keys:
Command: `<keys>`
Name: `<command-name>`
Description: <one short sentence>
(one block per command, blank line between blocks)

2. A yes/no question:
Answer: <Yes/No/Unsure>
Explanation: <one or two short sentences>

3. How to do a multi-step task:
A numbered list. Each step is a few words.
If a step needs code, put only the code on the next line, indented.

4. Anything else: at most two short sentences.

If the message asks several questions, answer each in turn, each in its own format.

Messages starting with 'Emacs checked your answer' or '(Facts from the user's Emacs' come from automatic checks of the user's real Emacs. Trust them over your own memory."
  "Instructions sent to the agent when it starts.")

;; ---------- State ----------
(defvar my/help-agent-process nil "The running Claude Code process.")
(defvar my/help-agent-partial "" "Output received so far that isn't a full line yet.")
(defvar my/help-agent-timer nil "Timer that clears memory after inactivity.")
(defvar my/help-agent-pending nil
  "The question being answered, as (QUESTION . ORIGIN-BUFFER), or nil.")
(defvar my/help-agent-retries 0 "Fix attempts used for the pending question.")
(defvar my/help-agent-notes nil "Facts from the last checks, sent with the next question.")
(defvar my/help-agent-history nil
  "All answered questions, oldest first: (QUESTION ANSWER ORIGIN REVISED).")
(defvar my/help-agent-index 0 "Which history entry is being shown (0 = oldest).")
(defvar my/help-agent--issues nil "Problems found by the check in progress.")
(defvar my/help-agent--facts nil "Neutral facts found by the check in progress.")

;; ---------- Start the agent ----------
(defun my/help-agent--start ()
  "Start Claude Code in the background, ready for questions."
  ;; Run from ~/.emacs.d so no project memory or CLAUDE.md gets loaded.
  (let ((default-directory (expand-file-name "~/.emacs.d/")))
    (setq my/help-agent-partial "")
    (setq my/help-agent-process
          (make-process
           :name "help-agent"
           :command (list "claude" "-p"
                          "--model" my/help-agent-model
                          "--tools" ""                    ; no built-in tools
                          "--disallowedTools" "mcp__*"    ; no connector tools
                          "--disable-slash-commands"      ; no skills
                          "--no-session-persistence"      ; don't save history
                          "--input-format" "stream-json"  ; questions arrive as JSON lines
                          "--output-format" "stream-json" ; answers leave as JSON lines
                          "--verbose"
                          "--system-prompt" my/help-agent-prompt)
           :connection-type 'pipe   ; a pipe, like `cat |` in the terminal test
           :noquery t               ; don't ask about this process when quitting Emacs
           :filter #'my/help-agent--filter))))

(defun my/help-agent--send (text)
  "Send TEXT to the agent as one user message."
  (process-send-string
   my/help-agent-process
   (concat (json-serialize
            `((type . "user")
              (message . ((role . "user") (content . ,text)))))
           "\n")))

;; ---------- Read its output ----------
(defun my/help-agent--filter (_proc output)
  "Collect OUTPUT; when a full `result' line arrives, handle the answer."
  ;; Output can arrive in pieces, so glue it onto what we had.
  (setq my/help-agent-partial (concat my/help-agent-partial output))
  (let ((lines (split-string my/help-agent-partial "\n")))
    ;; The last piece may be an unfinished line: keep it for next time.
    (setq my/help-agent-partial (car (last lines)))
    (dolist (line (butlast lines))
      (let ((event (ignore-errors (json-parse-string line :object-type 'alist))))
        (when (equal (alist-get 'type event) "result")
          (my/help-agent--handle-answer (alist-get 'result event)))))))

;; ---------- NEW: check before showing; ask for fixes ----------
(defun my/help-agent--handle-answer (answer)
  "Check ANSWER against this Emacs. Send problems back for a fix, or show it."
  (when my/help-agent-pending
    (let* ((origin (cdr my/help-agent-pending))
           (found (my/help-agent--check answer origin))
           (issues (car found))
           (facts (cdr found)))
      (if (and issues (< my/help-agent-retries my/help-agent-max-retries))
          ;; Problems, and attempts left: ask for a corrected answer.
          (progn
            (setq my/help-agent-retries (1+ my/help-agent-retries))
            (message "Helper: fixing %d problem(s)..." (length issues))
            (my/help-agent--send
             (concat "Emacs checked your answer against the user's actual Emacs and found problems:\n"
                     (mapconcat (lambda (s) (concat "- " s)) issues "\n")
                     "\nRewrite the whole answer in the same format, fixing these. "
                     "Use only commands and keys that exist.")))
        ;; Clean, or out of attempts: show it, and keep the facts for next time.
        (setq my/help-agent-notes (append issues facts))
        (setq my/help-agent-history
              (append my/help-agent-history
                      (list (list (car my/help-agent-pending) answer origin
                                  (> my/help-agent-retries 0)))))
        (setq my/help-agent-pending nil
              my/help-agent-retries 0
              my/help-agent-index (1- (length my/help-agent-history)))
        (my/help-agent--render)))))

(defun my/help-agent--check (answer origin)
  "Check ANSWER in a scratch buffer. Return (ISSUES . FACTS)."
  ;; These variables are defvar'd, so `let' rebinds them for everything
  ;; called inside, including the checking functions below.
  (let ((my/help-agent--issues nil)
        (my/help-agent--facts nil))
    (with-temp-buffer
      (insert answer)
      (my/help-agent--verify origin))
    ;; `push' adds to the front, so reverse to get the original order.
    (cons (nreverse my/help-agent--issues) (nreverse my/help-agent--facts))))

;; ---------- The helper buffer's own mode and keys ----------
(defvar my/help-agent-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-v") #'my/help-agent-newer)  ; forward = newer
    (define-key map (kbd "M-v") #'my/help-agent-older)  ; back = older
    map)
  "Keys active only in the *Emacs Helper* buffer.")

(define-derived-mode my/help-agent-mode special-mode "Helper"
  "Mode for the Emacs help agent's answers. Builds on `special-mode'
\(read-only, q to close) and adds the keys in `my/help-agent-mode-map'.")

;; ---------- Display ----------
(defun my/help-agent--render ()
  "Show the history entry at `my/help-agent-index'."
  (let* ((entry (nth my/help-agent-index my/help-agent-history))
         (question (nth 0 entry))
         (answer (nth 1 entry))
         (origin (nth 2 entry))
         (total (length my/help-agent-history)))
    (with-current-buffer (get-buffer-create "*Emacs Helper*")
      (unless (derived-mode-p 'my/help-agent-mode)
        (my/help-agent-mode))
      (let ((inhibit-read-only t)          ; allow our own edits
            (my/help-agent--issues nil)    ; display only: don't collect
            (my/help-agent--facts nil))
        (erase-buffer)
        (insert answer)
        (my/help-agent--verify origin))
      ;; Top line: position, the question, and whether it was corrected.
      ;; "%" is special there, so double it.
      (setq header-line-format
            (format " %d/%d   Q: %s%s"
                    (1+ my/help-agent-index) total
                    (string-replace "%" "%%" question)))
      (goto-char (point-min)))
    (display-buffer "*Emacs Helper*")))

;; ---------- Check everything in backticks ----------
(defun my/help-agent--verify (origin)
  "Check every backticked key or command name in the answer, then hide the backticks.
Keys are looked up as they behave in ORIGIN, the buffer you asked from."
  (goto-char (point-min))
  ;; Find each `...` on a single line. Point ends up after the closing backtick.
  (while (re-search-forward "`\\([^`\n]+\\)`" nil t)
    ;; Save positions now: the checks below overwrite Emacs's match data.
    (let* ((text (match-string 1))
           (open (match-beginning 0))         ; the opening backtick
           (close (1- (match-end 0)))         ; the closing backtick
           (beg (match-beginning 1))
           (end (match-end 1))
           (line-start (line-beginning-position))
           (sym (intern-soft text)))
      ;; Hide both backticks: still in the text for our checks, just not displayed.
      (put-text-property open (1+ open) 'invisible t)
      (put-text-property close (1+ close) 'invisible t)
      (cond
       ;; A valid key description like "C-x [" or "M-f".
       ((key-valid-p text)
        (my/help-agent--annotate-key text origin line-start))
       ;; "M-x some-command": check the command after M-x.
       ((string-match "\\`M-x \\([^ ]+\\)\\'" text)
        (my/help-agent--check-command (match-string 1 text) beg end))
       ;; A plain command or function name.
       ((and sym (fboundp sym))
        (my/help-agent--check-command text beg end))
       ;; Looks like a command name but doesn't exist here.
       ((string-match-p "\\`[a-z][a-z0-9/-]*\\'" text)
        (insert " [✗ not found]")
        (push (format "`%s` is not a command in this Emacs" text)
              my/help-agent--issues))))))

(defun my/help-agent--check-command (name beg end)
  "If NAME is a real command, make BEG..END a docs link and add ✓; else ✗."
  (let ((sym (intern-soft name)))
    (if (and sym (fboundp sym))
        (progn
          (make-text-button beg end
                            'action (lambda (_) (describe-function sym))
                            'help-echo "Open the real documentation")
          (insert " [✓]"))
      (insert " [✗ not found]")
      (push (format "`%s` is not a command in this Emacs" name)
            my/help-agent--issues))))

(defun my/help-agent--annotate-key (keys origin line-start)
  "Insert what KEYS actually runs in ORIGIN, and record problems and facts.
On a `Command:' line, compare it with the `Name:' on the next line."
  (let ((binding (my/help-agent--lookup keys origin))
        ;; If this is a Command line, read the Name the agent claimed.
        (claimed (save-excursion
                   (goto-char line-start)
                   (when (looking-at "Command:")
                     (forward-line 1)
                     (when (looking-at "Name: `\\([^`\n]+\\)`")
                       (match-string 1))))))
    (cond
     ((null binding)
      (insert " [unbound]")
      (if claimed
          (push (format "`%s` is unbound in this Emacs, so it does not run `%s`"
                        keys claimed)
                my/help-agent--issues)
        (push (format "`%s` is currently unbound" keys) my/help-agent--facts)))
     ((keymapp binding)                   ; e.g. C-x alone: waits for more keys
      (insert " [prefix key]")
      (if claimed
          (push (format "`%s` is a prefix key in this Emacs, not `%s`" keys claimed)
                my/help-agent--issues)
        (push (format "`%s` is a prefix key" keys) my/help-agent--facts)))
     ((and claimed (equal (format "%s" binding) claimed))
      (insert " [✓]"))
     (claimed                             ; key exists but does something else
      (insert " [⚠ here it runs ")
      (my/help-agent--insert-command binding)
      (insert "]")
      (push (format "`%s` runs `%s` in this Emacs, not `%s`" keys binding claimed)
            my/help-agent--issues))
     (t                                   ; outside command blocks: just report it
      (insert " [→ ")
      (my/help-agent--insert-command binding)
      (insert "]")
      (push (format "`%s` runs `%s` in this Emacs" keys binding)
            my/help-agent--facts)))))

(defun my/help-agent--lookup (keys origin)
  "Return the command KEYS runs in buffer ORIGIN, or globally if ORIGIN is gone."
  (let ((key (kbd keys)))                 ; turn "C-x [" into Emacs's key format
    (if (buffer-live-p origin)
        (with-current-buffer origin (key-binding key))
      ;; A blank temporary buffer has only global bindings.
      (with-temp-buffer (key-binding key)))))

(defun my/help-agent--insert-command (cmd)
  "Insert CMD's name as a clickable link to its documentation."
  (if (symbolp cmd)
      (insert-text-button (symbol-name cmd)
                          'action (lambda (_) (describe-function cmd))
                          'help-echo "Open the real documentation")
    (insert "an unnamed function")))

;; ---------- Move through history ----------
(defun my/help-agent-newer ()
  "Show the next, newer answer."
  (interactive)
  (if (< my/help-agent-index (1- (length my/help-agent-history)))
      (progn (setq my/help-agent-index (1+ my/help-agent-index))
             (my/help-agent--render))
    (message "Newest answer")))

(defun my/help-agent-older ()
  "Show the previous, older answer."
  (interactive)
  (if (> my/help-agent-index 0)
      (progn (setq my/help-agent-index (1- my/help-agent-index))
             (my/help-agent--render))
    (message "Oldest answer")))

;; ---------- Ask a question ----------
(defun my/help-agent-ask (question)
  "Ask the Emacs help agent QUESTION."
  (interactive "sAsk Emacs helper: ")    ; prompt for text at the bottom
  (if my/help-agent-pending
      (message "Helper is still answering; try again in a moment")
    ;; If the agent isn't running (first use, or it crashed), start it.
    (unless (process-live-p my/help-agent-process)
      (my/help-agent--start))
    ;; The buffer to check keys in. If you asked from the helper itself,
    ;; use the buffer you were in before it.
    (let ((origin (if (derived-mode-p 'my/help-agent-mode)
                      (other-buffer (current-buffer) t)
                    (current-buffer))))
      (setq my/help-agent-pending (cons question origin)
            my/help-agent-retries 0))
    ;; Attach facts from the last answer's checks, then the question.
    (my/help-agent--send
     (if my/help-agent-notes
         (concat "(Facts from the user's Emacs about your previous answer:\n"
                 (mapconcat (lambda (s) (concat "- " s)) my/help-agent-notes "\n")
                 ")\n\n" question)
       question))
    (setq my/help-agent-notes nil)
    (my/help-agent--restart-timer)
    (message "Asking helper...")))

;; ---------- Clear memory ----------
(defun my/help-agent-reset ()
  "Clear the agent's memory by restarting it. Your answer history is kept."
  (interactive)
  (when (process-live-p my/help-agent-process)
    (delete-process my/help-agent-process))
  (setq my/help-agent-pending nil        ; a stopped agent won't answer it
        my/help-agent-retries 0
        my/help-agent-notes nil)         ; facts about a forgotten conversation
  (my/help-agent--start))

(defun my/help-agent--restart-timer ()
  "Restart the inactivity countdown."
  (when my/help-agent-timer (cancel-timer my/help-agent-timer))
  (setq my/help-agent-timer
        (run-at-time my/help-agent-reset-after nil #'my/help-agent-reset)))

;; ---------- Hook it up ----------
(global-set-key (kbd "C-c h") #'my/help-agent-ask)
(add-hook 'after-init-hook #'my/help-agent--start) ; start when Emacs launches

(provide 'help-agent)
;;; help-agent.el ends here
