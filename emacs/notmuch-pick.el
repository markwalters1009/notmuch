;; notmuch-pick.el --- displaying notmuch forests.
;;
;; Copyright © Carl Worth
;; Copyright © David Edmondson
;; Copyright © Mark Walters
;;
;; This file is part of Notmuch.
;;
;; Notmuch is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Notmuch is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Notmuch.  If not, see <http://www.gnu.org/licenses/>.
;;
;; Authors: David Edmondson <dme@dme.org>
;;          Mark Walters <markwalters1009@gmail.com>

(require 'mail-parse)

(require 'notmuch-lib)
(require 'notmuch-query)
(require 'notmuch-show)
(eval-when-compile (require 'cl))

(declare-function notmuch-call-notmuch-process "notmuch" (&rest args))
(declare-function notmuch-show "notmuch-show" (&rest args))
(declare-function notmuch-tag "notmuch" (query &rest tags))
(declare-function notmuch-show-strip-re "notmuch-show" (subject))
(declare-function notmuch-show-clean-address "notmuch-show" (parsed-address))
(declare-function notmuch-show-spaces-n "notmuch-show" (n))
(declare-function notmuch-read-query "notmuch" (prompt))
(declare-function notmuch-read-tag-changes "notmuch" (&optional initial-input &rest search-terms))
(declare-function notmuch-update-tags "notmuch" (current-tags tag-changes))
(declare-function notmuch-hello-trim "notmuch-hello" (search))
(declare-function notmuch-search-find-thread-id "notmuch" ())
(declare-function notmuch-search-find-subject "notmuch" ())

;; the following variable is defined in notmuch.el
(defvar notmuch-search-query-string)

;;(defvar notmuch-pick-headers-hack "--output=no-body")
(defvar notmuch-pick-headers-hack "--output-body=false")

(defvar notmuch-pick-process-state nil
  "Parsing state of the search process filter.")

(defgroup notmuch-pick nil
  "Showing message and thread structure."
  :group 'notmuch)

(defcustom notmuch-pick-author-width 20
  "Width of the author field."
  :type 'integer
  :group 'notmuch-pick)

(defcustom notmuch-pick-asynchronous-parser nil
  "Use the asynchronous parser."
  :type 'boolean
  :group 'notmuch-pick)

(defface notmuch-pick-match-face
  '((((class color)
      (background dark))
     (:foreground "white"))
    (((class color)
      (background light))
     (:foreground "black"))
    (t (:bold t)))
  "Face used in pick mode for matching messages."
  :group 'notmuch-pick)

(defface notmuch-pick-no-match-face
  '((t (:foreground "gray")))
  "Face used in pick mode for messages not matching the query."
  :group 'notmuch-pick)

(defvar notmuch-pick-previous-subject "")
(make-variable-buffer-local 'notmuch-pick-previous-subject)

(defvar notmuch-pick-thread-id nil)
(make-variable-buffer-local 'notmuch-pick-thread-id)
(defvar notmuch-pick-query-context nil)
(make-variable-buffer-local 'notmuch-pick-query-context)
(defvar notmuch-pick-buffer-name nil)
(make-variable-buffer-local 'notmuch-pick-buffer-name)
(defvar notmuch-pick-message-window nil)
(make-variable-buffer-local 'notmuch-pick-message-window)
(put 'notmuch-pick-message-window 'permanent-local t)
(defvar notmuch-pick-message-buffer nil)
(make-variable-buffer-local 'notmuch-pick-message-buffer-name)
(put 'notmuch-pick-message-buffer-name 'permanent-local t)

(defvar notmuch-pick-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'notmuch-pick-show-message)
    (define-key map (kbd "M-RET") 'notmuch-pick-show-message-out)
    (define-key map [mouse-1] 'notmuch-pick-show-message)
    (define-key map "q" 'notmuch-pick-quit)
    (define-key map "x" 'notmuch-pick-quit)
    (define-key map "?" 'notmuch-help)
    (define-key map "a" 'notmuch-pick-archive-message)
    (define-key map "=" 'notmuch-pick-refresh-view)
    (define-key map "s" 'notmuch-search)
    (define-key map "z" 'notmuch-pick)
    (define-key map "m" 'notmuch-pick-new-mail)
    (define-key map "f" 'notmuch-pick-forward-message)
    (define-key map "r" 'notmuch-pick-reply-sender)
    (define-key map "R" 'notmuch-pick-reply)
    (define-key map "n" 'notmuch-pick-next-matching-message)
    (define-key map "p" 'notmuch-pick-prev-matching-message)
    (define-key map "N" 'notmuch-pick-next-message)
    (define-key map "P" 'notmuch-pick-prev-message)
    (define-key map "|" 'notmuch-pick-pipe-message)
    (define-key map "-" 'notmuch-pick-remove-tag)
    (define-key map "+" 'notmuch-pick-add-tag)
;;    (define-key map " " 'notmuch-pick-scroll-message-window)
    (define-key map " " 'notmuch-pick-scroll-or-next)
    (define-key map "b" 'notmuch-pick-scroll-message-window-back)
    map))
(fset 'notmuch-pick-mode-map notmuch-pick-mode-map)

(defun notmuch-pick-get-message-properties ()
  "Return the properties of the current message as a plist.

Some useful entries are:
:headers - Property list containing the headers :Date, :Subject, :From, etc.
:tags - Tags for this message"
  (save-excursion
    (beginning-of-line)
    (get-text-property (point) :notmuch-message-properties)))

(defun notmuch-pick-set-message-properties (props)
  (save-excursion
    (beginning-of-line)
    (put-text-property (point) (+ (point) 1) :notmuch-message-properties props)))

(defun notmuch-pick-set-prop (prop val &optional props)
  (let ((inhibit-read-only t)
	(props (or props
		   (notmuch-pick-get-message-properties))))
    (plist-put props prop val)
    (notmuch-pick-set-message-properties props)))

(defun notmuch-pick-get-prop (prop &optional props)
  (let ((props (or props
		   (notmuch-pick-get-message-properties))))
    (plist-get props prop)))

(defun notmuch-pick-set-tags (tags)
  "Set the tags of the current message."
  (notmuch-pick-set-prop :tags tags))

(defun notmuch-pick-get-tags ()
  "Return the tags of the current message."
  (notmuch-pick-get-prop :tags))

(defun notmuch-pick-tag-message (&rest tag-changes)
  "Change tags for the current message.

TAG-CHANGES is a list of tag operations for `notmuch-tag'."
  (let* ((current-tags (notmuch-pick-get-tags))
	 (new-tags (notmuch-update-tags current-tags tag-changes)))
    (unless (equal current-tags new-tags)
      (funcall 'notmuch-tag (notmuch-pick-get-message-id) tag-changes)
      (notmuch-pick-set-tags new-tags))))

(defun notmuch-pick-tag (&optional initial-input)
  "Change tags for the current message, read input from the minibuffer."
  (interactive)
  (let ((tag-changes (notmuch-read-tag-changes
		      initial-input (notmuch-pick-get-message-id))))
    (apply 'notmuch-pick-tag-message tag-changes)))

(defun notmuch-pick-add-tag ()
  "Same as `notmuch-pick-tag' but sets initial input to '+'."
  (interactive)
  (notmuch-pick-tag "+"))

(defun notmuch-pick-remove-tag ()
  "Same as `notmuch-pick-tag' but sets initial input to '-'."
  (interactive)
  (notmuch-pick-tag "-"))

(defun notmuch-pick-get-message-id ()
  "Return the message id of the current message."
  (concat "id:\"" (notmuch-pick-get-prop :id) "\""))

(defun notmuch-pick-get-match ()
  "Return whether the current message is a match."
  (interactive)
  (notmuch-pick-get-prop :match))

(defun notmuch-pick-from-hello (&optional search)
  "Run a query and display results in experimental notmuch-pick mode"
  (interactive)
  (unless (null search)
    (setq search (notmuch-hello-trim search))
    (let ((history-delete-duplicates t))
      (add-to-history 'notmuch-search-history search)))
  (notmuch-pick search))

;; This function should be in notmuch-show.el but be we trying to minimise
;; impact on the rest of the codebase.
(defun notmuch-pick-from-show-current-query ()
  "Call notmuch pick with the current query"
  (interactive)
  (notmuch-pick notmuch-show-thread-id notmuch-show-query-context))

;; This function should be in notmuch.el but be we trying to minimise
;; impact on the rest of the codebase.
(defun notmuch-pick-from-search-current-query ()
  "Call notmuch pick with the current query"
  (interactive)
  (notmuch-pick notmuch-search-query-string))

;; This function should be in notmuch.el but be we trying to minimise
;; impact on the rest of the codebase.
(defun notmuch-pick-from-search-thread ()
  "Show the selected thread with notmuch-pick"
  (interactive)
  (notmuch-pick (notmuch-search-find-thread-id)
                notmuch-search-query-string
                (notmuch-prettify-subject (notmuch-search-find-subject)))
  (notmuch-pick-show-match-message-with-wait))

(defun notmuch-pick-show-message ()
  "Show the current message (in split-pane)."
  (interactive)
  (let ((id (notmuch-pick-get-message-id))
	(inhibit-read-only t)
	buffer)
    (when id
      ;; we close and reopen the window to kill off un-needed buffers
      ;; this might cause flickering but seems ok
      (notmuch-pick-close-message-window)
      (setq notmuch-pick-message-window
	    (split-window-vertically (/ (window-height) 4)))
      (with-selected-window notmuch-pick-message-window
	(setq current-prefix-arg '(4))
	(setq buffer (notmuch-show id nil nil nil))))
    (setq notmuch-pick-message-buffer buffer)))

(defun notmuch-pick-show-message-out ()
  "Show the current message (in whole window)."
  (interactive)
  (let ((id (notmuch-pick-get-message-id))
	(inhibit-read-only t)
	buffer)
    (when id
      ;; we close the window to kill off un-needed buffers
      (notmuch-pick-close-message-window)
      (notmuch-show id nil nil nil))))

(defun notmuch-pick-scroll-message-window ()
  "Scroll the message window (if it exists)"
  (interactive)
  (when (window-live-p notmuch-pick-message-window)
    (with-selected-window notmuch-pick-message-window
      (if (pos-visible-in-window-p (point-max))
	  t
	(scroll-up)))))

(defun notmuch-pick-scroll-message-window-back ()
  "Scroll the message window back(if it exists)"
  (interactive)
  (when (window-live-p notmuch-pick-message-window)
    (with-selected-window notmuch-pick-message-window
      (if (pos-visible-in-window-p (point-min))
	  t
	(scroll-down)))))

(defun notmuch-pick-scroll-or-next ()
  "Scroll the message window. If it at end go to next message."
  (interactive)
  (when (notmuch-pick-scroll-message-window)
    (notmuch-pick-next-matching-message)))

(defun notmuch-pick-quit ()
  "Close the split view or exit pick."
  (interactive)
  (unless (notmuch-pick-close-message-window)
    (kill-buffer (current-buffer))))

(defun notmuch-pick-close-message-window ()
  "Close the message-window. Return t if close succeeds."
  (interactive)
  (when (and (window-live-p notmuch-pick-message-window)
	     (not (window-full-height-p notmuch-pick-message-window)))
    (delete-window notmuch-pick-message-window)
    (unless (get-buffer-window-list notmuch-pick-message-buffer)
      (kill-buffer notmuch-pick-message-buffer))
    t))

(defun notmuch-pick-archive-message ()
  "Archive the current message and move to next message."
  (interactive)
  (let ((id (notmuch-pick-get-message-id)))
    (when id
      (notmuch-tag id "-inbox" )
      (forward-line))))

(defun notmuch-pick-next-message ()
  "Move to next message."
  (interactive)
  (forward-line)
  (when (window-live-p notmuch-pick-message-window)
    (notmuch-pick-show-message)))

(defun notmuch-pick-prev-message ()
  "Move to previous message."
  (interactive)
  (forward-line -1)
  (when (window-live-p notmuch-pick-message-window)
    (notmuch-pick-show-message)))

(defun notmuch-pick-prev-matching-message ()
  "Move to previous matching message."
  (interactive)
  (forward-line -1)
  (while (and (not (bobp)) (not (notmuch-pick-get-match)))
    (forward-line -1))
  (when (window-live-p notmuch-pick-message-window)
    (notmuch-pick-show-message)))

(defun notmuch-pick-next-matching-message ()
  "Move to next matching message."
  (interactive)
  (forward-line)
  (while (and (not (eobp)) (not (notmuch-pick-get-match)))
    (forward-line))
  (when (window-live-p notmuch-pick-message-window)
    (notmuch-pick-show-message)))

(defun notmuch-pick-show-match-message-with-wait ()
  "Show the first matching message but wait for it to appear or search to finish."
  (interactive)
  (unless (notmuch-pick-get-match)
    (notmuch-pick-next-matching-message))
  (while (and (not (notmuch-pick-get-match))
	      (not (eq notmuch-pick-process-state 'end)))
    (message "waiting for message")
    (sit-for 0.1)
    (goto-char (point-min))
    (unless (notmuch-pick-get-match)
      (notmuch-pick-next-matching-message)))
  (message nil)
  (when (notmuch-pick-get-match)
    (notmuch-pick-show-message)))

(defun notmuch-pick-refresh-view ()
  "Refresh view."
  (interactive)
  (let ((inhibit-read-only t)
	(thread-id notmuch-pick-thread-id)
	(query-context notmuch-pick-query-context)
	(buffer-name notmuch-pick-buffer-name))
    (erase-buffer)
    (notmuch-pick-worker thread-id  query-context (get-buffer buffer-name))))

(defun notmuch-pick-string-width (string width &optional right)
  (let ((s (format (format "%%%s%ds" (if right "" "-") width)
		   string)))
    (if (> (length s) width)
	(substring s 0 width)
      s)))

(defmacro with-current-notmuch-pick-message (&rest body)
  "Evaluate body with current buffer set to the text of current message"
  `(save-excursion
     (let ((id (notmuch-pick-get-message-id)))
       (let ((buf (generate-new-buffer (concat "*notmuch-msg-" id "*"))))
         (with-current-buffer buf
	    (call-process notmuch-command nil t nil "show" "--format=raw" id)
           ,@body)
	 (kill-buffer buf)))))

(defun notmuch-pick-new-mail (&optional prompt-for-sender)
  "Compose new mail."
  (interactive "P")
  (notmuch-pick-close-message-window)
  (notmuch-mua-new-mail prompt-for-sender ))

(defun notmuch-pick-forward-message (&optional prompt-for-sender)
  "Forward the current message."
  (interactive "P")
  (notmuch-pick-close-message-window)
  (with-current-notmuch-pick-message
   (notmuch-mua-new-forward-message prompt-for-sender)))

(defun notmuch-pick-reply (&optional prompt-for-sender)
  "Reply to the sender and all recipients of the current message."
  (interactive "P")
  (notmuch-pick-close-message-window)
  (notmuch-mua-new-reply (notmuch-pick-get-message-id) prompt-for-sender t))

(defun notmuch-pick-reply-sender (&optional prompt-for-sender)
  "Reply to the sender of the current message."
  (interactive "P")
  (notmuch-pick-close-message-window)
  (notmuch-mua-new-reply (notmuch-pick-get-message-id) prompt-for-sender nil))

;; Shamelessly stolen from notmuch-show.el: maybe should be unified MJW
(defun notmuch-pick-pipe-message (command)
  "Pipe the contents of the current message to the given command.

The given command will be executed with the raw contents of the
current email message as stdin. Anything printed by the command
to stdout or stderr will appear in the *notmuch-pipe* buffer.

When invoked with a prefix argument, the command will receive all
open messages in the current thread (formatted as an mbox) rather
than only the current message."
  (interactive "sPipe message to command: ")
  (let ((shell-command
	 (concat notmuch-command " show --format=raw "
		 (shell-quote-argument (notmuch-pick-get-message-id)) " | " command))
	 (buf (get-buffer-create (concat "*notmuch-pipe*"))))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (let ((exit-code (call-process-shell-command shell-command nil buf)))
	(goto-char (point-max))
	(set-buffer-modified-p nil)
	(setq buffer-read-only t)
	(unless (zerop exit-code)
	  (switch-to-buffer-other-window buf)
	  (message (format "Command '%s' exited abnormally with code %d"
			   shell-command exit-code)))))))

;; Shamelessly stolen from notmuch-show.el: should be unified MJW
(defun notmuch-pick-clean-address (address)
  "Try to clean a single email ADDRESS for display.  Return
unchanged ADDRESS if parsing fails."
  (condition-case nil
    (let (p-name p-address)
      ;; It would be convenient to use `mail-header-parse-address',
      ;; but that expects un-decoded mailbox parts, whereas our
      ;; mailbox parts are already decoded (and hence may contain
      ;; UTF-8). Given that notmuch should handle most of the awkward
      ;; cases, some simple string deconstruction should be sufficient
      ;; here.
      (cond
       ;; "User <user@dom.ain>" style.
       ((string-match "\\(.*\\) <\\(.*\\)>" address)
	(setq p-name (match-string 1 address)
	      p-address (match-string 2 address)))

       ;; "<user@dom.ain>" style.
       ((string-match "<\\(.*\\)>" address)
	(setq p-address (match-string 1 address)))

       ;; Everything else.
       (t
	(setq p-address address)))

      (when p-name
	;; Remove elements of the mailbox part that are not relevant for
	;; display, even if they are required during transport:
	;;
	;; Backslashes.
	(setq p-name (replace-regexp-in-string "\\\\" "" p-name))

	;; Outer single and double quotes, which might be nested.
	(loop
	 with start-of-loop
	 do (setq start-of-loop p-name)

	 when (string-match "^\"\\(.*\\)\"$" p-name)
	 do (setq p-name (match-string 1 p-name))

	 when (string-match "^'\\(.*\\)'$" p-name)
	 do (setq p-name (match-string 1 p-name))

	 until (string= start-of-loop p-name)))

      ;; If the address is 'foo@bar.com <foo@bar.com>' then show just
      ;; 'foo@bar.com'.
      (when (string= p-name p-address)
	(setq p-name nil))

      ;; If we have a name return that otherwise return the address.
      (if (not p-name)
	  p-address
	p-name))
    (error address)))

(defun notmuch-pick-insert-msg (msg depth tree-status)
  (let* ((headers (plist-get msg :headers))
	 (match (plist-get msg :match))
	 (tags (plist-get msg :tags))
	 (bare-subject (notmuch-show-strip-re (plist-get headers :Subject)))
	 (message-face (if match
			   'notmuch-pick-match-face
			 'notmuch-pick-no-match-face)))

    (insert (propertize (concat
			 (notmuch-pick-string-width
			  (plist-get msg :date_relative) 12 t)
			 "  "
			 (format "%-75s"
				 (concat
				  (notmuch-pick-string-width
				   (notmuch-pick-clean-address (plist-get headers :From))
				   notmuch-pick-author-width)
				  " "
				  (mapconcat #'identity (reverse tree-status) "")
				  (if (string= notmuch-pick-previous-subject bare-subject)
				      " ..."
				    bare-subject)))
			 (if tags
			     (concat " ("
				     (mapconcat #'identity tags ", ") ")"))
			 "") 'face message-face))
    (notmuch-pick-set-message-properties msg)
    (insert "\n")

    (setq notmuch-pick-previous-subject bare-subject)))

(defun notmuch-pick-insert-tree (tree depth tree-status first last)
  "Insert the message tree TREE at depth DEPTH in the current thread."
  (let ((msg (car tree))
	(replies (cadr tree)))

      (cond
       ((and (< 0 depth) (not last))
	(push "├" tree-status))
       ((and (< 0 depth) last)
	(push "╰" tree-status))
       ((and (eq 0 depth) first last)
;;	  (push "─" tree-status)) choice between this and next line is matter of taste MJW
	(push " " tree-status))
       ((and (eq 0 depth) first (not last))
	  (push "┬" tree-status))
       ((and (eq 0 depth) (not first) last)
	(push "╰" tree-status))
       ((and (eq 0 depth) (not first) (not last))
	(push "├" tree-status)))

      (push (concat (if replies "┬" "─") "►") tree-status)
      (notmuch-pick-insert-msg msg depth tree-status)
      (pop tree-status)
      (pop tree-status)

      (if last
	  (push " " tree-status)
	(push "│" tree-status))

    (notmuch-pick-insert-thread replies (1+ depth) tree-status)))

(defun notmuch-pick-insert-thread (thread depth tree-status)
  "Insert the thread THREAD at depth DEPTH >= 1 in the current forest."
  (let ((n (length thread)))
    (loop for tree in thread
	  for count from 1 to n

	  do (notmuch-pick-insert-tree tree depth tree-status (eq count 1) (eq count n)))))

(defun notmuch-pick-insert-forest (forest)
  (mapc '(lambda (thread)
	   (let (tree-status)
	     ;; Reset at the start of each main thread.
	     (setq notmuch-pick-previous-subject nil)
	     (notmuch-pick-insert-thread thread 0 tree-status)))
	forest))

(defun notmuch-pick-mode ()
  "Major mode displaying messages (as opposed to threads) of of a notmuch search.

This buffer contains the results of a \"notmuch pick\" of your
email archives. Each line in the buffer represents a single
message giving the relative date, the author, subject, and any
tags.

Pressing \\[notmuch-pick-show-message] on any line displays that message.

Complete list of currently available key bindings:

\\{notmuch-pick-mode-map}"

  (interactive)
  (kill-all-local-variables)
  (use-local-map notmuch-pick-mode-map)
  (setq major-mode 'notmuch-pick-mode
	mode-name "notmuch-pick")
  (hl-line-mode 1)
  (setq buffer-read-only t
	truncate-lines t))

(defun notmuch-pick-process-sentinel (proc msg)
  "Add a message to let user know when \"notmuch pick\" exits"
  (let ((buffer (process-buffer proc))
	(status (process-status proc))
	(exit-status (process-exit-status proc))
	(never-found-target-thread nil))
    (when (memq status '(exit signal))
        (kill-buffer (process-get proc 'parse-buf))
	(if (buffer-live-p buffer)
	    (with-current-buffer buffer
	      (save-excursion
		(let ((inhibit-read-only t)
		      (atbob (bobp)))
		  (goto-char (point-max))
		  (if (eq status 'signal)
		      (insert "Incomplete search results (pick process was killed).\n"))
		  (when (eq status 'exit)
		    (insert "End of search results.")
		    (message "async parser finished %s"
			     (format-time-string "%r"))
		    (unless (= exit-status 0)
		      (insert (format " (process returned %d)" exit-status)))
		    (insert "\n")))))))))


(defun notmuch-pick-show-error (string &rest objects)
  (save-excursion
    (goto-char (point-max))
    (insert "Error: Unexpected output from notmuch search:\n")
    (insert (apply #'format string objects))
    (insert "\n")))


(defvar notmuch-pick-json-parser nil
  "Incremental JSON parser for the search process filter.")

(defun notmuch-pick-process-filter (proc string)
  "Process and filter the output of \"notmuch show\" (for pick)"
  (let ((results-buf (process-buffer proc))
        (parse-buf (process-get proc 'parse-buf))
        (inhibit-read-only t)
        done)
    (if (not (buffer-live-p results-buf))
        (delete-process proc)
      (with-current-buffer parse-buf
        ;; Insert new data
        (save-excursion
          (goto-char (point-max))
          (insert string)))
      (with-current-buffer results-buf
	(save-excursion
	  (goto-char (point-max))
	  (while (not done)
	    (condition-case nil
		(case notmuch-pick-process-state
		      ((begin)
		       ;; Enter the results list
		       (if (eq (notmuch-json-begin-compound
				notmuch-pick-json-parser) 'retry)
			   (setq done t)
			 (setq notmuch-pick-process-state 'result)))
		      ((result)
		       ;; Parse a result
		       (let ((result (notmuch-json-read notmuch-pick-json-parser)))
			 (case result
			       ((retry) (setq done t))
			       ((end) (setq notmuch-pick-process-state 'end))
			       (otherwise (notmuch-pick-insert-forest (list result))))))
		      ((end)
		       ;; Any trailing data is unexpected
		       (with-current-buffer parse-buf
			 (skip-chars-forward " \t\r\n")
			 (if (eobp)
			     (setq done t)
			   (signal 'json-error nil)))))
	      (json-error
	       ;; Do our best to resynchronize and ensure forward
	       ;; progress
	       (notmuch-pick-show-error
		"%s"
		(with-current-buffer parse-buf
		  (let ((bad (buffer-substring (line-beginning-position)
					       (line-end-position))))
		    (forward-line)
		    bad))))))
	  ;; Clear out what we've parsed
	  (with-current-buffer parse-buf
	    (delete-region (point-min) (point))))))))

(defun notmuch-pick-worker (thread-id &optional query-context buffer)
  (interactive)
  (notmuch-pick-mode)
  (setq notmuch-pick-thread-id thread-id)
  (setq notmuch-pick-query-context query-context)
  (setq notmuch-pick-buffer-name (buffer-name buffer))

  (erase-buffer)
  (goto-char (point-min))
  (let* (args
	 (basic-args thread-id)
	 (search-args (concat "\'" basic-args
		       (if query-context (concat " and (" query-context ")"))
		       "\'"))
	 (message-arg "--entire-thread"))
    (if (equal (car (process-lines notmuch-command "count" search-args)) "0")
	(setq search-args basic-args))
    (message "starting parser %s"
	     (format-time-string "%r"))
    (if notmuch-pick-asynchronous-parser
	(let ((proc (start-process
		     "notmuch-pick" buffer
		     notmuch-command "show" notmuch-pick-headers-hack "--format=json"
		     message-arg search-args))
	      ;; Use a scratch buffer to accumulate partial output.
              ;; This buffer will be killed by the sentinel, which
              ;; should be called no matter how the process dies.
              (parse-buf (generate-new-buffer " *notmuch pick parse*")))
          (set (make-local-variable 'notmuch-pick-process-state) 'begin)
          (set (make-local-variable 'notmuch-pick-json-parser)
               (notmuch-json-create-parser parse-buf))
          (process-put proc 'parse-buf parse-buf)
	  (set-process-sentinel proc 'notmuch-pick-process-sentinel)
	  (set-process-filter proc 'notmuch-pick-process-filter)
	  (set-process-query-on-exit-flag proc nil))
      (progn
	(notmuch-pick-insert-forest
	 (notmuch-query-get-threads
	  (list notmuch-pick-headers-hack message-arg search-args)))
	(message "sync parser finished %s"
		 (format-time-string "%r"))))))

;;
;; If the query context reduced the results to nothing, run
;; the basic query.
;;      (when (and (eq (buffer-size) 0)
;;		 query-context)
;;	(notmuch-pick-insert-forest
;;	 (notmuch-query-get-threads basic-args message-arg sort-arg))))))

(defvar notmuch-pick-initialized nil)

(defun notmuch-pick-init()
  (unless notmuch-pick-initialized
    (define-key 'notmuch-search-mode-map "z" 'notmuch-pick)
    (define-key 'notmuch-search-mode-map "Z" 'notmuch-pick-from-search-current-query)
    (define-key 'notmuch-search-mode-map (kbd "M-RET") 'notmuch-pick-from-search-thread)
    (define-key 'notmuch-hello-mode-map "z" 'notmuch-pick-from-hello)
    (define-key 'notmuch-show-mode-map "z" 'notmuch-pick)
    (define-key 'notmuch-show-mode-map "Z" 'notmuch-pick-from-show-current-query)
    (setq notmuch-pick-initialized t)
    (message "Initialised notmuch-pick")))

(defun notmuch-pick (&optional query query-context buffer-name show-first-match)
  "Run notmuch pick with the given `query' and display the results"
  (interactive "sNotmuch pick: ")
  (if (null query)
      (setq query (notmuch-read-query "Notmuch pick: ")))
  (let ((buffer (get-buffer-create (generate-new-buffer-name
				    (or buffer-name
					(concat "*notmuch-pick-" query "*")))))
	(inhibit-read-only t))

    (switch-to-buffer buffer)
    ;; Don't track undo information for this buffer
    (set 'buffer-undo-list t)

    (notmuch-pick-worker query query-context buffer)

    (setq truncate-lines t)
    (when show-first-match
      (notmuch-pick-show-match-message-with-wait))))

;;  (use-local-map notmuch-pick-mode-map))

;;

(provide 'notmuch-pick)
