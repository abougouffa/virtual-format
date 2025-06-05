;;; virtual-format.el --- Virtually format buffer without modifying it -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Abdelhak Bougouffa
;;
;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>
;; Maintainer: Abdelhak Bougouffa <abougouffa@fedoraproject.org>
;; Created: July 23, 2024
;; Modified: April 20, 2025
;; Version: 0.3.2
;; Keywords: convenience faces languages text
;; Homepage: https://github.com/abougouffa/virtual-format
;; Package-Requires: ((emacs "29.1"))
;; SPDX-License-Identifier: GPL-3.0
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Format buffer without modifying it
;;
;;; Code:

(require 'treesit)


(defgroup virtual-format nil
  "Format buffers visually without modification."
  :group 'faces)

(defcustom virtual-format-buffer-formatter-function
  (or (cl-find-if #'fboundp '(format-all-buffer apheleia-format-buffer))
      (lambda () (user-error "Please customize `virtual-format-buffer-formatter-function'")))
  "The command/function backend used to format the buffer."
  :group 'virtual-format
  :type 'function)

(defcustom virtual-format-keep-incomplete-formatting nil
  "Should we allow incomplete formatting?

An incomplete formatting can happen with some formatters that modify the
AST by adding instructions.

When set to non-nil, `virtual-format' will keep the formatted parts of
the buffer after failing, otherwise, `virtual-format' cleanup the
incomplete formatting."
  :group 'virtual-format
  :type 'boolean)

(defcustom virtual-format-jump-on-incomplete-formatting nil
  "Jump to the problematic node when `virtual-format' fails to continue."
  :group 'virtual-format
  :type 'boolean)

(defcustom virtual-format-persist-local-variables
  '(default-directory
    tab-width
    standard-indent
    virtual-format-buffer-formatter-function
    ;; Language specific indentation and coding style local-variables
    "-offset$"
    "-level$"
    "-style$")
  "Inherit these local variables in the temporary buffer used for formatting."
  :type '(repeat (choice symbol regexp))
  :group 'virtual-format)

(defcustom virtual-format-timeout
  (lambda () (if (eq virtual-format-buffer-formatter-function 'apheleia-format-buffer) 0.2 nil))
  "Timeout (in seconds) to wait for asynchronous formatters.

Set to nil to disable waiting if your formatter is synchronous. Or to a
function that returns a number or nil."
  :type '(choice float function (symbol nil))
  :group 'virtual-format)

(defcustom virtual-format-fontify-formatted-spaces nil
  "Display mid-dots in the formatted spaces.

Useful to visually identify which regions have been modified by the
formatter."
  :type 'boolean
  :group 'virtual-format)

(defface virtual-format-formatted-spaces-face
  '((((class color) (min-colors 88) (background light))
     (:foreground "azure3"))
    (((class color) (min-colors 88) (background dark))
     (:foreground "azure4")))
  "Face for highlighting Isearch matches."
  :group 'virtual-format)

(defvar-keymap virtual-format-view-mode-map
  "q" #'virtual-format-view-quit)

(defvar virtual-format-fontify-space-char "·")
(defvar virtual-format-fontify-newline-char "¶")
(defvar virtual-format-signal-error-on-incomplete-formatting t)


;;; Internals

(defmacro virtual-format-with-formatted-buffer (&rest body)
  "Run BODY in the formatted buffer."
  `(with-current-buffer (get-buffer-create (format " *virtual-format:%s*" (buffer-name)))
     ,@body))

(defun virtual-format--copy-formatting (beg end fmt)
  "Copy formatting to the current buffer at (BEG . END) from FMT."
  (unless (string= (buffer-substring beg end) fmt)
    ;; Fontify the spaces/newlines if requested
    (when (and virtual-format-fontify-formatted-spaces (not (string-empty-p fmt)))
      (let ((new-fmt (string-replace
                      "\n" (concat virtual-format-fontify-newline-char "\n")
                      (string-replace
                       " " virtual-format-fontify-space-char fmt))))
        (setq fmt (propertize new-fmt 'face 'virtual-format-formatted-spaces-face))))
    ;; In cases like "}print", the end of "}" is the same as the beginning of
    ;; "print", we cannot put text property on null string, so we take the "p"
    ;; from "print" and prepend the formatted spaces to it
    (when (= beg end)
      (setq fmt (concat fmt (buffer-substring end (1+ end)))
            end (1+ end)))
    ;; Display the region between `beg' and `end' in the original buffer as the
    ;; spaces/newlines taken from the formatted buffer
    (add-text-properties beg end `(display ,fmt virtual-format-block t))))

(defun virtual-format--formatted-buffer-p ()
  "Check if the current buffer have `virtual-format-block' text properties."
  (text-property-any (point-min) (point-max) 'virtual-format-block t))

(defun virtual-format--check-state (node node-fmt)
  "Check the state at NODE and NODE-FMT.
Signal the error according to settings."
  (unless (and (string= (treesit-node-type node) (treesit-node-type node-fmt))
               (= (treesit-node-child-count node) (treesit-node-child-count node-fmt)))
    (unless virtual-format-keep-incomplete-formatting
      (virtual-format-cleanup (treesit-node-start node) (treesit-node-end node)))
    (let* ((pos (treesit-node-start node))
           (line (line-number-at-pos pos))
           (col (save-excursion (goto-char pos) (- pos (pos-bol)))))
      (when virtual-format-jump-on-incomplete-formatting
        (goto-char pos) ; Go to the problematic position
        (recenter)
        ;; When `pulsar' is available, pulse the problematic line
        (and (fboundp 'pulsar-pulse-line) (pulsar-pulse-line)))
      (user-error "Incomplete formatting at node %S at %d:%d" (treesit-node-type node) line col))))

(defun virtual-format--call-formatter (beg end &optional transfer-formatting)
  "Call formatter for region (BEG . END).
When TRANSFER-FORMATTING is non-nil, do transfer the formatting (finely)."
  (let* ((mode major-mode)
         (node-in-region (treesit-node-on (1+ beg) (1- end)))
         (content (buffer-substring (treesit-node-start node-in-region)
                                    (treesit-node-end node-in-region)))
         ;; Persist the values for some local variables in the temporary buffer
         (local-vars
          (seq-filter
           (lambda (local-var)
             (or (memq (car local-var)
                       (seq-filter #'symbolp virtual-format-persist-local-variables))
                 (cl-some (lambda (regexp) (string-match-p regexp (symbol-name (car local-var))))
                          (seq-filter #'stringp virtual-format-persist-local-variables))))
           (buffer-local-variables))))
    (virtual-format-with-formatted-buffer
     (delete-region (point-min) (point-max))
     (unless (eq major-mode mode) (delay-mode-hooks (funcall mode)))
     (dolist (var-val local-vars)
       (set (make-local-variable (car var-val)) (cdr var-val)))
     (insert content)
     ;; We first save the hash of the buffer content, then we run the formatter.
     ;; When the formatter updates the buffer before returning, we can check at
     ;; the end if the buffer content has changed and return subsequently.
     ;; However, if the formatter does some async stuff or sets some special
     ;; hooks that will update the buffer later, we cannot return immediately
     ;; since the buffer content didn't change yet. So, we wait for some time
     ;; before returning, hoping that the buffer has been updated.
     (let ((buf-hash (buffer-hash)))
       ;; Inhibit messages so we can show the progress over any message that can
       ;; be displayed by the original formatter.
       (with-temp-message (or (current-message) "")
         (if (commandp virtual-format-buffer-formatter-function)
             (call-interactively virtual-format-buffer-formatter-function)
           (funcall virtual-format-buffer-formatter-function)))
       ;; Check If the buffer has been formatted or not. If not (for example,
       ;; the formatter works asynchronously), we wait for
       ;; `virtual-format-timeout' before returning.
       (when-let* ((timeout (or (numberp virtual-format-timeout)
                                (and (functionp virtual-format-timeout)
                                     (funcall virtual-format-timeout))))
                   ((equal buf-hash (buffer-hash))))
         (sleep-for timeout))))
    (when transfer-formatting
      (with-silent-modifications
        (virtual-format-depth-first-walk
         node-in-region
         (virtual-format-with-formatted-buffer
          (or (car ; Get the first node of the same type as the unformatted `node-in-region'
               (treesit-filter-child
                (treesit-buffer-root-node)
                (lambda (node)
                  (string= (treesit-node-type node)
                           (treesit-node-type node-in-region)))))
              (treesit-buffer-root-node)))))))) ; Default to root (when formatting the whole buffer)

(defun virtual-format-buffer-mode-setup ()
  "Display read-only formatted code of the current buffer."
  (virtual-format--call-formatter (point-min) (point-max))
  (if (string= (buffer-hash) (virtual-format-with-formatted-buffer (buffer-hash)))
      (progn
        (virtual-format-view-mode -1)
        (message "The buffer seems to be already formatted."))
    (with-silent-modifications
      (delete-region (point-min) (point-max))
      (insert (virtual-format-with-formatted-buffer (buffer-string)))
      (font-lock-update)
      (read-only-mode 1))))

(defun virtual-format-view-mode-teardown ()
  "Teardown `virtual-format-view-mode'."
  (revert-buffer t t t)
  (read-only-mode -1))

(defun virtual-format-depth-first-walk (node node-fmt &optional prev-node prev-node-fmt)
  "Recursively walk NODE and NODE-FMT, with PREV-NODE and PREV-NODE-FMT."
  (let ((prev-leaf prev-node)
        (prev-leaf-fmt prev-node-fmt))
    (virtual-format--check-state node node-fmt)
    (dotimes (i (treesit-node-child-count node))
      (let* ((n (treesit-node-child node i))
             (n-fmt (treesit-node-child node-fmt i)))
        (if (zerop (treesit-node-child-count n)) ; leaf
            (let* ((pos-beg (or (and prev-leaf (treesit-node-end prev-leaf))
                                (treesit-node-start node)))
                   (pos-end (treesit-node-start n))
                   (pos-beg-fmt (virtual-format-with-formatted-buffer
                                 (or (and prev-leaf-fmt (treesit-node-end prev-leaf-fmt))
                                     (treesit-node-start node-fmt))))
                   (pos-end-fmt (virtual-format-with-formatted-buffer (treesit-node-start n-fmt)))
                   (fmt-spaces (virtual-format-with-formatted-buffer (buffer-substring pos-beg-fmt pos-end-fmt))))
              (virtual-format--copy-formatting pos-beg pos-end fmt-spaces)
              (setq prev-leaf n
                    prev-leaf-fmt n-fmt))
          (let ((last-nodes (virtual-format-depth-first-walk n n-fmt prev-leaf prev-leaf-fmt)))
            (setq prev-leaf (car last-nodes)
                  prev-leaf-fmt (cdr last-nodes))))))
    (cons prev-leaf prev-leaf-fmt)))

(defun virtual-format-incremental-walk (&optional node)
  "Recursively walk NODE."
  (let (virtual-format-jump-on-incomplete-formatting
        virtual-format-keep-incomplete-formatting)
    (condition-case nil
        (progn
          (message "Incrementally formatting buffer [%d%%] at node %S"
                   (/ (* 100 (treesit-node-start node)) (point-max)) (treesit-node-type node))
          (virtual-format-region (treesit-node-start node) (treesit-node-end node)))
      (error
       (dolist (child (treesit-node-children node))
         (unless (zerop (treesit-node-child-count child))
           (virtual-format-incremental-walk child)))))))


;;; Commands

(defun virtual-format-cleanup (beg end)
  "Cleanup the visual formatting in region (BEG . END)."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (point-min) (point-max))))
  (with-silent-modifications
    (while (and (< beg end) (setq beg (text-property-any beg end 'virtual-format-block t)))
      (remove-text-properties
       beg
       (setq beg (or (text-property-not-all beg end 'virtual-format-block t) end))
       '(display nil virtual-format-block nil)))))

;;;###autoload
(defun virtual-format-region (beg end)
  "Visually format the (BEG . END) region without modifying it."
  (interactive "r")
  (virtual-format-cleanup beg end)
  (virtual-format--call-formatter beg end t))

;;;###autoload
(defun virtual-format-buffer ()
  "Visually format the buffer without modifying it."
  (interactive)
  (virtual-format-region (point-min) (point-max))
  (unless (virtual-format--formatted-buffer-p)
    (message "The buffer seems to be already formatted.")))

;;;###autoload
(defun virtual-format-buffer-incrementally ()
  "Incrementally format the buffer without modifying it."
  (interactive)
  (virtual-format-incremental-walk (treesit-buffer-root-node))
  (message "Incrementally formatting buffer [Done!]")
  (unless (virtual-format--formatted-buffer-p)
    (message "The buffer seems to be already formatted.")))

(defun virtual-format-view-quit ()
  "Quit the `virtual-format-view-mode'."
  (interactive)
  (virtual-format-view-mode -1))

;;;###autoload
(define-minor-mode virtual-format-mode
  "Visually format the buffer without modification."
  :lighter " VFmt"
  :global nil
  (if virtual-format-mode
      (virtual-format-buffer)
    (virtual-format-cleanup (point-min) (point-max))))

;;;###autoload
(define-minor-mode virtual-format-view-mode
  "Display read-only formatted code of the current buffer.
This command simply formats the code and displays it as read-only.
It doesn't do any thing special with text properties. This can be used
to view files if `virtual-format-buffer' and
`virtual-format-buffer-incrementally' both failed to produce decent
formatting."
  :lighter " VFmt"
  :keymap virtual-format-view-mode-map
  :global nil
  (if virtual-format-view-mode
      (virtual-format-buffer-mode-setup)
    (virtual-format-view-mode-teardown)))


(provide 'virtual-format)
;;; virtual-format.el ends here
