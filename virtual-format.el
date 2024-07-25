;;; virtual-format.el --- Virtually format buffer without modifying it -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Abdelhak Bougouffa
;;
;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>
;; Maintainer: Abdelhak Bougouffa <abougouffa@fedoraproject.org>
;; Created: July 23, 2024
;; Modified: July 25, 2024
;; Version: 0.0.2
;; Keywords: convenience faces languages text
;; Homepage: https://github.com/abougouffa/virtual-format
;; Package-Requires: ((emacs "28.1"))
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
  (or (cl-find-if #'fboundp '(apheleia-format-buffer format-all-buffer))
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


;;; Internals

(defvar virtual-format-signal-error-on-incomplete-formatting t)

(defmacro virtual-format--with-fmt-buf (&rest body)
  "Run BODY in the formatted buffer."
  `(let ((dir default-directory))
    (with-current-buffer (get-buffer-create (format " *virtual-format:%s*" (buffer-name)))
     (setq-local default-directory dir)
     ,@body)))

(defun virtual-format--region ()
  "Return the region bounds or the buffer bounds."
  (if (use-region-p) (car (region-bounds)) (cons (point-min) (point-max))))

;; Debug

(defvar virtual-format--debug-faces '(highlight region isearch holiday))
(defvar virtual-format--debug-face-id 0)

(defvar virtual-format-debug nil)

(defun virtual-format--debug-highlight-fmt-spaces (pos-beg-fmt pos-end-fmt)
  (when virtual-format-debug
    (virtual-format--with-fmt-buf
     (put-text-property
      pos-beg-fmt pos-end-fmt 'face
      (nth (setq virtual-format--debug-face-id
                 (mod (1+ virtual-format--debug-face-id) (length virtual-format--debug-faces)))
           virtual-format--debug-faces)))))

;; Core

(defun virtual-format--copy-formatting (beg end fmt)
  "Copy formatting to the current buffer at (BEG . END) from FMT."
  ;; In cases like "}print", the end of "}" is the same as the
  ;; beginning of "print", we cannot put text property on null
  ;; string, so we take the "p" from "print" and prepend the
  ;; formatted spaces to it
  (when (= beg end)
    (setq fmt (concat fmt (buffer-substring end (1+ end)))
          end (1+ end)))
  (unless (string= (buffer-substring beg end) fmt)
    (add-text-properties beg end `(display ,fmt virtual-format-text t))))

(defun virtual-format--depth-first-walk (&optional node node-fmt prev-node prev-node-fmt)
  "Recursively walk NODE and NODE-FMT, with PREV-NODE and PREV-NODE-FMT."
  (let ((prev-leaf prev-node)
        (prev-leaf-fmt prev-node-fmt))
    (when (/= (treesit-node-child-count node) (treesit-node-child-count node-fmt))
      (unless virtual-format-keep-incomplete-formatting (virtual-format-cleanup (treesit-node-start node) (treesit-node-end node)))
      (let* ((pos (treesit-node-start node))
             (line (line-number-at-pos pos))
             (col (save-excursion (goto-char pos) (- pos (pos-bol)))))
        (when virtual-format-jump-on-incomplete-formatting
          (goto-char pos) ; Go to the problematic position
          (recenter)
          ;; When `pulsar' is available, pulse the problematic line
          (and (fboundp 'pulsar-pulse-line) (pulsar-pulse-line)))
        (user-error "Incomplete formatting at node %S at %d:%d" (treesit-node-type node) line col)))
    (dotimes (i (treesit-node-child-count node))
      (let* ((n (treesit-node-child node i))
             (n-fmt (treesit-node-child node-fmt i)))
        (if (zerop (treesit-node-child-count n)) ; leaf
            (let* ((pos-beg (or (and prev-leaf (treesit-node-end prev-leaf))
                                (treesit-node-start node)))
                   (pos-end (treesit-node-start n))
                   (pos-beg-fmt (virtual-format--with-fmt-buf
                                 (or (and prev-leaf-fmt (treesit-node-end prev-leaf-fmt))
                                     (treesit-node-start node-fmt))))
                   (pos-end-fmt (virtual-format--with-fmt-buf (treesit-node-start n-fmt)))
                   (fmt-spaces (virtual-format--with-fmt-buf (buffer-substring pos-beg-fmt pos-end-fmt))))
              (virtual-format--copy-formatting pos-beg pos-end fmt-spaces)
              (setq prev-leaf n
                    prev-leaf-fmt n-fmt))
          (let ((last-nodes (virtual-format--depth-first-walk n n-fmt prev-leaf prev-leaf-fmt)))
            (setq prev-leaf (car last-nodes)
                  prev-leaf-fmt (cdr last-nodes))))))
    (cons prev-leaf prev-leaf-fmt)))

(defun virtual-format--incremental-walk (&optional node)
  "Recursively walk NODE."
  (let ((virtual-format-jump-on-incomplete-formatting nil)
        (virtual-format-keep-incomplete-formatting t))
    (condition-case nil
        (progn
          (message "Incrementally formatting buffer [%d%%] at node %S"
                   (/ (* 100 (treesit-node-start node)) (point-max)) (treesit-node-type node))
          (virtual-format-region (treesit-node-start node) (treesit-node-end node)))
      (error
       (dolist (child (treesit-node-children node))
         (unless (zerop (treesit-node-child-count child))
           (virtual-format--incremental-walk child)))))))

;;; Commands

(defun virtual-format-cleanup (beg end)
  "Cleanup the visual formatting in region (BEG . END)."
  (interactive (let ((reg (virtual-format--region))) (list (car reg) (cdr reg))))
  (with-silent-modifications
    (while (and (< beg end) (setq beg (text-property-any beg end 'virtual-format-text t)))
      (remove-text-properties
       beg
       (setq beg (or (text-property-not-all beg end 'virtual-format-text t) end))
       '(display nil virtual-format-text nil)))))

;;;###autoload
(defun virtual-format-buffer ()
  "Visually format the buffer without modifying it."
  (interactive)
  (virtual-format-region (point-min) (point-max)))

(defvar virtual-format-stupid-delay 0.2)

;;;###autoload
(defun virtual-format-region (beg end)
  "Visually format the (BEG . END) region without modifying it."
  (interactive "r")
  (virtual-format-cleanup beg end)
  (let* ((mode major-mode)
         (buf-tab-width tab-width)
         (buf-standard-indent standard-indent)
         (node-in-region (treesit-node-on (1+ beg) (1- end)))
         (content (buffer-substring (treesit-node-start node-in-region) (treesit-node-end node-in-region)))
         (formatter virtual-format-buffer-formatter-function)) ; To pass locally bound value to the other buffer
    (virtual-format--with-fmt-buf
     (setq-local tab-width buf-tab-width
                 standard-indent buf-standard-indent)
     (delay-mode-hooks (funcall mode))
     (delete-region (point-min) (point-max))
     (insert content)
     (with-temp-message (or (current-message) "") ; Inhibit messages so we can show the progress
       (if (commandp formatter)
           (call-interactively formatter)
         (funcall formatter)))
     ;; TODO: get rid of this dirty hack by finding a proper way to trigger an AST update!
     (sit-for virtual-format-stupid-delay))
    (with-silent-modifications
      (virtual-format--depth-first-walk
       node-in-region
       (virtual-format--with-fmt-buf
        (or (car ; Get the first node of the same type as the unformatted `node-in-region'
             (treesit-filter-child
              (treesit-buffer-root-node)
              (lambda (node) (string= (treesit-node-type node) (treesit-node-type node-in-region)))))
            (treesit-buffer-root-node))))))) ; Default to root (when formatting the whole buffer)

;;;###autoload
(defun virtual-format-buffer-incrementally ()
  "Incrementally format the buffer without modifying it."
  (interactive)
  (virtual-format--incremental-walk (treesit-buffer-root-node))
  (message "Incrementally formatting buffer [Done!]"))

;;;###autoload
(define-minor-mode virtual-format-mode
  "Visually format the buffer without modification."
  :lighter " VFmt"
  :global nil
  (if virtual-format-mode
      (virtual-format-buffer)
    (virtual-format-cleanup (point-min) (point-max))))


(provide 'virtual-format)
;;; virtual-format.el ends here
