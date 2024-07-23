;;; virtual-format.el --- Virtually format buffer without modifing it -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Abdelhak Bougouffa
;;
;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>
;; Maintainer: Abdelhak Bougouffa <abougouffa@fedoraproject.org>
;; Created: July 23, 2024
;; Modified: July 23, 2024
;; Version: 0.0.1
;; Keywords: convenience faces languages text
;; Homepage: https://github.com/abougouffa/virtual-format
;; Package-Requires: ((emacs "28.1"))
;; SPDX-License-Identifier: GPL-3.0
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Format buffer without modifing it
;;
;;; Code:

(require 'treesit)

(declare-function #'apheleia-format-buffer "apheleia")

(defgroup virtual-format nil
  "Format buffers visually without modification."
  :group 'faces)

(defcustom vf-buffer-formatter-function #'apheleia-format-buffer
  "The command/function backend used to format the buffer.

Depending of the package you are using for formatting, this can be set
to: `clang-format-buffer', `rustic-format-buffer',
`apheleia-format-buffer', `format-all-buffer'."
  :group 'virtual-format
  :type 'function)

(defcustom vf-keep-incomplete-formatting nil
  "Should we allow incomplete formatting?

An incomplete formatting can happen with some formatters that modify the
AST by adding instructions.

When set to non-nil, `virtual-format' will keep the formatted parts of
the buffer after failing, otherwise, `virtual-format' cleanup the
incomplete formatting."
  :group 'virtual-format
  :type 'boolean)


;;; Internals

(defvar-local vf--buffer-text-props nil)

(defmacro vf--with-fmt-buf (&rest body)
  "Run BODY in the formatted buffer."
  `(let ((dir default-directory))
    (with-current-buffer (get-buffer-create (format " *virtual-format: %s*" (buffer-name)))
     (setq-local default-directory dir)
     ,@body)))

(defun vf--depth-first-walk (&optional node node-fmt prev-node prev-node-fmt)
  "Recursively walk NODE and NODE-FMT, with PREV-NODE and PREV-NODE-FMT."
  (let ((node (or node (treesit-buffer-root-node)))
        (node-fmt (or node-fmt (vf--with-fmt-buf (treesit-buffer-root-node))))
        (prev-leaf prev-node)
        (prev-leaf-fmt prev-node-fmt))
    (when (/= (treesit-node-child-count node) (treesit-node-child-count node-fmt))
      (unless vf-keep-incomplete-formatting (vf-cleanup))
      (user-error "Incomplete formatting"))
    (dotimes (i (treesit-node-child-count node))
      (let* ((n (treesit-node-child node i))
             (n-fmt (treesit-node-child node-fmt i)))
        (if (zerop (treesit-node-child-count n)) ; leaf
            (let* ((pos-beg
                    (or (and prev-leaf (max 1 (1- (treesit-node-end prev-leaf))))
                        (point-min)))
                   (pos-end (treesit-node-start n))
                   (pos-beg-fmt
                    (vf--with-fmt-buf
                     (or (and prev-leaf-fmt (max 1 (1- (treesit-node-end prev-leaf-fmt))))
                         (point-min))))
                   (pos-end-fmt (treesit-node-start n-fmt))
                   (fmt-spaces (vf--with-fmt-buf (buffer-substring pos-beg-fmt pos-end-fmt))))
              (unless (string= (buffer-substring pos-beg pos-end) fmt-spaces)
                (push (list pos-beg pos-end fmt-spaces) vf--buffer-text-props)
                (put-text-property pos-beg pos-end 'display fmt-spaces))
              (setq prev-leaf n
                    prev-leaf-fmt n-fmt))
          (let ((last-nodes (vf--depth-first-walk n n-fmt prev-leaf prev-leaf-fmt)))
            (setq prev-leaf (car last-nodes)
                  prev-leaf-fmt (cdr last-nodes))))))
    (cons prev-leaf prev-leaf-fmt)))


;;; Commands

(defun vf-cleanup ()
  "Cleanup the visual formatting."
  (interactive)
  (with-silent-modifications
    (let ((props vf--buffer-text-props))
      (setq vf--buffer-text-props nil)
      (dolist (spec props)
        (let ((pos-beg (nth 0 spec))
              (pos-end (nth 1 spec)))
          (remove-text-properties pos-beg pos-end '(display nil)))))))

;;;###autoload
(defun virtual-format-buffer ()
  "Visually format the buffer without modifing it."
  (interactive)
  (vf-cleanup)
  (let ((content (buffer-string))
        (mode major-mode)
        (buf-tab-width tab-width)
        (buf-standard-indent standard-indent))
    (vf--with-fmt-buf
     (setq-local tab-width buf-tab-width
                 standard-indent buf-standard-indent)
     (delay-mode-hooks (funcall mode))
     (delete-region (point-min) (point-max))
     (insert content)
     (call-interactively vf-buffer-formatter-function)
     (sit-for 1)) ; TODO: get rid of this dirty hack by finding a proper way to trigger an AST update!
    (with-silent-modifications
      (vf--depth-first-walk))))

;;;###autoload
(define-minor-mode virtual-format-mode
  "Visually format the buffer without modification."
  :lighter " VFmt"
  :global nil
  (if vf-mode (vf-buffer) (vf-cleanup)))


(provide 'virtual-format)
;;; virtual-format.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("vf-" . "virtual-format-"))
;; End:
