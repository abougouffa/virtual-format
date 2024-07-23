;;; visual-format.el --- Format buffer without modifing it -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Abdelhak Bougouffa
;;
;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>
;; Maintainer: Abdelhak Bougouffa <abougouffa@fedoraproject.org>
;; Created: July 23, 2024
;; Modified: July 23, 2024
;; Version: 0.0.1
;; Keywords: convenience faces languages text
;; Homepage: https://github.com/abougouffa/visual-format
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

(defgroup visual-format nil
  "Format buffers visually without modification."
  :group 'faces)

(defcustom vf-function #'apheleia-format-buffer
  "The command/function backend used to format the buffer.

Depending of the package you are using for formatting, this can be set
to: `clang-format-buffer', `rustic-format-buffer',
`apheleia-format-buffer', `format-all-buffer'."
  :group 'visual-format
  :type 'function)

(defcustom vf-keep-incomplete-formatting nil
  "Should we allow incomplete formatting?

An incomplete formatting can happen with some formatters that modify the
AST by adding instructions.

When set to non-nil, `visual-format' will keep the formatted parts of
the buffer after failing, otherwise, `visual-format' cleanup the
incomplete formatting."
  :group 'visual-format
  :type 'boolean)


;;; Internals

(defvar-local vf--buffer nil)
(defvar-local vf--buffer-fmt nil)
(defvar-local vf--buffer-text-props nil)

(defmacro vf--with-buff (&rest body)
  "Run BODY in the target buffer."
  `(with-current-buffer vf--buffer
    ,@body))

(defmacro vf--with-buff-fmt (&rest body)
  "Run BODY in the formatted buffer."
  `(with-current-buffer vf--buffer-fmt
    ,@body))

(defun vf--depth-first-walk (&optional node node-fmt prev-node prev-node-fmt)
  "Recursively walk NODE and NODE-FMT, with PREV-NODE and PREV-NODE-FMT."
  (let ((node (or node (vf--with-buff (treesit-buffer-root-node))))
        (node-fmt (or node-fmt (vf--with-buff-fmt (treesit-buffer-root-node))))
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
                    (vf--with-buff
                     (or (and prev-leaf (max 1 (1- (treesit-node-end prev-leaf))))
                         (point-min))))
                   (pos-end (treesit-node-start n))
                   (pos-beg-fmt
                    (vf--with-buff-fmt
                     (or (and prev-leaf-fmt (max 1 (1- (treesit-node-end prev-leaf-fmt))))
                         (point-min))))
                   (pos-end-fmt (treesit-node-start n-fmt))
                   (fmt-spaces (vf--with-buff-fmt (buffer-substring pos-beg-fmt pos-end-fmt))))
              (vf--with-buff
               (unless (string= (buffer-substring pos-beg pos-end) fmt-spaces)
                 (push (list pos-beg pos-end fmt-spaces) vf--buffer-text-props)
                 (put-text-property pos-beg pos-end 'display fmt-spaces)))
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
(defun visual-format-buffer ()
  "Visually format the buffer without modifing it."
  (interactive)
  (vf-cleanup)
  (let ((vf--buffer (current-buffer))
        (vf--buffer-fmt (get-buffer-create (format " *visual-format: %s*" (buffer-name))))
        (content (buffer-string))
        (mode major-mode))
    (vf--with-buff-fmt
     (delay-mode-hooks (funcall mode))
     (delete-region (point-min) (point-max))
     (insert content)
     (call-interactively vf-function)
     (sit-for 1)) ; TODO: get rid of this dirty hack by finding a proper way to trigger an AST update!
    (with-silent-modifications
      (vf--depth-first-walk))))

;;;###autoload
(define-minor-mode visual-format-mode
  "Visually format the buffer without modification."
  :lighter " VFmt"
  :global nil
  (if vf-mode (vf-buffer) (vf-cleanup)))


(provide 'visual-format)
;;; visual-format.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("vf-" . "visual-format-"))
;; End:
