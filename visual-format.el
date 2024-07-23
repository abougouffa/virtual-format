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
  "The command/function backend used to format the buffer."
  :group 'visual-format
  :type 'function)


(defvar-local vf--buffer nil)
(defvar-local vf--buffer-formatted nil)
(defvar-local vf--buffer-text-props nil)

(defmacro vf--with-buff (&rest body)
  "Run BODY in the original buffer."
  `(with-current-buffer vf--buffer
    ,@body))

(defmacro vf--with-buff-fmt (&rest body)
  "Run BODY in the formatted buffer."
  `(with-current-buffer vf--buffer-formatted
    ,@body))

(defun vf--depth-first-walk (&optional node node-fmt prev-node prev-node-fmt)
  "Recursively walk NODE and NODE-FMT, with PREV-NODE and PREV-NODE-FMT."
  (let ((node (or node (vf--with-buff (treesit-buffer-root-node))))
        (node-fmt (or node-fmt (vf--with-buff-fmt (treesit-buffer-root-node))))
        (prev-leaf prev-node)
        (prev-leaf-fmt prev-node-fmt))
    (cl-assert (= (treesit-node-child-count node) (treesit-node-child-count node-fmt)))
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

(defun vf-cleanup ()
  "Cleanup the visual formatting."
  (interactive)
  (dolist (spec vf--buffer-text-props)
    (let ((pos-beg (nth 0 spec))
          (pos-end (nth 1 spec))
          (prop (nth 2 spec)))
      (remove-text-properties
       pos-beg (or (text-property-not-all start end 'display prop) pos-end)
       '(display nil)))))

;;;###autoload
(defun visual-format-buffer ()
  "Visually format the buffer without modifing it."
  (interactive)
  (vf-cleanup)
  (let ((vf--buffer (current-buffer))
        (vf--buffer-formatted (get-buffer-create (format " *visual-format: %s*" (buffer-name))))
        (content (buffer-string))
        (mode major-mode)
        (lang (treesit-language-at (point-min))))
    (vf--with-buff-fmt
     (delay-mode-hooks (funcall mode))
     (delete-region (point-min) (point-max))
     (insert content)
     (call-interactively vf-function)
     (treesit-parser-create lang))
    (with-silent-modifications
      (vf--depth-first-walk))))

;;;###autoload
(define-minor-mode visual-format-mode
  "Format code without modifying the buffer."
  :lighter " VFmt"
  :global nil
  (if vf-mode
      (vf-buffer)
    (vf-cleanup)))


(provide 'visual-format)
;;; visual-format.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("vf-" . "visual-format-"))
;; End:
