;;; prism.el --- My highlighting for lisp            -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Like `rainbow-blocks', but respects existing non-color face properties.

;;; Code:

;;;; Requirements

(require 'cl-lib)

(require 'anaphora)
(require 'dash)

;;;; Variables

(defvar prism-num-faces nil
  ;; FIXME: Docstring.
  )

(defvar prism-face nil
  "Set by `prism-matcher' during fontification.")

;;;; Customization

(defgroup prism nil
  "FIXME: Docstring.")

(defcustom prism-desaturations '(40 50 60)
  "FIXME: Docstring.")

(defcustom prism-lightens '(0 5 10)
  "FIXME: Docstring.")

(defcustom prism-match-end (lambda (&rest _ignore)
                             (forward-char 1)
                             (point))
  "FIXME: Docstring.")
(defcustom prism-match-end (lambda (&rest _ignore)
                             (cond ((looking-at-p (rx (or "(" ")")))
                                    (forward-char 1)
                                    (point))
                                   ((looking-at-p (rx (syntax string-quote)))
                                    (point))
                                   (t
                                    (ignore-errors
                                      (forward-symbol 1)
                                      (point))))
                             (point))
  "FIXME: Docstring.")

;;;; Commands


;;;; Functions

(defun prism-debug-buffer ()
  (or (get-buffer "*prism-debug*")
      (with-current-buffer (get-buffer-create "*prism-debug*")
        (buffer-disable-undo)
        (current-buffer))))

(defun prism-extend-region ()
  ;; FIXME: Docstring.
  ""
  ;; FIXME: Not sure if `ignore-errors' is required here.
  (prism-debug (list (cons 'extend-region 1)
                     (cons 'point (point))))
  (save-excursion
    (when (ignore-errors
            (backward-up-list 1 t t))
      (setf font-lock-beg (point))))
  (setf font-lock-end (let ((end (save-excursion
                                   (thing-at-point--end-of-sexp)
                                   (point))))
                        (if (> end font-lock-end)
                            end
                          font-lock-end)))
  (prism-debug (list (cons 'extend-region 2)
                     (cons 'point (point))
                     (cons 'font-lock-beg font-lock-beg)
                     (cons 'font-lock-end font-lock-end))))

(defun prism-match (limit)
  ;; FIXME: Docstring.
  ""
  (prism-debug (list (cons 'match 1)
                     (cons 'point (point))
                     (cons 'limit limit)))
  (cond ((looking-at-p (rx (or space eol))) (while (and (looking-at-p (rx (or space eol)))
                                                        (not (eobp)))
                                              (forward-whitespace 1)))
        ((looking-at-p (rx (or ")" (syntax string-quote)))) nil)
        )
  (-let* ((start (point))
          ((depth _start-of-innermost-list _start-of-last-complete-sexp-terminated
                  in-string-p comment-level-p _following-quote-p
                  _min-paren-depth _comment-style _comment-or-string-start
                  _open-parens-list _two-char-construct-syntax . _rest)
           (syntax-ppss))
          (looking-at-closing-paren-p (looking-at-p (rx ")")))
          (end (if (looking-at-p (rx (or "(" ")" (syntax string-quote))))
                   (progn
                     (forward-char 1)
                     (point))
                 (ignore-errors
                   (forward-symbol 1)
                   (point)))))
    (prism-debug (list (cons 'match 2)
                       (cons 'point (point))
                       (cons 'limit limit)
                       (cons 'end end)
                       (cons 'depth depth)
                       (cons 'in-string-p in-string-p)
                       (cons 'comment-level-p comment-level-p)
                       (cons 'string (buffer-substring-no-properties start end))))
    (if (not (= start end))
        (if (not (or in-string-p comment-level-p))
            (progn
              (when looking-at-closing-paren-p
                (cl-decf depth))
              (set-match-data (list start end (current-buffer)))
              (setf prism-face (intern (concat "prism-face-" (number-to-string depth)))))
          (setf prism-face nil)
          t)
      (prism-debug "Start = end")
      nil)))

(defun prism-match (limit)
  ;; FIXME: Docstring.
  ""
  (prism-debug (list (cons 'match 1)
                     (cons 'point (point))
                     (cons 'limit limit)))
  (cond ((looking-at-p (rx (or space eol))) (while (and (looking-at-p (rx (or space eol)))
                                                        (not (eobp)))
                                              (forward-whitespace 1)))
        ((looking-at-p (rx (or ")" (syntax string-quote)))) nil)
        )
  (-let* ((start (point))
          ((depth _start-of-innermost-list _start-of-last-complete-sexp-terminated
                  in-string-p comment-level-p _following-quote-p
                  _min-paren-depth _comment-style _comment-or-string-start
                  _open-parens-list _two-char-construct-syntax . _rest)
           (syntax-ppss))
          (end (funcall prism-match-end)))
    (prism-debug (list (cons 'match 2)
                       (cons 'point (point))
                       (cons 'limit limit)
                       (cons 'end end)
                       (cons 'depth depth)
                       (cons 'in-string-p in-string-p)
                       (cons 'comment-level-p comment-level-p)
                       (cons 'string (buffer-substring-no-properties start end))))
    (if (not (= start end))
        (if (not (or in-string-p comment-level-p))
            (progn
              (set-match-data (list start end (current-buffer)))
              (setf prism-face (intern (concat "prism-face-" (number-to-string depth)))))
          (setf prism-face nil)
          t)
      (prism-debug "Start = end")
      nil)))

(defun prism-debug (obj)
  (with-current-buffer (prism-debug-buffer)
    (save-excursion
      (goto-char (point-max))
      (print obj (current-buffer)))))

(cl-defun prism-remove-faces (&optional (beg (point-min)))
  ;; FIXME: Docstring.
  ""
  (cl-macrolet ((without-prism-faces (faces)
                                     `(cl-loop for face in ,faces
                                               unless (or (not (facep face))
                                                          (string-prefix-p "prism-face-" (symbol-name face)))
                                               collect face)))
    (save-excursion
      (goto-char beg)
      (cl-loop for end = (or (next-single-property-change (point) 'face) (point-max))
               for faces = (get-text-property (point) 'face)
               when faces
               do (put-text-property (point) end 'face (without-prism-faces faces))
               for next-change = (next-single-property-change (point) 'face)
               while (and next-change
                          (/= next-change (point-max)))
               do (goto-char next-change)))))

;;;;; Colors

(cl-defun prism-set-colors (&key colors (num 16) shuffle)
  "Set NUM `prism' faces according to COLORS."
  (when shuffle
    (setf colors (prism-shuffle colors)))
  (setf colors (-cycle colors))
  (cl-loop with colors = (prism-modify-colors :colors colors :num num)
           for i from 0 upto num
           for face = (intern (format "prism-face-%d" i))
           for color = (nth i colors)
           unless (internal-lisp-face-p face)
           do (custom-declare-face face '((t)) (format "`prism' face #%d" i))
           do (set-face-attribute face nil :foreground color))
  (setf prism-num-faces num))

(cl-defun prism-modify-colors (&key num (desaturations prism-desaturations) (lightens prism-lightens)
                                    colors shuffle &allow-other-keys)
  ;; FIXME: Docstring.
  "Return list of NUM colors for use in `rainbow-identifiers', `rainbow-blocks', etc.
Modifies COLORS according to DESATURATIONS and LIGHTENS."
  (cl-flet ((modify-color (color desaturate lighten)
                          (--> color
                               (color-desaturate-name it desaturate)
                               (color-lighten-name it lighten)))
            (choose (choices num-items i)
                    (let* ((num-choices (1- (length choices)))
                           (steps (/ num-items num-choices))
                           (choice (/ i steps)))
                      (nth choice choices))))
    (cl-loop for i from 1 to num
             for desaturate = (choose desaturations num i)
             for lighten = (choose lightens num i)
             collect (modify-color (nth i colors) desaturate lighten))))

(defun prism-shuffle (seq)
  "Destructively shuffle SEQ.
Copied from `elfeed-shuffle'."
  (let ((n (length seq)))
    (prog1 seq                  ; don't use dotimes result (bug#16206)
      (dotimes (i n)
        (cl-rotatef (elt seq i) (elt seq (+ i (cl-random (- n i)))))))))

;;;; Minor mode

(define-minor-mode prism-mode
  ;; FIXME: Docstring.
  ""
  :global nil
  (let ((keywords '((prism-match 0 prism-face prepend))))
    (if prism-mode
        (progn
          (unless (facep 'prism-face-1)
            (prism-mode -1)
            (user-error "Please set `prism' colors with `prism-set-colors'"))
          (with-current-buffer (prism-debug-buffer)
            (erase-buffer))
          (font-lock-add-keywords nil keywords 'append)
          (add-hook 'font-lock-extend-region-functions #'prism-extend-region nil 'local)
          (font-lock-flush))
      (font-lock-remove-keywords nil keywords)
      (remove-hook 'font-lock-extend-region-functions #'prism-extend-region 'local)
      (prism-remove-faces))))

;;;; Footer

(provide 'prism)

;;; prism.el ends here
