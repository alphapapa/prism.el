;;; prism.el --- Disperse lisp forms into a spectrum of color by depth  -*- lexical-binding: t; -*-

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

;; Disperse lisp forms into a spectrum of color by depth.  Like
;; `rainbow-blocks', but respects existing non-color face properties,
;; and allows flexible configuration of faces and colors.

;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'thingatpt)

(require 'anaphora)
(require 'dash)

;;;; Variables

(defvar prism-num-faces nil
  "Number of `prism' faces.  Set automatically by `prism-set-faces'.")

(defvar prism-faces nil
  "Alist mapping depth levels to faces.")

(defvar prism-face nil
  "Set by `prism-match' during fontification.")

(defvar prism-debug nil
  "Enables `prism' debug output.
Only takes effect by recompiling the `prism' package with setting
non-nil.")

;;;; Customization

(defgroup prism nil
  "Disperse lisp forms into a spectrum of colors according to depth."
  :group 'font-lock)

(defcustom prism-color-attribute :foreground
  "Face attribute set in `prism' faces."
  :type '(choice (const :tag "Foreground" :foreground)
                 (const :tag "Background" :background)))

(defcustom prism-desaturations '(40 50 60)
  "Default desaturation values applied to faces at successively deeper depths.
Extrapolated to the length of `prism-faces'."
  :type '(repeat number))

(defcustom prism-lightens '(0 5 10)
  "Default lightening values applied to faces at successively deeper depths.
Extrapolated to the length of `prism-faces'."
  :type '(repeat number))

;;;; Minor mode

(define-minor-mode prism-mode
  "Disperse lisp forms into a spectrum of colors according to depth."
  :global nil
  (let ((keywords '((prism-match 0 prism-face prepend))))
    (if prism-mode
        (progn
          (unless prism-faces
            (setq prism-mode nil)
            (user-error "Please set `prism' colors with `prism-set-faces'"))
          (font-lock-add-keywords nil keywords 'append)
          (add-hook 'font-lock-extend-region-functions #'prism-extend-region nil 'local)
          (font-lock-flush))
      (font-lock-remove-keywords nil keywords)
      (remove-hook 'font-lock-extend-region-functions #'prism-extend-region 'local)
      (prism-remove-faces))))

;;;; Functions

(defmacro prism-debug (obj)
  (when prism-debug
    `(with-current-buffer (or (get-buffer "*prism-debug*")
                              (with-current-buffer (get-buffer-create "*prism-debug*")
                                (buffer-disable-undo)
                                (current-buffer)))
       (save-excursion
         (goto-char (point-max))
         (print ,obj (current-buffer))))))

;; Silence byte-compiler for these special variables that are bound
;; around `font-lock-extend-region-functions'.
(defvar font-lock-beg)
(defvar font-lock-end)

(defun prism-extend-region ()
  "Extend region to the current sexp.
For `font-lock-extend-region-functions'."
  (prism-debug (list (cons 'extend-region 1)
                     (cons 'point (point))))
  (let (changed-p)
    (unless (= 0 (nth 0 (syntax-ppss)))
      ;; Not at top level: extend region backward/up.
      (let ((orig-pos (point)))
        (save-excursion
          (when (ignore-errors
                  (backward-up-list 1 t t))
            (setf font-lock-beg (point))
            (unless (= font-lock-beg orig-pos)
              (setf changed-p t))))))
    (save-excursion
      (goto-char font-lock-end)
      (unless (= 0 (nth 0 (syntax-ppss)))
        ;; Not at top level: extend region forward.
        (let ((end (save-excursion
                     (ignore-errors
                       ;; This function signals an error, (scan-error "Containing
                       ;; expression ends prematurely"), when called with point
                       ;; immediately before the closing paren of an sexp.  In that
                       ;; case, we're already at the end, so ignore the error.
                       ;; FIXME: Maybe use something other than `thing-at-point--end-of-sexp',
                       ;; although its implementation looks very simple.
                       (thing-at-point--end-of-sexp))
                     (point))))
          (when (> end font-lock-end)
            (setf font-lock-end end
                  changed-p t)))))
    (prism-debug (list (cons 'extend-region 2)
                       (cons 'point (point))
                       (cons 'font-lock-beg font-lock-beg)
                       (cons 'font-lock-end font-lock-end)))
    changed-p))

(defun prism-match (limit)
  "Matcher function for `font-lock-keywords'."
  (prism-debug (list (cons 'match 1)
                     (cons 'point (point))
                     (cons 'limit limit)))
  ;; Skip things.
  (while (cond ((eobp) nil)
               ((looking-at-p (rx (syntax string-quote)))
                (forward-char 1))
               ((eolp)
                (forward-line 1))
               ((looking-at-p (rx (syntax comment-start)))
                (forward-line 1))
               ((looking-at-p (rx space))
                (when (re-search-forward (rx (not space)) limit t)
                  (goto-char (match-beginning 0))))
               ((nth 4 (syntax-ppss))
                (forward-line 1))))
  ;; Gather data.
  (-let* ((start (point))
          ((depth _start-of-innermost-list _start-of-last-complete-sexp-terminated
                  in-string-p comment-level-p _following-quote-p
                  _min-paren-depth _comment-style _comment-or-string-start
                  _open-parens-list _two-char-construct-syntax . _rest)
           (syntax-ppss))
          (at-comment-p (or (looking-at-p (rx (syntax comment-start)))
                            comment-level-p))
          (end (save-excursion
                 (cond ((looking-at-p (rx (syntax string-quote)))
                        (point))
                       ((looking-at-p (rx (in "([)]")))
                        (1+ (point)))
                       ((re-search-forward (rx (or (syntax string-quote) (syntax comment-start) (in "()[]"))) limit t)
                        (cond ((cl-member (char-before) '(?\") :test #'char-equal)
                               (1- (point)))
                              ((cl-member (char-before) '(?\) ?\]) :test #'char-equal)
                               (1- (point)))
                              ((cl-member (char-before) '(?\( ?\[) :test #'char-equal)
                               (point))
                              (t (1- (point)))))
                       (t (point))))))
    (prism-debug (list (cons 'match 2)
                       (cons 'point (point))
                       (cons 'limit limit)
                       (cons 'end end)
                       (cons 'depth depth)
                       (cons 'in-string-p in-string-p)
                       (cons 'comment-level-p comment-level-p)
                       (cons 'string (buffer-substring-no-properties start end))))
    ;; NOTE: Always return non-nil unless at eobp, basically.
    (cond ((eobp) nil)
          (in-string-p
           (setf prism-face nil)
           (re-search-forward (rx (syntax string-delimiter)) nil t)
           (goto-char end)
           t)
          (at-comment-p
           (setf prism-face nil)
           (goto-char end)
           t)
          (t (when (looking-at-p (rx ")"))
               (cl-decf depth))
             (set-match-data (list start end (current-buffer)))
             (setf prism-face (alist-get depth prism-faces))
             (goto-char end)
             t))))

(cl-defun prism-remove-faces (&optional (beg (point-min)))
  "Remove `prism' faces from buffer.
Note a minor bug at the moment: anonymous faces are also
removed."
  (cl-macrolet ((without-prism-faces (faces)
                                     `(cl-loop for face in ,faces
                                               ;; FIXME: This removes anonymous faces.
                                               unless (or (not (facep face))
                                                          (string-prefix-p "prism-level-" (symbol-name face)))
                                               collect face)))
    (with-silent-modifications
      (save-excursion
        (goto-char beg)
        (cl-loop for end = (or (next-single-property-change (point) 'face) (point-max))
                 for faces = (get-text-property (point) 'face)
                 when faces
                 do (put-text-property (point) end 'face (without-prism-faces faces))
                 for next-change = (next-single-property-change (point) 'face)
                 while (and next-change
                            (/= next-change (point-max)))
                 do (goto-char next-change))))))

;;;;; Colors

(cl-defun prism-set-faces (&key colors (num 16) shuffle (attribute prism-color-attribute)
                                (desaturations prism-desaturations) (lightens prism-lightens))
  ;; FIXME: Docstring.
  "Set NUM `prism' faces according to COLORS.
COLORS is a list of one or more color name strings (like
\"green\" or \"#ff0000\") or face symbols (of which the
foreground color is used)."
  (declare (indent defun))
  (when shuffle
    (setf colors (prism-shuffle colors)))
  (let* ((colors (->> colors
                      (--map (cl-etypecase it
                               (face (face-attribute it :foreground nil 'inherit))
                               (string it)))
                      -cycle
                      (prism-modify-colors :num num :desaturations desaturations :lightens lightens
                                           :colors)))
         (faces (cl-loop for i from 0 upto num
                         for face = (intern (format "prism-level-%d" i))
                         for color = (nth i colors)
                         ;; Delete existing face, important if e.g. changing :foreground to :background.
                         when (internal-lisp-face-p face)
                         do (face-spec-set face nil 'customized-face)
                         do (custom-declare-face face '((t)) (format "`prism' face #%d" i))
                         do (set-face-attribute face nil attribute color)
                         collect (cons i face))))
    (setf prism-faces faces
          prism-num-faces num)))

(cl-defun prism-modify-colors (&key num colors desaturations lightens &allow-other-keys)
  ;; FIXME: Docstring.
  "Return list of NUM colors for use in `rainbow-identifiers', `rainbow-blocks', etc.
Modifies COLORS according to DESATURATIONS and LIGHTENS."
  (cl-flet ((modify-color (color desaturate lighten)
                          (--> color
                               (color-desaturate-name it desaturate)
                               (color-lighten-name it lighten))))
    (when (< (length desaturations) num)
      (setf desaturations (prism-expand-list num desaturations)))
    (when (< (length lightens) num)
      (setf lightens (prism-expand-list num lightens)))
    (cl-loop for i from 0 below num
             for desaturate = (nth i desaturations)
             for lighten = (nth i lightens)
             collect (modify-color (nth i colors) desaturate lighten))))

(defun prism-shuffle (seq)
  "Destructively shuffle SEQ.
Copied from `elfeed-shuffle'."
  (let ((n (length seq)))
    (prog1 seq                  ; don't use dotimes result (bug#16206)
      (dotimes (i n)
        (cl-rotatef (elt seq i) (elt seq (+ i (cl-random (- n i)))))))))

(defun prism-expand-list (new-length list)
  "Return LIST expanded to NEW-LENGTH.
Each element of LIST is repeated an equal number of times, except
that the last element may be repeated an extra time when
necessary."
  (let* ((length (length list))
         (_longer-p (or (> new-length length)
                        (user-error "NEW-LENGTH must be longer than LIST")))
         (repeat-n (/ new-length (if (zerop (mod new-length length))
                                     length
                                   (1- length))))
         (final-element-p (not (zerop (mod new-length length))))
         (new-list (->> list
                        (--map (-repeat repeat-n it))
                        (-flatten))))
    (if final-element-p
        (-snoc new-list (-last-item list))
      new-list)))

;;;; Footer

(provide 'prism)

;;; prism.el ends here
