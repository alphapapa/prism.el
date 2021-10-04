;;; prism.el --- Customizable, depth-based syntax coloring  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>
;; URL: https://github.com/alphapapa/prism.el
;; Version: 0.3-pre
;; Package-Requires: ((emacs "26.1") (dash "2.14.1"))
;; Keywords: faces lisp

;;; License:

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

;; `prism' disperses Lisp forms (and other syntax bounded by
;; parentheses, brackets, and braces) into a spectrum of color by
;; depth.  It's similar to `rainbow-blocks', but it respects existing
;; non-color face properties, and allows flexible configuration of
;; faces and colors.  It also optionally colorizes strings and/or
;; comments by code depth in a similar, customizable way.

;; Usage:

;; 1.  Run the appropriate command for the current buffer:

;;   - For Lisp and C-like languages, use `prism-mode'.

;;   - For significant-whitespace languages like Python, or ones whose
;;     depth is not always indicated by parenthetical characters, like
;;     shell, use `prism-whitespace-mode' instead.

;; 2.  Enjoy.

;; When a theme is loaded or disabled, colors are automatically
;; updated.

;; To customize, see the `prism' customization group, e.g. by using
;; "M-x customize-group RET prism RET".  For example, by default,
;; comments and strings are colorized according to depth, similarly to
;; code, but this can be disabled.

;; Advanced:

;; More advanced customization of faces is done by calling
;; `prism-set-colors', which can override the default settings and
;; perform additional color manipulations.  The primary argument is
;; COLORS, which should be a list of colors, each of which may be a
;; name, a hex RGB string, or a face name (of which the foreground
;; color is used).  Note that the list of colors need not be as long
;; as the number of faces that's actually set (e.g. the default is 16
;; faces), because the colors are automatically repeated and adjusted
;; as necessary.

;; If `prism-set-colors' is called with the SAVE argument, the results
;; are saved to customization options so that `prism-mode' will use
;; those colors by default.

;; Here's an example that the author finds pleasant:

;;   (prism-set-colors :num 16
;;     :desaturations (cl-loop for i from 0 below 16
;;                             collect (* i 2.5))
;;     :lightens (cl-loop for i from 0 below 16
;;                        collect (* i 2.5))
;;     :colors (list "sandy brown" "dodgerblue" "medium sea green")
;;
;;     :comments-fn
;;     (lambda (color)
;;       (prism-blend color
;;         (face-attribute 'font-lock-comment-face :foreground) 0.25))
;;
;;     :strings-fn
;;     (lambda (color)
;;       (prism-blend color "white" 0.5)))

;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'color)
(require 'face-remap)
(require 'thingatpt)
(require 'subr-x)

(require 'dash)

;;;; Variables

(defvar prism-faces nil
  "Alist mapping depth levels to faces.")

(defvar prism-faces-comments nil
  "Alist mapping depth levels to string faces.")

(defvar prism-faces-strings nil
  "Alist mapping depth levels to string faces.")

(defvar prism-faces-parens nil
  "Alist mapping depth levels to parens faces.")

(defvar prism-face nil
  "Set by `prism-match' during fontification.")

(defvar-local prism-syntax-table nil
  "Syntax table used by `prism-mode'.
Set automatically.")

(defvar-local prism-whitespace-indent-offset 4
  "Number of spaces which represents a semantic level of indentation.
Set automatically by `prism-whitespace-mode'.  Should be set
appropriately for the current mode, e.g. `python-indent-offset'
for `python-mode'.")

;; Defined as custom variables later in the file, but declared here to
;; silence the byte-compiler, because they're used in `prism-set-colors',
;; which is defined before their defcustoms.  It's circular, but this
;; breaks the loop.
(defvar prism-colors)
(defvar prism-color-attribute)
(defvar prism-color-distance)
(defvar prism-desaturations)
(defvar prism-lightens)
(defvar prism-opacities)
(defvar prism-num-faces)
(defvar prism-comments-fn)
(defvar prism-comments)
(defvar prism-parens)
(defvar prism-parens-fn)
(defvar prism-strings-fn)
(defvar prism-strings)
(defvar prism-whitespace-mode-indents)

;;;; Macros

(defmacro prism-extrapolate (start times length form)
  "Return list of numbers extrapolated from FORM.
Starting from number START, repeating below TIMES, collect the
value of FORM.  Each iteration, `i' is bound to the iteration
number (the incremented value of START), and `c' is bound to the
number of the current cycle through LENGTH, starting at 1.

For example, this form:

    (prism-extrapolate 0 24 3 (* c 3))

Evaluates to:

    (3 3 3 6 6 6 9 9 9 12 12 12 15 15 15 18 18 18 21 21 21 24 24 24)

Intended for use as the DESATURATIONS and LIGHTENS arguments to
`prism-set-colors'."
  `(cl-loop with c = 1 with reset = 1
            for i from ,start below ,times
            collect ,form
            do (if (= reset ,length)
                   (setf reset 1
                         c (1+ c))
                 (cl-incf reset))))

;; NOTE: Since this will likely be useful in the future, I'm leaving it in, commented.

;; (cl-defmacro prism-debug (&rest args)
;;   "Display a debug warning showing the runtime value of ARGS.
;; The warning automatically includes the name of the containing
;; function, and it is only displayed if `warning-minimum-log-level'
;; is `:debug' at runtime (which avoids formatting messages that
;; won't be shown).
;;
;; Each of ARGS may be a string, which is displayed as-is, or a
;; symbol, the value of which is displayed prefixed by its name, or
;; a Lisp form, which is displayed prefixed by its first symbol.
;;
;; Before the actual ARGS arguments, you can write keyword
;; arguments, i.e. alternating keywords and values.  The following
;; keywords are supported:
;;
;; :buffer BUFFER   Name of buffer to pass to `display-warning'.
;; :level  LEVEL    Level passed to `display-warning', which see.
;;                  Default is :debug."
;;   (pcase-let* ((fn-name (with-current-buffer
;;                             (or byte-compile-current-buffer (current-buffer))
;;                           ;; This is a hack, but a nifty one.
;;                           (save-excursion
;;                             (beginning-of-defun)
;;                             (cl-second (read (current-buffer))))))
;;                (plist-args (cl-loop while (keywordp (car args))
;;                                     collect (pop args)
;;                                     collect (pop args)))
;;                ((map (:buffer buffer) (:level level)) plist-args)
;;                (level (or level :debug))
;;                (string (cl-loop for arg in args
;;                                 concat (pcase arg
;;                                          ((pred stringp) "%s ")
;;                                          ((pred symbolp)
;;                                           (concat (upcase (symbol-name arg)) ":%s "))
;;                                          ((pred listp)
;;                                           (concat "(" (upcase (symbol-name (car arg)))
;;                                                   (pcase (length arg)
;;                                                     (1 ")")
;;                                                     (_ "...)"))
;;                                                   ":%s "))))))
;;     `(when (eq :debug warning-minimum-log-level)
;;        (display-warning ',fn-name (format ,string ,@args) ,level ,buffer))))

;;;; Minor mode

(defun prism-active-mode ()
  "Return any already-active `prism' modes in this buffer.
There should only ever be one, but the return value is a list of
modes."
  (cl-loop for mode in '(prism-mode prism-whitespace-mode)
           when (symbol-value mode)
           collect mode))

;;;###autoload
(define-minor-mode prism-mode
  "Disperse lisp forms (and other non-whitespace-sensitive syntax) into a spectrum of colors according to depth.
Depth is determined by list nesting.  Suitable for Lisp, C-like
languages, etc."
  :global nil
  (let ((keywords '((prism-match 0 prism-face prepend))))
    (if prism-mode
        (progn
          (dolist (mode (cl-remove 'prism-mode (prism-active-mode)))
            ;; Deactivate alternative mode so this one can be enabled.
            (funcall mode -1))
          (unless prism-faces
            (prism-set-colors))
          (setq prism-syntax-table (prism-syntax-table (syntax-table)))
          (font-lock-add-keywords nil keywords 'append)
          (font-lock-flush)
          (add-hook 'font-lock-extend-region-functions #'prism-extend-region nil 'local)
          (unless (advice-member-p #'prism-after-theme #'load-theme)
            ;; Don't add the advice again, because this mode is
            ;; buffer-local, but the advice is global.
            (advice-add #'load-theme :after #'prism-after-theme)
            (advice-add #'disable-theme :after #'prism-after-theme)))
      (font-lock-remove-keywords nil keywords)
      (prism-remove-faces)
      (unless (--any (or (buffer-local-value 'prism-mode it)
                         (buffer-local-value 'prism-whitespace-mode it))
                     (buffer-list))
        ;; Don't remove advice if `prism' is still active in any buffers.
        (advice-remove #'load-theme #'prism-after-theme)
        (advice-remove #'disable-theme #'prism-after-theme))
      (remove-hook 'font-lock-extend-region-functions #'prism-extend-region 'local))))

;;;###autoload
(define-minor-mode prism-whitespace-mode
  "Disperse whitespace-sensitive syntax into a spectrum of colors according to depth.
Depth is determined by indentation and list nesting.  Suitable
for Python, Haskell, etc."
  :global nil
  (let ((keywords '((prism-match-whitespace 0 prism-face prepend))))
    (if prism-whitespace-mode
        (progn
          (dolist (mode (cl-remove 'prism-whitespace-mode (prism-active-mode)))
            ;; Deactivate alternative mode so this one can be enabled.
            (funcall mode -1))
          (unless prism-faces
            (prism-set-colors))
          (setf prism-syntax-table (prism-syntax-table (syntax-table))
                prism-whitespace-indent-offset (let ((indent (or (alist-get major-mode prism-whitespace-mode-indents)
                                                                 (alist-get t prism-whitespace-mode-indents))))
                                                 (cl-etypecase indent
                                                   (symbol (symbol-value indent))
                                                   (integer indent))))
          (font-lock-add-keywords nil keywords 'append)
          (font-lock-flush)
          (add-hook 'font-lock-extend-region-functions #'prism-extend-region nil 'local)
          (unless (advice-member-p #'prism-after-theme #'load-theme)
            ;; Don't add the advice again, because this mode is
            ;; buffer-local, but the advice is global.
            (advice-add #'load-theme :after #'prism-after-theme)
            (advice-add #'disable-theme :after #'prism-after-theme)))
      (font-lock-remove-keywords nil keywords)
      (prism-remove-faces)
      (unless (--any (or (buffer-local-value 'prism-mode it)
                         (buffer-local-value 'prism-whitespace-mode it))
                     (buffer-list))
        ;; Don't remove advice if `prism' is still active in any buffers.
        (advice-remove #'load-theme #'prism-after-theme)
        (advice-remove #'disable-theme #'prism-after-theme))
      (remove-hook 'font-lock-extend-region-functions #'prism-extend-region 'local))))

;;;; Functions

(defun prism-after-theme (&rest args)
  "For `load-theme' advice.
ARGS may be what `load-theme' and `disable-theme' expect.  Unless
NO-ENABLE (optional third argument, like `load-theme') is
non-nil, call `prism-set-colors' to update `prism' faces."
  (unless (cl-third args)
    (prism-set-colors)))

;; Silence byte-compiler for these special variables that are bound
;; around `font-lock-extend-region-functions'.
(defvar font-lock-beg)
(defvar font-lock-end)

(defun prism-extend-region ()
  "Extend region to the current sexp.
For `font-lock-extend-region-functions'."
  ;;  (prism-debug (current-buffer) (point) font-lock-beg font-lock-end)
  (let (changed-p)
    ;; NOTE: It doesn't seem to be necessary to extend the region backward/up, but I'm
    ;; not completely sure that this is never needed, so I'm leaving it in, commented.
    ;; (unless (= 0 (nth 0 (syntax-ppss)))
    ;;   ;; Not at top level: extend region backward/up.
    ;;   (let ((orig-pos (point)))
    ;;     (save-excursion
    ;;       (when (ignore-errors
    ;;               (backward-up-list 1 t t))
    ;;         (setf font-lock-beg (point))
    ;;         (unless (= font-lock-beg orig-pos)
    ;;           (setf changed-p t))))))
    (save-excursion
      (goto-char font-lock-end)
      (unless (= 0 (nth 0 (syntax-ppss)))
        ;; Not at top level: extend region forward.
        (let ((end (save-excursion
                     (when (ignore-errors
                             (backward-up-list -1 t t))
                       (point)))))
          (when (and end (> end font-lock-end))
            (setf font-lock-end (1- end)
                  changed-p t)
            changed-p))))))

(defun prism-syntax-table (syntax-table)
  "Return SYNTAX-TABLE modified for `prism'."
  ;; Copied from `rainbow-blocks-make-syntax-table'.
  (let ((table (copy-syntax-table syntax-table)))
    (modify-syntax-entry ?\( "()  " table)
    (modify-syntax-entry ?\) ")(  " table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    table))

(defun prism-match (limit)
  "Matcher function for `font-lock-keywords'.
Matches up to LIMIT."
  ;;  (prism-debug (current-buffer) (point) limit)
  (cl-macrolet ((parse-syntax ()
                              `(-setq (depth _ _ in-string-p comment-level-p)
                                 (syntax-ppss)))
                (comment-p ()
                           ;; This macro should only be used after `parse-syntax'.
                           `(or comment-level-p (looking-at-p (rx (syntax comment-start)))))
                (looking-at-paren-p
                 () `(looking-at-p (rx (or (syntax open-parenthesis)
                                           (syntax close-parenthesis)))))
                (face-at ()
                         ;; Return face to apply.  Should be called with point at `start'.
                         `(cond ((and prism-parens (looking-at-paren-p))
                                 (alist-get depth prism-faces-parens))
                                ((comment-p)
                                 (pcase depth
                                   (0 'font-lock-comment-face)
                                   (_ (if prism-faces-comments
                                          (alist-get depth prism-faces-comments)
                                        (alist-get depth prism-faces)))))
                                ((or in-string-p (looking-at-p (rx (syntax string-quote))))
                                 (pcase depth
                                   (0 'font-lock-string-face)
                                   (_ (if prism-faces-strings
                                          (alist-get depth prism-faces-strings)
                                        (alist-get depth prism-faces)))))
                                (t (alist-get depth prism-faces)))))
    (with-syntax-table prism-syntax-table
      (catch 'eobp
        (let ((parse-sexp-ignore-comments t)
              depth in-string-p comment-level-p comment-or-string-start start end
              found-comment-p found-string-p)
          (while ;; Skip to start of where we should match.
              (cond ((eobp)
                     ;; Stop matching and return nil if at end-of-buffer.
                     (throw 'eobp nil))
                    ((eolp)
                     (forward-line 1))
                    ((looking-at-p (rx blank))
                     (forward-whitespace 1))
                    ((unless prism-strings
                       (when (looking-at-p (rx (syntax string-quote)))
                         ;; At a string: skip it.
                         (forward-sexp))))
                    ((unless prism-comments
                       (forward-comment (buffer-size))))))
          (parse-syntax)
          (when in-string-p
            ;; In a string: go back to its beginning (before its delimiter).
            ;; It would be nice to leave this out and rely on the check in
            ;; the `while' above, but if partial fontification starts inside
            ;; a string, we have to handle that.
            ;; NOTE: If a string contains a Lisp comment (e.g. in
            ;; `custom-save-variables'), `in-string-p' will be non-nil, but
            ;; `comment-or-string-start' will be nil.  I don't know if this
            ;; is a bug in `parse-partial-sexp', but we have to handle it.
            (when comment-or-string-start
              (goto-char comment-or-string-start)
              (unless prism-strings
                (forward-sexp))
              (parse-syntax)))
          ;; Set start and end positions.
          (setf start (point)
                ;; I don't know if `ignore-errors' is going to be slow, but since
                ;; `scan-lists' and `scan-sexps' signal errors, it seems necessary if we want
                ;; to use them (and they seem to be cleaner to use than regexp searches).
                end (min limit
                         (save-excursion
                           (or (when (looking-at-p (rx (syntax close-parenthesis)))
                                 ;; I'd like to just use `scan-lists', but I can't find a way
                                 ;; around this initial check.  The code (scan-lists start 1
                                 ;; 1), when called just inside a list, scans past the end of
                                 ;; it, to just outside it, which is not what we want, because
                                 ;; we want to highlight the closing paren with the shallower
                                 ;; depth.  But if we just back up one character, we never
                                 ;; exit the list.  So we have to check whether we're looking
                                 ;; at the close of a list, and if so, move just past it.
                                 (cl-decf depth)
                                 (1+ start))
                               (when (and prism-comments (comment-p))
                                 (forward-comment (buffer-size))
                                 (setf found-comment-p t)
                                 (point))
                               (when (looking-at-p (rx (syntax string-quote)))
                                 (if in-string-p
                                     ;; At end of string: break out of it.
                                     (forward-char 1)
                                   ;; At beginning of string: skip it.
                                   (forward-sexp 1))
                                 (setf found-string-p t)
                                 (point))
                               (ignore-errors
                                 ;; Scan to the past the delimiter of the next deeper list.
                                 (scan-lists start 1 -1))
                               (ignore-errors
                                 ;; Scan to the end of the current list delimiter.
                                 (1- (scan-lists start 1 1)))
                               ;; If we can't find anything, return `limit'.  I'm not sure if
                               ;; this is the correct thing to do, but it avoids an error (and
                               ;; possibly hanging Emacs) in the event of an undiscovered bug.
                               ;; Although, signaling an error might be better, because I have
                               ;; seen "redisplay" errors related to font-lock in the messages
                               ;; buffer before, which might mean that Emacs can handle that.
                               ;; I think the important thing is not to hang Emacs, to always
                               ;; either return nil or advance point to `limit'.
                               limit))))
          (when (< end start)
            ;; Set search bound properly when `start' is greater than
            ;; `end' (i.e. when `start' is moved past `limit', I think).
            (setf end start))
          (when end
            ;; End found: Try to fontify.
            (save-excursion
              (or (unless (or in-string-p found-string-p found-comment-p)
                    ;; Neither in a string nor looking at nor in a
                    ;; comment: set `end' to any comment found before it.
                    (when (re-search-forward (rx (syntax comment-start)) end t)
                      (setf end (match-beginning 0))))
                  (unless (or found-comment-p found-string-p)
                    ;; Neither in nor looking at a comment: set `end'
                    ;; to any string or comment found before it.
                    (when (re-search-forward (rx (syntax string-quote)) end t)
                      (setf end (match-beginning 0))))))
            (when prism-parens
              (unless (= 1 (- end start))
                ;; Not fontifying a single open paren (i.e. we are trying to fontify more
                ;; than just an open paren): so if we are looking at one, fontify only it.
                (when (eq 4 (syntax-class (syntax-after (1- end))))
                  ;; End is past an open paren: back up one character.
                  (cl-decf end))))
            (if (and (comment-p) (= 0 depth))
                (setf prism-face nil)
              (setf prism-face (face-at)))
            (goto-char end)
            (set-match-data (list start end (current-buffer)))
            ;;  (prism-debug (current-buffer) "END" start end)
            ;; Be sure to return non-nil!
            t))))))

(defun prism-match-whitespace (limit)
  "Matcher function for `font-lock-keywords' in whitespace-sensitive buffers.
Matches up to LIMIT.  Requires `prism-whitespace-indent-offset' be set
appropriately, e.g. to `python-indent-offset' for `python-mode'."
  (cl-macrolet ((parse-syntax ()
                              `(-setq (list-depth _ _ in-string-p comment-level-p _ _ _ comment-or-string-start)
                                 (syntax-ppss)))
                (indent-depth ()
                              `(or (save-excursion
                                     (forward-line -1)
                                     (when (looking-at-p (rx (1+ nonl) "\\" eol))
                                       ;; Found backslask-continued line: move
                                       ;; to where the continued line starts.
                                       (cl-loop do (forward-line -1)
                                                while (looking-at-p (rx (1+ nonl) "\\" eol)))
                                       (forward-line 1)  ; Yes, go back down a line.
                                       (/ (current-indentation) prism-whitespace-indent-offset)))
                                   (/ (current-indentation) prism-whitespace-indent-offset)))
                (depth-at ()
                          ;; Yes, this is entirely too complicated--just like Python's syntax in
                          ;; comparison to Lisp.  But, "Eww, all those parentheses!"  they say.
                          ;; Well, all those parentheses avoid lots of special cases like these.
                          `(pcase list-depth
                             (0 (cond ((looking-at-p (rx (syntax close-parenthesis) eol))
                                       (save-excursion
                                         (forward-char 1)
                                         (backward-sexp 1)
                                         (+ (nth 0 (syntax-ppss)) (indent-depth))))
                                      ((looking-back (rx (syntax close-parenthesis)) (1- (point)))
                                       (save-excursion
                                         (backward-sexp 1)
                                         (+ (nth 0 (syntax-ppss)) (indent-depth))))
                                      (t (indent-depth))))
                             ;; This handles the case of code that is both enclosed in a
                             ;; character-delimited list and indented on a new line within that
                             ;; list to match the list's opening indentation (e.g. in Python,
                             ;; when an if's condition is parenthesized and split across lines).
                             (_ (let* ((current-depth (car (syntax-ppss)))  ;; This `syntax-ppss' call *is* necessary!
                                       (enclosing-list-depth
                                        (pcase current-depth
                                          (0 0)
                                          (_ (save-excursion
                                               ;; Escape current list and return the level of
                                               ;; the enclosing list plus its indent depth.

                                               ;; FIXME: When a preceding comment contains an apostrophe, this
                                               ;; call to `scan-lists' interprets the apostrophe as delimiting a
                                               ;; list, and it skips back to another preceding apostrophe, even
                                               ;; inside a different top-level form, which causes the wrong
                                               ;; depth to be calculated. ... Well, good news, I guess: this
                                               ;; happens on Emacs 26.3 but not on Emacs 27.1.  I guess
                                               ;; something was fixed, which means that it's not a bug in Prism.
                                               (goto-char (scan-lists (point) -1 current-depth))
                                               (+ (indent-depth) (car (syntax-ppss))))))))
                                  (pcase enclosing-list-depth
                                    (0 (+ list-depth (1- (indent-depth))))
                                    (_  (+ enclosing-list-depth list-depth)))))))
                (comment-p ()
                           ;; This macro should only be used after `parse-syntax'.
                           `(or comment-level-p (looking-at-p (rx (or (syntax comment-start)
                                                                      (syntax comment-delimiter))))))
                (face-at ()
                         ;; Return face to apply.  Should be called with point at `start'.
                         `(let ((depth (depth-at)))
                            (cond ((comment-p)
                                   (pcase depth
                                     (0 'font-lock-comment-face)
                                     (_ (if prism-faces-comments
                                            (alist-get depth prism-faces-comments)
                                          (alist-get depth prism-faces)))))
                                  ((or in-string-p (looking-at-p (rx (or (syntax string-quote)
                                                                         (syntax string-delimiter)))))
                                   (pcase depth
                                     (0 'font-lock-string-face)
                                     (_ (if prism-faces-strings
                                            (alist-get depth prism-faces-strings)
                                          (alist-get depth prism-faces)))))
                                  (t (alist-get depth prism-faces))))))
    (with-syntax-table prism-syntax-table
      (unless (eobp)
        ;; Not at end-of-buffer: start matching.
        (let ((parse-sexp-ignore-comments t)
              list-depth in-string-p comment-level-p comment-or-string-start start end
              found-comment-p found-string-p)
          (while ;; Skip to start of where we should match.
              (and (not (eobp))
                   (cond ((eolp)
                          (forward-line 1))
                         ((looking-at-p (rx blank))
                          (forward-whitespace 1))
                         ((unless prism-strings
                            (when (looking-at-p (rx (syntax string-quote)))
                              ;; At a string: skip it.
                              (forward-sexp))))
                         ((unless prism-comments
                            (forward-comment (buffer-size)))))))
          (parse-syntax)
          (when in-string-p
            ;; In a string: go back to its beginning (before its delimiter).
            ;; It would be nice to leave this out and rely on the check in
            ;; the `while' above, but if partial fontification starts inside
            ;; a string, we have to handle that.
            ;; NOTE: If a string contains a Lisp comment (e.g. in
            ;; `custom-save-variables'), `in-string-p' will be non-nil, but
            ;; `comment-or-string-start' will be nil.  I don't know if this
            ;; is a bug in `parse-partial-sexp', but we have to handle it.
            (when comment-or-string-start
              (goto-char comment-or-string-start)
              (unless prism-strings
                (forward-sexp))
              (parse-syntax)))
          ;; Set start and end positions.
          (setf start (point)
                ;; I don't know if `ignore-errors' is going to be slow, but since
                ;; `scan-lists' and `scan-sexps' signal errors, it seems necessary if we want
                ;; to use them (and they seem to be cleaner to use than regexp searches).
                end (min limit
                         (save-excursion
                           (or (when (and prism-comments (comment-p))
                                 (setf found-comment-p t)
                                 (when comment-or-string-start
                                   (goto-char comment-or-string-start))
                                 ;; We must only skip one comment, because before there is
                                 ;; non-comment, non-whitespace text, the indent depth might change.
                                 (forward-comment 1)
                                 (point))
                               (when (looking-at-p (rx (syntax close-parenthesis)))
                                 ;; I'd like to just use `scan-lists', but I can't find a way around this initial check.
                                 ;; The code (scan-lists start 1 1), when called just inside a list, scans past the end
                                 ;; of it, to just outside it, which is not what we want, because we want to highlight
                                 ;; the closing paren with the shallower depth.  But if we just back up one character,
                                 ;; we never exit the list.  So we have to check whether we're looking at the close of a
                                 ;; list, and if so, move just past it.
                                 (cl-decf list-depth)
                                 (1+ start))
                               (when (looking-at-p (rx (or (syntax string-quote)
                                                           (syntax string-delimiter))))
                                 (forward-sexp 1)
                                 (setf found-string-p t)
                                 (point))
                               ;; Don't go past the end of the line.
                               (apply #'min
                                      (-non-nil
                                       (list
                                        (or (ignore-errors
                                              ;; Scan to the past the delimiter of the next deeper list.
                                              (scan-lists start 1 -1))
                                            (ignore-errors
                                              ;; Scan to the end of the current list delimiter.
                                              (1- (scan-lists start 1 1))))
                                        (line-end-position))))
                               ;; If we can't find anything, return `limit'.  I'm not sure if this is the correct
                               ;; thing to do, but it avoids an error (and possibly hanging Emacs) in the event of
                               ;; an undiscovered bug.  Although, signaling an error might be better, because I
                               ;; have seen "redisplay" errors related to font-lock in the messages buffer before,
                               ;; which might mean that Emacs can handle that.  I think the important thing is not
                               ;; to hang Emacs, to always either return nil or advance point to `limit'.
                               limit))))
          (when (< end start)
            ;; Set search bound properly when `start' is greater than
            ;; `end' (i.e. when `start' is moved past `limit', I think).
            (setf end start))
          (when end
            ;; End found: Try to fontify.
            (unless (or in-string-p found-string-p found-comment-p)
              ;; Neither in a string nor looking at nor in a comment.
              (save-excursion
                (or (when (re-search-forward (rx (syntax comment-start)) end t)
                      ;; Set `end' to any comment found before it.
                      (setf end (match-beginning 0)))
                    (when (re-search-forward (rx (or (syntax string-quote)
                                                     (syntax string-delimiter)))
                                             end t)
                      ;; Set `end' to any string found before it.
                      (unless (nth 4 (syntax-ppss))
                        ;; Not in a comment.
                        (setf end (match-beginning 0)))))))
            (if (and (comment-p) (= 0 (depth-at)))
                (setf prism-face nil)
              (setf prism-face (face-at)))
            (goto-char end)
            (set-match-data (list start end (current-buffer)))
            ;; Be sure to return non-nil!
            t))))))

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

(cl-defun prism-set-colors
    (&key shuffle save local
          (num prism-num-faces) (colors prism-colors)
          (attribute prism-color-attribute)
          (desaturations prism-desaturations)
          (lightens prism-lightens)
          (opacities prism-opacities)
          (comments-fn prism-comments-fn)
          (strings-fn prism-strings-fn)
          (parens-fn prism-parens-fn))
  "Set `prism' faces.  Call after loading a new theme.
Call also when COLORS has been set to a list of faces and those
faces have been modified.

NUM is the number of faces to set, i.e. the depth to make faces
for.

When SAVE is non-nil, save attributes to `prism-' customization
options for future use by default.

When LOCAL is t (interactively, with one universal prefix), remap
faces buffer-locally; when `reset' (interactively, with two
prefixes), clear local remapping and don't set any faces; when
nil (the default), set faces globally.

COLORS is a list of one or more color name strings (like
\"green\" or \"#ff0000\") or face symbols (of which the
foreground color is used).

DESATURATIONS and LIGHTENS are lists of integer percentages
applied to colors as depth increases; they need not be as long as
NUM, because they are extrapolated automatically.

COMMENTS-FN, PARENS-FN, and STRINGS-FN are functions of one
argument, a color name or hex RGB string, which return the color
having been modified as desired for comments, parens, or strings,
respectively."
  (declare (indent defun))
  (interactive)
  (when (called-interactively-p 'any)
    (setf local (pcase current-prefix-arg
                  ('(16) 'reset)
                  ('(4) t))))
  (when shuffle
    (setf colors (prism-shuffle colors)))
  ;; MAYBE: Extrapolate desaturations and lightens cleverly, instead
  ;; of requiring the user to call `prism-extrapolate'.
  (cl-labels ((faces (colors &optional suffix (fn #'identity))
                     (setf suffix (if suffix
                                      (concat "-" suffix)
                                    ""))
                     (cl-loop for i from 0 below num
                              for face = (intern (format "prism-level-%d%s" i suffix))
                              for color = (funcall fn (nth i colors))
                              for description = (format "`prism' face%s #%d" suffix i)
                              do (set-face face attribute color description)
                              collect (cons i face)))
              (set-face (face attribute color description)
                        (pcase local
                          ('nil
                           (when (internal-lisp-face-p face)
                             ;; Delete existing face, important if e.g. changing :foreground to :background.
                             (face-spec-set face nil 'customized-face))
                           (custom-declare-face face '((t)) description :group 'prism-faces)
                           (set-face-attribute face nil attribute color))
                          ('reset (reset-face face))
                          (_ (face-remap-add-relative face (list attribute color)))))
              (reset-face (face)
                          (--when-let (alist-get face face-remapping-alist)
                            (face-remap-remove-relative (cons (-last-item it) (car (butlast it)))))))
    (let* ((colors (->> colors
                     (--map (pcase-exhaustive it
                              ((pred facep) (face-attribute it :foreground nil 'default))
                              ((pred stringp) it)
                              ((pred functionp) (funcall it))
                              (`(themed ,color) (prism-theme-color color))))
                     (--remove (string-prefix-p "unspecified-" it))
                     -cycle
                     (prism-modify-colors :num num
                                          :desaturations desaturations
                                          :lightens lightens
                                          :opacities opacities
                                          :colors)
                     ;; Use only two digits per component.  HTML export of code (e.g. with Org
                     ;; Export, htmlize, etc.)  doesn't work well with colors like "#01234567890a",
                     ;; even if Emacs can handle them internally.  Maybe it's Web browsers that
                     ;; can't handle them.  Anyway, we shouldn't use them if it breaks that.
                     (--map (--> (color-name-to-rgb it)
                              (-let (((r g b) it))
                                (color-rgb-to-hex r g b 2)))))))
      (cl-macrolet ((set-vars (&rest pairs)
                              `(progn
                                 ,@(cl-loop for (var val) on pairs by #'cddr
                                            collect `(pcase local
                                                       ('nil  ;; Set global faces.
                                                        (set ',var ,val))
                                                       ('reset  ;; Clear local remappings.
                                                        ,val)
                                                       (_  ;; Remap locally.
                                                        (set (make-local-variable ',var) ,val)))))))
        (set-vars prism-faces (faces colors)
                  prism-faces-strings (faces colors "strings" strings-fn)
                  prism-faces-comments (faces colors "comments" comments-fn)
                  prism-faces-parens (faces colors "parens" parens-fn)))
      (when (and save (not local))
        ;; Save arguments for later saving as customized variables,
        ;; including the unmodified (but shuffled) colors.
        (setf prism-colors colors
              prism-desaturations desaturations
              prism-lightens lightens
              prism-opacities opacities
              prism-num-faces num
              prism-comments-fn comments-fn
              prism-strings-fn strings-fn
              prism-parens-fn parens-fn)
        (prism-save-colors)))))

(defun prism-randomize-colors (&optional arg)
  "Randomize `prism' colors using themed `font-lock' faces.
ARG may be a number (which limits the number of colors used), or
a universal prefix (to use all `font-lock' faces), or nil (to use
unique colors from `font-lock' faces)."
  (interactive "P")
  (cl-labels ((colorize  ;; Return color NAME propertized with its foreground as its color.
	       (name) (propertize name 'face (list :foreground name)))
              (faces  ;; Return list of used colors with foreground color face applied.
	       () (->> (face-list)
                    (--select (and (string-prefix-p "prism-level" (symbol-name it))
                                   (string-match-p (rx digit eos) (symbol-name it))))
                    nreverse (-map #'face-foreground) (-map #'colorize)))
              (select-colors (colors threshold)
                             ;; Return shuffled list of COLORS ensuring that the
                             ;; distance between each one meets THRESHOLD.
                             (cl-loop with selected = (list (pop colors))
                                      while colors
                                      do (setf colors (prism-shuffle colors))
                                      for index = (--find-index
                                                   (>= (color-distance (car selected) it)
                                                       threshold)
                                                   colors)
                                      while index
                                      do (progn
                                           (push (nth index colors) selected)
                                           (setf colors (-remove-at index colors)))
                                      finally return selected))
	      (background-contrast-p (color &optional (min-distance 32768))
				     (>= (color-distance color (face-attribute 'default :background))
					 min-distance))
              (option-customized-p
	       (option) (not (equal (pcase-exhaustive (get option 'standard-value)
				      (`((funcall (function ,fn))) (funcall fn)))
				    (symbol-value option)))))
    (let* ((faces (--select (string-prefix-p "font-lock-" (symbol-name it))
                            (face-list)))
           (colors (->> faces
                     (--map (face-attribute it :foreground))
                     (--remove (eq 'unspecified it))
                     (-remove #'color-gray-p)
                     (-select #'background-contrast-p)))
	   (colors (pcase arg
		     ((pred integerp) (-take arg (prism-shuffle (-uniq colors))))
		     ('(4) colors)
		     (_ (-uniq colors))))
	   (colors (select-colors colors prism-color-distance))
	   (colors (-rotate (random (length colors)) colors))
           (desaturations (if (option-customized-p 'prism-desaturations)
                              prism-desaturations
                            (prism-extrapolate 0 prism-num-faces (length colors)
                                               (* c (+ 2 (length colors))))))
           (lightens (if (option-customized-p 'prism-lightens)
                         prism-lightens
                       (prism-extrapolate 0 prism-num-faces (length colors)
                                          (* c (+ 2 (length colors)))))))
      (prism-set-colors :colors colors
        :desaturations desaturations
	:lightens lightens
        :comments-fn (if (option-customized-p 'prism-comments-fn)
                         prism-comments-fn
                       (lambda (color)
                         (--> color
                           ;; The default function desaturates by 30%, but 40%
                           ;; seems to help a bit when using random colors.
                           (color-desaturate-name it 40)
                           (color-lighten-name it -10)))))
      (message "Randomized%s colors: %s\nFaces: %s"
               (pcase arg
		 ('(4) "")
		 (_ ", unique"))
               (string-join (-map #'colorize colors) " ")
               (string-join (faces) " ")))))

(defun prism-save-colors ()
  "Save current `prism' colors.
Function `prism-set-colors' does not save its argument values
permanently.  This command saves them using the customization
system so that `prism-set-colors' can then be called without
arguments to set the same faces."
  (cl-letf (((symbol-function 'custom-save-all)
             (symbol-function 'ignore)))
    ;; Avoid saving the file for each variable, which is very slow.
    ;; Save it once at the end.
    (dolist (var (list 'prism-desaturations 'prism-lightens 'prism-opacities 'prism-num-faces
                       'prism-comments-fn 'prism-strings-fn 'prism-parens-fn))
      (customize-save-variable var (symbol-value var))))
  (customize-save-variable 'prism-colors prism-colors))

(cl-defun prism-modify-colors (&key num colors desaturations lightens opacities &allow-other-keys)
  "Return list of NUM COLORS modified according to DESATURATIONS, LIGHTENS, and OPACITIES."
  (cl-flet ((modify-color (color desaturate lighten opacify)
                          (--> color
                            (if (> desaturate 0)
                                (color-desaturate-name it desaturate)
                              it)
                            (if (> lighten 0)
                                (color-lighten-name it lighten)
                              it)
                            (if (> opacify 0)
                                (prism-opacify-name it opacify)
                              it)
			    ;; FIXME: It seems that these two functions called in sequence
                            ;; always modify the color, e.g. #ff2afc becomes #fe29fb.
                            (color-name-to-rgb it)
                            (-let (((r g b) it))
			      (color-rgb-to-hex r g b 2)))))
    (when (< (length desaturations) num)
      (setf desaturations (prism-expand-list num desaturations)))
    (when (< (length lightens) num)
      (setf lightens (prism-expand-list num lightens)))
    (when (< (length opacities) num)
      (setf opacities (prism-expand-list num opacities)))
    (cl-loop for i from 0 below num
             for desaturate = (nth i desaturations)
             for lighten = (nth i lightens)
             for opacify = (nth i opacities)
             collect (modify-color (nth i colors) desaturate lighten opacify))))

(defun prism-blend (a b alpha)
  "Return color A blended with color B by amount ALPHA."
  (cl-flet ((blend (a b alpha)
                   (+ (* alpha a) (* b (- 1 alpha)))))
    (-let* (((ar ag ab) (color-name-to-rgb a))
            ((br bg bb) (color-name-to-rgb b)))
      (color-rgb-to-hex (blend ar br alpha)
                        (blend ag bg alpha)
                        (blend ab bb alpha)))))

(defun prism-opacify-name (color percent)
  "Return color A blended with the background by amount ALPHA."
  (let ((alpha (/ percent 100.0)))
    (prism-blend color (face-attribute 'default :background) alpha)))

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

(defun prism-customize-set (option value)
  "Set OPTION to VALUE, and call `prism-set-colors' when possible."
  (set-default option value)
  (when (--all? (and (boundp it) (symbol-value it))
                '(prism-num-faces prism-color-attribute prism-desaturations
                                  prism-lightens prism-opacities prism-comments-fn prism-strings-fn prism-colors))
    ;; We can't call `prism-set-colors' until *all* relevant options
    ;; have been set.
    (prism-set-colors)))

(declare-function doom-color "ext:doom-themes" t)

(defun prism-theme-color (color)
  "Return COLOR (a string) from current `doom' or `solarized' theme.
If no `doom' or `solarized' theme is active, return COLOR.
Assumes the first `doom' or `solarized' theme found in
`custom-enabled-themes' is the active one."
  (if (string-empty-p color)
      color
    (if-let* ((active-theme (--first (or (string-match (rx bos "doom-" (group (1+ anything)))
                                                       (symbol-name it))
                                         (string-match (rx bos "solarized-" (group (1+ anything)))
                                                       (symbol-name it)))
                                     custom-enabled-themes))
              (theme-name (symbol-name active-theme)))
        (pcase theme-name
          ((rx bos "solarized-")
           (let ((variant (intern (string-trim theme-name (rx "solarized-"))))
                 (color (intern color)))
             ;; Yes, `eval' is evil, but for some reason I can't figure out,
             ;; it's the only way this works here.  In a test function,
             ;; `symbol-value' worked fine, but not here.  Go figure.
             (eval `(solarized-with-color-variables ',variant
                      ,color))))
          ((rx bos "doom-")
           (or (doom-color (intern color))
               color)))
      color)))

;;;; Customization

;; These are at the bottom because the setters call `prism-set-faces',
;; which is defined above.

(defgroup prism nil
  "Disperse lisp forms into a spectrum of colors according to depth."
  :group 'font-lock
  :link '(url-link "https://github.com/alphapapa/prism.el"))

(defcustom prism-num-faces 16
  "Number of `prism' faces."
  :type 'integer
  :set #'prism-customize-set)

(defcustom prism-color-attribute :foreground
  "Face attribute set in `prism' faces."
  :type '(choice (const :tag "Foreground" :foreground)
                 (const :tag "Background" :background))
  :set #'prism-customize-set)

(defcustom prism-desaturations '(40 50 60)
  "Default desaturation percentages applied to colors as depth increases.
This need not be as long as the number of faces used, because
it's extrapolated to the length of `prism-faces'."
  :type '(repeat number)
  :set #'prism-customize-set)

(defcustom prism-lightens '(0 5 10)
  "Default lightening percentages applied to colors as depth increases.
This need not be as long as the number of faces used, because
it's extrapolated to the length of `prism-faces'."
  :type '(repeat number)
  :set #'prism-customize-set)

(defcustom prism-opacities '(100 100 100)
  "Default opacifying percentages applied to colors as depth increases.
This need not be as long as the number of faces used, because
it's extrapolated to the length of `prism-faces'."
  :type '(repeat number)
  :set #'prism-customize-set)

(defcustom prism-comments t
  "Whether to colorize comments.
Note that comments at depth 0 are not colorized, which preserves
the appearance of e.g. commented Lisp headings."
  :type 'boolean)

(defcustom prism-comments-fn
  (lambda (color)
    (prism-blend color (face-attribute 'font-lock-comment-face :foreground) 0.25))
  "Function which adjusts colors for comments.
Receives one argument, a color name or hex RGB string."
  :type 'function
  :set #'prism-customize-set)

(defcustom prism-strings t
  "Whether to fontify strings."
  :type 'boolean)

(defcustom prism-strings-fn
  (lambda (color)
    (prism-blend color "white" 0.5))
  "Function which adjusts colors for strings.
Receives one argument, a color name or hex RGB string."
  :type 'function
  :set #'prism-customize-set)

(defcustom prism-parens-fn
  (lambda (color)
    (prism-blend color (face-attribute 'default :background) 0.5))
  "Function which adjusts colors for strings.
Receives one argument, a color name or hex RGB string."
  :type 'function
  :set #'prism-customize-set)

(defcustom prism-parens nil
  "Whether to colorize parens separately.
When disabled, parens are colorized with the same face as the
other elements at their depth.  When enabled, parens may be
colorized distinctly, e.g. to make them fade away or stand out.
See the PARENS-FN argument to the `prism-set-colors' function."
  :type 'boolean
  :set #'prism-customize-set)

(defcustom prism-colors
  (list 'font-lock-type-face 'font-lock-function-name-face
        'font-lock-keyword-face 'font-lock-doc-face)
  "List of colors used by default."
  :type '(repeat (choice (face :tag "Face (using its foreground color)")
                         color
                         (list :tag "Doom/Solarized theme color (requires active theme)"
                               (const themed)
                               (string :tag "Color name"))
                         (function :tag "Function which returns a color")))
  :set #'prism-customize-set)

(defcustom prism-color-distance 32768
  "Minimum distance between randomized colors.
See `color-distance'."
  :type 'integer)

(defgroup prism-faces nil
  "Faces for `prism'.  Set automatically with `prism-set-colors'.  Do not set manually."
  ;; Define a group for the faces to keep them out of the main
  ;; customization group, otherwise users might customize them there
  ;; and get confused.  Define this group after all other `defcustom's
  ;; so the "current group" isn't changed before they're all defined.
  :group 'prism)

(defcustom prism-whitespace-mode-indents
  (list (cons 'python-mode 'python-indent-offset)
        (cons 'haskell-mode 'haskell-indentation-left-offset)
        (cons t 4))
  "Alist mapping major modes to indentation offsets for `prism-whitespace-mode'.
Each key should be a major mode function symbol, and the value
either a variable whose value to use or an integer number of
spaces.  The last cell is the default, and its key should be t."
  :type '(alist :key-type (choice (const :tag "Default" t)
                                  (symbol :tag "Major mode"))
                :value-type (choice (variable :tag "Value of variable")
                                    (integer :tag "Number of spaces"))))

;;;; Footer

(provide 'prism)

;;; prism.el ends here
