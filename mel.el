;;; mel.el --- HTML Elisp Templating               -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Nicholas Vollmer

;; Author: Nicholas Vollmer <nv@parenthetic.dev>
;; URL: https://github.com/progfolio/mel
;; Keywords: convenience, data, hypermedia
;; Created: March 15, 2024
;; Package-Requires: ((emacs "28.1"))
;; Version: 0.0.0

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

;; Mel provides basic elisp HTML templating

;;; Code:
(require 'cl-lib)
(require 'dom)

(defgroup mel nil "HTML Elisp Templating." :group 'programming :prefix "mel-")

(defcustom mel-print-compact nil "When non-nil, print HTML on a single line."
  :type 'boolean)
(defvar mel-data nil)

(defun mel--chars-to-string (chars)
  "Return string from CHARS."
  (cons (car chars) (apply #'string (nreverse (cdr chars)))))

(defun mel--parse-symbol (symbol)
  "Return alist of form ((TYPE . VAL)) from SYMBOL.
Possible types are tag, id, and class."
  (cl-loop with (tokens escaped)
           with target = 'tag
           for c across (symbol-name symbol)
           do (cond ((eq c ?\\) (setq escaped t))
                    ((and (eq c ?.) (not escaped))
                     (when (alist-get 'class tokens)
                       (push ?\s (alist-get 'class tokens)))
                     (setq target 'class))
                    ((and (memq c '(?# ?@)) (not escaped))
                     (and (alist-get 'id tokens) (error "More than one id in %s" symbol))
                     (setq target 'id))
                    (t (push c (alist-get target tokens))
                       (setq escaped nil)))
           finally return (nreverse (mapcar #'mel--chars-to-string tokens))))

(defun mel--merge-attributes (a &optional b)
  "Merge attribute alists A and B.
Common keys have their values appended."
  (when (and (alist-get 'id a) (alist-get 'id b)) (signal 'duplicate-id (list a b)))
  (cl-loop for (k v) on (flatten-tree a) by #'cddr do
           (setf (alist-get k b) (string-trim (concat (alist-get k b) " " v)))
           finally return (nreverse b)))

(defun mel--parse-attributes (vector)
  "Return attribute alist from VECTOR."
  (cl-loop for (k v) on (cl-coerce vector 'list) by #'cddr
           for key = (symbol-name k)
           collect (cons (if (string-prefix-p ":" key) (intern (substring key 1)) k)
                         (if v (format "%s" v) ""))))

(defun mel-read (filename &optional eval)
  "Read forms in FILENAME.
If EVAL is non-nil, evaluate forms."
  (with-temp-buffer
    (insert-file-contents filename)
    (goto-char (point-min))
    (let ((forms nil))
      (condition-case err
          (while t (push (read (current-buffer)) forms))
        ((end-of-file) nil)
        ((error) (signal (car err) (cdr err))))
      (if eval
          (mapcar (lambda (form) (eval `(backquote ,form)))
                  (nreverse forms))
        (nreverse forms)))))

(defun mel-node (spec)
  "Return a DOM fragment from mel SPEC."
  (cl-loop with tokens = (mel--parse-symbol (pop spec))
           with tag = (intern (alist-get 'tag tokens "div"))
           with rest = nil
           initially (setf (alist-get 'tag tokens nil t) nil)
           for el in spec collect
           (if (vectorp el)
               (setq tokens
                     (condition-case err
                         (mel--merge-attributes (mel--parse-attributes el) tokens)
                       ((duplicate-id) (error "Duplicate ID %s: %s" spec (cdr err)))))
             (push (if (consp el) (mel-node el) el) rest))
           finally return `(,tag ,tokens ,@(nreverse rest))))

(defun mel (&rest specs)
  "Return HTML string from SPECS."
  (when (symbolp (car specs)) (setq specs (list specs)))
  (let ((prettyp (not mel-print-compact)))
    (with-temp-buffer
      (mapc (lambda (fragment)
              (dom-print (mel-node fragment) prettyp)
              (when prettyp (insert "\n")))
            specs)
      (when prettyp
        (delay-mode-hooks (html-mode))
        (indent-region (point-min) (point-max)))
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun mel-write-html ()
  "Write current mel file to HTML."
  (interactive)
  (let ((f (buffer-file-name)))
    (with-temp-buffer
      (insert "<!DOCTYPE html>")
      (let* ((dom (mel-read f 'eval))
             (html (apply #'mel dom)))
        (insert html)
        (write-file (concat (file-name-sans-extension f) ".html") 'confirm)))))

(defvar html-tag-help)
(define-derived-mode mel-mode lisp-data-mode "mel-mode"
  "Major mode for editing .mel files."
  (require 'sgml-mode)
  (font-lock-add-keywords
   nil `((,(concat "([[:space:]]*\\("
                   (regexp-opt (mapcar #'car html-tag-help) nil)
                   "\\)[.\\#@[) \t\n\r]")
          1 font-lock-function-name-face)
         ("\\(\\.[[:alpha:]-]+\\)" 1 font-lock-type-face)
         ("[^,]\\(\\(#\\|@\\)[[:alpha:]-]+\\)" 1 font-lock-keyword-face))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.mel\\'" . mel-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.htmel\\'" . mel-mode))

(provide 'mel)
;;; mel.el ends here
