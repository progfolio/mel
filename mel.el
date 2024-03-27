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
(defcustom mel-print-compact nil "When non-nil minimize HTML ouput." :type 'boolean)
(defcustom mel-pandoc-executable (executable-find "pandoc")
  "Path to the pandoc executable." :type 'string)
(defcustom mel-parser-extensions '((".htmel" . mel--template)
                                   (".mel" . mel--partial)
                                   (".txt" . buffer-string)
                                   (".org" . mel--org)
                                   (".md" . mel--markdown))
  "When non-nil minimize HTML ouput."
  :type '(repeat (choice (string :tag "file extension") (function :tag "parser"))))

(defvar mel-data nil)

(defun mel-get (key &optional noerror)
  "Return KEY's `mel-data' value.
If NOERROR is non-nil, return an empty string when key is not found."
  (or (alist-get key mel-data nil)
      (or (and noerror "") (error "No mel-data value for %S" key))))

(defun mel--template ()
  "Eval `current-buffer' as elisp. Return value of last expression."
  (eval (read (format "(progn %s)" (buffer-substring-no-properties (point-min) (point-max))))
        t))

(defun mel--partial ()
  "Parser for file with multiple top-level specs."
  (let ((forms nil))
    (condition-case err
        (while t (push (read (current-buffer)) forms))
      ((end-of-file) nil)
      ((error) (signal (car err) (cdr err))))
    (mapcar (lambda (form) (eval `(backquote ,form))) (nreverse forms))))

(defun mel--pandoc (format)
  "Convert `current-buffer' from FORMAT to HTML via pandoc."
  (if (zerop (call-process-region (point-min) (point-max) mel-pandoc-executable
                                  'delete t nil "-f" format))
      (list :raw (buffer-substring-no-properties (point-min) (point-max)))
    (error "Unable to parse buffer: %s" (buffer-string))))

(defun mel--markdown ()
  "Convert `current-buffer' from Markdown to HTML via pandoc."
  (mel--pandoc "markdown"))

(defun mel-md (&rest strings)
  "Return HTML from markdown STRINGS."
  (with-temp-buffer
    (insert (string-join strings "\n"))
    (mel--markdown)))

(defun mel--org-pandoc ()
  "Convert `current-buffer' from Org to HTML via pandoc."
  (mel--pandoc "markdown"))

(declare-function org-html-convert-region-to-html "ox-html")
(defun mel--org ()
  "Convert `current-buffer' from Org to HTML."
  (require 'ox-html)
  (set-mark (point-min))
  (goto-char (point-max))
  (org-html-convert-region-to-html)
  (list :raw (buffer-substring-no-properties (point-min) (point-max))))

(defun mel-parser (filename)
  "Dispatch to parser in `mel-parser-extensions' via FILENAME.
If no parser matches, `buffer-string' is used."
  (funcall (alist-get (file-name-extension filename) mel-parser-extensions
                      #'buffer-string nil (lambda (k v) (string-match-p v k)))))

(defun mel-load (filename &optional parser)
  "Eval forms in FILENAME via PARSER.
PARSER defaults to evaluate as elisp and return value of last form."
  (with-temp-buffer
    (insert-file-contents filename)
    (goto-char (point-min))
    (if parser (funcall parser) (mel-parser filename))))

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

(defun mel-node (spec)
  "Return a list of nodes from mel SPEC."
  (if (stringp spec) (list spec)
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
               (setq rest
                     (if (consp el) (append (mel-node el) rest)
                       (cons el rest))))
             finally return
             (list `(,tag ,tokens ,@(nreverse rest))))))

(defun mel-nodelist (&rest specs)
  "Return List of nodes from SPECS."
  (apply #'append (mapcar #'mel-node specs)))

(defun mel--dom-print (fn &rest args)
  "Advice for `dom-print' FN to handle ARGS."
  (let* ((node (car args))
         (tag (car-safe node)))
    (cond ((stringp node) (insert (url-insert-entities-in-string node)))
          ((eq tag :raw) (apply #'insert (nthcdr 2 node)))
          ((eq tag :comment)
           (insert (with-current-buffer (get-buffer-create " *mel-comment*")
                     (erase-buffer)
                     (unless (derived-mode-p 'html-mode) (delay-mode-hooks (html-mode)))
                     (apply #'insert (nthcdr 2 node))
                     (comment-region (point-min) (point-max))
                     (buffer-substring-no-properties (point-min) (point-max)))))
          (t (funcall fn node (not mel-print-compact))))))

(defun mel--insert-node (node)
  "Insert NODE."
  (advice-add #'dom-print :around #'mel--dom-print)
  (unwind-protect (dom-print node (not mel-print-compact))
    (advice-remove #'dom-print #'mel--dom-print)))

(defun mel (&rest specs)
  "Return HTML string from SPECS."
  (with-current-buffer (get-buffer-create " *mel-html*")
    (erase-buffer)
    (unless (derived-mode-p 'html-mode) (delay-mode-hooks (html-mode)))
    (mapc #'mel--insert-node (apply #'mel-nodelist specs))
    (unless mel-print-compact (indent-region (point-min) (point-max)))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun mel-file-p (filename)
  "Return non-nil if FILENAME has a .mel or .htmel file extension."
  (when-let (((stringp filename))
             (ext (file-name-extension filename)))
    (string-match-p "\\(?:ht\\)?mel\\'" ext)))

(defun mel-write-html (file)
  "Write current mel FILE to HTML."
  (interactive "fmel file:")
  (with-temp-buffer
    (insert "<!DOCTYPE html>\n" (mel (mel-load file)))
    (write-file (concat (file-name-sans-extension file) ".html") 'confirm)))

(defvar html-tag-help)
(define-derived-mode mel-mode emacs-lisp-mode "mel-mode"
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
