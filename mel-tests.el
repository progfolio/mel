;;; mel-tests.el --- Tests                        -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025  Nicholas Vollmer

;; Author: Nicholas Vollmer <nv@parenthetic.dev>
;; Keywords:

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

;;

;;; Code:
(require 'ert)
(require 'mel)

(defun mel-test-equal (a b)
  "Return t if alists A and B have equal set of keys and values (order independent)."
  (not (cl-set-difference (flatten-tree a) (flatten-tree b) :test #'equal)))

(ert-deftest mel-node ()
  ;;@MAYBE not?
  ;;(should (equal (mel-node '(p nil)) '(p nil)))
  (should (mel-test-equal (mel-node '(p)) '((p nil))))

  (should (mel-test-equal (mel-node '(p.class)) '((p ((class . "class"))))))
  (should (mel-test-equal (mel-node '(p.class.two)) '((p ((class . "class two"))))))

  (should (mel-test-equal (mel-node '(p\#id)) '((p ((id . "id"))))))
  (should (mel-test-equal (mel-node '(p\#id.class)) '((p ((id . "id") (class . "class"))))))
  (should-error (mel-node '(p\#id\#again)))

  (should (mel-test-equal (mel-node '(p.class\#id)) '((p ((class . "class") (id . "id"))))))
  (should (mel-test-equal (mel-node '(p.class.two\#id)) '((p ((class . "class two") (id . "id"))))))

  (should (mel-test-equal (mel-node '(p [attr])) '((p ((attr . ""))))))
  (should (mel-test-equal (mel-node '(p.mixed [class class])) '((p ((class . "mixed class"))))))
  (should (mel-test-equal (mel-node '(p.class [class])) '((p ((class . "class"))))))
  (should (mel-test-equal (mel-node '(p.class [attr])) '((p ((class . "class") (attr . ""))))))
  (should (mel-test-equal (mel-node '(p.class.two [attr])) '((p ((class . "class two") (attr . ""))))))
  (should (mel-test-equal (mel-node '(p\#id [attr])) '((p ((id . "id") (attr . ""))))))
  (should (mel-test-equal (mel-node '(p\#id.class [attr]))
                          '((p ((id . "id") (class . "class") (attr . ""))))))

  (should (mel-test-equal (mel-node '(p (p))) '((p nil (p nil))))))

(defun mel-test-output ()
  (interactive)
  (with-current-buffer (get-buffer-create "output.html")
    (erase-buffer)
    (let ((dom (mel-read "/tmp/test.mel" t)))
      (insert (apply #'mel dom))
      (pop-to-buffer (current-buffer)))))

(provide 'mel-tests)
;;; mel-tests.el ends here
