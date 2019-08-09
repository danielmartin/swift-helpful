;;; swift-helpful-test.el --- Tests for `swift-helpful'            -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Daniel Martín

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(require 'ert)
(require 'swift-helpful)

;; Doc snippet generation tests
(ert-deftest generate-doc-snippet-blank-keyword-test ()
  (should-error (swift-helpful--generate-doc-snippet "")))

(ert-deftest generate-doc-snippet-unknown-keyword-test ()
  (should-error (swift-helpful--generate-doc-snippet "abc")))

(ert-deftest generate-doc-snippet-test ()
  (should (equal (swift-helpful--generate-doc-snippet "func")
                 "
   All of this information is rolled up into the function’s
_definition_, which is prefixed with the ‘func’ keyword.  You indicate
the function’s return type with the _return arrow_ ‘->’ (a hyphen
followed by a right angle bracket), which is followed by the name of the
 [...]"))
  (should (equal (swift-helpful--generate-doc-snippet "init")
                 "_Initializers_ are called to create a new instance of a particular type.
In its simplest form, an initializer is like an instance method with no
parameters, written using the ‘init’ keyword:

     init() {
 [...]"))
  (should (equal (swift-helpful--generate-doc-snippet "@autoclosure")
                 "‘serve(customer:)’ below performs the same operation but, instead of
taking an explicit closure, it takes an autoclosure by marking its
parameter’s type with the ‘@autoclosure’ attribute.  Now you can call
the function as if it took a ‘String’ argument instead of a closure.
The argument is automatically converted to a closure, because the
 [...]"))
  (should (equal (swift-helpful--generate-doc-snippet "@dynamicCallable")
                 "
8.1.3 dynamicCallable
---------------------

Apply this attribute to a class, structure, enumeration, or protocol to
treat instances of the type as callable functions.  The type must
 [...]"))
  (should (equal (swift-helpful--generate-doc-snippet "if")
                 "---------

In its simplest form, the ‘if’ statement has a single ‘if’ condition.
It executes a set of statements only if that condition is ‘true’.

 [...]")))

(ert-deftest generate-doc-snippet-num-lines-context-test ()
  (let ((swift-helpful-doc-snippet-number-of-lines-context 2))
    (should (equal (swift-helpful--generate-doc-snippet "if")
                   "
In its simplest form, the ‘if’ statement has a single ‘if’ condition.
 [...]"))))

(defun swift-helpful-test-contents (file)
  "Return FILE contents as string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun swift-helpful-shell-command-to-string (command)
  "Mock `shell-command-to-string' to not access the system shell."
  (cond
   ((string-equal command (swift-helpful-test-contents "test-data/filter-rg-command.txt"))
    (swift-helpful-test-contents "test-data/filter-rg-results.txt"))
   ((string-equal command (swift-helpful-test-contents "test-data/contains-rg-command.txt"))
    (swift-helpful-test-contents "test-data/contains-rg-results.txt"))
   (t (error "Couldn't mock shell command %s" command))))

(ert-deftest standard-library-grep ()
  (cl-letf (((symbol-function 'shell-command-to-string) #'swift-helpful-shell-command-to-string))
    (should (equal (swift-helpful--stdlib-grep (swift-helpful-test-contents "test-data/filter-type-signature.txt"))
                   (swift-helpful-test-contents "test-data/filter-definition.txt")))
    (should (equal (swift-helpful--stdlib-grep (swift-helpful-test-contents "test-data/contains-type-signature.txt"))
                   (swift-helpful-test-contents "test-data/contains-definition.txt")))))
