;;; test-helper.el --- Helper for tests            -*- lexical-binding: t; -*-

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

(require 'f)

(defvar swift-helpful-default-directory
  (f-parent (f-dirname (f-this-file)))
  "Return the root directory of the `swift-helpful' project.")

(defun swift-helpful-test-contents (file)
  "Return FILE contents as string."
  (with-temp-buffer
    (insert-file-contents
     (format "%s/test/test-data/%s"
             swift-helpful-default-directory
             file))
    (buffer-string)))

(defun swift-helpful-shell-command-to-string (command)
  "Mock `shell-command-to-string' to not access the system shell."
  (cond
   ((string-equal command (swift-helpful-test-contents
                           "filter-rg-command.txt"))
    (swift-helpful-test-contents "filter-rg-results.txt"))
   ((string-equal command (swift-helpful-test-contents
                           "contains-rg-command.txt"))
    (swift-helpful-test-contents "contains-rg-results.txt"))
   (t (error "Couldn't mock shell command %s" command))))

(defun swift-helpful-display-color-p (&optional display)
  "Mock `display-color-p' to assume the DISPLAY always support color.
We need to mock this function because we rely on overlays created
by `info-look' to provide a better context for the documentation
snippet. Per `info-look' implementation, overlays are only
created when the display supports color, and this is not the case
when Emacs runs in batch mode."
  t)

(defun swift-helpful--type-signature-to-grep-mock (signature)
  "Mock `swift-helpful--type-signature-to-grep' to simulate a no results
  status for a stdlib search for SIGNATURE."
  "")

(defun swift-helpful--standard-library-identifier-mock-p (source-buffer)
  "Mock `swift-helpful--standard-library-identifier-p' to
  simulate that the symbol at point at SOURCE-BUFFER comes from
  the Swift standard library."
  t)

;;; test-helper.el ends here
