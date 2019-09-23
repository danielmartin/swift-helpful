;;; swift-helpful-regex.el --- A minor mode to show information about Swift things at point.            -*- lexical-binding: t; -*-

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Part of swift-helpful that contains regular expression logic.

;;; Code:
(require 's)
(require 'dash)

(defvar swift-helpful--where-clause-match nil
  "Regular expression match of a `where' clause in a Swift generic declaration.")

(defun swift-helpful--generic-replacement (where-str)
  "Convert `where' generic syntax in WHERE-STR to a format more appropriate for searching."
  (s-join ", "
          (-remove-first
           (lambda (str)
             (when
                 (string-match "\\(where \\)?\\([A-Z] : [a-z]+\\)"
                               (s-trim str))
               (setq swift-helpful--where-clause-match (match-string 2 str))))
           (-map
            's-trim
            (split-string where-str ",")))))

(defun swift-helpful--adapt-for-generics (signature)
  "Replace a generic Swift SIGNATURE so that \"where T : StringProtocol\" becomes <T: StringProtocol>."
  (setq swift-helpful--where-clause-match "")
  (let ((str (replace-regexp-in-string
             "\\(where .*\\)"
             (lambda (match)
               (save-match-data
                 (swift-helpful--generic-replacement match)))
             signature)))
    (if (not (string= "" swift-helpful--where-clause-match))
        (s-trim
         (replace-regexp-in-string
          "<\\([^>]*\\)>"
          swift-helpful--where-clause-match
          str
          t
          nil
          1))
      (s-trim str))))

(defun swift-helpful--prepend-public-keywords (signature)
  "Prepend Swift keywords like `public', `func', `var' in SIGNATURE."
  (let ((case-fold-search nil))
    (replace-regexp-in-string
     (format "%s%s\\)"
             "\\(__.+\\|mutating \\)?\\("
             (regexp-opt
              '("func" "let" "var" "class" "struct" "protocol" "extension")))
     "public \\1\\2"
     signature)))

(defun swift-helpful--prepare-regex-for-sequence-api (signature)
  "Replace \"Element\" with the actual type element in SIGNATURE."
  (if (string-match
         "\\[\\([a-z]\\{2,\\}\\)\\]"
         signature)
      (let ((type (match-string 1 signature)))
        (replace-regexp-in-string
         type
         "Element"
         signature))
    signature))

(defun swift-helpful--remove-non-interesting-syntax (signature)
  "Remove SIGNATURE parts that are not specially useful when grepping the standard library code."
  (s-trim (replace-regexp-in-string
   "Self."
   ""
   (replace-regexp-in-string
    "{ \\(get\\)?\\(set\\)? }"
    ""
    (replace-regexp-in-string
     "KeyedEncodingContainer<K>."
     ""
     (replace-regexp-in-string
      "KeyedDecodingContainer<K>."
      ""
      signature))))))

(defun swift-helpful--prepare-type-signature-for-grep (signature)
  "Perform a series of transformations on SIGNATURE for grepping the standard library code."
  (swift-helpful--adapt-for-generics
   (swift-helpful--prepend-public-keywords
    (swift-helpful--prepare-regex-for-sequence-api
     (swift-helpful--remove-non-interesting-syntax
      signature)))))

(defun swift-helpful--regex-new-lines-escape-chars (str escape-chars)
  "Build a regex sequence flexibly matching STR and escaping some ESCAPE-CHARS.
Insert a regex between each char that matches a few
non-interesting Swift attributes and new line/whitespace
characters (we need to do this because the signature prototype
may be formatted in multiple lines in the standard library
code)."
  (mapconcat (lambda (str)
               (if (member str escape-chars)
                   (format "\\%s" str)
                 str))
             (mapcar #'char-to-string str)
             "[\\n \\(__owned\\|@inline(__always)\\|@discardableResult\\|__consuming\\)]*"))

(provide 'swift-helpful-regex)

;;; swift-helpful-regex.el ends here
