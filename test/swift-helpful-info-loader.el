;;; swift-helpful-info-loader.el --- Configures `swift-info' for running tests  -*- lexical-binding: t; -*-

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

(require 'info-look)

(add-to-list 'Info-directory-list (format "%s/swift-info" swift-helpful-default-directory))

(info-lookup-maybe-add-help
 :mode 'swift-mode
 :regexp "[#@_a-zA-Z][_a-zA-Z0-9\\?!]*"
 :doc-spec '(("(swift)Index" nil "['`‘]" "['’]")
             ("(swift-reference)Index" nil "['`‘]" "['’]")))
