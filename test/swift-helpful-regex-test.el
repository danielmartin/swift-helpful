;;; swift-helpful-regex-test.el --- Tests for `swift-helpful-regex'            -*- lexical-binding: t; -*-

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
(require 'swift-helpful-regex)

(ert-deftest swift-helpful-regex--remove-non-interesting-syntax ()
  (should (equal (swift-helpful--remove-non-interesting-syntax "@inlinable public var first: Element? { get }")
                 "@inlinable public var first: Element?"))
  (should (equal (swift-helpful--remove-non-interesting-syntax "func decode(_ type: String.Type, forKey key: KeyedDecodingContainer<K>.Key) throws -> String")
                 "func decode(_ type: String.Type, forKey key: Key) throws -> String")))

(ert-deftest swift-helpful-regex--adapt-generics-test ()
  (should (equal (swift-helpful--adapt-for-generics "public func contains<T>(_ other: T) -> Bool where T : StringProtocol")
                        "public func contains<T : StringProtocol>(_ other: T) -> Bool")))

(ert-deftest swift-helpful-regex--prepare-signature-to-grep-test ()
  (should (equal (swift-helpful--prepare-type-signature-for-grep
                  "func contains<T>(_ other: T) -> Bool where T : StringProtocol")
                 "public func contains<T : StringProtocol>(_ other: T) -> Bool"))
  (should (equal (swift-helpful--prepare-type-signature-for-grep
                  "@inlinable __consuming func filter(_ isIncluded: (String) throws -> Bool) rethrows -> [String]")
                 "@inlinable public __consuming func filter(_ isIncluded: (Element) throws -> Bool) rethrows -> [Element]"))
  (should (equal (swift-helpful--prepare-type-signature-for-grep
                  "@inlinable func map<U>(_ transform: (Wrapped) throws -> U) rethrows -> U?")
                 "@inlinable public func map<U>(_ transform: (Wrapped) throws -> U) rethrows -> U?"))
  (should (equal (swift-helpful--prepare-type-signature-for-grep
                  "@inlinable func map<T>(_ transform: (Self.Element) throws -> T) rethrows -> [T]")
                 "@inlinable public func map<T>(_ transform: (Element) throws -> T) rethrows -> [T]"))
  (should (equal (swift-helpful--prepare-type-signature-for-grep
                  "@inlinable mutating func append<S>(contentsOf newElements: __owned S) where Element == S.Element, S : Sequence")
                 "@inlinable public mutating func append<S : Sequence>(contentsOf newElements: __owned S) where Element == S.Element")))
