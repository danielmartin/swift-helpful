;;; swift-helpful.el --- Show documentation for Swift programs. -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Daniel Martín

;; Author: Daniel Martín <mardani29@yahoo.es>
;; URL: https://github.com/danielmartin/swift-helpful
;; Keywords: help, swift
;; Version: 0.2
;; Package-Requires: ((emacs "25.1") (dash "2.12.0") (lsp-mode "6.0") (swift-mode "8.0.0"))

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

;; This extension adds a command, `swift-helpful', which, when
;; executed, shows documentation about a Swift token under
;; point.  Sources of documentation include:
;;
;;    - Swift official manual and reference manual (installed by this
;;    extension in GNU Info format).
;;
;;    - Language Server Protocol (LSP), if configured for the project.
;;
;;    - Swift standard library source code (if available locally and
;;    its path is correctly configured).

;;; Code:

(require 'dash)
(require 'info-look)
(require 'lsp-mode)
(require 'swift-mode)
(require 'swift-helpful-regex)

(defgroup swift-helpful nil
  "Show contextual information about Swift code."
  :link '(url-link "https://github.com/danielmartin/swift-helpful")
  :group 'swift)

(defcustom swift-helpful-switch-buffer-function
  #'pop-to-buffer
  "Function called to display the *swift-helpful* buffer."
  :type 'function
  :group 'swift-helpful)

(defcustom swift-helpful-doc-snippet-number-of-lines-context 5
  "Number of lines of documentation shown as context in documentation snippets."
  :type 'number
  :group 'swift-helpful)

(defcustom swift-helpful-ripgrep-executable
  "rg"
  "Path of the ripgrep executable."
  :type 'string
  :group 'swift-helpful)

(defcustom swift-helpful-stdlib-path nil
  "Path of a local Swift standard library checkout."
  :type 'string
  :group 'swift-helpful)

(defcustom swift-helpful-debug-log nil
  "Whether `swift-helpful' will debug information to the Messages buffer during execution."
  :type 'boolean
  :group 'swift-helpful)

(defface swift-helpful-section-title
  '((t (:weight bold)))
  "Face used for section titles in *swift-helpful* buffers.")

;; Buttons
(define-button-type 'swift-helpful-manual-button
  'action #'swift-helpful--manual
  'symbol nil
  'follow-link t
  'help-echo "View this symbol in the Swift manual")

(define-button-type 'swift-helpful-references-button
  'action #'swift-helpful--references
  'symbol nil
  'source-buffer nil
  'follow-link t
  'help-echo "Search for references of this symbol in the codebase")

(define-button-type 'swift-helpful-definition-button
  'action #'swift-helpful--definition
  'symbol nil
  'source-buffer nil
  'follow-link t
  'help-echo "Search for definition of this symbol in the codebase")

(define-button-type 'swift-helpful-navigate-button
  'action #'swift-helpful--navigate
  'path nil
  'position nil
  'follow-link t
  'help-echo "Navigate to definition")

(defun swift-helpful--navigate-button (text path &optional pos)
  "Return a button with TEXT caption that opens PATH and puts point at POS."
  (swift-helpful--button
   text
   'swift-helpful-navigate-button
   'path path
   'position pos))

(defun swift-helpful--navigate (button)
  "Navigate to the path this BUTTON represents."
  (find-file-other-window (substring-no-properties (button-get button 'path)))
  ;; We use `get-text-property' to work around an Emacs 25 bug:
  ;; http://git.savannah.gnu.org/cgit/emacs.git/commit/?id=f7c4bad17d83297ee9a1b57552b1944020f23aea
  (-when-let (pos (get-text-property button 'position
                                     (marker-buffer button)))
    (swift-helpful--goto-char-widen (swift-helpful--point-from-line pos))))

(defun swift-helpful--make-manual-button (sym)
  "Make manual button for SYM."
  (swift-helpful--button
   "Read more in manual"
   'swift-helpful-manual-button
   'symbol sym))

(defun swift-helpful--make-references-button (source-buffer sym)
  "Make search references button for SOURCE-BUFFER and SYM."
  (swift-helpful--button
   "Search references"
   'swift-helpful-references-button
   'symbol sym
   'source-buffer source-buffer))

(defun swift-helpful--make-definition-button (source-buffer sym)
  "Make search definition button for SOURCE-BUFFER and SYM."
  (swift-helpful--button
   "Go to definition"
   'swift-helpful-definition-button
   'symbol sym
   'source-buffer source-buffer))

(defun swift-helpful--button (text type &rest properties)
  "Create a button with some TEXT, TYPE, and PROPERTIES."
  (setq text (substring-no-properties text))
  (apply #'make-text-button
         text nil
         :type type
         properties))

(defun swift-helpful--manual (button)
  "Open the manual for the system that this BUTTON represents."
  (let ((sym (button-get button 'symbol)))
    (info-lookup 'symbol sym #'swift-mode)))

(defun swift-helpful--references (button)
  "Open the references for the system that this BUTTON represents."
  (let ((sym (button-get button 'symbol))
        (source-buffer (button-get button 'source-buffer)))
    (with-current-buffer source-buffer
      (xref-find-references sym))))

(defun swift-helpful--definition (button)
  "Open the definition for the system that this BUTTON represents."
  (let ((sym (button-get button 'symbol))
        (source-buffer (button-get button 'source-buffer)))
    (with-current-buffer source-buffer
      (xref-find-definitions-other-window sym))))

(defun swift-helpful--section-title (text)
  "Propertize TEXT as a section title."
  (format "%s\n" (propertize text 'face 'swift-helpful-section-title)))

(defun swift-helpful--buffer (sym)
  "Return a buffer *swift-helpful* to display information about SYM."
  (let* ((buf-name
          (format "*swift-helpful %s*"
                  sym))
         (buf (get-buffer buf-name)))
    (unless buf
      (setq buf (get-buffer-create buf-name)))
    (with-current-buffer buf
      (swift-helpful-mode))
    buf))

(defun swift-helpful--in-manual-p (sym)
  "Return t if SYM is in a Swift or Swift-Reference manual."
  (let ((completions
         (cl-letf (((symbol-function #'message)
                    (lambda (_format-string &rest _args))))
           (info-lookup->completions 'symbol 'swift-mode))))
    (-when-let (buf (get-buffer " temp-info-look"))
      (kill-buffer buf))
    (or (assoc sym completions)
        (assoc-string sym completions))))

(defun swift-helpful--point-at-beginning-of-doc-snippet ()
  "Return the point at the beginning of the documentation snippet."
  (save-excursion
    (goto-char (point-min))
    (search-forward "\n")))

(defun swift-helpful--goto-char-widen (pos)
  "Move point to POS in the current buffer.
If narrowing is in effect, widen if POS isn't in the narrowed area."
  (when (or (< pos (point-min))
            (> pos (point-max)))
    (widen))
  (goto-char pos))

(defun swift-helpful--generate-doc-snippet (sym)
  "Generate a documentation snippet from the manuals about SYM."
  (save-window-excursion
    ;; Reset the overlay, in case there was a recent Info search.
    (setq info-lookup-highlight-overlay nil)
    (info-lookup 'symbol sym #'swift-mode)
    (with-current-buffer "*info*"
      (unwind-protect
          ;; Get `info-lookup-highlight-overlay' and use it to produce a
          ;; better context for the snippet.
          (let ((highlight-overlay info-lookup-highlight-overlay))
            ;; Two cases: If we have a highlight overlay, that means we
            ;; found the keyword in the Info node page and we can provide
            ;; a more accurate doc snippet. If we don't have an overlay,
            ;; just show a few lines from the beginning of the useful part
            ;; of the Info content.
            (if highlight-overlay
                (swift-helpful--generate-doc-snippet-using-overlay highlight-overlay)
              (swift-helpful--generate-doc-snippet-general-algorithm)))
        (kill-buffer "*info*")))))

(defvar swift-helpful--overlay-found nil
  "Flag that indicates if an `info-look' highlight overlay has been found.")

(defvar swift-helpful--overlay-start-local nil
  "Overlay start position relative to a `swift-helpful' documentation section.")

(defvar swift-helpful--overlay-end-local nil
  "Overlay end position relative to a `swift-helpful' documentation section.")

(defun swift-helpful--generate-doc-snippet-using-overlay (overlay)
  "Generate a documentation snippet around an OVERLAY.
OVERLAY represents a highlighted symbol reference in the Swift manuals."
  (swift-helpful--log "The documentation snippet is generated around a text match in the Info node.")
  (setq swift-helpful--overlay-found t)
  (let* ((overlay-start (overlay-start overlay))
         (overlay-end (overlay-end overlay))
         (beg-line (- (line-number-at-pos overlay-start)
                      (/ swift-helpful-doc-snippet-number-of-lines-context 2)))
         (end-line (+ (line-number-at-pos overlay-end)
                      (- swift-helpful-doc-snippet-number-of-lines-context
                         (/ swift-helpful-doc-snippet-number-of-lines-context 2))))
         (info-buffer-string (buffer-string)))
    ;; Convert `overlay-start' coordinates to "local" coordinates,
    ;; that is, coordinates starting from `beg-line'.
    (setq swift-helpful--overlay-start-local (- overlay-start
                                 (swift-helpful--point-from-line beg-line)))
    ;; Convert `overlay-end' coordinates to "local" coordinates,
    ;; that is, coordinates starting from `beg-line'.
    (setq swift-helpful--overlay-end-local (- overlay-end
                               (swift-helpful--point-from-line beg-line)))
    (with-temp-buffer
      (insert info-buffer-string)
      (let ((beg-pos (swift-helpful--point-from-line beg-line))
            (end-pos (swift-helpful--point-from-line end-line)))
        (format "%s [...]" (buffer-substring-no-properties
                            beg-pos
                            end-pos))))))

(defun swift-helpful--generate-doc-snippet-general-algorithm ()
  "Generate a documentation snippet from the start of a Swift manual section.
The number of lines in the documentation snippet is controlled by
the `swift-helpful-doc-snippet-number-of-lines-context'
variable."
  (swift-helpful--log "The documentation snippet is NOT generated around a text match in the Info node.")
  (setq swift-helpful--overlay-found nil)
  (let* ((beg-line (line-number-at-pos
                    (swift-helpful--point-at-beginning-of-doc-snippet)))
         (end-line (1+
                    (+ beg-line
                       swift-helpful-doc-snippet-number-of-lines-context)))
         (info-buffer-string (buffer-string)))
    (with-temp-buffer
      (insert info-buffer-string)
      (let ((beg-pos (swift-helpful--point-from-line beg-line))
            (end-pos (swift-helpful--point-from-line end-line)))
        (format "%s [...]" (buffer-substring-no-properties
                            beg-pos
                            end-pos))))))

(defun swift-helpful--point-from-line (line)
  "Return the point at the first character in LINE."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line))
    (point)))

(defun swift-helpful--generate-lsp-snippet (source-buffer)
  "Produce a LSP-based snippet at current point in SOURCE-BUFFER."
  (when (swift-helpful--lsp-info-p source-buffer)
    (save-window-excursion
      (with-current-buffer source-buffer
        (lsp-describe-thing-at-point))
      (let ((message (current-message)))
        (when (and
               (or (get-buffer "*lsp-help*") (get-buffer (help-buffer)))
               (or (not message)
                   (not (string-match "No content at point" message))))
          (with-current-buffer (or (get-buffer "*lsp-help*") (help-buffer))
            (buffer-substring (point-min) (point-max))))))))

(defun swift-helpful--standard-library-identifier-p (source-buffer)
  "Return t if the thing at point in SOURCE-BUFFER is a Swift stdlib identifier."
  (when (swift-helpful--lsp-info-p source-buffer)
    (with-current-buffer source-buffer
      (let ((loc (lsp-request "textDocument/definition"
                              (lsp--text-document-position-params))))
        (if loc
          (let ((uri (gethash "uri" (car loc))))
            (or
             (string-match-p "Foundation.swiftmodule" uri)
             (string-match-p "Swift.swiftmodule" uri)))
          ;; Assume that if we don't have a definition, it may be a
          ;; standard library.
          t)))))

(defun swift-helpful--lsp-info-p (source-buffer)
  "Return t if LSP could offer information about Swift code in SOURCE-BUFFER."
  (with-current-buffer source-buffer
    (and (bound-and-true-p lsp-mode)
         (derived-mode-p 'swift-mode)
         (lsp-workspaces))))

(defun swift-helpful--type-signature-to-grep (lsp-snippet)
  "Return the type signature information from LSP-SNIPPET."
  (with-temp-buffer
    (insert lsp-snippet)
    ;; The type signature is placed in the second line of the snippet
    ;; and goes to the end of that line.
    (let ((beg-signature (progn (goto-char (point-min))
                                (forward-line (1- 2))
                                (point)))
          (end-signature (progn (end-of-line)
                                (point))))
      (buffer-substring-no-properties
       beg-signature
       end-signature))))

(defconst swift-helpful--highlighting-funcs
  '(ert--activate-font-lock-keywords
    highlight-quoted-mode
    rainbow-delimiters-mode)
  "Highlighting functions that are safe to run in a temporary buffer.
This is used in `swift-helpful--syntax-highlight' to support extra
highlighting that the user may have configured in their mode
hooks.")

(defconst swift-helpful-max-highlight 5000
  "Don't highlight code with more than this many characters.")

(defun swift-helpful--syntax-highlight (source &optional mode)
  "Return a propertized version of SOURCE in MODE."
  (unless mode
    (setq mode #'swift-mode))
  (if (or
       (< (length source) swift-helpful-max-highlight)
       (eq mode 'swift-mode))
      (with-temp-buffer
        (insert source)

        ;; Switch to major-mode MODE, but don't run any hooks.
        (delay-mode-hooks (funcall mode))

        ;; `delayed-mode-hooks' contains mode hooks like
        ;; `emacs-lisp-mode-hook'. Build a list of functions that are run
        ;; when the mode hooks run.
        (let (hook-funcs)
          (dolist (hook delayed-mode-hooks)
            (let ((funcs (symbol-value hook)))
              (setq hook-funcs (append hook-funcs funcs))))

          ;; Filter hooks to those that relate to highlighting, and run them.
          (setq hook-funcs (-intersection hook-funcs
                                          swift-helpful--highlighting-funcs))
          (-map #'funcall hook-funcs))

        (if (fboundp 'font-lock-ensure)
            (font-lock-ensure)
          (with-no-warnings
            (font-lock-fontify-buffer)))
          (buffer-string))
    ;; SOURCE was too long to highlight in a reasonable amount of
    ;; time.
    (concat
     (propertize
      "// Skipping highlighting due to "
      'face 'font-lock-comment-face)
     source)))

(defun swift-helpful--rg-format-command (search-term dir)
  "Prepare a ripgrep command for SEARCH-TERM that search on DIR."
  (format
   "%s -U --line-number %s %s"
   swift-helpful-ripgrep-executable
   (shell-quote-argument search-term)
   dir))

(defun swift-helpful--insert-source-code-in-buffer (source-file line source-code-buf)
  "Insert a Swift function at SOURCE-FILE:LINE into SOURCE-CODE-BUF."
  (with-temp-buffer
    (insert-file-contents source-file)
    (goto-char (point-min))
    (forward-line (1- line))
    (delay-mode-hooks (swift-mode))
    (swift-mode:end-of-defun)
    (swift-mode:beginning-of-defun)
    (swift-mode:narrow-to-defun)
    (let ((code-snippet (buffer-string)))
      (with-current-buffer source-code-buf
        (insert
         (concat "From "
                 (swift-helpful--navigate-button
                  (f-abbrev source-file)
                  source-file
                  line)))
        (insert "\n")
        (insert (swift-helpful--syntax-highlight
                 code-snippet
                 'swift-mode))
        (insert "\n\n")))))

(defun swift-helpful--sort-stdlib-grep-results (x y)
  "Sort standard library grep results X and Y by filename, alphabetically."
  (let* ((locx (compilation--message->loc x))
         (filex (caar (compilation--loc->file-struct locx)))
         (locy (compilation--message->loc y))
         (filey (caar (compilation--loc->file-struct locy))))
    (string< filex filey)))

(defun swift-helpful--stdlib-grep (signature)
  "Grep for a Swift standard library SIGNATURE."
  (swift-helpful--log signature)
  (with-temp-buffer
    (let* ((stdlib-search-query (swift-helpful--prepare-type-signature-for-grep
                                 signature))
           (search-query-new-lines (swift-helpful--regex-new-lines-escape-chars
                                    stdlib-search-query '("(" ")" "[" "]")))
           (grep-command-format (swift-helpful--rg-format-command
                                 search-query-new-lines
                                 (expand-file-name swift-helpful-stdlib-path)))
           (search-results (shell-command-to-string grep-command-format))
           (current-file))
      (swift-helpful--log "Search query: %s" stdlib-search-query)
      (swift-helpful--log "Grep command: %s" grep-command-format)
      (swift-helpful--log "Query results: %s" search-results)
      (insert search-results)
      (grep-mode)
      (setq current-file nil)
      (let ((source-code-buf (get-buffer-create "*stdlib-source-code*"))
            (messages))
        (dotimes (i (count-lines (point-min) (point-max)))
          (goto-char (point-min))
          (let* ((msg (compilation-next-error i nil
                                              (or compilation-current-error
                                                  compilation-messages-start
                                                  (point-min)))))
            (push msg messages)))
        ;; Sort the grep results by filename.
        (sort messages #'swift-helpful--sort-stdlib-grep-results)
        (dolist (msg messages)
          (let* ((loc (compilation--message->loc msg))
                 (file (caar (compilation--loc->file-struct loc)))
                 (line (compilation--loc->line loc)))
            ;; Assume only one result per file.
            (unless (eq current-file file)
              (swift-helpful--log "Ripgrep result from file %s" file)
              (setq current-file file)
              (swift-helpful--insert-source-code-in-buffer
               file line source-code-buf))))
        (with-current-buffer source-code-buf
          (unwind-protect
              (buffer-string)
            (kill-buffer source-code-buf)))))))

(defun swift-helpful--lsp-snippet-p (lsp-snippet)
  "Return if LSP-SNIPPET is a non-empty LSP snippet."
  (and lsp-snippet
       (char-or-string-p lsp-snippet)
       (not (string-blank-p lsp-snippet))))

(defun swift-helpful--insert-header (sym)
  "Insert a section header for the symbol SYM."
  (insert
   (swift-helpful--section-title (format "%s" sym))
   "\n"))

(defun swift-helpful--insert-lsp (sym source-buffer lsp-snippet)
  "Insert LSP information for the symbol SYM.
SOURCE-BUFFER is the buffer where SYM comes from and the LSP
information is passed in LSP-SNIPPET."
  (insert
   (swift-helpful--section-title "Signature Information (from LSP)")
   "\n"
   lsp-snippet
   "\n\n"
   (swift-helpful--section-title "Cross-references (from LSP)")
   "\n"
   (format "%s %s"
           (swift-helpful--make-references-button source-buffer sym)
           (swift-helpful--make-definition-button source-buffer sym))
   "\n\n"))

(defun swift-helpful--insert-manuals (sym)
  "Insert Swift manual information for the symbol SYM in the current buffer."
  (insert
   (swift-helpful--section-title "Swift Language Documentation")
   "\n")
  (let ((point-before-snippet (point)))
    (insert (swift-helpful--generate-doc-snippet sym))
    (when swift-helpful--overlay-found
      (let ((new-overlay-start (+ point-before-snippet
                                  swift-helpful--overlay-start-local))
            (new-overlay-end (+ point-before-snippet
                                swift-helpful--overlay-end-local)))
        (overlay-put (make-overlay new-overlay-start new-overlay-end)
                     'face info-lookup-highlight-face))))
  ;; Insert a "Read more in manual" button in case the user is
  ;; curious about this topic and wants to learn more.
  (insert
   "\n\n"
   (swift-helpful--make-manual-button sym)
   "\n\n"))

(defun swift-helpful--insert-library-source-code (lsp-snippet)
  "Insert Swift standard library information for LSP-SNIPPET.
Inserting source code from the standard library only requires
that the `swift-helpful-stdlib-path' variable correctly points to
a checkout of the Swift standard library and that the
`swift-helpful-ripgrep-executable' variable points to a recent
version of the rg command line
tool (https://github.com/BurntSushi/ripgrep)."
  (let ((swift-helpful--stdlib-grep (substring-no-properties
                                     (swift-helpful--type-signature-to-grep
                                      lsp-snippet))))
    (unless (string-blank-p swift-helpful--stdlib-grep)
      (insert
       (swift-helpful--section-title "Source Code")
       "\n"
       swift-helpful--stdlib-grep))))

(defun swift-helpful--update (sym source-buffer lsp-snippet)
  "Update the *swift-helpful* buffer to document SYM from SOURCE-BUFFER.
This function creates several sections with all the documentation
that could be found about the Swift symbol SYM.  LSP-SNIPPET
contains information from Language Server Protocol about that
symbol."
  (let ((inhibit-read-only t)
        (standard-library-p
         (swift-helpful--standard-library-identifier-p source-buffer)))
    (if standard-library-p
        (swift-helpful--log "Symbol %s probably comes from the Swift standard library" sym)
      (swift-helpful--log "Symbol %s probably does not come from the Swift standard library" sym))
    (erase-buffer)
    (remove-overlays)
    ;; Header.
    (swift-helpful--insert-header sym)
    ;; LSP.
    (when (swift-helpful--lsp-snippet-p lsp-snippet)
      (swift-helpful--insert-lsp sym source-buffer lsp-snippet))
    ;; Manuals.
    (when (swift-helpful--in-manual-p sym)
      (swift-helpful--insert-manuals sym))
    ;; Standard library source code.
    (when (and (and swift-helpful-stdlib-path
                    (file-exists-p swift-helpful-stdlib-path))
               (executable-find swift-helpful-ripgrep-executable)
               standard-library-p
               (swift-helpful--lsp-snippet-p lsp-snippet))
      (swift-helpful--insert-library-source-code lsp-snippet))
    ;; Position the point at the beginning of the buffer when
    ;; everything is in place.
    (goto-char (point-min))))

(defun swift-helpful--symbol-at-point ()
  "Return the Swift symbol at point."
  (save-excursion
    (forward-symbol -1)
    (swift-mode:token:text (swift-mode:forward-token-simple))))

(defun swift-helpful--log (msg &rest args)
  "Log MSG with ARGS to the Messages buffer if `swift-helpful-debug-log' is t."
  (when swift-helpful-debug-log
    (message msg args)))

(defun swift-helpful-maybe-configure-manuals ()
  "Configure `info-look' so that Swift reference indices can be queried."
  (info-lookup-maybe-add-help
   :mode 'swift-mode
   :regexp "[#@_a-zA-Z][_a-zA-Z0-9\\?!]*"
   :doc-spec '(("(swift)Index" nil "['`‘]" "['’]")
               ("(swift-reference)Index" nil "['`‘]" "['’]"))))

;;;###autoload
(defun swift-helpful ()
  "Open a panel with information about a Swift token at point.
Information comes from different configurable sources: Language
Server Protocol (LSP), the Swift reference manuals, or the
standard library source code (if locally available and
accessible)."
  (interactive)
  (let* ((sym (swift-helpful--symbol-at-point))
         (source-buffer (current-buffer))
         (lsp-snippet
          (swift-helpful--generate-lsp-snippet source-buffer)))
    (swift-helpful--log "Symbol under point %s" sym)
    (swift-helpful-maybe-configure-manuals)
    (unless sym
      (user-error "There isn't a Swift symbol under point"))
    (unless (or (swift-helpful--lsp-snippet-p lsp-snippet)
                (swift-helpful--in-manual-p sym))
      (user-error "%s: Not documented as a symbol" sym))
    (funcall swift-helpful-switch-buffer-function (swift-helpful--buffer sym))
    (swift-helpful--update sym source-buffer lsp-snippet)))

(defvar swift-helpful-mode-map
  (let* ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") #'forward-button)
    (define-key map (kbd "<backtab>") #'backward-button)
    map)
  "Keymap for `swift-helpful-mode'.")

(define-derived-mode swift-helpful-mode special-mode "Swift-Helpful"
  "Major mode for *swift-helpful* buffers.")

(provide 'swift-helpful)

;;; swift-helpful.el ends here
