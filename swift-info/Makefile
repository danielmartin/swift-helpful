all: swift.info swift.html swift-reference.info swift-reference.html
html: swift.html swift-reference.html

# Swift manual

swift.texi: swift.org
	emacs -batch -l helper.el --eval "(org-to-texi \"swift.org\")"

swift.info: swift.texi
	makeinfo --no-split $^

swift.html: swift.texi
	makeinfo --html --css-ref=https://gnu.org/software/emacs/manual.css $^

# Swift reference manual

swift-reference.texi: swift-reference.org
	emacs -batch -l helper.el --eval "(org-to-texi \"swift-reference.org\")"

swift-reference.info: swift-reference.texi
	makeinfo --no-split $^

swift-reference.html: swift-reference.texi
	makeinfo --html --css-ref=https://gnu.org/software/emacs/manual.css $^

.PHONY: all
