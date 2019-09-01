all: swift.info swift-reference.info

# Swift manual

swift.texi: swift.org
	emacs -batch -l helper.el --eval "(org-to-texi \"swift.org\")"

swift.info: swift.texi
	makeinfo --no-split $^

# Swift reference manual

swift-reference.texi: swift-reference.org
	emacs -batch -l helper.el --eval "(org-to-texi \"swift-reference.org\")"

swift-reference.info: swift-reference.texi
	makeinfo --no-split $^

.PHONY: all
