EMACS ?= emacs
CASK ?= cask
RM ?= rm -f

LOADPATH = -L .
EMACSBATCH = $(CASK) exec $(EMACS) -q -batch $(LOADPATH)

test:
	$(EMACSBATCH) \
		-l test/test-helper.el \
		-l test/swift-helpful-test.el \
		-l test/swift-helpful-regex-test.el \
		-l test/swift-helpful-info-loader.el \
		-f ert-run-tests-batch-and-exit

checkdoc:
	$(EMACS) -batch -l targets/checkdoc.el

clean:
	$(RM) *.elc

.PHONY : test
