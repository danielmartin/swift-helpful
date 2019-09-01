EMACS ?= emacs
CASK ?= cask
RM ?= rm -f

LOADPATH = -L .
EMACSBATCH = $(CASK) exec $(EMACS) -q -batch $(LOADPATH)

compile:
	$(EMACSBATCH) \
		-f batch-byte-compile swift-helpful.el swift-helpful-regex.el

test:
	$(EMACSBATCH) \
		-l test/test-helper.el \
		-l test/swift-helpful-test.el \
		-l test/swift-helpful-regex-test.el \
		-l test/swift-helpful-info-loader.el \
		-f ert-run-tests-batch-and-exit

test-ert-runner:
	$(CASK) exec ert-runner

checkdoc:
	$(EMACS) -batch -l targets/checkdoc.el

clean:
	$(RM) *.elc

.PHONY : test test-ert-runner checkdoc clean
