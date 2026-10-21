EMACS ?= emacs
# lisp/ of LispM Desktop, whose application this is: a checkout beside
# this one by default.
LISPM_DESKTOP ?= ../lispm-desktop/lisp

.PHONY: test compile

test:
	$(EMACS) -Q --batch -L . -L $(LISPM_DESKTOP) -l test/lispm-stickies-test.el \
	  -f ert-run-tests-batch-and-exit

# A check: the .elc file is removed again, as it would shadow the source
# of a checkout Emacs loads from.
compile:
	$(EMACS) -Q --batch -L . -L $(LISPM_DESKTOP) --eval '(setq byte-compile-error-on-warn t)' \
	  -f batch-byte-compile lispm-stickies.el; \
	status=$$?; rm -f lispm-stickies.elc; exit $$status
