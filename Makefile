EMACS ?= emacs
# LispM UI (including Kit), which asks before a note is thrown away:
# a checkout beside this one by default.
LISPM_UI ?= ../lispm-ui
LOAD_PATH = -L . -L $(LISPM_UI)

.PHONY: test compile

test:
	$(EMACS) -Q --batch $(LOAD_PATH) -l test/lispm-stickies-test.el -f ert-run-tests-batch-and-exit

# A check: the .elc file is removed again, as it would shadow the source
# of a checkout Emacs loads from.
compile:
	$(EMACS) -Q --batch $(LOAD_PATH) --eval '(setq byte-compile-error-on-warn t)' \
	  -f batch-byte-compile lispm-stickies.el; \
	status=$$?; rm -f lispm-stickies.elc; exit $$status
