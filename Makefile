EMACS ?= emacs
# LispM UI, which asks before a note is thrown away, and LispM Kit, whose
# engine translates LispM UI's words: checkouts beside this one by default.
LISPM_UI ?= ../lispm-ui
LISPM_KIT ?= ../lispm-kit
LOAD_PATH = -L . -L $(LISPM_UI) -L $(LISPM_KIT)

.PHONY: test compile

test:
	$(EMACS) -Q --batch $(LOAD_PATH) -l test/lispm-stickies-test.el -f ert-run-tests-batch-and-exit

# A check: the .elc file is removed again, as it would shadow the source
# of a checkout Emacs loads from.
compile:
	$(EMACS) -Q --batch $(LOAD_PATH) --eval '(setq byte-compile-error-on-warn t)' \
	  -f batch-byte-compile lispm-stickies.el; \
	status=$$?; rm -f lispm-stickies.elc; exit $$status
