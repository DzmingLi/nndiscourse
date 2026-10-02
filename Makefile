EMACS ?= emacs

.PHONY: test compile
compile:
	$(EMACS) --batch --eval '(package-activate-all)' -L . \
	  --eval '(setq byte-compile-error-on-warn t)' \
	  -f batch-byte-compile discourse-auth.el nndiscourse.el

test:
	$(EMACS) --batch --eval '(package-activate-all)' -L . \
	  -l tests/discourse-auth-test.el -l tests/nndiscourse-test.el \
	  -f ert-run-tests-batch-and-exit
