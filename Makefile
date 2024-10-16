.DEFAULT_GOAL := test

.PHONY: test

test:
	@printf "\n------- Checking Emacs Version...\n"
	@$(EMACS) --version | head -1
	@printf "\n------- Byte-Compiling elisp files...\n"
	${EMACS} -Q --batch -L . -f batch-byte-compile *.el
	@printf "\n------- Testing...\n"
	${EMACS} -Q --batch -L . -L tests -l ert -l sgf-io-test.el -l sgf-mode-test.el -f ert-run-tests-batch-and-exit
