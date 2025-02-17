.DEFAULT_GOAL := test

EMACS ?= emacs

EL_FILES := $(wildcard *.el)
TEST_FILES := $(wildcard tests/*-test.el)

.PHONY: all test byte-compile clean

all: test

test:
	@printf "\n------- Checking Emacs Version...\n"
	@$(EMACS) --version | head -1
	@printf "\n------- Testing...\n"
	$(EMACS) -Q --batch -L . -L tests -l ert $(foreach file,$(TEST_FILES),-l $(file)) -f ert-run-tests-batch-and-exit

clean:
	@printf "\n------- Cleaning byte-compiled files...\n"
	rm -f $(patsubst %.el,%.elc,$(EL_FILES))
	rm -f $(patsubst %.el,%.elc,$(TEST_FILES))
