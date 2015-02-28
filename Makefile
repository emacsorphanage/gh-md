CASK  ?= cask
WGET  ?= wget
EMACS ?= emacs

EMACSFLAGS =
EMACSBATCH = $(EMACS) --batch -Q $(EMACSFLAGS)

export EMACS

PKGDIR := $(shell EMACS=$(EMACS) $(CASK) package-directory)

SRCS = gh-md.el
OBJS = $(SRCS:.el=.elc)

.PHONY: all build package test clean

all: compile README.md

build: $(PKGDIR)
	$(CASK) build

package:
	$(CASK) package

test:
	$(CASK) exec $(EMACSBATCH) -L . -l gh-md-test.el --eval "(ert t)"

clean:
	$(RM) $(OBJS)

%.elc: %.el $(PKGDIR)
	$(CASK) exec $(EMACSBATCH) -f batch-byte-compile $<

$(PKGDIR): Cask
	$(CASK) install
	touch $(PKGDIR)

README.md: el2markdown.el $(SRCS)
	$(CASK) exec $(EMACSBATCH) -l $< $(SRCS) -f el2markdown-write-readme

el2markdown.el:
	$(WGET) -q -O $@ "https://github.com/Lindydancer/el2markdown/raw/master/el2markdown.el"

.INTERMEDIATE: el2markdown.el
