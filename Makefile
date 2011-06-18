# Copyright (C) 2010, 2011 by Lluís Vilanova
#
# Maintainer: CEDET developers <http://sf.net/projects/cedet>
# Created: 16 Sep 2010
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation; either version 2, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GNU Emacs; see the file COPYING.  If not, write to the
# Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
# Boston, MA 02110-1301, USA.

###### User-customizable part of the Makefile ##################################

## Echo commands during compilation (you can set it at call time: "make V=1")
V=0

## Paths and flags to common programs
EMACS=emacs
EMACSFLAGS=-batch --no-site-file
FIND=find
RM=rm
RMFLAGS=-f
MAKEINFO=makeinfo

## Which packages to compile, test, etc.
PACKAGES=eieio speedbar cedet

###### Internal part of the Makefile ###########################################

### Paths
lispdir=$(CURDIR)/lisp
docdir=$(CURDIR)/doc
testdir=$(CURDIR)/tests


### Helpers
EEVAL=$(EMACS) $(EMACSFLAGS) --eval
ECOMPILE=(or (byte-compile-file \"$(1)\") (kill-emacs 1))
EGRAMMAR=(find-file \"$(1)\") (semantic-mode) (semantic-grammar-create-package)
LISP_PATH=$(foreach pkg,$(PACKAGES),(add-to-list (quote load-path) \"$(lispdir)/$(pkg)/\"))
ifeq ($(V),1)
Q=
else
Q=@echo "    > $@";
endif


### Top-level rules

all: generate compile doc

generate: $(patsubst %,generate-%,$(PACKAGES))

compile: compile-common $(patsubst %,compile-%,$(PACKAGES))

doc: $(patsubst %,doc-%,$(PACKAGES))

test: $(patsubst %,test-%,$(PACKAGES))

clean: clean-common $(patsubst %,clean-%,$(PACKAGES))


### Specialized rules

cedet_BOVINE=$(shell $(FIND) $(lispdir)/cedet/ -name \*.by)
cedet_WISENT=$(shell $(FIND) $(lispdir)/cedet/ -name \*.wy -and -not -path $(lispdir)/cedet/semantic/grammar.wy)
cedet_GENERATE_LISP=$(patsubst %.by,%-by.el,$(cedet_BOVINE)) $(patsubst %.wy,%-wy.el,$(cedet_WISENT))


### Dynamic rules

define PACKAGE_template
$(1)_LISP=$(shell $(FIND) $(lispdir)/$(1)/ -name \*.el)
$(1)_CODE=$$(patsubst %.el,%.elc,$$($(1)_LISP))
$(1)_TEXINFO=$(shell $(FIND) $(docdir) -name $(1).texi) $(shell test ! -d $(docdir)/$(1) || $(FIND) $(docdir)/$(1)/ -name *.texi)
$(1)_INFO=$$(patsubst %.texi,%.info,$$($(1)_TEXINFO))
$(1)_TEST_LISP=$(shell test ! -d $(testdir)/$(1)/ || $(FIND) $(testdir)/$(1)/ -name \*.el)
$(1)_TEST_CODE=$$(patsubst %.el,%.elc,$$($(1)_TEST_LISP))

generate-$(1): $$($(1)_GENERATE_LISP)

compile-$(1): $$($(1)_CODE)

doc-$(1): $$($(1)_INFO)

test-$(1): $$($(1)_TEST_CODE)

clean-$(1):
	$(RM) $(RMFLAGS) $$($(1)_GENERATE_LISP)
	$(RM) $(RMFLAGS) $$($(1)_CODE)
	$(RM) $(RMFLAGS) $$($(1)_INFO)
endef

$(eval $(call PACKAGE_template,common))
$(foreach pkg,$(PACKAGES),$(eval $(call PACKAGE_template,$(pkg))))

# Require the list of packages given as argument
require=$(foreach r,$(1),(require (quote $(r))))


### Generic rules

%-wy.el: REQUIRES=semantic/grammar semantic/wisent semantic/wisent/grammar
%-wy.el: %.wy
	$(Q)$(EEVAL) "(progn $(LISP_PATH) $(call require,$(REQUIRES)) $(call EGRAMMAR,$<))"

%-by.el: REQUIRES=semantic/grammar semantic/wisent semantic/bovine/grammar
%-by.el: %.by
	$(Q)$(EEVAL) "(progn $(LISP_PATH) $(call require,$(REQUIRES)) $(call EGRAMMAR,$<))"

%-wy.elc: REQUIRES=semantic/grammar

%-by.elc: REQUIRES=semantic/bovine
%-by.elc: EEXTRA=(setq max-specpdl-size (max 3000 max-specpdl-size) max-lisp-eval-depth (max 1000 max-lisp-eval-depth))

%.elc: %.el
	$(Q)$(EEVAL) "(progn $(LISP_PATH) $(call require,$(REQUIRES)) $(EEXTRA) $(call ECOMPILE,$<))"

%.info: %.texi
	$(Q)$(MAKEINFO) $< -o $@
