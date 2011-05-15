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
LISP_PATH=$(foreach pkg,$(PACKAGES),(add-to-list (quote load-path) \"$(lispdir)/$(pkg)/\"))


### Top-level rules

all: compile doc

compile: compile-common $(patsubst %,compile-%,$(PACKAGES))

doc: $(patsubst %,doc-%,$(PACKAGES))

test: $(patsubst %,test-%,$(PACKAGES))

clean: clean-common $(patsubst %,clean-%,$(PACKAGES))


### Specialized rules

cedet_PRE_CODE = grammar-cedet
cedet_BOVINE=$(shell $(FIND) $(lispdir)/cedet/semantic/bovine/ -name \*.by)
cedet_WISENT=$(shell $(FIND) $(lispdir)/cedet/semantic/wisent/ -name \*.wy)
cedet_BOVINE_CODE=$(patsubst %.by,%-by.elc,$(cedet_BOVINE))
cedet_WISENT_CODE=$(patsubst %.wy,%-wy.elc,$(cedet_WISENT))

grammar-cedet: $(lispdir)/cedet/semantic/grammar-wy.elc $(cedet_BOVINE_CODE) $(cedet_WISENT_CODE)

### Dynamic rules

define PACKAGE_template
$(1)_LISP=$(shell $(FIND) $(lispdir)/$(1)/ -name \*.el)
$(1)_CODE=$$(patsubst %.el,%.elc,$$($(1)_LISP))
$(1)_TEXINFO=$(shell $(FIND) $(docdir) -name $(1).texi) $(shell test ! -d $(docdir)/$(1) || $(FIND) $(docdir)/$(1)/ -name *.texi)
$(1)_INFO=$$(patsubst %.texi,%.info,$$($(1)_TEXINFO))
$(1)_TEST_LISP=$(shell test ! -d $(testdir)/$(1)/ || $(FIND) $(testdir)/$(1)/ -name \*.el)
$(1)_TEST_CODE=$$(patsubst %.el,%.elc,$$($(1)_TEST_LISP))

compile-$(1): $$($(1)_PRE_CODE) $$($(1)_CODE)

doc-$(1): $$($(1)_INFO)

test-$(1): $$($(1)_TEST_CODE)

clean-$(1):
	$(RM) $(RMFLAGS) $$($(1)_CODE)
	$(RM) $(RMFLAGS) $$($(1)_INFO)
endef

$(eval $(call PACKAGE_template,common))
$(foreach pkg,$(PACKAGES),$(eval $(call PACKAGE_template,$(pkg))))


### Generic rules

%-wy.el: %.wy
	$(EEVAL) "(progn $(LISP_PATH) (require 'semantic/grammar) (require 'semantic/wisent/grammar) (find-file \"$<\") (or (and (semantic-grammar-create-package) (save-buffer)) (kill-emacs 1)))"

%-by.el: %.by
	$(EEVAL) "(progn $(LISP_PATH) (require 'semantic/grammar) (require 'semantic/bovine/grammar) (find-file \"$<\") (or (and (semantic-grammar-create-package) (save-buffer)) (kill-emacs 1)))"

%.elc: %.el
	$(EEVAL) "(progn $(LISP_PATH) (or (byte-compile-file \"$<\") (kill-emacs 1)))"

%.info: %.texi
	$(MAKEINFO) $< -o $@
