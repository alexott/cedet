# Copyright (C) 2010 by Lluís Vilanova
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
PACKAGES=eieio

###### Internal part of the Makefile ###########################################

### Paths
lispdir=$(CURDIR)/lisp
docdir=$(CURDIR)/doc
testdir=$(CURDIR)/tests


### Helpers
EEVAL=$(EMACS) $(EMACSFLAGS) --eval
LISP_PATH=(add-to-list 'load-path nil) \
$(foreach pkg,$(PACKAGES),(add-to-list 'load-path \"$(lispdir)/$(pkg)/\"))


### Top-level rules

all: compile doc

compile: $(patsubst %,compile-%,$(PACKAGES))

doc: $(patsubst %,doc-%,$(PACKAGES))

test: $(patsubst %,test-%,$(PACKAGES))

clean: $(patsubst %,clean-%,$(PACKAGES))


### Dynamic rules

define PACKAGE_template
$(1)_LISP=$(shell $(FIND) $(lispdir)/$(1)/ -name \*.el)
$(1)_CODE=$$(patsubst %.el,%.elc,$$($(1)_LISP))
$(1)_TEXINFO=$(shell $(FIND) $(docdir) -name $(1).texi -or -path $(1)/*.texi)
$(1)_INFO=$$(patsubst %.texi,%.info,$$($(1)_TEXINFO))
$(1)_TEST_LISP=$(shell $(FIND) $(testdir)/$(1)/ -name \*.el)
$(1)_TEST_CODE=$$(patsubst %.el,%.elc,$$($(1)_TEST_LISP))

compile-$(1): $$($(1)_CODE)

doc-$(1): $$($(1)_INFO)

test-$(1): $$($(1)_TEST_CODE)

clean-$(1):
	$(RM) $(RMFLAGS) $$($(1)_CODE)
	$(RM) $(RMFLAGS) $$($(1)_INFO)
endef

$(foreach pkg,$(PACKAGES),$(eval $(call PACKAGE_template,$(pkg))))


### Generic rules

%.elc: %.el
	$(EEVAL) "(progn $(LISP_PATH) (or (byte-compile-file \"$<\") (kill-emacs 1)))"

%.info: %.texinfo
	$(MAKEINFO) $<
