# Toplevel Makefile
#
# (C) 2011 CEDET Developers
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

PROJECTS=lisp lisp/cedet lisp/eieio lisp/speedbar lisp/cedet/cogre lisp/cedet/semantic \
lisp/cedet/ede lisp/cedet/srecode lisp/cedet/semantic/bovine lisp/cedet/semantic/wisent \
lisp/cedet/semantic/analyze lisp/cedet/semantic/decorate lisp/cedet/semantic/ectags \
lisp/cedet/semantic/symref

PROJECTS_AUTOLOADS=lisp lisp/cedet lisp/eieio lisp/speedbar lisp/cedet/cogre lisp/cedet/semantic \
lisp/cedet/ede lisp/cedet/srecode

EMACS=emacs
EMACSFLAGS=-batch --no-site-file -l cedet-remove-builtin.el
LOADDEFS=loaddefs.el
BOOTSTRAP=(progn (global-ede-mode) (find-file "$(CURDIR)/lisp/Project.ede") (ede-proj-regenerate))
UTEST=(progn (add-to-list (quote load-path) "$(CURDIR)/tests") (require (quote cedet-utests)) (semantic-mode))
RM=rm

all: autoloads makefiles compile

compile:
	$(MAKE) -C lisp

makefiles: $(addsuffix /Makefile,$(PROJECTS))
$(addsuffix /Makefile,$(PROJECTS)): $(addsuffix /Project.ede,$(PROJECTS))
	@echo "Creating Makefiles using EDE"
	$(EMACS) $(EMACSFLAGS) --eval '(setq cedet-bootstrap-in-progress t)' -l cedet-devel-load.el --eval '$(BOOTSTRAP)'

autoloads:
	$(foreach proj,$(PROJECTS_AUTOLOADS),cd $(CURDIR)/$(proj) && $(MAKE) autoloads;)

clean-autoloads:
	$(foreach proj,$(PROJECTS_AUTOLOADS),cd $(CURDIR)/$(proj) && if [ -f $(LOADDEFS) ];then $(RM) -f $(LOADDEFS);fi;)

clean-all: clean-autoloads
	$(foreach proj,$(PROJECTS),cd $(CURDIR)/$(proj) && $(MAKE) clean;)

utest: 
	$(EMACS) -Q -l cedet-devel-load.el --eval '$(UTEST)' -f cedet-utest

utest-batch:
	$(EMACS) $(EMACSFLAGS) -l cedet-devel-load.el --eval '$(UTEST)' -f cedet-utest-batch

itest: itest-make itest-automake

itest-make:
	cd $(CURDIR)/tests;./cit-test.sh Make

itest-automake:
	cd $(CURDIR)/tests;./cit-test.sh Automake

itest-android:
	cd $(CURDIR)/tests;./cit-test.sh Android

itest-batch: itest-make-batch itest-automake-batch

itest-make-batch:
	cd $(CURDIR)/tests;./cit-test.sh Make --batch

itest-automake-batch:
	cd $(CURDIR)/tests;./cit-test.sh Automake --batch

itest-android-batch:
	cd $(CURDIR)/tests;./cit-test.sh Android --batch
