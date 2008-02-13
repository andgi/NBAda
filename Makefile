#------------------------------------------------------------------------------
#-  NBAda - A library of non-blocking algorithms and data structures.
#-
#-  Copyright (C) 2007 - 2008  Anders Gidenstam
#-
#-  This program is free software; you can redistribute it and/or modify
#-  it under the terms of the GNU General Public License as published by
#-  the Free Software Foundation; either version 2 of the License, or
#-  (at your option) any later version.
#-
#-  This program is distributed in the hope that it will be useful,
#-  but WITHOUT ANY WARRANTY; without even the implied warranty of
#-  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#-  GNU General Public License for more details.
#-
#-  You should have received a copy of the GNU General Public License
#-  along with this program; if not, write to the Free Software
#-  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
#-
#------------------------------------------------------------------------------
# $Id: Makefile,v 1.2 2008/02/13 10:23:57 andersg Exp $
#------------------------------------------------------------------------------

DIST_TAG=RELEASE_0_1_0
DIST_VERSION=0.1.0

DIST_ADTs = src/Atomic_1-Writer_N-Reader_Register \
	    src/Atomic_Multi-Writer_Snapshot \
	    src/Lock-Free_LL_SC \
	    src/Lock-Free_Stack \
	    src/Lock-Free_Bounded_Queue \
	    src/Lock-Free_Queue \
	    src/Lock-Free_Queue_2 \
	    src/Lock-Free_Deque \
	    src/Lock-Free_Priority_Queue

DIST_MRs =  src/Lock-Free_Storage_Pools \
	    src/Epoch-Based_Memory_Reclamation \
	    src/Hazard_Pointers \
	    src/Pass_The_Buck \
	    src/Lock-Free_Reference_Counting \
	    src/Lock-Free_Memory_Reclamation \
	    src/debug

DIST_BASE = src/common \
	    src/Primitives \
	    src/benchmarks

DIST_UTIL = src/util

DIST_benchmarks = src/benchmarks

DIST_DOCs = docs/reference_manual.pdf

DIST_COMMON = COPYING README \
              $(DIST_BASE) $(DIST_UTIL) $(DIST_MRs) $(DIST_ADTs) \
              $(DIST_benchmarks) $(DIST_DOCs)

# This target makes an archive containing everything tagged for the specified
# release.
dist_archive: docs
	(cd /tmp/; \
	 rm -rf /tmp/NBAda; \
	 cvs -d /home/andersg/cvsroot export -d NBAda -r $(DIST_TAG) Ada/Non-Blocking/NBAda;\
	)
	cp $(DIST_DOCs) /tmp/NBAda/
	(cd /tmp/; \
	 tar zcf NBAda-$(DIST_VERSION).tar.gz \
           --exclude=CVS --exclude='*~' --exclude='*.o' --exclude='*.ali'\
           NBAda;\
	)

dist: docs
	tar zcf NBAda-$(DIST_VERSION).tar.gz \
          --exclude=CVS --exclude='*~' --exclude='*.o' --exclude='*.ali'\
          $(DIST_COMMON)

docs: docs/reference_manual.tex docs/references.bib
	cd docs; latex reference_manual; bibtex reference_manual; \
	latex reference_manual; latex reference_manual; dvipdf reference_manual
