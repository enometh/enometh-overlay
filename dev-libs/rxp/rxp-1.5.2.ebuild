# Copyright 1999-2026 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Tue Apr 14 14:49:33 2026 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2026 Madhu.  All Rights Reserved.
#
# ;madhu 260414 initial 1.5.2, put it in dev-libs because it is required for ltxml2, but it also installs usr/bin/rxp

EAPI=8

inherit autotools

DESCRIPTION="RXP is a validating XML parser written in C."
HOMEPAGE="https://www.cogsci.ed.ac.uk/~richard/rxp.html
https://www.inf.ed.ac.uk/research/isddarch/admin/package-view-200?view=1&id=200"

SRC_URI="https://www.inf.ed.ac.uk/research/isddarch/admin/rxp-1.5.2.tar.gz"

DOCS=( doc/Manual doc/Threads doc/COPYRIGHT )

LICENSE="GPL-2"
SLOT="0"
KEYWORDS="~amd64 ~x86"

src_install() {
	emake DESTDIR="${D}" install || die "make install failed"
	find "${ED}" -name '*.la' -delete || die
	einstalldocs
}
