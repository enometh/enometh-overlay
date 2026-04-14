# Copyright 1999-2026 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Tue Apr 14 15:06:07 2026 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2026 Madhu.  All Rights Reserved.
#
# ;madhu 260414 initial ltxml2-1.1.1

EAPI=8

inherit autotools

DESCRIPTION="XML Toolkit from the Edinburgh Language Technology Group"
HOMEPAGE="https://www.ltg.ed.ac.uk/software/ltxml2/
https://www.inf.ed.ac.uk/research/isddarch/admin/package-view-200?view=1&id=200"

SRC_URI="https://www.inf.ed.ac.uk/research/isddarch/admin/ltxml2-1.1.1.tar.gz"

LICENSE="GPL-2"
SLOT="0"
KEYWORDS="~amd64 ~x86"
IUSE="mysql postgresql"

RDEPEND="
	app-text/docbook-sgml
	dev-libs/rxp
	mysql? ( dev-deb/mysql-connector-c )
	postgresql? ( dev-db/postgresql:= )
"

DOCS=( COPYRIGHT TODO INSTALL )

src_configure() {
	econf \
		$(use_with mysql) \
		$(use_with postgresql)
}

src_install() {
	emake DESTDIR="${D}" install || die "make install failed"
	find "${ED}" -name '*.la' -delete || die
	einstalldocs
}
