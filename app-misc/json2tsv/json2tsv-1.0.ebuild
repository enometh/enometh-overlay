# Copyright 2022 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Thu Sep 15 23:46:53 2022 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2022 Madhu.  All Rights Reserved.
#
# ;madhu 220916 1.0		GIT ONLY

EAPI=8

MY_COMMIT=1f9c82d7086b221cb7a16cb41c3f870f8b08a552

inherit git-r3

DESCRIPTION="Convert JSON to TSV or separated output."
HOMEPAGE="https://codemadness.org/json2tsv.html"
EGIT_REPO_URI="git://git.codemadness.org/json2tsv"
EGIT_COMMIT=${MY_COMMIT}

LICENSE="ISC"
SLOT="0"
KEYWORDS="x86 amd64"

RDEPEND=""
DEPEND="${RDEPEND}"

src_install() {
	emake \
		DESTDIR="${D}" \
		PREFIX="${EPREFIX}/usr" \
		MANPREFIX="${EPREFIX}/usr/share/man" \
		DOCPREFIX="${EPREFIX}/usr/share/doc/${PF}" \
		install
}
