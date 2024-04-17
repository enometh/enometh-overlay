# Copyright 1999-2024 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Tue Dec 07 07:40:57 2021 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2024 Madhu.  All Rights Reserved.
#
# ;madhu 240417 3.9.1-r2
# ;madhu 240417 3.9.5 - patch quote in gci_convert.h, force MAKEOPTS=-j1

EAPI=8

inherit autotools

DESCRIPTION="Portable Rexx interpreter"
HOMEPAGE="https://regina-rexx.sourceforge.io/"
# ;madhu 240417 gentoo'sis a cruel joke
SRC_URI="http://liquidtelecom.dl.sourceforge.net/project/regina-rexx/regina-rexx/3.9.5/regina-rexx-3.9.5.tar.gz"
#SRC_URI="mirror://sourceforge/${PN}/Regina-REXX-${PV}.tar.gz"
S="${WORKDIR}/regina-rexx-${PV}"

LICENSE="LGPL-2.1 MPL-1.0"
SLOT="0"
KEYWORDS="~amd64 ~ppc ~x86"

RDEPEND="virtual/libcrypt:="
DEPEND="${RDEPEND}"

PATCHES=(
	${FILESDIR}/regina-rexx-3.9.5-gci_convert.linux.-86_64-powerpc64-fix-stray-quo.patch
)

#src_prepare() {
#	default
# ;madhu 240417 use tc-export CC to force gcc, makefile uses CLANG if installed.
#	tc-export CC
#
# ;madhu 2404173.9.1 makefile patch changes MH_PROG_CC to AC_PROG_CC before renaming configure.in to configure.ac
#	mv configure.{in,ac} || die
#	eautoconf
#}

src_compile() {
	emake -j1
}

src_install() {
	emake -j1 DESTDIR="${D}" install
	DOCS=( BUGS HACKERS.txt README.Unix README_SAFE TODO README* doc/reg{util,ina}.pdf )
	einstalldocs

	newinitd "${FILESDIR}"/rxstack-r1 rxstack
}

pkg_postinst() {
	elog "You may want to run"
	elog
	elog "\trc-update add rxstack default"
	elog
	elog "to enable Rexx queues (optional)."
}
