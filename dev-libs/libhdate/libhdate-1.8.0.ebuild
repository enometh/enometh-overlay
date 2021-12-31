# Copyright 1999-2022 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Thu Dec 30 14:23:26 2021 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2021 Madhu.  All Rights Reserved.
#
# ;madhu 211230 libhdate-1.8.0 (UNRELEASED). a6105614. This should be
# ;multilib and python-any-r1 but portage is not designed to handle the
# ;explosion.


EAPI=8

PYTHON_COMPAT=( python3_{8..10} )
USE_GIT=true
MY_COMMIT=a610561479b37cae512d528c9583771788fa7223

inherit autotools python-single-r1 perl-functions

DESCRIPTION="C library for Hebrew calendar information"
HOMEPAGE="https://sourceforge.net/p/libhdate/wiki/Home/"

if ${USE_GIT}; then
	inherit git-r3
	EGIT_REPO_URI="https://git.code.sf.net/p/libhdate/git"
	if [ -n "${MY_COMMIT}" ]; then
		EGIT_COMMIT="${MY_COMMIT}"
	fi
	EGIT_CLONE_TYPE=shallow
else
	SRC_URI="mirror://sourceforge/grift/glrr/${P}.tar.bz2"
fi

KEYWORDS="~amd64 ~x86"
LICENSE=GPL-3
SLOT="0"
IUSE="nls perl php python tools"

RDEPEND="
	python? ( ${PYTHON_DEPS} dev-lang/swig:=  )
	perl? ( dev-lang/perl:=  dev-lang/swig:= )
	php? ( dev-lang/php:5 dev-lang/swig:= )
	virtual/pkgconfig
"

# XXX "php? (dev-lang/php:= dev-lang/swig:=)"

DEPENDS="${RDEPENDS}"

PATCHES=(
${FILESDIR}/0001-Makefile.am-use-docdir-from-autoconf-instead-of-usr-.patch
${FILESDIR}/0002-configure.ac-fix-logic-errors-in-AC_ARG_ENABLE-claus.patch
${FILESDIR}/0003-configure.ac-python3-print-syntax.patch
${FILESDIR}/0004-examples-hcal-timezone_functions.c-fix-incorrect-ssc.patch
${FILESDIR}/0005-fix-incorrect-latitute-longitude-parsing-of-zone.tab.patch
${FILESDIR}/0006-src-zdump3-avoid-derefencing-NULL.-e.g.-on-Asia-Kolk.patch
${FILESDIR}/0007-autotools-don-t-force-CFLAGS.patch
${FILESDIR}/0008-zdump-ship-zdump3.h-and-fix-manpage.patch
)

src_prepare() {
	default
	eautoreconf
	# FIXME - autoreconf -i
	touch INSTALL
	ln -sv LICENSE COPYING
#	multilib_copy_sources
}

src_configure() {
	myconf=(
		$(use_enable nls)
		$(use_enable php)		# untested, only php5
		$(use_enable tools hcal)
		--disable-fpc			# free pascal?
		--disable-gpc			# gnu pascal?
		--disable-rpath
	)

	if use python; then
		myconf+=(
			--enable-python
			--with-python-sitelib-dir="$(python_get_sitedir)"
		)
	fi
	if use perl; then
		perl_set_version
		myconf+=(
					--enable-perl
					--with-perl-sitelib-dir="${VENDOR_ARCH}"
		)
	fi
	econf "${myconf[@]}"
}

src_install() {
	default
	find "${D}" -name '*.la' -delete || die
	docompress -x /usr/share/doc/${PF}/examples
}
