# Copyright 1999-2022 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Thu Dec 30 21:36:13 2021 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2021 Madhu.  All Rights Reserved.
#
# ;madhu 211231 0.5.0 (056a92 2015-05-16) vala bindings not optional

EAPI=7

USE_GIT=true

MY_COMMIT="056a92cacd4082ead47a131464c46b0cf08d7690"

inherit autotools vala toolchain-funcs

DESCRIPTION="LibHdate Gobject and vala bindings"
HOMEPAGE="https://github.com/yaacov/libhdate-glib"
if ${USE_GIT}; then
	inherit git-r3
	EGIT_REPO_URI="https://github.com/yaacov/libhdate-glib"
	if [ -n "$MY_COMMIT" ]; then
		EGIT_COMMIT="$MY_COMMIT"
	fi
	EGIT_CLONE_TYPE="shallow"
else
	die SRC_URI="https://github.com/yaacov/libhdate-glib/${PN}/${P}.tar.gz"
fi

LICENSE="GPL-3"
SLOT="0"
KEYWORDS="~amd64 ~x86"
IUSE=""

#RDEPEND=">=dev-libs/libhdate-1.8.0"

RDEPEND="
	dev-libs/glib:2
	dev-libs/gobject-introspection:=
"

PATCHES=(
	$FILESDIR/libhdate-glib-0.5.0-Makefile.am-use-docdir-from-autoconf-instead-of-usr-.patch
)

RDEPEND="$RDEPEND"
BDEPEND="
	dev-util/glib-utils
	$(vala_depend)
	virtual/pkgconfig
"

src_prepare() {
	default
	eautoreconf
	vala_src_prepare
}

src_install() {
	default
	dodoc -r examples
	# upstream Makefile is bogus dud
	rm -fv ${ED}/usr/share/doc/${PF}/examples/Make*
	find "${D}" -name '*.la' -delete || die
	docompress -x /usr/share/doc/${PF}/examples
}