# Copyright 1999-2023 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Sun Sep 26 08:13:17 2021 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2021 Madhu.  All Rights Reserved.
#
# ;madhu 210926 3.20210903.1 - use git
# ;madhu 230830 3.20211022.1 v3.20211022.1-14-g7b63eb4

EAPI=8

USE_GIT=true
DOCS_BUILDER="doxygen"

inherit docs toolchain-funcs

DESCRIPTION="Simulate keyboard input and mouse activity, move and resize windows"
HOMEPAGE="https://www.semicomplete.com/projects/xdotool/"
if ${USE_GIT}; then
	inherit git-r3
	EGIT_REPO_URI=https://github.com/jordansissel/xdotool
	EGIT_BRANCH=master
else

SRC_URI="https://github.com/jordansissel/xdotool/releases/download/v${PV}/${P}.tar.gz"
fi

LICENSE="BSD"
SLOT="0"
KEYWORDS="amd64 ~arm ~arm64 ppc ppc64 x86"
IUSE="examples"

# Many the tests want to manually start Xvfb regardless of whether there
# is an X server running or not (i.e. does not play nicely with virtualx),
# some tests require x11-wm/openbox, some try to run a complete Gnome
# session. All of them require a Ruby interpreter with dev-ruby/minitest
# installed. In short, supporting tests here will need MUCH work.
RESTRICT="test"

RDEPEND="x11-libs/libX11
	x11-libs/libXi
	x11-libs/libXinerama
	x11-libs/libXtst
	x11-libs/libxkbcommon"
DEPEND="${RDEPEND}"
BDEPEND="virtual/pkgconfig
	x11-base/xorg-proto"

PATCHES=(
	"${FILESDIR}"/${PN}-3.20210804.2-no_hardcoded_pkg-config.patch
	"${FILESDIR}"/${PN}-3.20210804.2-no_ldconfig.patch
)

DOCS=( CHANGELIST README.md )

src_compile() {
	tc-export CC LD PKG_CONFIG
	emake PREFIX="${EPREFIX}/usr"
	use doc && docs_compile
}

src_install() {
	emake PREFIX="${ED}/usr" INSTALLMAN="${ED}/usr/share/man" INSTALLLIB="${ED}/usr/$(get_libdir)" install

	use examples && DOCS+=( examples )
	einstalldocs
}
