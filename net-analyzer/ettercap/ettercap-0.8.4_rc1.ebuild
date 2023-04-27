# Copyright 1999-2023 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <2023-04-27 09:53:21 IST>
#   Touched: Wed Feb 05 01:46:54 2020 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2020-2023 Madhu.  All Rights Reserved.
#
# ;madhu 200205 0.8.3->0.8.3-r1 - actually use git  + gtk2
# ;madhu 200206 gtk3 again but patch FindGTK3.cmake
# ;madhu 200608 9999-r1 - patch git directly, maxminddb
# ;madhu 230427 0.8.4_rc1 v0.8.0-3163-gf03a1174 (gtk3 only, USE=geoip to
#               use maxminddb) USE="lua lua_targets_luajit to enable lua.

EAPI=8
USE_GIT=true
LUA_COMPAT=( luajit )

inherit cmake lua-single

DESCRIPTION="Suite for man in the middle attacks"
HOMEPAGE="https://github.com/Ettercap/ettercap"

LICENSE="GPL-2+"
SLOT="0"

if ${USE_GIT} || [[ ${PV} == 9999 ]] ; then
	inherit git-r3
	EGIT_REPO_URI="https://github.com/Ettercap/${PN}.git"
	EGIT_COMMIT=${MY_COMMIT}
else
	SRC_URI="https://github.com/Ettercap/${PN}/archive/v${PV}.tar.gz -> ${P}.tar.gz"
fi

KEYWORDS="~alpha amd64 arm ppc ppc64 sparc x86"

IUSE="doc geoip gtk ipv6 ncurses +plugins test lua"
RESTRICT="!test? ( test )"

REQUIRDE_USE="
	lua? ( ${LUA_REQUIRED_USE} )
"

RDEPEND="
	dev-libs/libbsd
	dev-libs/libpcre
	dev-libs/openssl:=
	net-libs/libnet:1.1
	>=net-libs/libpcap-0.8.1
	sys-libs/zlib
	geoip? ( dev-libs/libmaxminddb )
	gtk? (
		>=app-accessibility/at-spi2-core-2.46.0
		>=dev-libs/glib-2.2.2:2
		media-libs/freetype
		x11-libs/cairo
		x11-libs/gdk-pixbuf:2
		>=x11-libs/gtk+-3.22:3
		>=x11-libs/pango-1.2.3
	)
	ncurses? ( >=sys-libs/ncurses-5.3:= )
	plugins? ( >=net-misc/curl-7.26.0 )
	lua? ( ${LUA_DEPS} )
"
DEPEND="
	app-alternatives/yacc
	sys-devel/flex
	lua? ( ${LUA_DEPS} )
"
BDEPEND="
	doc? (
		app-text/ghostscript-gpl
		sys-apps/groff
	)
	test? ( dev-libs/check )
"

PATCHES=(
	$FILESDIR/ettercap-0.8.3-cmake-Modules-FindGTK3.cmake.patch
)

src_prepare() {
	sed -i "s:Release:Release Gentoo:" CMakeLists.txt || die
	cmake_src_prepare
}

src_configure() {
	local mycmakeargs=(
		-DENABLE_CURSES="$(usex ncurses)"
		-DENABLE_GTK="$(usex gtk)"
		-DENABLE_PLUGINS="$(usex plugins)"
		-DENABLE_IPV6="$(usex ipv6)"
		-DENABLE_TESTS="$(usex test)"
		-DENABLE_PDF_DOCS="$(usex doc)"
		-DENABLE_GEOIP="$(usex geoip)"
		-DENABLE_LUA="$(usex lua)"
		-DBUNDLED_LIBS=OFF
		-DSYSTEM_LIBS=ON
		-DINSTALL_SYSCONFDIR="${EPREFIX}"/etc
	)
	! use gtk && mycmakeargs+=(-DINSTALL_DESKTOP=OFF)

	#;madhu 230427 USE="lua lua_targets_luajit" QA Notice: Unresolved
	# soname dependencies: libec_lua.so in both the executable and the
	# shared library
	use lua && mycmakeargs+=(-DBUILD_SHARED_LIBS=OFF)

	cmake_src_configure
}
