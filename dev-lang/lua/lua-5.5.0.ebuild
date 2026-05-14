# Copyright 1999-2026 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Thu May 14 08:26:39 2026 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2026 Madhu.  All Rights Reserved.
#
# ;madhu 260514 - 5.5.0, alternate to gentoo install under ALT_ROOT, no sharedlibs or relation to gentoo lua "infrastructure"

EAPI=8

inherit flag-o-matic libtool optfeature

ALT_PREFIX=/opt/hyprland

DESCRIPTION="A powerful light-weight programming language designed for extending applications"
HOMEPAGE="https://www.lua.org/"
SRC_URI="https://www.lua.org/ftp/lua-${PV}.tar.gz"

LICENSE="MIT"
SLOT="5.5"

KEYWORDS="amd64"
IUSE="+deprecated readline"

DEPEND="
	readline? ( sys-libs/readline:= )
	!dev-lang/lua:0"
RDEPEND="${DEPEND}"
BDEPEND="virtual/pkgconfig"

src_compile() {
	local MYCFLAGS="$CFLAGS -fPIC"
	local MYLDFLAGS="$LDFLAGS"
	local MYLIB=""
	use deprecated && MYCFLAGS="$MYCFLAGS -DLUA_COMPAT_4_3"
	if 	use readline; then
		MYCFLAGS="$MYCFLAGS -DLUA_USE_READLINE"
		MYLIBS="$MYLIBS -lreadline"
	fi
	emake CC="$(tc-getCC)" \
		  MYCFLAGS="$MYCFLAGS" \
		  MYLDFLAGS="$MYLDFLAGS" \
		  MYLIBS="$MYLIBS"
}

src_install() {
	emake INSTALL_TOP=${ED}/${ALT_PREFIX} install INSTALL_MAN=${ED}/${ALT_PREFIX}/share/man/man1  INSTALL_INC=${ED}/${ALT_PREFIX}/include/lua55 INSTALL_LIB=${ED}/${ALT_PREFIX}/$(get_libdir)/
	mkdir -pv  ${ED}/${ALT_PREFIX}/$(get_libdir)/pkgconfig
	mkdir -pv  ${ED}/${ALT_PREFIX}/share/doc/${PF}/
	cp -apiv ${FILESDIR}/lua55.pc ${ED}/${ALT_PREFIX}/$(get_libdir)/pkgconfig
	local HTML_DOCS=( doc/manual.html doc/manual.css doc/readme.html doc/index.css doc/contents.html doc/lua.css doc/OSIApproved.png doc/logo.png )
	cp -apiv ${HTML_DOCS[@]} ${ED}/${ALT_PREFIX}/share/doc/${PF}/
}
