# Copyright 1999-2026 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Mon Feb 09 14:44:13 2026 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2026 Madhu.  All Rights Reserved.
#
# ;madhu 260209 6.22.0 alt qt - drop qtdeclarative dep

EAPI=8

QTVER=6.8.1
inherit ecm frameworks.kde.org optfeature

DESCRIPTION="Provider for platform independent hardware discovery, abstraction and management"

LICENSE="LGPL-2.1+"
KEYWORDS="~amd64 ~arm64 ~loong ~ppc64 ~riscv ~x86"
IUSE="ios"

RDEPEND="
	dev-qt/qtbase:6[dbus,gui,xml]
	sys-apps/util-linux
	sys-fs/udisks:2
	virtual/libudev:=
	ios? (
		app-pda/libimobiledevice:=
		app-pda/libplist:=
	)
"
DEPEND="${RDEPEND}
	test? ( dev-qt/qtbase:6[concurrent] )
"
BDEPEND="
	app-alternatives/lex
	app-alternatives/yacc
	dev-qt/qttools:6[linguist]
"

src_prepare() {
	ecm_src_prepare
	find poqm -type f -exec rm -fv '{}' ';'
}

src_configure() {
	local mycmakeargs=(
		$(cmake_use_find_package ios IMobileDevice)
		$(cmake_use_find_package ios PList)
	)
	CMAKE_PREFIX_PATH=/opt/Qt${QTVER} \
	ecm_src_configure
}

pkg_postinst() {
	if [[ -z "${REPLACING_VERSIONS}" ]]; then
		optfeature "media player devices support" app-misc/media-player-info
	fi
}
