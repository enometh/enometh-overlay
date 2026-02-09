# Copyright 1999-2026 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Mon Feb 09 14:21:59 2026 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2026 Madhu.  All Rights Reserved.
#
# ;madhu 260209 6.22.0 alt Qt
EAPI=8

QTVER=6.8.1
inherit ecm frameworks.kde.org

DESCRIPTION="Framework to handle global shortcuts"

LICENSE="LGPL-2+"
KEYWORDS="~amd64 ~arm64 ~loong ~ppc64 ~riscv ~x86"
IUSE="X"

# slot op: WITH_X11 uses Qt6::GuiPrivate for qtx11extras_p.h
RDEPEND="
	dev-qt/qtbase:6[dbus,gui,widgets]
	X? ( dev-qt/qtbase:6=[X] )
"
DEPEND="${RDEPEND}"
BDEPEND="dev-qt/qttools:6[linguist]"

src_prepare() {
	ecm_src_prepare
	find poqm -type f -exec rm -fv '{}' ';'
}

src_configure() {
	local mycmakeargs=(
		-DWITH_X11=$(usex X)
	)
	CMAKE_PREFIX_PATH=/opt/Qt${QTVER} \
	ecm_src_configure
}
