# Copyright 1999-2026 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Mon Feb 09 15:20:27 2026 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2026 Madhu.  All Rights Reserved.
#
# ;madhu 260209 6.22.0 alt qt

EAPI=8

QTVER=6.8.1
VIRTUALDBUS_TEST="true"
inherit ecm frameworks.kde.org

DESCRIPTION="Framework for registering services and applications per freedesktop standards"

LICENSE="LGPL-2+"
KEYWORDS="~amd64 ~arm64 ~loong ~ppc64 ~riscv ~x86"
IUSE="X"

# slot op: Uses Qt6::GuiPrivate for qtx11extras_p.h
DEPEND="
	dev-qt/qtbase:6[dbus]
	X? ( dev-qt/qtbase:6=[gui,X] )
"
RDEPEND="${DEPEND}"
BDEPEND="dev-qt/qttools:6[linguist]"

src_configure() {
	local mycmakeargs=(
		-DWITH_X11=$(usex X)
	)
	CMAKE_PREFIX_PATH=/opt/Qt${QTVER} \
	ecm_src_configure
}
