# Copyright 1999-2026 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Mon Feb 09 14:53:21 2026 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2026 Madhu.  All Rights Reserved.
#
# ;madhu 260209 6.22 alt qt

EAPI=8

ECM_TEST="forceoptional"
QTVER=6.8.1
inherit ecm frameworks.kde.org

DESCRIPTION="Framework for intercepting and handling application crashes"

LICENSE="LGPL-2+"
KEYWORDS="~amd64 ~arm64 ~loong ~ppc64 ~riscv ~x86"
IUSE="X"

# requires running Plasma environment
RESTRICT="test"

RDEPEND="
	dev-qt/qtbase:6[gui]
	kde-frameworks/kcoreaddons:6
	X? ( x11-libs/libX11 )
"
DEPEND="${RDEPEND}
	X? ( x11-base/xorg-proto )
	test? ( dev-qt/qtbase:6[widgets] )
"
BDEPEND="dev-qt/qttools:6[linguist]"

src_configure() {
	local mycmakeargs=(
		-DWITH_X11=$(usex X)
	)
	CMAKE_PREFIX_PATH=/opt/Qt${QTVER} \
	ecm_src_configure
}
