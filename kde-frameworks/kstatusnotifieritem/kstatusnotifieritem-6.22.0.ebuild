# Copyright 1999-2026 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Tue Apr 21 19:04:48 2026 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2026 Madhu.  All Rights Reserved.
#
# ;;madhu 260421

EAPI=8
QTVER=6.8.1

ECM_PYTHON_BINDINGS="off"
QTMIN=6.10.1
inherit ecm frameworks.kde.org

DESCRIPTION="Implementation of Status Notifier Items"

LICENSE="LGPL-2+"
KEYWORDS="~amd64 ~arm64 ~loong ~ppc64 ~riscv ~x86"
IUSE="X"

# slot op: Qt6::WidgetsPrivate use
RDEPEND="
	>=dev-qt/qtbase-${QTMIN}:6=[dbus,gui,widgets]
	=kde-frameworks/kwindowsystem-${KDE_CATV}*:6[X?]
"
DEPEND="${RDEPEND}"
BDEPEND=">=dev-qt/qttools-${QTMIN}:6[linguist]"

src_configure() {
	local mycmakeargs=(
		-DWITHOUT_X11=$(usex !X)
	)

	CMAKE_PREFIX_PATH=/opt/Qt${QTVER} \
	ecm_src_configure
}
