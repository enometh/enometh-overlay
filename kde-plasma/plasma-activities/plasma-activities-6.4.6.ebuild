# Copyright 1999-2026 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Mon Feb 09 16:26:33 2026 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2026 Madhu.  All Rights Reserved.
#
# ;madhu 260209 6.5.5 alt qt

EAPI=8

ECM_QTHELP="true"
ECM_TEST="true"
KFMIN=6.18.0
QTVER=6.8.1
inherit ecm plasma.kde.org

DESCRIPTION="Core components for KDE's Activities System"

LICENSE="|| ( LGPL-2.1 LGPL-3 )"
SLOT="6/7"
KEYWORDS="~amd64 ~arm64 ~loong ~ppc64 ~riscv ~x86"
IUSE=""

RDEPEND="
	dev-qt/qtbase:6[dbus,gui,sql,widgets]
	dev-qt/qtdeclarative:6[widgets]
	kde-frameworks/kconfig:6
	kde-frameworks/kcoreaddons:6
"
DEPEND="${RDEPEND}
	test? ( kde-frameworks/kwindowsystem:6[X] )
"

src_configure() {
	CMAKE_PREFIX_PATH=/opt/Qt${QTVER} \
	ecm_src_configure
}
