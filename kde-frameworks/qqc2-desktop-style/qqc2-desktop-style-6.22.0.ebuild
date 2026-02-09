# Copyright 1999-2026 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Mon Feb 09 12:51:23 2026 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2026 Madhu.  All Rights Reserved.
#
# ;madhu 260209 6.22.0 alt Qt

EAPI=8

ECM_QTHELP="false"
QTVER=6.8.1
inherit ecm frameworks.kde.org

DESCRIPTION="Style for QtQuickControls 2 that uses QWidget's QStyle for painting"

LICENSE="|| ( GPL-2+ LGPL-3+ )"
KEYWORDS="~amd64 ~arm64 ~loong ~ppc64 ~riscv ~x86"
IUSE=""

# Qt_6_PRIVATE_API matches org.kde.desktop.so, see also:
# https://invent.kde.org/frameworks/qqc2-desktop-style/-/merge_requests/379
DEPEND="
	dev-qt/qtbase[dbus,gui,widgets]
	dev-qt/qtdeclarative
	kde-frameworks/kcolorscheme
	kde-frameworks/kconfig
	kde-frameworks/kiconthemes
	kde-frameworks/kirigami
	kde-frameworks/sonnet
"
RDEPEND="${DEPEND}
	dev-qt/qt5compat
"
BDEPEND="dev-qt/qttools[linguist]"

src_prepare() {
	ecm_src_prepare
	find poqm -type f -exec rm -fv '{}' ';'
}

src_configure() {
	CMAKE_PREFIX_PATH=/opt/Qt${QTVER} \
	ecm_src_configure
}
