# Copyright 1999-2026 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Mon Feb 09 11:45:03 2026 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2026 Madhu.  All Rights Reserved.
#
# ;madhu 260209 6.22.0 (alt Qt6)

EAPI=8

ECM_DESIGNERPLUGIN="true"
QTVER=6.8.1
inherit ecm frameworks.kde.org

DESCRIPTION="Framework for icon theming and configuration"

LICENSE="LGPL-2+"
KEYWORDS="~amd64 ~arm64 ~loong ~ppc64 ~riscv ~x86"
IUSE=""

RESTRICT="test" # bug 574770

# slot op: Uses Qt6::GuiPrivate for qiconloader_p.h, qguiapplication_p.h
RDEPEND="
	dev-qt/qtbase[dbus,gui,widgets]
	dev-qt/qtdeclarative
	dev-qt/qtsvg
	kde-frameworks/breeze-icons
	kde-frameworks/karchive
	kde-frameworks/kcolorscheme
	kde-frameworks/kconfig
	kde-frameworks/ki18n
	kde-frameworks/kwidgetsaddons
"
DEPEND="${RDEPEND}"

src_prepare() {
	ecm_src_prepare
	find po -type f -exec rm -fv '{}' ';'
}

src_configure() {
	CMAKE_PREFIX_PATH=/opt/Qt${QTVER} \
	ecm_src_configure
}
