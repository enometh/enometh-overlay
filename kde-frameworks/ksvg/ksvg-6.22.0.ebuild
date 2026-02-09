# Copyright 1999-2026 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Mon Feb 09 15:55:27 2026 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2026 Madhu.  All Rights Reserved.
#
# ;madhu 260209 6.22.0 alt qt

EAPI=8

QTVER=6.8.1
inherit ecm frameworks.kde.org

DESCRIPTION="Components for handling SVGs"

LICENSE="LGPL-2+"
KEYWORDS="~amd64 ~arm64 ~loong ~ppc64 ~riscv ~x86"
IUSE=""

DEPEND="
	dev-qt/qtbase:6[gui]
	dev-qt/qtdeclarative:6
	dev-qt/qtsvg:6
	kde-frameworks/karchive:6
	kde-frameworks/kcolorscheme:6
	kde-frameworks/kconfig:6
	kde-frameworks/kcoreaddons:6
	kde-frameworks/kguiaddons:6
	kde-frameworks/kirigami:6
"
RDEPEND="${DEPEND}"

src_prepare() {
	ecm_src_prepare
	find po -type f -exec rm -fv '{}' ';'
}

src_configure() {
	CMAKE_PREFIX_PATH=/opt/Qt${QTVER} \
	ecm_src_configure
}
