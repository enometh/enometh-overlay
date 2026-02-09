# Copyright 1999-2026 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Mon Feb 09 17:18:24 2026 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2026 Madhu.  All Rights Reserved.
#
# ;madhu 260209 6.22.0 alt qt

EAPI=8

ECM_DESIGNERPLUGIN="true"
QTVER=6.8.1
inherit ecm frameworks.kde.org

DESCRIPTION="Framework for common completion tasks such as filename or URL completion"

LICENSE="LGPL-2+"
KEYWORDS="~amd64 ~arm64 ~loong ~ppc64 ~riscv ~x86"
IUSE=""

DEPEND="
	dev-qt/qtbase:6[gui]
	kde-frameworks/kcodecs:6
	kde-frameworks/kconfig:6
	kde-frameworks/kcoreaddons:6
	kde-frameworks/kwidgetsaddons:6
"
RDEPEND="${DEPEND}"
BDEPEND="dev-qt/qttools:6[linguist]"

src_prepare() {
	ecm_src_prepare
	find poqm -type f -exec rm -fv '{}' ';'
}

src_configure() {
	CMAKE_PREFIX_PATH=/opt/Qt${QTVER} \
	ecm_src_configure
}
