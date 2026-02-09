# Copyright 1999-2026 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Mon Feb 09 17:15:59 2026 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2026 Madhu.  All Rights Reserved.
#
# ;madhu 260209 6.22.0 alt qt

EAPI=8

ECM_DESIGNERPLUGIN="true"
QTVER=6.8.1
inherit ecm frameworks.kde.org

DESCRIPTION="Framework providing an assortment of widgets for displaying and editing text"

LICENSE="LGPL-2+ LGPL-2.1+"
KEYWORDS="~amd64 ~arm64 ~loong ~ppc64 ~riscv ~x86"
IUSE="speech"

DEPEND="
	dev-qt/qtbase:6[gui,widgets]
	kde-frameworks/kcompletion:6
	kde-frameworks/kconfig:6
	kde-frameworks/kcoreaddons:6
	kde-frameworks/ki18n:6
	kde-frameworks/kwidgetsaddons:6
	kde-frameworks/sonnet:6
	speech? ( dev-qt/qtspeech:6 )
"
RDEPEND="${DEPEND}"

src_prepare() {
	ecm_src_prepare
	find po -type f -exec rm -fv '{}' ';'
}

src_configure() {
	local mycmakeargs=(
		-DWITH_TEXT_TO_SPEECH=$(usex speech)
	)
	CMAKE_PREFIX_PATH=/opt/Qt${QTVER} \
	ecm_src_configure
}
