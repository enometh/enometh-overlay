# Copyright 2021-2026 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Mon Feb 09 13:04:55 2026 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2026 Madhu.  All Rights Reserved.
#
# ;madhu 260209 1.11.0 alt Qt

EAPI=8

# TODO: ECMGenerateQDoc
ECM_TEST="true"
KFMIN=6.16.0
QTVER=6.8.1
inherit ecm kde.org

if [[ ${KDE_BUILD_TYPE} = release ]]; then
	SRC_URI="mirror://kde/stable/${PN}/${P}.tar.xz"
	KEYWORDS="~amd64 ~arm64 ~loong ~ppc64 ~riscv ~x86"
fi

DESCRIPTION="Visual end user components for Kirigami-based applications"
HOMEPAGE="https://invent.kde.org/libraries/kirigami-addons"

LICENSE="|| ( GPL-2 GPL-3 LGPL-3 ) LGPL-2.1+"
SLOT="6"
IUSE=""

# would profit from VIRTUALX_REQUIRED=test, but then still requires
# org.qt-project.qt.mediaplayer service and fails, bug 911186
RESTRICT="test"

COMMON_DEPEND="
	dev-qt/qtbase[gui]
	dev-qt/qtdeclarative
	kde-frameworks/kcolorscheme
	kde-frameworks/kconfig
	kde-frameworks/kcoreaddons
	kde-frameworks/kcrash
	kde-frameworks/kglobalaccel
	kde-frameworks/kguiaddons
	kde-frameworks/ki18n
	kde-frameworks/kiconthemes
	kde-frameworks/kirigami
"
RDEPEND="${COMMON_DEPEND}
	dev-qt/qtmultimedia[qml]
	kde-frameworks/qqc2-desktop-style
	kde-plasma/libplasma:6
"
DEPEND="${COMMON_DEPEND}
	test? (
		dev-qt/qtmultimedia[qml]
		x11-themes/sound-theme-freedesktop
	)
"

src_prepare() {
	ecm_src_prepare
	find poqm -type f -exec rm -fv '{}' ';'
}

src_configure() {
	CMAKE_PREFIX_PATH=/opt/Qt${QTVER} \
	ecm_src_configure
}
