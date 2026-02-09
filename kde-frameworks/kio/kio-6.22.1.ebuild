# Copyright 1999-2026 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Mon Feb 09 14:26:57 2026 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2026 Madhu.  All Rights Reserved.
#
# ;madhu 260209 6.22.1 alt qt KIOGUI_ONLY, BUILD_TESTING=off

EAPI=8
QTVER=6.8.1

ECM_DESIGNERPLUGIN="true"
ECM_HANDBOOK="optional"
ECM_HANDBOOK_DIR="docs"
ECM_TEST="forceoptional"
inherit ecm frameworks.kde.org xdg

DESCRIPTION="Framework providing transparent file and data management"

LICENSE="LGPL-2+"
KEYWORDS="~amd64 ~arm64 ~loong ~ppc64 ~riscv ~x86"
IUSE="acl +kwallet wayland X"

# tests hang
RESTRICT="test"

# slot op: Uses Qt6::GuiPrivate for qtx11extras_p.h

# GUI_ONLY

COMMON_DEPEND="
	dev-qt/qtbase:6[dbus,gui,network,ssl,widgets,X?]
	dev-qt/qtdeclarative:6
	kde-frameworks/kbookmarks:6
	kde-frameworks/kcodecs:6
	kde-frameworks/kcolorscheme:6
	kde-frameworks/kcompletion:6
	kde-frameworks/kconfig:6
	kde-frameworks/kcoreaddons:6
	kde-frameworks/kcrash:6
	kde-frameworks/kdbusaddons:6
	kde-frameworks/kguiaddons:6
	kde-frameworks/ki18n:6
	kde-frameworks/kiconthemes:6
	kde-frameworks/kitemviews:6
	kde-frameworks/kjobwidgets:6
	kde-frameworks/knotifications:6
	kde-frameworks/ktextwidgets:6
	kde-frameworks/kservice:6
	kde-frameworks/kwidgetsaddons:6
	kde-frameworks/kwindowsystem:6[wayland?,X?]
	kde-frameworks/solid:6
	sys-apps/util-linux
	acl? (
		sys-apps/attr
		virtual/acl
	)
	handbook? (
		dev-libs/libxml2:=
		dev-libs/libxslt
		kde-frameworks/karchive:6
		kde-frameworks/kdoctools:6
	)
	kwallet? ( kde-frameworks/kwallet:6 )
	X? ( dev-qt/qtbase:6=[gui] )
"
DEPEND="${COMMON_DEPEND}
	dev-qt/qtbase:6[concurrent]
"

#	sys-power/switcheroo-control
RDEPEND="${COMMON_DEPEND}
	dev-qt/qtbase:6[libproxy]
"

# bug 944812: File Properties is accessible from KFileWidget (KIO); this
# provides access to keditfiletype binary via KWidgetsAddons (Tier1)
# Typical KIO revdeps (dolphin, krusader et al.) can rely on this dep

# GUI_ONLY
#	kde-frameworks/kded:6
#	kde-plasma/keditfiletype
PDEPEND=""

src_configure() {
	local mycmakeargs=(
		$(cmake_use_find_package acl ACL)
		$(cmake_use_find_package kwallet KF6Wallet)
		-DWITH_WAYLAND=$(usex wayland)
		-DWITH_X11=$(usex X)
#		-DKIOGUI_ONLY=on
		-DBUILD_TESTING=off
	)
	CMAKE_PREFIX_PATH=/opt/Qt${QTVER} \
	ecm_src_configure
}
