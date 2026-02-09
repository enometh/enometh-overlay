# Copyright 1999-2026 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Mon Feb 09 09:43:06 2026 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2026 Madhu.  All Rights Reserved.
#
# ;madhu 260209 6.22.1 (Alt Qt)

EAPI=8

ECM_PYTHON_BINDINGS="off"
QTVER=6.8.1
inherit ecm frameworks.kde.org xdg

DESCRIPTION="Framework providing assorted high-level user interface components"

LICENSE="LGPL-2+"
KEYWORDS="~amd64 ~arm64 ~loong ~ppc64 ~riscv ~x86"
IUSE="dbus wayland X"

# slot op: includes qpa/qplatformnativeinterface.h
COMMON_DEPEND="
	dev-qt/qtbase[dbus?,gui]
	dev-qt/qtdeclarative
	wayland? (
		dev-libs/wayland
		dev-qt/qtbase[wayland]
	)
	X? (
		dev-qt/qtbase[X]
		x11-libs/libX11
	)
"
DEPEND="${COMMON_DEPEND}
	wayland? (
		>=dev-libs/plasma-wayland-protocols-1.15.0
		>=dev-libs/wayland-protocols-1.39
	)
	X? (
		x11-base/xorg-proto
		x11-libs/libxcb
	)
"
RDEPEND="${COMMON_DEPEND}
	!<kde-frameworks/kguiaddons-5.116.0-r2:5[-kf6compat(-)]
"
BDEPEND="
	wayland? (
		dev-qt/qtbase[wayland]
		dev-util/wayland-scanner
	)
"

src_configure() {
	local mycmakeargs=(
		-DBUILD_GEO_SCHEME_HANDLER=ON
		-DUSE_DBUS=$(usex dbus)
		-DWITH_WAYLAND=$(usex wayland)
		-DWITH_X11=$(usex X)
	)
	CMAKE_PREFIX_PATH=/opt/Qt${QTVER} \
	ecm_src_configure
}
