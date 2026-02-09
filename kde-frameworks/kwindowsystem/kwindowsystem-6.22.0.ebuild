# Copyright 1999-2026 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Mon Feb 09 14:57:17 2026 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2026 Madhu.  All Rights Reserved.
#
# ;madhu 260209 6.22.0 alt qt (if thare is a symlink to /usr/share/ECM in /usr/local/share, then there needs to be a similar symlink in /usr/local to /usr/local/share/plasma-wayland-protocols), remove qtdeclarative dep

EAPI=8

QTVER=6.8.1
inherit ecm frameworks.kde.org

DESCRIPTION="Framework providing access to properties and features of the window manager"

LICENSE="|| ( LGPL-2.1 LGPL-3 ) MIT"
KEYWORDS="~amd64 ~arm64 ~loong ~ppc64 ~riscv ~x86"
IUSE="wayland X"

RESTRICT="test"

# slot op: Uses Qt6::GuiPrivate for qtx11extras_p.h
# slot op: Uses private/qwayland*_p.h headers
# x11-base/xorg-proto: X11/Xlib.h included in public header kkeyserver.h,
#   req. by KF6WindowSystemConfig.cmake; see also bug #939177
RDEPEND="
	dev-qt/qtbase:6[gui]
	wayland? ( dev-qt/qtbase:6[wayland] )
	X? (
		dev-qt/qtbase:6[gui,X]
		x11-base/xorg-proto
		x11-libs/libX11
		x11-libs/libXfixes
		x11-libs/libxcb
		x11-libs/xcb-util-keysyms
	)
"
DEPEND="${RDEPEND}
	test? ( dev-qt/qtbase:6[widgets] )
	wayland? (
		dev-libs/plasma-wayland-protocols
		>=dev-libs/wayland-protocols-1.21
	)
"
BDEPEND="
	dev-qt/qttools:6[linguist]
	wayland? (
		dev-qt/qtbase:6[wayland]
		dev-util/wayland-scanner
	)
"

DOCS=( docs/README.kstartupinfo )

src_prepare() {
	ecm_src_prepare
	find poqm -type f -exec rm -fv '{}' ';'
}

src_configure() {
	local mycmakeargs=(
		-DKWINDOWSYSTEM_WAYLAND=$(usex wayland)
		-DKWINDOWSYSTEM_X11=$(usex X)
	)

	CMAKE_PREFIX_PATH=/opt/Qt${QTVER} \
	ecm_src_configure
}
